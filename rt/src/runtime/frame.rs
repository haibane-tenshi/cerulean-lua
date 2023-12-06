use std::hash::Hash;
use std::ops::Deref;
use std::rc::Rc;

use repr::chunk::{Chunk, ClosureRecipe};
use repr::debug_info::OpCodeDebugInfo;
use repr::index::{ConstId, FunctionId, InstrId, RecipeId, StackSlot, UpvalueSlot};
use repr::literal::Literal;
use repr::opcode::{AriBinOp, BinOp, BitBinOp, OpCode, RelBinOp, StrBinOp, UnaOp};
use repr::tivec::{TiSlice, TiVec};

use super::stack::UpvalueId;
use super::stack::{RawStackSlot, StackView};
use super::upvalue_stack::{ProtectedSize as RawUpvalueSlot, UpvalueView};
use super::RuntimeView;
use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::error::opcode as opcode_err;
use crate::error::RuntimeError;
use crate::value::callable::Callable;
use crate::value::Value;

pub type ControlFlow = std::ops::ControlFlow<ChangeFrame>;

pub enum ChangeFrame {
    Return(StackSlot),
    Invoke(StackSlot),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct FunctionPtr {
    pub chunk_id: ChunkId,
    pub function_id: FunctionId,
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub(super) fn_ptr: FunctionPtr,
    pub(super) upvalues: TiVec<UpvalueSlot, UpvalueId>,
}

#[derive(Debug, Clone)]
pub struct ClosureRef(Rc<Closure>);

impl ClosureRef {
    pub fn new(closure: Closure) -> Self {
        ClosureRef(Rc::new(closure))
    }

    pub(crate) fn construct_frame<C>(
        self,
        rt: &mut RuntimeView<C>,
        start: StackSlot,
    ) -> Result<Frame<C>, RuntimeError>
    where
        C: ChunkCache<ChunkId>,
    {
        use repr::chunk::Function;

        let function = rt
            .chunk_cache
            .chunk(self.fn_ptr.chunk_id)
            .ok_or(RuntimeError::CatchAll)?
            .get_function(self.fn_ptr.function_id)
            .ok_or(RuntimeError::CatchAll)?;

        let Function { signature, .. } = function;

        // Verify that closure provides exact same number of upvalues
        // that is expected by the function.
        if signature.upvalue_count != self.upvalues.len() {
            return Err(RuntimeError::CatchAll);
        }

        // Adjust stack, move varargs into register if needed.
        let stack_start = rt.stack.protected_size() + start;
        let call_height = StackSlot(0) + signature.arg_count;
        let mut stack = rt.stack.view(stack_start).ok_or(RuntimeError::CatchAll)?;

        let register_variadic = if signature.is_variadic {
            stack.adjust_height_with_variadics(call_height)
        } else {
            stack.adjust_height(call_height);
            Default::default()
        };

        // Load upvalues onto upvalue stack.
        let upvalue_start = rt.upvalue_stack.protected_size();
        rt.upvalue_stack.extend(
            self.upvalues
                .iter()
                .map(|upvalue_id| rt.stack.get_upvalue(*upvalue_id).unwrap().clone()),
        );

        let r = Frame {
            closure: self,
            ip: Default::default(),
            stack_start,
            upvalue_start,
            register_variadic,
        };

        Ok(r)
    }
}

impl PartialEq for ClosureRef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for ClosureRef {}

impl Hash for ClosureRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl Deref for ClosureRef {
    type Target = Closure;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl From<Closure> for ClosureRef {
    fn from(value: Closure) -> Self {
        ClosureRef::new(value)
    }
}

#[derive(Debug)]
pub struct Frame<C> {
    closure: ClosureRef,
    ip: InstrId,
    stack_start: RawStackSlot,
    upvalue_start: RawUpvalueSlot,
    register_variadic: Vec<Value<C>>,
}

impl<C> Frame<C> {
    pub(crate) fn activate<'a>(
        self,
        rt: &'a mut RuntimeView<C>,
    ) -> Result<ActiveFrame<'a, C>, RuntimeError>
    where
        C: ChunkCache<ChunkId>,
    {
        let RuntimeView {
            chunk_cache,
            stack,
            upvalue_stack,
            ..
        } = rt;

        let Frame {
            closure,
            ip,
            stack_start,
            upvalue_start,
            register_variadic,
        } = self;

        let fn_ptr = closure.fn_ptr;

        let chunk = chunk_cache
            .chunk(fn_ptr.chunk_id)
            .ok_or(RuntimeError::CatchAll)?;
        let function = chunk
            .get_function(fn_ptr.function_id)
            .ok_or(RuntimeError::CatchAll)?;

        let constants = &chunk.constants;
        let opcodes = &function.opcodes;
        let stack = stack.view(stack_start).unwrap();

        // Restore upvalue stack and update its values.
        let mut upvalue_stack = upvalue_stack.view(upvalue_start).unwrap();
        for (&upvalue_id, upvalue) in closure.upvalues.iter().zip(upvalue_stack.iter_mut()) {
            *upvalue = stack.get_upvalue(upvalue_id).unwrap().clone();
        }

        let r = ActiveFrame {
            closure,
            chunk,
            constants,
            opcodes,
            ip,
            stack,
            upvalue_stack,
            register_variadic,
        };

        Ok(r)
    }
}

#[derive(Debug)]
pub struct ActiveFrame<'rt, C> {
    closure: ClosureRef,
    chunk: &'rt Chunk,
    constants: &'rt TiSlice<ConstId, Literal>,
    opcodes: &'rt TiSlice<InstrId, OpCode>,
    ip: InstrId,
    stack: StackView<'rt, C>,
    upvalue_stack: UpvalueView<'rt, C>,
    register_variadic: Vec<Value<C>>,
}

impl<'rt, C> ActiveFrame<'rt, C> {
    pub fn get_constant(&self, index: ConstId) -> Option<&Literal> {
        self.constants.get(index)
    }

    pub fn step(&mut self) -> Result<ControlFlow, opcode_err::Error> {
        let Some(opcode) = self.next_opcode() else {
            return Ok(ControlFlow::Break(ChangeFrame::Return(self.stack.top())));
        };

        self.exec(opcode).map_err(|cause| opcode_err::Error {
            opcode,
            debug_info: self.opcode_debug_info(self.ip - 1),
            cause,
        })
    }

    fn exec(&mut self, opcode: OpCode) -> Result<ControlFlow, opcode_err::Cause> {
        use crate::value::table::TableRef;
        use opcode_err::Cause;
        use repr::index::InstrOffset;
        use repr::opcode::OpCode::*;

        let r = match opcode {
            Panic => return Err(Cause::CatchAll),
            Invoke(slot) => ControlFlow::Break(ChangeFrame::Invoke(slot)),
            Return(slot) => ControlFlow::Break(ChangeFrame::Return(slot)),
            MakeClosure(fn_id) => {
                let closure = self.construct_closure(fn_id).map_err(|_| Cause::CatchAll)?;
                self.stack
                    .push(Value::Function(Callable::LuaClosure(closure.into())));

                ControlFlow::Continue(())
            }
            LoadConstant(index) => {
                let constant = self.get_constant(index).ok_or(Cause::CatchAll)?.clone();
                self.stack.push(constant.into());

                ControlFlow::Continue(())
            }
            LoadVariadic => {
                self.stack.extend(self.register_variadic.iter().cloned());

                ControlFlow::Continue(())
            }
            LoadStack(slot) => {
                let value = self.stack.get(slot).map_err(|_| Cause::CatchAll)?.clone();
                self.stack.push(value);

                ControlFlow::Continue(())
            }
            StoreStack(slot) => {
                let [value] = self.stack.take1()?;
                let slot = self.stack.get_mut(slot).map_err(|_| Cause::CatchAll)?;

                *slot = value;

                ControlFlow::Continue(())
            }
            AdjustStack(height) => {
                self.stack.adjust_height(height);

                ControlFlow::Continue(())
            }
            LoadUpvalue(slot) => {
                let value = self
                    .upvalue_stack
                    .get(slot)
                    .map_err(|_| Cause::CatchAll)?
                    .clone();
                self.stack.push(value);

                ControlFlow::Continue(())
            }
            StoreUpvalue(slot) => {
                let [value] = self.stack.take1()?;
                let location = self
                    .upvalue_stack
                    .get_mut(slot)
                    .map_err(|_| Cause::CatchAll)?;

                *location = value;

                ControlFlow::Continue(())
            }
            UnaOp(op) => {
                let args = self.stack.take1()?;
                self.exec_una_op(args, op)?;

                ControlFlow::Continue(())
            }
            BinOp(op) => {
                let args = self.stack.take2()?;
                self.exec_bin_op(args, op)?;

                ControlFlow::Continue(())
            }
            Jump { offset } => {
                self.ip -= InstrOffset(1);
                self.ip += offset;

                ControlFlow::Continue(())
            }
            JumpIf { cond, offset } => {
                let [value] = self.stack.take1()?;

                if value.to_bool() == cond {
                    self.ip -= InstrOffset(1);
                    self.ip += offset;
                }

                ControlFlow::Continue(())
            }
            Loop { offset } => {
                self.ip -= InstrOffset(1);
                self.ip = self.ip.checked_sub_offset(offset).ok_or(Cause::CatchAll)?;

                ControlFlow::Continue(())
            }
            TabCreate => {
                let value = TableRef::new();

                self.stack.push(Value::Table(value));

                ControlFlow::Continue(())
            }
            TabGet => {
                let args = self.stack.take2()?;
                self.exec_tab_get(args).map_err(Cause::TabGet)?;

                ControlFlow::Continue(())
            }
            TabSet => {
                let args = self.stack.take3()?;
                self.exec_tab_set(args).map_err(Cause::TabSet)?;

                ControlFlow::Continue(())
            }
        };

        tracing::trace!(stack = ?self.stack, "executed opcode");

        Ok(r)
    }

    fn exec_una_op(
        &mut self,
        args: [Value<C>; 1],
        op: UnaOp,
    ) -> Result<(), opcode_err::UnaOpCause> {
        let [val] = args;

        let err = opcode_err::UnaOpCause { arg: val.type_() };

        let r = match op {
            UnaOp::AriNeg => match val {
                Value::Int(val) => Value::Int(-val),
                Value::Float(val) => Value::Float(-val),
                _ => return Err(err),
            },
            UnaOp::BitNot => match val {
                Value::Int(val) => Value::Int(!val),
                _ => return Err(err),
            },
            UnaOp::StrLen => match val {
                Value::String(val) => Value::Int(val.len().try_into().unwrap()),
                Value::Table(val) => {
                    let border = val.borrow().unwrap().border();

                    Value::Int(border)
                }
                _ => return Err(err),
            },
            UnaOp::LogNot => Value::Bool(!val.to_bool()),
        };

        self.stack.push(r);

        Ok(())
    }

    fn exec_bin_op(
        &mut self,
        args: [Value<C>; 2],
        op: BinOp,
    ) -> Result<(), opcode_err::BinOpCause> {
        let err = opcode_err::BinOpCause {
            lhs: args[0].type_(),
            rhs: args[1].type_(),
        };

        let result = match op {
            BinOp::Ari(op) => self.exec_bin_op_ari(args, op),
            BinOp::Bit(op) => self.exec_bin_op_bit(args, op),
            BinOp::Rel(op) => self.exec_bin_op_rel(args, op),
            BinOp::Str(op) => self.exec_bin_op_str(args, op),
        };

        if let Some(value) = result {
            self.stack.push(value);

            Ok(())
        } else {
            Err(err)
        }
    }

    fn exec_bin_op_str(&mut self, args: [Value<C>; 2], op: StrBinOp) -> Option<Value<C>> {
        match args {
            [Value::String(lhs), Value::String(rhs)] => match op {
                StrBinOp::Concat => Some(Value::String(lhs + &rhs)),
            },
            _ => None,
        }
    }

    fn exec_bin_op_rel(&mut self, args: [Value<C>; 2], op: RelBinOp) -> Option<Value<C>> {
        use RelBinOp::*;
        use Value::*;

        let [lhs, rhs] = args;

        let r = match (op, lhs, rhs) {
            (Eq, lhs, rhs) => lhs == rhs,
            (Neq, lhs, rhs) => lhs != rhs,

            (Lt, Int(lhs), Int(rhs)) => lhs < rhs,
            (Lt, Float(lhs), Float(rhs)) => lhs < rhs,
            (Lt, String(lhs), String(rhs)) => lhs < rhs,

            (LtEq, Int(lhs), Int(rhs)) => lhs <= rhs,
            (LtEq, Float(lhs), Float(rhs)) => lhs <= rhs,
            (LtEq, String(lhs), String(rhs)) => lhs <= rhs,

            (Gt, Int(lhs), Int(rhs)) => lhs > rhs,
            (Gt, Float(lhs), Float(rhs)) => lhs > rhs,
            (Gt, String(lhs), String(rhs)) => lhs > rhs,

            (GtEq, Int(lhs), Int(rhs)) => lhs >= rhs,
            (GtEq, Float(lhs), Float(rhs)) => lhs >= rhs,
            (GtEq, String(lhs), String(rhs)) => lhs >= rhs,

            _ => return None,
        };

        Some(Value::Bool(r))
    }

    fn exec_bin_op_bit(&mut self, args: [Value<C>; 2], op: BitBinOp) -> Option<Value<C>> {
        let r = match args {
            [Value::Int(lhs), Value::Int(rhs)] => match op {
                BitBinOp::And => Value::Int(lhs & rhs),
                BitBinOp::Or => Value::Int(lhs | rhs),
                BitBinOp::Xor => Value::Int(lhs ^ rhs),
                BitBinOp::ShL => {
                    let r = if let Ok(rhs) = rhs.try_into() {
                        lhs.checked_shl(rhs).unwrap_or_default()
                    } else if let Ok(rhs) = (-rhs).try_into() {
                        let lhs = lhs as u64;
                        lhs.checked_shr(rhs).unwrap_or_default() as i64
                    } else {
                        0
                    };

                    Value::Int(r)
                }
                BitBinOp::ShR => {
                    let r = if let Ok(rhs) = rhs.try_into() {
                        let lhs = lhs as u64;
                        lhs.checked_shr(rhs).unwrap_or_default() as i64
                    } else if let Ok(rhs) = (-rhs).try_into() {
                        lhs.checked_shl(rhs).unwrap_or_default()
                    } else {
                        0
                    };

                    Value::Int(r)
                }
            },
            _ => return None,
        };

        Some(r)
    }

    fn exec_bin_op_ari(&mut self, args: [Value<C>; 2], op: AriBinOp) -> Option<Value<C>> {
        let r = match args {
            [Value::Int(lhs), Value::Int(rhs)] => match op {
                AriBinOp::Add => Value::Int(lhs.wrapping_add(rhs)),
                AriBinOp::Sub => Value::Int(lhs.wrapping_sub(rhs)),
                AriBinOp::Mul => Value::Int(lhs.wrapping_mul(rhs)),
                AriBinOp::Rem => Value::Int(lhs.rem_euclid(rhs)),
                AriBinOp::Div => {
                    let r = (lhs as f64) / (rhs as f64);
                    Value::Float(r)
                }
                AriBinOp::FloorDiv => {
                    let r = ((lhs as f64) / (rhs as f64)).floor() as i64;
                    Value::Int(r)
                }
                AriBinOp::Exp => {
                    let r = (lhs as f64).powf(rhs as f64);
                    Value::Float(r)
                }
            },
            [Value::Float(lhs), Value::Float(rhs)] => match op {
                AriBinOp::Add => Value::Float(lhs + rhs),
                AriBinOp::Sub => Value::Float(lhs - rhs),
                AriBinOp::Mul => Value::Float(lhs * rhs),
                AriBinOp::Div => Value::Float(lhs / rhs),
                AriBinOp::FloorDiv => Value::Float((lhs / rhs).floor()),
                AriBinOp::Rem => Value::Float(lhs - rhs * (lhs / rhs).floor()),
                AriBinOp::Exp => Value::Float(lhs.powf(rhs)),
            },
            _ => return None,
        };

        Some(r)
    }

    fn exec_tab_get(&mut self, args: [Value<C>; 2]) -> Result<(), opcode_err::TabCause> {
        use opcode_err::TabCause::*;

        let [table, index] = args;

        let table = match table {
            Value::Table(t) => t,
            t => return Err(TableTypeMismatch(t.type_())),
        };

        let key = index.try_into().map_err(InvalidKey)?;
        let value = table.borrow().unwrap().get(key);

        self.stack.push(value);

        Ok(())
    }

    fn exec_tab_set(&mut self, args: [Value<C>; 3]) -> Result<(), opcode_err::TabCause> {
        use opcode_err::TabCause::*;

        let [table, index, value] = args;

        let table = match table {
            Value::Table(t) => t,
            t => return Err(TableTypeMismatch(t.type_())),
        };

        let key = index.try_into().map_err(InvalidKey)?;

        table.borrow_mut().unwrap().set(key, value);

        Ok(())
    }

    fn opcode_debug_info(&self, ip: InstrId) -> Option<OpCodeDebugInfo> {
        self.chunk
            .debug_info
            .as_ref()
            .and_then(|debug_info| debug_info.functions.get(self.closure.fn_ptr.function_id))
            .and_then(|fn_debug_info| fn_debug_info.opcodes.get(ip))
            .cloned()
    }

    pub fn next_opcode(&mut self) -> Option<OpCode> {
        let r = *self.opcodes.get(self.ip)?;

        tracing::trace!(ip = self.ip.0, opcode = %r, "next opcode");

        self.ip += 1;

        Some(r)
    }

    pub fn construct_closure(&mut self, recipe_id: RecipeId) -> Result<Closure, RuntimeError> {
        let recipe = self
            .chunk
            .get_recipe(recipe_id)
            .ok_or(RuntimeError::CatchAll)?;

        let ClosureRecipe {
            function_id,
            upvalues,
        } = recipe;
        let function_id = *function_id;

        let fn_ptr = FunctionPtr {
            chunk_id: self.closure.fn_ptr.chunk_id,
            function_id,
        };

        let upvalues = upvalues
            .iter()
            .map(|&source| {
                use repr::chunk::UpvalueSource;

                match source {
                    UpvalueSource::Temporary(slot) => self.stack.mark_as_upvalue(slot),
                    UpvalueSource::Upvalue(slot) => self
                        .closure
                        .upvalues
                        .get(slot)
                        .copied()
                        .ok_or(RuntimeError::CatchAll),
                }
            })
            .collect::<Result<_, _>>()?;

        let r = Closure { fn_ptr, upvalues };

        Ok(r)
    }

    pub fn suspend(self) -> Frame<C> {
        let ActiveFrame {
            closure,
            ip,
            mut stack,
            upvalue_stack,
            register_variadic,
            ..
        } = self;

        let stack_start = stack.protected_size();
        let upvalue_start = upvalue_stack.protected_size();

        for (&upvalue_id, upvalue) in closure.upvalues.iter().zip(upvalue_stack.iter()) {
            *stack.get_upvalue_mut(upvalue_id).unwrap() = upvalue.clone();
        }

        Frame {
            closure,
            ip,
            stack_start,
            upvalue_start,
            register_variadic,
        }
    }

    pub fn exit(mut self, drop_under: StackSlot) -> Result<(), RuntimeError> {
        self.stack.drop_under(drop_under)?;
        self.upvalue_stack.clear();

        Ok(())
    }
}
