use std::hash::Hash;
use std::ops::ControlFlow;
use std::ops::Deref;
use std::rc::Rc;

use enumoid::EnumMap;
use repr::chunk::{Chunk, ClosureRecipe};
use repr::debug_info::OpCodeDebugInfo;
use repr::index::{ConstId, FunctionId, InstrId, InstrOffset, RecipeId, StackSlot, UpvalueSlot};
use repr::literal::Literal;
use repr::opcode::{AriBinOp, BinOp, BitBinOp, EqBinOp, OpCode, RelBinOp, StrBinOp, UnaOp};
use repr::tivec::{TiSlice, TiVec};

use super::stack::UpvalueId;
use super::stack::{RawStackSlot, StackView};
use super::upvalue_stack::{RawUpvalueSlot, UpvalueStackView};
use super::{DialectBuilder, RuntimeView};
use crate::backtrace::BacktraceFrame;
use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::error::opcode::{
    self as opcode_err, IpOutOfBounds, MissingConstId, MissingStackSlot, MissingUpvalue,
};
use crate::error::RuntimeError;
use crate::value::callable::Callable;
use crate::value::{LuaTypeWithoutMetatable, MetaValue, TableRef, Value};

pub enum ChangeFrame<C> {
    Return(StackSlot),
    Invoke(Option<MetaValue>, Callable<C>, StackSlot),
}

trait MapControlFlow<F> {
    type Output;

    fn map_br(self, f: F) -> Self::Output;
}

impl<B, C, T, F> MapControlFlow<F> for ControlFlow<B, C>
where
    F: FnOnce(B) -> T,
{
    type Output = ControlFlow<T, C>;

    fn map_br(self, f: F) -> Self::Output {
        match self {
            ControlFlow::Continue(t) => ControlFlow::Continue(t),
            ControlFlow::Break(t) => ControlFlow::Break(f(t)),
        }
    }
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
        start: RawStackSlot,
        metavalue: Option<MetaValue>,
    ) -> Result<Frame<C>, RuntimeError<C>>
    where
        C: ChunkCache,
    {
        use crate::error::{MissingChunk, MissingFunction, OutOfBoundsStack, UpvalueCountMismatch};
        use repr::chunk::Function;

        let function = rt
            .chunk_cache
            .chunk(self.fn_ptr.chunk_id)
            .ok_or(MissingChunk(self.fn_ptr.chunk_id))?
            .get_function(self.fn_ptr.function_id)
            .ok_or(MissingFunction(self.fn_ptr))?;

        let Function { signature, .. } = function;

        // Verify that closure provides exact same number of upvalues
        // that is expected by the function.
        if signature.upvalue_count != self.upvalues.len() {
            let err = UpvalueCountMismatch {
                expected: signature.upvalue_count,
                closure: self.upvalues.len(),
            };

            return Err(err.into());
        }

        // Adjust stack, move varargs into register if needed.
        let stack_start = start;
        let call_height = StackSlot(0) + signature.arg_count;
        let mut stack = rt.stack.view(stack_start).ok_or(OutOfBoundsStack)?;

        let register_variadic = if signature.is_variadic {
            stack.adjust_height_with_variadics(call_height)
        } else {
            stack.adjust_height(call_height);
            Default::default()
        };

        // Load upvalues onto upvalue stack.
        let upvalue_start = rt.upvalue_stack.next_raw_slot();
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
            metavalue,
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
    /// Frame is evaluated as metamethod.
    metavalue: Option<MetaValue>,
}

impl<C> Frame<C> {
    pub(crate) fn fn_ptr(&self) -> FunctionPtr {
        self.closure.fn_ptr
    }

    pub(crate) fn stack_boundary(&self) -> RawStackSlot {
        self.stack_start
    }
}

impl<C> Frame<C>
where
    C: ChunkCache,
{
    pub(crate) fn activate<'a>(
        self,
        rt: &'a mut RuntimeView<C>,
    ) -> Result<ActiveFrame<'a, C>, RuntimeError<C>> {
        use crate::error::{MissingChunk, MissingFunction};

        let RuntimeView {
            chunk_cache,
            stack,
            upvalue_stack,
            primary_metatables,
            dialect,
            ..
        } = rt;

        let Frame {
            closure,
            ip,
            stack_start,
            upvalue_start,
            register_variadic,
            metavalue,
        } = self;

        let fn_ptr = closure.fn_ptr;

        let chunk = chunk_cache
            .chunk(fn_ptr.chunk_id)
            .ok_or(MissingChunk(fn_ptr.chunk_id))?;
        let function = chunk
            .get_function(fn_ptr.function_id)
            .ok_or(MissingFunction(fn_ptr))?;

        let constants = &chunk.constants;
        let opcodes = &function.opcodes;
        let stack = stack.view(stack_start).unwrap();

        // Restore upvalue stack and update its values.
        let mut upvalue_stack = upvalue_stack.view(upvalue_start).unwrap();
        for (&upvalue_id, upvalue) in closure.upvalues.iter().zip(upvalue_stack.iter_mut()) {
            *upvalue = stack.get_upvalue(upvalue_id).unwrap().clone();
        }

        tracing::trace!(stack = stack.to_pretty_string(), "activated Lua frame");

        let r = ActiveFrame {
            closure,
            chunk,
            constants,
            opcodes,
            ip,
            stack,
            upvalue_stack,
            register_variadic,
            primary_metatables,
            dialect: *dialect,
            metavalue,
        };

        Ok(r)
    }
}

impl<C> Frame<C>
where
    C: ChunkCache,
{
    pub(crate) fn backtrace(&self, chunk_cache: &C) -> BacktraceFrame {
        use crate::backtrace::{FrameSource, Location};

        let ptr = self.closure.fn_ptr;
        // Instruction pointer always points at the *next* instruction.
        let ip = self.ip - 1;
        let (name, location) = chunk_cache
            .chunk(ptr.chunk_id)
            .and_then(|chunk| chunk.debug_info.as_ref())
            .and_then(|info| info.functions.get(ptr.function_id).map(|func| (info, func)))
            .map(|(chunk, function)| {
                let name = function.name.clone();
                let location = function.opcodes.get(ip).map(|info| {
                    let chunk_location =
                        chunk_cache
                            .location(ptr.chunk_id)
                            .unwrap_or_else(|| Location {
                                file: "<no file location is present>".to_string(),
                                line: 0,
                                column: 0,
                            });

                    let (line, column) = chunk.line_column(info.start());

                    // The first line of chunk might be offset from the beginning of the line.
                    let column = if line == 1 {
                        column + chunk_location.column
                    } else {
                        column
                    };
                    let line = line + chunk_location.line;

                    Location {
                        file: chunk_location.file,
                        line,
                        column,
                    }
                });

                (Some(name), location)
            })
            .unwrap_or_default();

        BacktraceFrame {
            name,
            source: FrameSource::Lua,
            location,
        }
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
    upvalue_stack: UpvalueStackView<'rt, C>,
    register_variadic: Vec<Value<C>>,
    primary_metatables: &'rt EnumMap<LuaTypeWithoutMetatable, Option<TableRef<C>>>,
    dialect: DialectBuilder,
    /// Frame is being evaluated as metamethod.
    metavalue: Option<MetaValue>,
}

impl<'rt, C> ActiveFrame<'rt, C> {
    pub fn get_constant(&self, index: ConstId) -> Result<&Literal, MissingConstId> {
        self.constants.get(index).ok_or(MissingConstId(index))
    }

    fn get_upvalue(&self, index: UpvalueSlot) -> Result<&Value<C>, MissingUpvalue> {
        self.upvalue_stack.get(index).ok_or(MissingUpvalue(index))
    }

    fn get_upvalue_mut(&mut self, index: UpvalueSlot) -> Result<&mut Value<C>, MissingUpvalue> {
        self.upvalue_stack
            .get_mut(index)
            .ok_or(MissingUpvalue(index))
    }

    fn get_stack(&self, index: StackSlot) -> Result<&Value<C>, MissingStackSlot> {
        self.stack.get(index).ok_or(MissingStackSlot(index))
    }

    fn get_stack_mut(&mut self, index: StackSlot) -> Result<&mut Value<C>, MissingStackSlot> {
        self.stack.get_mut(index).ok_or(MissingStackSlot(index))
    }

    fn increment_ip(&mut self, offset: InstrOffset) -> Result<(), IpOutOfBounds> {
        let err = IpOutOfBounds(self.ip);

        let new_ip = self.ip.checked_add(offset).ok_or(err)?;
        if new_ip > self.opcodes.next_key() {
            Err(err)
        } else {
            self.ip = new_ip;
            Ok(())
        }
    }

    fn decrement_ip(&mut self, offset: InstrOffset) -> Result<(), IpOutOfBounds> {
        let err = IpOutOfBounds(self.ip);

        self.ip = self.ip.checked_sub_offset(offset).ok_or(err)?;
        Ok(())
    }

    pub fn step(&mut self) -> Result<ControlFlow<ChangeFrame<C>>, opcode_err::Error> {
        let Some(opcode) = self.next_opcode() else {
            return Ok(ControlFlow::Break(ChangeFrame::Return(
                self.stack.next_slot(),
            )));
        };

        self.exec(opcode).map_err(|cause| opcode_err::Error {
            opcode,
            debug_info: self.opcode_debug_info(self.ip - 1),
            cause,
        })
    }

    fn exec(&mut self, opcode: OpCode) -> Result<ControlFlow<ChangeFrame<C>>, opcode_err::Cause> {
        use opcode_err::Cause;
        use repr::opcode::OpCode::*;

        let r = match opcode {
            Panic => return Err(opcode_err::Panic.into()),
            Invoke(slot) => {
                // It is extremely annoying to keep callable on the stack (either caller or callee),
                // however current approach causes stack adjustments on every single fn call.
                // I would like to implement it differently,
                // but somewhat frustratingly this seems to be the simplest workable option.
                // The only other approach I can think of is constructing dedicated callable *stack*
                // (single-value register doesn't work due to nested calls)
                // and make fn invocation into two instructions: StoreCallable + Invoke.
                // Not sure if it will work better.
                let value = self.stack.remove(slot).ok_or(MissingStackSlot(slot))?;
                let type_ = value.type_();

                let (callable, start) = self
                    .prepare_invoke(value, slot)
                    .ok_or(opcode_err::Invoke(type_))?;

                ControlFlow::Break(ChangeFrame::Invoke(None, callable, start))
            }
            Return(slot) => ControlFlow::Break(ChangeFrame::Return(slot)),
            MakeClosure(fn_id) => {
                let closure = self.construct_closure(fn_id)?;
                self.stack
                    .push(Value::Function(Callable::LuaClosure(closure.into())));

                ControlFlow::Continue(())
            }
            LoadConstant(index) => {
                let constant = self.get_constant(index)?.clone();
                self.stack.push(constant.into());

                ControlFlow::Continue(())
            }
            LoadVariadic => {
                self.stack.extend(self.register_variadic.iter().cloned());

                ControlFlow::Continue(())
            }
            LoadStack(slot) => {
                let value = self.get_stack(slot)?.clone();
                self.stack.push(value);

                ControlFlow::Continue(())
            }
            StoreStack(slot) => {
                let [value] = self.stack.take1()?;
                let place = self.get_stack_mut(slot)?;

                *place = value;

                ControlFlow::Continue(())
            }
            AdjustStack(height) => {
                self.stack.adjust_height(height);

                ControlFlow::Continue(())
            }
            LoadUpvalue(slot) => {
                let value = self.get_upvalue(slot)?.clone();
                self.stack.push(value);

                ControlFlow::Continue(())
            }
            StoreUpvalue(slot) => {
                let [value] = self.stack.take1()?;
                let place = self.get_upvalue_mut(slot)?;

                *place = value;

                ControlFlow::Continue(())
            }
            UnaOp(op) => {
                let args = self.stack.take1()?;
                self.exec_una_op(args, op)?
                    .map_br(|(metavalue, callable, start)| {
                        ChangeFrame::Invoke(Some(metavalue), callable, start)
                    })
            }
            BinOp(op) => {
                let args = self.stack.take2()?;
                self.exec_bin_op(args, op)?
                    .map_br(|(metavalue, callable, start)| {
                        ChangeFrame::Invoke(Some(metavalue), callable, start)
                    })
            }
            Jump { offset } => {
                self.ip -= InstrOffset(1);
                self.increment_ip(offset)?;

                ControlFlow::Continue(())
            }
            JumpIf { cond, offset } => {
                let [value] = self.stack.take1()?;

                if value.to_bool() == cond {
                    self.ip -= InstrOffset(1);
                    self.increment_ip(offset)?;
                }

                ControlFlow::Continue(())
            }
            Loop { offset } => {
                self.ip -= InstrOffset(1);
                self.decrement_ip(offset)?;

                ControlFlow::Continue(())
            }
            TabCreate => {
                let value = TableRef::new();

                self.stack.push(Value::Table(value));

                ControlFlow::Continue(())
            }
            TabGet => {
                let args = self.stack.take2()?;
                self.exec_tab_get(args)
                    .map_err(Cause::TabGet)?
                    .map_br(|(callable, start)| {
                        ChangeFrame::Invoke(Some(MetaValue::Index), callable, start)
                    })
            }
            TabSet => {
                let args = self.stack.take3()?;
                self.exec_tab_set(args)
                    .map_err(Cause::TabSet)?
                    .map_br(|(callable, start)| {
                        ChangeFrame::Invoke(Some(MetaValue::NewIndex), callable, start)
                    })
            }
        };

        tracing::trace!(stack = self.stack.to_pretty_string(), "executed opcode");

        Ok(r)
    }

    fn exec_una_op(
        &mut self,
        args: [Value<C>; 1],
        op: UnaOp,
    ) -> Result<ControlFlow<(MetaValue, Callable<C>, StackSlot)>, opcode_err::UnaOpCause> {
        use crate::value::{Float, Int};
        use ControlFlow::*;

        let [val] = &args;

        let err = opcode_err::UnaOpCause { arg: val.type_() };

        let eval = match op {
            UnaOp::AriNeg => match args {
                [Value::Int(val)] => Continue((-Int(val)).into()),
                [Value::Float(val)] => Continue((-Float(val)).into()),
                args => Break((MetaValue::Neg, args)),
            },
            UnaOp::BitNot => {
                use super::CoerceArgs;

                let args = self.dialect.coerce_una_op_bit(args);

                match args {
                    [Value::Int(val)] => Continue((!Int(val)).into()),
                    args => Break((MetaValue::BitNot, args)),
                }
            }
            UnaOp::StrLen => match args {
                [Value::String(val)] => {
                    let len = val.len().try_into().unwrap();

                    Continue(Value::Int(len))
                }
                // Table builtin triggers after metamethod attempt.
                args => Break((MetaValue::Len, args)),
            },
            UnaOp::LogNot => {
                let [arg] = args;
                let r = Value::Bool(!arg.to_bool());
                self.stack.push(r);

                return Ok(Continue(()));
            }
        };

        match eval {
            Continue(value) => {
                self.stack.push(value);
                Ok(Continue(()))
            }
            Break((key, args)) => {
                let [arg] = &args;
                let metavalue = arg
                    .metatable(self.primary_metatables)
                    .map(|mt| mt.borrow().unwrap().get(key.into()));

                match metavalue {
                    Some(metavalue) => {
                        let start = self.stack.next_slot();

                        self.stack.extend(args);
                        let (callable, start) = self.prepare_invoke(metavalue, start).ok_or(err)?;

                        Ok(Break((key, callable, start)))
                    }
                    None => match (op, args) {
                        // Trigger table len builtin on failed metamethod lookup.
                        (UnaOp::StrLen, [Value::Table(tab)]) => {
                            let border = tab.borrow().unwrap().border();
                            self.stack.push(Value::Int(border));

                            Ok(Continue(()))
                        }
                        _ => Err(err),
                    },
                }
            }
        }
    }

    fn exec_bin_op(
        &mut self,
        args: [Value<C>; 2],
        op: BinOp,
    ) -> Result<std::ops::ControlFlow<(MetaValue, Callable<C>, StackSlot)>, opcode_err::BinOpCause>
    {
        let err = opcode_err::BinOpCause {
            lhs: args[0].type_(),
            rhs: args[1].type_(),
        };

        let eval = match op {
            BinOp::Ari(op) => self.exec_bin_op_ari(args, op),
            BinOp::Bit(op) => self.exec_bin_op_bit(args, op),
            BinOp::Eq(op) => ControlFlow::Continue(Some(self.exec_bin_op_eq(args, op))),
            BinOp::Rel(op) => self.exec_bin_op_rel(args, op),
            BinOp::Str(op) => self.exec_bin_op_str(args, op),
        };

        match eval {
            ControlFlow::Continue(Some(value)) => {
                self.stack.push(value);
                Ok(ControlFlow::Continue(()))
            }
            ControlFlow::Continue(None) => Err(err),
            ControlFlow::Break(args) => {
                let key: MetaValue = op.into();

                // Swap arguments for greater/greater-or-eq comparisons.
                // Those desugar into Lt/LtEq metamethods with swapped arguments.
                let args = match op {
                    BinOp::Rel(RelBinOp::Gt | RelBinOp::GtEq) => {
                        let [rhs, lhs] = args;
                        [lhs, rhs]
                    }
                    _ => args,
                };

                let [lhs, rhs] = args;
                let (callable, start) = lhs
                    .metatable(self.primary_metatables)
                    .or_else(|| rhs.metatable(self.primary_metatables))
                    .map(|mt| mt.borrow().unwrap().get(key.into()))
                    .and_then(|metavalue| {
                        let start = self.stack.next_slot();
                        self.stack.extend([lhs, rhs]);
                        self.prepare_invoke(metavalue, start)
                    })
                    .ok_or(err)?;

                Ok(ControlFlow::Break((key, callable, start)))
            }
        }
    }

    fn exec_bin_op_str(
        &mut self,
        args: [Value<C>; 2],
        op: StrBinOp,
    ) -> std::ops::ControlFlow<[Value<C>; 2], Option<Value<C>>> {
        use super::CoerceArgs;

        let args = self.dialect.coerce_bin_op_str(op, args);

        match args {
            [Value::String(lhs), Value::String(rhs)] => match op {
                StrBinOp::Concat => ControlFlow::Continue(Some(Value::String(lhs + &rhs))),
            },
            args => ControlFlow::Break(args),
        }
    }

    fn exec_bin_op_eq(&mut self, args: [Value<C>; 2], op: EqBinOp) -> Value<C> {
        use super::CoerceArgs;
        use crate::value::{Float, Int};
        use EqBinOp::*;

        let cmp = self.dialect.cmp_float_and_int();

        let equal = match args {
            [Value::Int(lhs), Value::Float(rhs)] if cmp => Int(lhs) == Float(rhs),
            [Value::Float(lhs), Value::Int(rhs)] if cmp => Float(lhs) == Int(rhs),

            [lhs, rhs] => lhs == rhs,
        };

        let r = match op {
            Eq => equal,
        };

        Value::Bool(r)
    }

    fn exec_bin_op_rel(
        &mut self,
        args: [Value<C>; 2],
        op: RelBinOp,
    ) -> std::ops::ControlFlow<[Value<C>; 2], Option<Value<C>>> {
        use super::CoerceArgs;
        use crate::value;
        use RelBinOp::*;
        use Value::*;

        let cmp = self.dialect.cmp_float_and_int();

        let ord = match &args {
            [Int(lhs), Int(rhs)] => PartialOrd::partial_cmp(lhs, rhs),
            [Float(lhs), Float(rhs)] => PartialOrd::partial_cmp(lhs, rhs),
            [String(lhs), String(rhs)] => PartialOrd::partial_cmp(lhs, rhs),
            [Int(lhs), Float(rhs)] if cmp => {
                PartialOrd::partial_cmp(&value::Int(*lhs), &value::Float(*rhs))
            }
            [Float(lhs), Int(rhs)] if cmp => {
                PartialOrd::partial_cmp(&value::Float(*lhs), &value::Int(*rhs))
            }
            _ => None,
        };

        let Some(ord) = ord else {
            return ControlFlow::Break(args);
        };

        let r = match op {
            Lt => ord.is_lt(),
            LtEq => ord.is_le(),
            Gt => ord.is_gt(),
            GtEq => ord.is_ge(),
        };

        ControlFlow::Continue(Some(Value::Bool(r)))
    }

    fn exec_bin_op_bit(
        &mut self,
        args: [Value<C>; 2],
        op: BitBinOp,
    ) -> std::ops::ControlFlow<[Value<C>; 2], Option<Value<C>>> {
        use super::CoerceArgs;
        use crate::value::Int;

        let args = self.dialect.coerce_bin_op_bit(op, args);

        let r = match args {
            [Value::Int(lhs), Value::Int(rhs)] => match op {
                BitBinOp::And => (Int(lhs) & Int(rhs)).into(),
                BitBinOp::Or => (Int(lhs) | Int(rhs)).into(),
                BitBinOp::Xor => (Int(lhs) ^ Int(rhs)).into(),
                BitBinOp::ShL => (Int(lhs) << Int(rhs)).into(),
                BitBinOp::ShR => (Int(lhs) >> Int(rhs)).into(),
            },
            args => return ControlFlow::Break(args),
        };

        ControlFlow::Continue(Some(r))
    }

    fn exec_bin_op_ari(
        &mut self,
        args: [Value<C>; 2],
        op: AriBinOp,
    ) -> std::ops::ControlFlow<[Value<C>; 2], Option<Value<C>>> {
        use super::CoerceArgs;
        use crate::value::{Float, Int};
        use AriBinOp::*;

        // Coercions.
        let args = self.dialect.coerce_bin_op_ari(op, args);

        let r = match args {
            [Value::Int(lhs), Value::Int(rhs)] => match op {
                Add => Some((Int(lhs) + Int(rhs)).into()),
                Sub => Some((Int(lhs) - Int(rhs)).into()),
                Mul => Some((Int(lhs) * Int(rhs)).into()),
                Div | FloorDiv | Rem if rhs == 0 => None,
                Rem => Some((Int(lhs) % Int(rhs)).into()),
                Div => Some((Int(lhs) / Int(rhs)).into()),
                FloorDiv => Some(Int(lhs).floor_div(Int(rhs)).into()),
                Pow => Int(lhs).exp(Int(rhs)).map(Into::into),
            },
            [Value::Float(lhs), Value::Float(rhs)] => match op {
                Add => Some((Float(lhs) + Float(rhs)).into()),
                Sub => Some((Float(lhs) - Float(rhs)).into()),
                Mul => Some((Float(lhs) * Float(rhs)).into()),
                Div => Some((Float(lhs) / Float(rhs)).into()),
                FloorDiv => Some(Float(lhs).floor_div(Float(rhs)).into()),
                Rem => Some((Float(lhs) % Float(rhs)).into()),
                Pow => Some(Float(lhs).exp(Float(rhs)).into()),
            },
            args => return ControlFlow::Break(args),
        };

        ControlFlow::Continue(r)
    }

    fn exec_tab_get(
        &mut self,
        args: [Value<C>; 2],
    ) -> Result<ControlFlow<(Callable<C>, StackSlot)>, opcode_err::TabCause> {
        use super::CoerceArgs;
        use opcode_err::TabCause::*;
        use ControlFlow::*;

        let [mut table, index] = args;

        'outer: loop {
            // First: try raw table access.
            if let Value::Table(table) = &table {
                let index = self.dialect.coerce_tab_get(index.clone());

                let key = index.try_into().map_err(InvalidKey)?;
                let value = table.borrow().unwrap().get(key);

                // It succeeds if any non-nil value is produced.
                if value != Value::Nil {
                    self.stack.push(value);
                    return Ok(Continue(()));
                }
            };

            // Second: try detecting compatible metavalue
            loop {
                let metavalue = table
                    .metatable(self.primary_metatables)
                    .map(|mt| mt.borrow().unwrap().get(MetaValue::Index.into()))
                    .unwrap_or_default();

                match metavalue {
                    // Keyes associated with nil are considered to be absent from table.
                    Value::Nil => {
                        if matches!(table, Value::Table(_)) {
                            // Third: Fallback to producing nil.
                            // This can only be reached if raw table access returned nil and there is no metamethod.

                            self.stack.push(Value::Nil);
                            return Ok(Continue(()));
                        } else {
                            return Err(TableTypeMismatch(table.type_()));
                        }
                    }
                    // Table simply goes on another spin of recursive table lookup.
                    tab @ Value::Table(_) => {
                        table = tab;
                        continue 'outer;
                    }
                    // Function is invoked with table and *original* (before coercions!) index.
                    // It only picks up the latest "table" value which metamethod we tried to look up.
                    Value::Function(callable) => {
                        let start = self.stack.next_slot();
                        self.stack.extend([table, index]);
                        return Ok(Break((callable, start)));
                    }
                    // If everything fails, try to recursively lookup metamethod in the new value.
                    // Lua affirms to ignore function-like (e.g. with __call metamethod) objects in table indexing.
                    value => {
                        table = value;
                    }
                }
            }
        }
    }

    fn exec_tab_set(
        &mut self,
        args: [Value<C>; 3],
    ) -> Result<ControlFlow<(Callable<C>, StackSlot)>, opcode_err::TabCause> {
        use super::CoerceArgs;
        use opcode_err::TabCause::*;
        use ControlFlow::*;

        let [mut table, index, value] = args;

        'outer: loop {
            // First: try raw table access.
            if let Value::Table(table) = &table {
                let index = self.dialect.coerce_tab_set(index.clone());
                let key = index.try_into().map_err(InvalidKey)?;

                let mut table = table.borrow_mut().unwrap();

                // It succeeds if key is already populated.
                if table.contains_key(&key) {
                    table.set(key, value);

                    return Ok(Continue(()));
                }
            };

            // Second: try detecting compatible metavalue
            loop {
                let metavalue = table
                    .metatable(self.primary_metatables)
                    .map(|mt| mt.borrow().unwrap().get(MetaValue::NewIndex.into()))
                    .unwrap_or_default();

                match metavalue {
                    // Keyes associated with nil are considered to be absent from table.
                    Value::Nil => {
                        if let Value::Table(table) = table {
                            // Third: Fallback to raw assignment.
                            // This can only be reached if raw table access returned nil and there is no metamethod.

                            let index = self.dialect.coerce_tab_set(index);
                            let key = index.try_into().map_err(InvalidKey)?;

                            table.borrow_mut().unwrap().set(key, value);

                            return Ok(Continue(()));
                        } else {
                            return Err(TableTypeMismatch(table.type_()));
                        }
                    }
                    // Table simply goes on another spin of recursive table assignment.
                    tab @ Value::Table(_) => {
                        table = tab;
                        continue 'outer;
                    }
                    // Function is invoked with table, *original* (before coercions!) index and value.
                    // It only picks up the latest "table" value which metamethod we tried to look up.
                    Value::Function(callable) => {
                        let start = self.stack.next_slot();
                        self.stack.extend([table, index, value]);
                        return Ok(Break((callable, start)));
                    }
                    // If everything fails, try to recursively lookup metamethod in the new value.
                    // Lua affirms to ignore function-like (e.g. with __call metamethod) objects in table indexing.
                    value => {
                        table = value;
                    }
                }
            }
        }
    }

    fn prepare_invoke(
        &mut self,
        mut callable: Value<C>,
        start: StackSlot,
    ) -> Option<(Callable<C>, StackSlot)> {
        loop {
            callable = match callable {
                Value::Function(r) => return Some((r, start)),
                t => t,
            };

            let new_callable = callable
                .metatable(self.primary_metatables)
                .map(|mt| mt.borrow().unwrap().get(MetaValue::Call.into()))
                .unwrap_or_default();

            // Keys associated with nil are not considered part of the table.
            if new_callable == Value::Nil {
                return None;
            }

            self.stack.insert(start, callable);
            callable = new_callable;
        }
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

    pub fn construct_closure(&mut self, recipe_id: RecipeId) -> Result<Closure, opcode_err::Cause> {
        let recipe = self
            .chunk
            .get_recipe(recipe_id)
            .ok_or(opcode_err::MissingRecipe(recipe_id))?;

        let ClosureRecipe {
            function_id,
            upvalues,
        } = recipe;

        let fn_ptr = FunctionPtr {
            chunk_id: self.closure.fn_ptr.chunk_id,
            function_id: *function_id,
        };

        let upvalues = upvalues
            .iter()
            .map(|&source| -> Result<_, opcode_err::Cause> {
                use repr::chunk::UpvalueSource;

                match source {
                    UpvalueSource::Temporary(slot) => self
                        .stack
                        .mark_as_upvalue(slot)
                        .ok_or(opcode_err::MissingStackSlot(slot).into()),
                    UpvalueSource::Upvalue(slot) => self
                        .closure
                        .upvalues
                        .get(slot)
                        .copied()
                        .ok_or(opcode_err::MissingUpvalue(slot).into()),
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
            metavalue,
            ..
        } = self;

        let stack_start = stack.boundary();
        let upvalue_start = upvalue_stack.boundary();

        for (&upvalue_id, upvalue) in closure.upvalues.iter().zip(upvalue_stack.iter()) {
            *stack.get_upvalue_mut(upvalue_id).unwrap() = upvalue.clone();
        }

        Frame {
            closure,
            ip,
            stack_start,
            upvalue_start,
            register_variadic,
            metavalue,
        }
    }

    pub fn exit(
        mut self,
        drop_under: StackSlot,
    ) -> Result<Option<(MetaValue, RawStackSlot)>, RuntimeError<C>> {
        self.stack.remove_range(StackSlot(0)..drop_under);
        self.upvalue_stack.clear();

        let r = self.metavalue.map(|mv| (mv, self.stack.boundary()));
        Ok(r)
    }
}
