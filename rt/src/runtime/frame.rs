use std::hash::Hash;
use std::ops::Deref;
use std::rc::Rc;

use repr::chunk::Chunk;
use repr::index::{ConstId, FunctionId, InstrId, StackSlot, UpvalueSlot};
use repr::index_vec::{IndexSlice, IndexVec};
use repr::literal::Literal;
use repr::opcode::OpCode;

use super::stack::UpvalueId;
use super::stack::{RawStackSlot, StackView};
use super::upvalue_stack::{ProtectedSize as RawUpvalueSlot, UpvalueView};
use super::RuntimeView;
use super::Value;
use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::RuntimeError;

pub type ControlFlow = std::ops::ControlFlow<ChangeFrame>;

pub enum ChangeFrame {
    Return(StackSlot),
    Invoke(ClosureRef, StackSlot),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct FunctionPtr {
    pub chunk_id: ChunkId,
    pub function_id: FunctionId,
}

impl FunctionPtr {
    pub(crate) fn construct_closure<C>(
        self,
        rt: &mut RuntimeView<C>,
    ) -> Result<Closure, RuntimeError>
    where
        C: ChunkCache,
    {
        let signature = &rt
            .chunk_cache
            .chunk(self.chunk_id)
            .ok_or(RuntimeError)?
            .get_function(self.function_id)
            .ok_or(RuntimeError)?
            .signature;

        if signature.upvalues.is_empty() {
            let r = Closure {
                fn_ptr: self,
                upvalues: Default::default(),
            };

            Ok(r)
        } else {
            Err(RuntimeError)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    fn_ptr: FunctionPtr,
    upvalues: IndexVec<UpvalueSlot, UpvalueId>,
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
    ) -> Result<Frame, RuntimeError>
    where
        C: ChunkCache,
    {
        use repr::chunk::Function;

        let function = rt
            .chunk_cache
            .chunk(self.fn_ptr.chunk_id)
            .ok_or(RuntimeError)?
            .get_function(self.fn_ptr.function_id)
            .ok_or(RuntimeError)?;

        let Function { signature, .. } = function;

        // Adjust stack, move varargs into register if needed.
        let stack_start = rt.stack.protected_size() + start;
        let call_height = StackSlot(0) + signature.height;
        let mut stack = rt.stack.view(stack_start).ok_or(RuntimeError)?;

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

#[derive(Debug)]
pub struct Frame {
    closure: ClosureRef,
    ip: InstrId,
    stack_start: RawStackSlot,
    upvalue_start: RawUpvalueSlot,
    register_variadic: Vec<Value>,
}

impl Frame {
    pub(crate) fn activate<'a, 'rt, C>(
        self,
        rt: &'a mut RuntimeView<'rt, C>,
    ) -> Result<ActiveFrame<'a>, RuntimeError>
    where
        C: ChunkCache,
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

        let chunk = chunk_cache.chunk(fn_ptr.chunk_id).ok_or(RuntimeError)?;
        let function = chunk.get_function(fn_ptr.function_id).ok_or(RuntimeError)?;

        let constants = &chunk.constants;
        let opcodes = &function.codes;
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
pub struct ActiveFrame<'rt> {
    closure: ClosureRef,
    chunk: &'rt Chunk,
    constants: &'rt IndexSlice<ConstId, Literal>,
    opcodes: &'rt IndexSlice<InstrId, OpCode>,
    ip: InstrId,
    stack: StackView<'rt>,
    upvalue_stack: UpvalueView<'rt>,
    register_variadic: Vec<Value>,
}

impl<'rt> ActiveFrame<'rt> {
    pub fn get_constant(&self, index: ConstId) -> Option<&Literal> {
        self.constants.get(index)
    }

    pub fn step(&mut self) -> Result<ControlFlow, RuntimeError> {
        use repr::opcode::OpCode::*;
        use repr::opcode::{AriBinOp, BinOp, BitBinOp, RelBinOp, StrBinOp, UnaOp};
        use repr::value::table::TableRef;

        let Some(code) = self.next_code() else {
            return Ok(ControlFlow::Break(ChangeFrame::Return(self.stack.top())));
        };

        let r = match code {
            Panic => return Err(RuntimeError),
            Invoke(slot) => {
                let closure = match self.stack.get(slot) {
                    Ok(Value::Function(closure)) => closure.clone(),
                    _ => return Err(RuntimeError),
                };

                ControlFlow::Break(ChangeFrame::Invoke(closure, slot))
            }
            Return(slot) => ControlFlow::Break(ChangeFrame::Return(slot)),
            LoadConstant(index) => {
                let constant = self.get_constant(index).ok_or(RuntimeError)?.clone();
                self.stack.push(constant.into());

                ControlFlow::Continue(())
            }
            LoadVariadic => {
                self.stack.extend(self.register_variadic.iter().cloned());

                ControlFlow::Continue(())
            }
            LoadStack(slot) => {
                let value = self.stack.get(slot)?.clone();
                self.stack.push(value);

                ControlFlow::Continue(())
            }
            StoreStack(slot) => {
                let value = self.stack.pop()?;
                let slot = self.stack.get_mut(slot)?;

                *slot = value;

                ControlFlow::Continue(())
            }
            AdjustStack(height) => {
                self.stack.adjust_height(height);

                ControlFlow::Continue(())
            }
            UnaOp(op) => {
                let val = self.stack.pop()?;

                let r = match op {
                    UnaOp::AriNeg => match val {
                        Value::Int(val) => Value::Int(-val),
                        Value::Float(val) => Value::Float(-val),
                        _ => return Err(RuntimeError),
                    },
                    UnaOp::BitNot => match val {
                        Value::Int(val) => Value::Int(!val),
                        _ => return Err(RuntimeError),
                    },
                    UnaOp::StrLen => match val {
                        Value::String(val) => Value::Int(val.len().try_into().unwrap()),
                        Value::Table(val) => {
                            let border = val.borrow().map_err(|_| RuntimeError)?.border();

                            Value::Int(border)
                        }
                        _ => return Err(RuntimeError),
                    },
                    UnaOp::LogNot => Value::Bool(!val.as_boolish()),
                };

                self.stack.push(r);

                ControlFlow::Continue(())
            }
            BinOp(BinOp::Ari(op)) => {
                let rhs = self.stack.pop()?;
                let lhs = self.stack.pop()?;

                let r = match (lhs, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => match op {
                        AriBinOp::Add => Value::Int(lhs.wrapping_add(rhs)),
                        AriBinOp::Sub => Value::Int(lhs.wrapping_sub(rhs)),
                        AriBinOp::Mul => Value::Int(lhs.wrapping_mul(rhs)),
                        AriBinOp::Rem => Value::Int(lhs.rem_euclid(rhs)),
                        AriBinOp::Div => {
                            let r = (lhs as f64) / (rhs as f64);
                            Value::Float(r)
                        }
                        AriBinOp::FloorDiv => {
                            let r = ((lhs as f64) / (rhs as f64)).floor();
                            Value::Float(r)
                        }
                        AriBinOp::Exp => {
                            let r = (lhs as f64).powf(rhs as f64);
                            Value::Float(r)
                        }
                    },
                    (Value::Float(lhs), Value::Float(rhs)) => match op {
                        AriBinOp::Add => Value::Float(lhs + rhs),
                        AriBinOp::Sub => Value::Float(lhs - rhs),
                        AriBinOp::Mul => Value::Float(lhs * rhs),
                        AriBinOp::Div => Value::Float(lhs / rhs),
                        AriBinOp::FloorDiv => Value::Float((lhs / rhs).floor()),
                        AriBinOp::Rem => Value::Float(lhs - rhs * (lhs / rhs).floor()),
                        AriBinOp::Exp => Value::Float(lhs.powf(rhs)),
                    },
                    _ => return Err(RuntimeError),
                };

                self.stack.push(r);

                ControlFlow::Continue(())
            }
            BinOp(BinOp::Bit(op)) => {
                let rhs = self.stack.pop()?;
                let lhs = self.stack.pop()?;

                let r = match (lhs, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => match op {
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
                    _ => return Err(RuntimeError),
                };

                self.stack.push(r);

                ControlFlow::Continue(())
            }
            BinOp(BinOp::Rel(op)) => {
                let rhs = self.stack.pop()?;
                let lhs = self.stack.pop()?;

                let r = match op {
                    RelBinOp::Eq => lhs == rhs,
                    RelBinOp::Neq => lhs != rhs,
                    RelBinOp::Lt => {
                        if lhs.type_() == rhs.type_() {
                            lhs < rhs
                        } else {
                            return Err(RuntimeError);
                        }
                    }
                    RelBinOp::Le => {
                        if lhs.type_() == rhs.type_() {
                            lhs <= rhs
                        } else {
                            return Err(RuntimeError);
                        }
                    }
                    RelBinOp::Gt => {
                        if lhs.type_() == rhs.type_() {
                            lhs > rhs
                        } else {
                            return Err(RuntimeError);
                        }
                    }
                    RelBinOp::Ge => {
                        if lhs.type_() == rhs.type_() {
                            lhs >= rhs
                        } else {
                            return Err(RuntimeError);
                        }
                    }
                };

                self.stack.push(Value::Bool(r));

                ControlFlow::Continue(())
            }
            BinOp(BinOp::Str(op)) => {
                let rhs = self.stack.pop()?;
                let lhs = self.stack.pop()?;

                let r = match (lhs, rhs) {
                    (Value::String(lhs), Value::String(rhs)) => match op {
                        StrBinOp::Concat => Value::String(lhs + &rhs),
                    },
                    _ => return Err(RuntimeError),
                };

                self.stack.push(r);

                ControlFlow::Continue(())
            }
            Jump { offset } => {
                self.ip += offset;

                ControlFlow::Continue(())
            }
            JumpIf { cond, offset } => {
                let value = self.stack.last()?;

                if value.as_boolish() == cond {
                    self.ip += offset;
                }

                ControlFlow::Continue(())
            }
            Loop { offset } => {
                self.ip = self.ip.checked_sub_offset(offset).ok_or(RuntimeError)?;

                ControlFlow::Continue(())
            }
            LoopIf { cond, offset } => {
                let value = self.stack.last()?;

                if value.as_boolish() == cond {
                    self.ip = self.ip.checked_sub_offset(offset).ok_or(RuntimeError)?;
                }

                ControlFlow::Continue(())
            }
            TabCreate => {
                let value = TableRef::new();

                self.stack.push(Value::Table(value));

                ControlFlow::Continue(())
            }
            TabGet => {
                let index = self.stack.pop()?;
                let table = match self.stack.pop()? {
                    Value::Table(t) => t,
                    _ => return Err(RuntimeError),
                };

                let key = index.try_into().map_err(|_| RuntimeError)?;
                let value = table.borrow().map_err(|_| RuntimeError)?.get(key);

                self.stack.push(value);

                ControlFlow::Continue(())
            }
            TabSet => {
                let value = self.stack.pop()?;
                let index = self.stack.pop()?;
                let table = match self.stack.pop()? {
                    Value::Table(t) => t,
                    _ => return Err(RuntimeError),
                };

                let key = index.try_into().map_err(|_| RuntimeError)?;

                table
                    .borrow_mut()
                    .map_err(|_| RuntimeError)?
                    .set(key, value);

                ControlFlow::Continue(())
            }
        };

        tracing::trace!(stack = ?self.stack, "executed opcode");

        Ok(r)
    }

    pub fn next_code(&mut self) -> Option<OpCode> {
        let r = *self.opcodes.get(self.ip)?;

        tracing::trace!(ip = self.ip.0, opcode = %r, "next opcode");

        self.ip += 1;

        Some(r)
    }

    pub fn construct_closure(&mut self, fn_id: FunctionId) -> Result<Closure, RuntimeError> {
        let fn_ptr = FunctionPtr {
            chunk_id: self.closure.fn_ptr.chunk_id,
            function_id: fn_id,
        };

        let signature = &self
            .chunk
            .get_function(fn_id)
            .ok_or(RuntimeError)?
            .signature;

        // To fix: remove this later when IndexVec is fixed.
        let mut upvalues = IndexVec::new();
        for value in signature.upvalues.iter().map(|&source| {
            use repr::chunk::UpvalueSource;

            match source {
                UpvalueSource::Temporary(slot) => self.stack.mark_as_upvalue(slot),
                UpvalueSource::Upvalue(slot) => {
                    self.closure.upvalues.get(slot).copied().ok_or(RuntimeError)
                }
            }
        }) {
            let value = value?;
            let _ = upvalues.push(value);
        }

        let r = Closure { fn_ptr, upvalues };

        Ok(r)
    }

    pub fn suspend(self) -> Frame {
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
