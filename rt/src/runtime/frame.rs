use repr::index::{ConstId, FunctionId, InstrId, StackSlot};
use repr::index_vec::IndexSlice;
use repr::literal::Literal;
use repr::opcode::OpCode;
use repr::value::Value;

use super::stack::{ProtectedSize, StackView};
use super::Runtime;
use crate::chunk_cache::{ChunkCache, FunctionPtr};
use crate::RuntimeError;

pub type ControlFlow = std::ops::ControlFlow<ChangeFrame>;

pub enum ChangeFrame {
    Return(StackSlot),
    Invoke(FunctionId, StackSlot),
}

#[derive(Debug)]
pub struct Frame {
    pub(crate) function_ptr: FunctionPtr,
    pub(crate) ip: InstrId,
    pub(crate) stack_start: ProtectedSize,
}

impl Frame {
    pub fn activate<C>(self, rt: &mut Runtime<C>) -> Result<ActiveFrame, RuntimeError>
    where
        C: ChunkCache,
    {
        let Runtime {
            chunk_cache, stack, ..
        } = rt;

        let Frame {
            function_ptr,
            ip,
            stack_start,
        } = self;

        let chunk = chunk_cache
            .chunk(function_ptr.chunk_id)
            .ok_or(RuntimeError)?;
        let function = chunk
            .get_function(function_ptr.function_id)
            .ok_or(RuntimeError)?;

        let constants = &chunk.constants;
        let opcodes = &function.codes;
        let stack = StackView::new(stack, stack_start).unwrap();

        let r = ActiveFrame {
            function_ptr,
            constants,
            opcodes,
            ip,
            stack,
        };

        Ok(r)
    }
}

#[derive(Debug)]
pub struct ActiveFrame<'rt> {
    function_ptr: FunctionPtr,
    constants: &'rt IndexSlice<ConstId, Literal>,
    opcodes: &'rt IndexSlice<InstrId, OpCode>,
    ip: InstrId,
    stack: StackView<'rt>,
}

impl<'rt> ActiveFrame<'rt> {
    pub fn get_constant(&self, index: ConstId) -> Option<&Literal> {
        self.constants.get(index)
    }

    pub fn step(&mut self) -> Result<ControlFlow, RuntimeError> {
        use repr::opcode::OpCode::*;
        use repr::opcode::{AriBinOp, AriUnaOp, BitBinOp, BitUnaOp, RelBinOp, StrBinOp};
        use repr::table::TableRef;

        let Some(code) = self.next_code() else {
            return Ok(ControlFlow::Break(ChangeFrame::Return(self.stack.top())))
        };

        let r = match code {
            Panic => return Err(RuntimeError),
            Invoke(slot) => {
                let func_id = match self.stack.get(slot) {
                    Ok(Value::Function(func_id)) => *func_id,
                    _ => return Err(RuntimeError),
                };

                ControlFlow::Break(ChangeFrame::Invoke(func_id, slot))
            }
            Return(slot) => ControlFlow::Break(ChangeFrame::Return(slot)),
            LoadConstant(index) => {
                let constant = self.get_constant(index).ok_or(RuntimeError)?.clone();
                self.stack.push(constant.into());

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
            AriUnaOp(op) => {
                let val = self.stack.pop()?;

                let r = match val {
                    Value::Int(val) => match op {
                        AriUnaOp::Neg => Value::Int(-val),
                    },
                    Value::Float(val) => match op {
                        AriUnaOp::Neg => Value::Float(-val),
                    },
                    _ => return Err(RuntimeError),
                };

                self.stack.push(r);

                ControlFlow::Continue(())
            }
            AriBinOp(op) => {
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
            BitUnaOp(op) => {
                let val = self.stack.pop()?;

                let r = match val {
                    Value::Int(val) => match op {
                        BitUnaOp::Not => Value::Int(!val),
                    },
                    _ => return Err(RuntimeError),
                };

                self.stack.push(r);

                ControlFlow::Continue(())
            }
            BitBinOp(op) => {
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
            RelBinOp(op) => {
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
            StrBinOp(op) => {
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

    pub fn suspend(self) -> Frame {
        let ActiveFrame {
            function_ptr,
            ip,
            stack,
            ..
        } = self;

        let stack_start = stack.protected_size();

        Frame {
            function_ptr,
            ip,
            stack_start,
        }
    }

    pub fn exit(mut self, drop_under: StackSlot) -> Result<(), RuntimeError> {
        self.stack.drop_under(drop_under)
    }
}
