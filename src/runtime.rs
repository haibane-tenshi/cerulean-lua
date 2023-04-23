use std::error::Error;
use std::fmt::Display;

use crate::opcode::{Chunk, ConstId, FunctionId, OpCode, StackSlot};
use crate::value::{Literal, Value};

pub type ControlFlow = std::ops::ControlFlow<ControlFrame>;

pub enum ControlFrame {
    Return(StackSlot),
    Invoke(FunctionId, StackSlot),
}

#[derive(Debug)]
pub struct RuntimeError;

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "runtime error")
    }
}

impl Error for RuntimeError {}

#[derive(Debug, Default)]
struct Stack {
    stack: Vec<Value>,
    protected_size: usize,
}

impl Stack {
    fn index(&self, slot: StackSlot) -> Result<usize, RuntimeError> {
        let offset: usize = slot.0.try_into().map_err(|_| RuntimeError)?;
        let index = self.protected_size + offset;

        Ok(index)
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    pub fn pop(&mut self) -> Result<Value, RuntimeError> {
        if self.stack.len() <= self.protected_size {
            return Err(RuntimeError);
        }

        self.stack.pop().ok_or(RuntimeError)
    }

    pub fn top(&mut self) -> StackSlot {
        let offset = self.stack.len() - self.protected_size;
        let offset = offset.try_into().unwrap();

        StackSlot(offset)
    }

    pub fn get(&self, slot: StackSlot) -> Result<&Value, RuntimeError> {
        let index = self.index(slot)?;
        self.stack.get(index).ok_or(RuntimeError)
    }

    pub fn get_mut(&mut self, slot: StackSlot) -> Result<&mut Value, RuntimeError> {
        let index = self.index(slot)?;
        self.stack.get_mut(index).ok_or(RuntimeError)
    }

    pub fn set_protected_size(&mut self, protected_size: usize) -> Result<(), RuntimeError> {
        if self.stack.len() < protected_size {
            return Err(RuntimeError);
        }

        self.protected_size = protected_size;

        Ok(())
    }

    pub fn protect_from(&mut self, slot: StackSlot) -> Result<usize, RuntimeError> {
        self.protected_size = self.index(slot)?;
        Ok(self.protected_size)
    }

    pub fn adjust_height(&mut self, height: u32) {
        use std::cmp::Ordering;

        let height: usize = height.try_into().unwrap();
        let requested_height = self.protected_size + height;

        match requested_height.cmp(&self.stack.len()) {
            Ordering::Equal => (),
            Ordering::Greater => self
                .stack
                .extend(std::iter::repeat(Value::Nil).take(requested_height - self.stack.len())),
            Ordering::Less => {
                self.stack.truncate(requested_height);
            }
        }
    }

    pub fn drop_under(&mut self, slot: StackSlot) -> Result<(), RuntimeError> {
        let height = self.index(slot)?;
        self.stack.drain(self.protected_size..height);

        Ok(())
    }
}

#[derive(Debug, Default)]
struct Frame<'chunk> {
    constants: &'chunk [Literal],
    codes: &'chunk [OpCode],
    ip: usize,
    stack_start: usize,
}

impl<'chunk> Frame<'chunk> {
    pub fn get_constant(&self, index: ConstId) -> Option<&Literal> {
        let index: usize = index.0.try_into().ok()?;
        self.constants.get(index)
    }
}

#[derive(Debug, Default)]
struct CurrentFrame<'chunk> {
    frame: Frame<'chunk>,
    stack: Stack,
}

impl<'chunk> CurrentFrame<'chunk> {
    pub fn step(&mut self) -> Result<ControlFlow, RuntimeError> {
        use crate::opcode::OpCode::*;
        use crate::opcode::{AriBinOp, AriUnaOp, BitBinOp, BitUnaOp, RelBinOp, StrBinOp};

        let Some(code) = self.next_code() else {
            return Ok(ControlFlow::Break(ControlFrame::Return(self.stack.top())))
        };

        let r = match code {
            Invoke(slot) => {
                let func_id = match self.stack.get(slot) {
                    Ok(Value::Function(func_id)) => *func_id,
                    _ => return Err(RuntimeError),
                };

                ControlFlow::Break(ControlFrame::Invoke(func_id, slot))
            }
            Return(slot) => ControlFlow::Break(ControlFrame::Return(slot)),
            LoadConstant(index) => {
                let constant = self.frame.get_constant(index).ok_or(RuntimeError)?.clone();
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
            AdjustStack(StackSlot(height)) => {
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
                    RelBinOp::Le => {
                        if lhs.type_() == rhs.type_() {
                            lhs < rhs
                        } else {
                            return Err(RuntimeError);
                        }
                    }
                    RelBinOp::Lt => {
                        if lhs.type_() == rhs.type_() {
                            lhs <= rhs
                        } else {
                            return Err(RuntimeError);
                        }
                    }
                    RelBinOp::Ge => {
                        if lhs.type_() == rhs.type_() {
                            lhs > rhs
                        } else {
                            return Err(RuntimeError);
                        }
                    }
                    RelBinOp::Gt => {
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
                let offset: usize = offset.try_into().map_err(|_| RuntimeError)?;
                self.frame.ip += offset;

                ControlFlow::Continue(())
            }
            JumpIf { cond, offset } => {
                let value = self.stack.pop()?;

                if value.as_boolish() == cond {
                    let offset: usize = offset.try_into().map_err(|_| RuntimeError)?;
                    self.frame.ip += offset;
                }

                ControlFlow::Continue(())
            }
            Loop { offset } => {
                let offset: usize = offset.try_into().map_err(|_| RuntimeError)?;
                self.frame.ip = self.frame.ip.checked_sub(offset).ok_or(RuntimeError)?;

                ControlFlow::Continue(())
            }
            LoopIf { cond, offset } => {
                let value = self.stack.pop()?;

                if value.as_boolish() == cond {
                    let offset: usize = offset.try_into().map_err(|_| RuntimeError)?;
                    self.frame.ip = self.frame.ip.checked_sub(offset).ok_or(RuntimeError)?;
                }

                ControlFlow::Continue(())
            }
        };

        tracing::trace!(stack = ?self.stack, "executed opcode");

        Ok(r)
    }

    pub fn next_code(&mut self) -> Option<OpCode> {
        let r = *self.frame.codes.get(self.frame.ip)?;

        tracing::trace!(ip = self.frame.ip, opcode = %r, "next opcode");

        self.frame.ip += 1;

        Some(r)
    }

    pub fn push(&mut self, frame: Frame<'chunk>) -> Result<(), RuntimeError> {
        self.frame = frame;
        self.stack.set_protected_size(self.frame.stack_start)
    }
}

pub struct Runtime<'chunk> {
    chunk: &'chunk Chunk,
    suspended: Vec<Frame<'chunk>>,
    current: CurrentFrame<'chunk>,
}

impl<'chunk> Runtime<'chunk> {
    pub fn new(chunk: &'chunk Chunk) -> Self {
        tracing::trace!(chunk = %chunk, "constructed runtime");

        let codes = chunk
            .functions
            .first()
            .map(|fun| fun.codes.as_slice())
            .unwrap_or_default();

        let frame = Frame {
            constants: &chunk.constants,
            codes,
            ip: 0,
            stack_start: 0,
        };

        let current = CurrentFrame {
            frame,
            stack: Default::default(),
        };

        Runtime {
            chunk,
            suspended: Default::default(),
            current,
        }
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            match self.current.step()? {
                ControlFlow::Break(ControlFrame::Return(slot)) => {
                    self.current.stack.drop_under(slot)?;

                    let Some(frame) = self.suspended.pop() else {
                        // If we hit an early return from script, that leaves us pointing at
                        // some arbitrary instructions immediately after.
                        // If the user attempts to resume execution it can lead to bogged state.
                        // To prevent that push in a dummy (empty) frame.
                        self.current.push(Default::default())?;
                        break
                    };

                    self.current.push(frame)?;
                }
                ControlFlow::Break(ControlFrame::Invoke(func_id, slot)) => {
                    let func = self.chunk.get_function(func_id).ok_or(RuntimeError)?;
                    let codes = &func.codes;
                    let constants = &self.chunk.constants;
                    let stack_start = self.current.stack.protect_from(slot)?;
                    self.current.stack.adjust_height(func.height);

                    let mut frame = Frame {
                        codes,
                        constants,
                        ip: 0,
                        stack_start,
                    };

                    std::mem::swap(&mut frame, &mut self.current.frame);
                    self.suspended.push(frame);
                }
                ControlFlow::Continue(()) => (),
            }
        }

        Ok(())
    }
}
