use std::error::Error;
use std::fmt::Display;

use crate::opcode::{Chunk, OpCode};
use crate::value::Value;

pub type ControlFlow = std::ops::ControlFlow<()>;

#[derive(Debug)]
pub struct RuntimeError;

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "runtime error")
    }
}

impl Error for RuntimeError {}

pub struct Runtime {
    chunk: Chunk,
    stack: Vec<Value>,
    ip: usize,
}

impl Runtime {
    pub fn new(chunk: Chunk) -> Self {
        tracing::trace!(chunk = %chunk, "constructed runtime");

        Runtime {
            chunk,
            stack: Default::default(),
            ip: 0,
        }
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        while let ControlFlow::Continue(()) = self.step()? {}

        Ok(())
    }

    pub fn step(&mut self) -> Result<ControlFlow, RuntimeError> {
        use crate::opcode::OpCode::*;
        use crate::opcode::{AriBinOp, AriUnaOp, BitUnaOp};

        let Some(code) = self.next_code() else {
            return Ok(ControlFlow::Break(()))
        };

        let r = match code {
            Return => ControlFlow::Break(()),
            LoadConstant(index) => {
                let constant = *self.chunk.get_constant(index).ok_or(RuntimeError)?;
                self.stack.push(constant.into());

                ControlFlow::Continue(())
            }
            AriUnaOp(op) => {
                let val = self.stack.pop().ok_or(RuntimeError)?;

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
                let rhs = self.stack.pop().ok_or(RuntimeError)?;
                let lhs = self.stack.pop().ok_or(RuntimeError)?;

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
                let val = self.stack.pop().ok_or(RuntimeError)?;

                let r = match val {
                    Value::Int(val) => match op {
                        BitUnaOp::Not => Value::Int(!val),
                    },
                    _ => return Err(RuntimeError),
                };

                self.stack.push(r);

                ControlFlow::Continue(())
            }
        };

        tracing::trace!(stack = ?self.stack, "executed opcode");

        Ok(r)
    }

    pub fn next_code(&mut self) -> Option<OpCode> {
        let r = *self.chunk.codes.get(self.ip)?;

        tracing::trace!(ip = self.ip, opcode = %r, "next opcode");

        self.ip += 1;

        Some(r)
    }
}
