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
        use crate::opcode::BinaryOp;
        use crate::opcode::OpCode::*;

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
            BinaryOp(op) => {
                let rhs = self.stack.pop().ok_or(RuntimeError)?;
                let lhs = self.stack.pop().ok_or(RuntimeError)?;

                let r = match (lhs, rhs) {
                    (Value::Uint(lhs), Value::Uint(rhs)) => match op {
                        BinaryOp::Add => Value::Uint(lhs.wrapping_add(rhs)),
                        BinaryOp::Sub => Value::Uint(lhs.wrapping_sub(rhs)),
                        BinaryOp::Mul => Value::Uint(lhs.wrapping_mul(rhs)),
                        BinaryOp::Rem => Value::Uint(lhs.rem_euclid(rhs)),
                        BinaryOp::Div => {
                            let r = (lhs as f64) / (rhs as f64);
                            Value::Float(r)
                        }
                        BinaryOp::FloorDiv => {
                            let r = ((lhs as f64) / (rhs as f64)).floor();
                            Value::Float(r)
                        }
                        BinaryOp::Exp => {
                            let r = (lhs as f64).powf(rhs as f64);
                            Value::Float(r)
                        }
                    },
                    (Value::Float(lhs), Value::Float(rhs)) => match op {
                        BinaryOp::Add => Value::Float(lhs + rhs),
                        BinaryOp::Sub => Value::Float(lhs - rhs),
                        BinaryOp::Mul => Value::Float(lhs * rhs),
                        BinaryOp::Div => Value::Float(lhs / rhs),
                        BinaryOp::FloorDiv => Value::Float((lhs / rhs).floor()),
                        BinaryOp::Rem => Value::Float(lhs - rhs * (lhs / rhs).floor()),
                        BinaryOp::Exp => Value::Float(lhs.powf(rhs)),
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
