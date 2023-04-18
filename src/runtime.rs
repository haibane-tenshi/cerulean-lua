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

        let Some(code) = self.next_code() else {
            return Ok(ControlFlow::Break(()))
        };

        tracing::trace!(stack = ?self.stack, "executing opcode");

        let r = match code {
            Return => ControlFlow::Break(()),
            LoadConstant(index) => {
                let constant = *self.chunk.get_constant(index).ok_or(RuntimeError)?;
                self.stack.push(constant.into());

                ControlFlow::Continue(())
            }
        };

        Ok(r)
    }

    pub fn next_code(&mut self) -> Option<OpCode> {
        let r = *self.chunk.codes.get(self.ip)?;

        tracing::trace!(ip = self.ip, opcode = %r, "next opcode");

        self.ip += 1;

        Some(r)
    }
}
