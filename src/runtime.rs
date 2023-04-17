use crate::opcode::{Chunk, OpCode};
use crate::value::Value;

pub type ControlFlow = std::ops::ControlFlow<()>;

pub struct RuntimeError;

pub struct Runtime {
    chunk: Chunk,
    stack: Vec<Value>,
    ip: usize,
}

impl Runtime {
    pub fn run(&mut self) -> Result<(), RuntimeError> {
        while let ControlFlow::Continue(()) = self.step()? {}

        Ok(())
    }

    pub fn step(&mut self) -> Result<ControlFlow, RuntimeError> {
        use crate::opcode::OpCode::*;

        let Some(code) = self.next_code() else {
            return Ok(ControlFlow::Break(()))
        };

        let r = match code {
            Return => ControlFlow::Break(()),
            LoadConstant(index) => {
                let constant = *self.chunk.get_constant(index).ok_or(RuntimeError)?;
                self.stack.push(constant);

                ControlFlow::Continue(())
            }
        };

        Ok(r)
    }

    pub fn next_code(&mut self) -> Option<OpCode> {
        let r = *self.chunk.codes.get(self.ip)?;
        self.ip += 1;

        Some(r)
    }
}
