pub mod opcode;

use std::error::Error;
use std::fmt::Display;

use opcode::Error as OpCodeError;

#[derive(Debug)]
pub enum RuntimeError {
    CatchAll,
    OpCode(OpCodeError),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "runtime error")
    }
}

impl Error for RuntimeError {}
