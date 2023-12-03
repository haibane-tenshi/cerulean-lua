pub mod opcode;

use codespan_reporting::diagnostic::Diagnostic;
use std::error::Error;
use std::fmt::Display;

use opcode::Error as OpCodeError;

#[derive(Debug)]
pub enum RuntimeError {
    CatchAll,
    OpCode(OpCodeError),
}

impl RuntimeError {
    pub fn into_diagnostic<FileId>(self, file_id: FileId) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use RuntimeError::*;

        match self {
            CatchAll => Diagnostic::error().with_message("runtime error occurred"),
            OpCode(err) => err.into_diagnostic(file_id),
        }
    }
}

impl From<OpCodeError> for RuntimeError {
    fn from(value: OpCodeError) -> Self {
        RuntimeError::OpCode(value)
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "runtime error")
    }
}

impl Error for RuntimeError {}
