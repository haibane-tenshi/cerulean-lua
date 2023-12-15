pub mod missing_chunk;
pub mod missing_function;
pub mod opcode;
pub mod out_of_bounds_stack;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::error::Error;
use std::fmt::Display;

pub use missing_chunk::MissingChunk;
pub use missing_function::MissingFunction;
pub use opcode::Error as OpCodeError;
pub use out_of_bounds_stack::OutOfBoundsStack;

#[derive(Debug)]
pub enum RuntimeError {
    CatchAll,
    MissingChunk(MissingChunk),
    MissingFunction(MissingFunction),
    OutOfBoundsStack(OutOfBoundsStack),
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
            MissingChunk(err) => err.into_diagnostic(),
            MissingFunction(err) => err.into_diagnostic(),
            OutOfBoundsStack(err) => err.into_diagnostic(),
            OpCode(err) => err.into_diagnostic(file_id),
        }
    }
}

impl From<MissingChunk> for RuntimeError {
    fn from(value: MissingChunk) -> Self {
        RuntimeError::MissingChunk(value)
    }
}

impl From<MissingFunction> for RuntimeError {
    fn from(value: MissingFunction) -> Self {
        RuntimeError::MissingFunction(value)
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

trait ExtraDiagnostic<FileId> {
    fn with_label(&mut self, iter: impl IntoIterator<Item = Label<FileId>>);
    fn with_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
    fn with_help(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
    fn with_compiler_bug_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
    fn with_runtime_bug_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
    fn no_debug_info(&mut self);
}

impl<FileId> ExtraDiagnostic<FileId> for Diagnostic<FileId> {
    fn with_label(&mut self, iter: impl IntoIterator<Item = Label<FileId>>) {
        self.labels.extend(iter)
    }

    fn with_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>) {
        self.notes
            .extend(iter.into_iter().map(|s| format!("note: {}", s.as_ref())));
    }

    fn with_help(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>) {
        self.notes
            .extend(iter.into_iter().map(|s| format!("help: {}", s.as_ref())));
    }

    fn with_compiler_bug_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>) {
        self.with_note(["this is not bug in your Lua code, error is caused by malformed bytecode"]);
        self.with_note(iter);
        self.with_note(["this most likely resulted from bug in compiler and should be reported"]);
    }

    fn with_runtime_bug_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>) {
        self.with_note([
            "this is not bug in your Lua code, error is caused by imporperly executed bytecode",
        ]);
        self.with_note(iter);
        self.with_note(["this most likely resulted from bug in runtime and should be reported"]);
    }

    fn no_debug_info(&mut self) {
        self.with_note([
                "no debug info is available, it is possible debug info was stripped",
                "it is also possible that erroneous bytecode was handcrafted\nplease check with where you got it",
            ]);
    }
}
