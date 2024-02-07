pub mod already_dropped;
pub mod borrow;
pub mod diagnostic;
pub mod missing_chunk;
pub mod missing_function;
pub mod opcode;
pub mod out_of_bounds_stack;
pub mod upvalue_count_mismatch;
pub mod value;

use codespan_reporting::diagnostic::{Diagnostic as Message, Label};
use std::error::Error;
use std::fmt::{Debug, Display};

use crate::value::{TypeProvider, Value};

pub use crate::chunk_cache::ImmutableCacheError;
pub use already_dropped::AlreadyDroppedError;
pub use borrow::BorrowError;
pub use diagnostic::Diagnostic;
pub use missing_chunk::MissingChunk;
pub use missing_function::MissingFunction;
pub use opcode::Error as OpCodeError;
pub use out_of_bounds_stack::OutOfBoundsStack;
pub use upvalue_count_mismatch::UpvalueCountMismatch;
pub use value::ValueError;

pub enum RuntimeError<Gc: TypeProvider> {
    Value(ValueError<Gc>),
    Borrow(BorrowError),
    AlreadyDropped(AlreadyDroppedError),
    Immutable(ImmutableCacheError),
    Diagnostic(Diagnostic),
    MissingChunk(MissingChunk),
    MissingFunction(MissingFunction),
    OutOfBoundsStack(OutOfBoundsStack),
    UpvalueCountMismatch(UpvalueCountMismatch),
    OpCode(OpCodeError),
}

impl<Gc: TypeProvider> From<Value<Gc>> for RuntimeError<Gc> {
    fn from(value: Value<Gc>) -> Self {
        RuntimeError::Value(ValueError(value))
    }
}

impl<Gc> From<BorrowError> for RuntimeError<Gc>
where
    Gc: TypeProvider,
{
    fn from(value: BorrowError) -> Self {
        Self::Borrow(value)
    }
}

impl<Gc> From<AlreadyDroppedError> for RuntimeError<Gc>
where
    Gc: TypeProvider,
{
    fn from(value: AlreadyDroppedError) -> Self {
        Self::AlreadyDropped(value)
    }
}

impl<Gc: TypeProvider> From<MissingChunk> for RuntimeError<Gc> {
    fn from(value: MissingChunk) -> Self {
        RuntimeError::MissingChunk(value)
    }
}

impl<Gc: TypeProvider> From<MissingFunction> for RuntimeError<Gc> {
    fn from(value: MissingFunction) -> Self {
        RuntimeError::MissingFunction(value)
    }
}

impl<Gc: TypeProvider> From<OutOfBoundsStack> for RuntimeError<Gc> {
    fn from(value: OutOfBoundsStack) -> Self {
        RuntimeError::OutOfBoundsStack(value)
    }
}

impl<Gc: TypeProvider> From<UpvalueCountMismatch> for RuntimeError<Gc> {
    fn from(value: UpvalueCountMismatch) -> Self {
        RuntimeError::UpvalueCountMismatch(value)
    }
}

impl<Gc: TypeProvider> From<ValueError<Gc>> for RuntimeError<Gc> {
    fn from(value: ValueError<Gc>) -> Self {
        RuntimeError::Value(value)
    }
}

impl<Gc: TypeProvider> From<OpCodeError> for RuntimeError<Gc> {
    fn from(value: OpCodeError) -> Self {
        RuntimeError::OpCode(value)
    }
}

impl<Gc> Debug for RuntimeError<Gc>
where
    Gc: TypeProvider,
    Value<Gc>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(arg0) => f.debug_tuple("Value").field(arg0).finish(),
            Self::Borrow(arg0) => f.debug_tuple("Borrow").field(arg0).finish(),
            Self::AlreadyDropped(arg0) => f.debug_tuple("AlreadyDropped").field(arg0).finish(),
            Self::Immutable(arg0) => f.debug_tuple("Immutable").field(arg0).finish(),
            Self::Diagnostic(arg0) => f.debug_tuple("Diagnostic").field(arg0).finish(),
            Self::MissingChunk(arg0) => f.debug_tuple("MissingChunk").field(arg0).finish(),
            Self::MissingFunction(arg0) => f.debug_tuple("MissingFunction").field(arg0).finish(),
            Self::OutOfBoundsStack(arg0) => f.debug_tuple("OutOfBoundsStack").field(arg0).finish(),
            Self::UpvalueCountMismatch(arg0) => {
                f.debug_tuple("UpvalueCountMismatch").field(arg0).finish()
            }
            Self::OpCode(arg0) => f.debug_tuple("OpCode").field(arg0).finish(),
        }
    }
}

impl<Gc: TypeProvider> Display for RuntimeError<Gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "runtime error")
    }
}

impl<Gc: TypeProvider> Error for RuntimeError<Gc> where Self: Debug + Display {}

#[derive(Debug)]
pub enum DroppedOrBorrowedError {
    Dropped(AlreadyDroppedError),
    Borrowed(BorrowError),
}

impl Display for DroppedOrBorrowedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dropped(err) => write!(f, "{err}"),
            Self::Borrowed(err) => write!(f, "{err}"),
        }
    }
}

impl Error for DroppedOrBorrowedError {}

impl From<AlreadyDroppedError> for DroppedOrBorrowedError {
    fn from(value: AlreadyDroppedError) -> Self {
        DroppedOrBorrowedError::Dropped(value)
    }
}

impl From<BorrowError> for DroppedOrBorrowedError {
    fn from(value: BorrowError) -> Self {
        DroppedOrBorrowedError::Borrowed(value)
    }
}

impl<Gc> From<DroppedOrBorrowedError> for RuntimeError<Gc>
where
    Gc: TypeProvider,
{
    fn from(value: DroppedOrBorrowedError) -> Self {
        match value {
            DroppedOrBorrowedError::Dropped(err) => err.into(),
            DroppedOrBorrowedError::Borrowed(err) => err.into(),
        }
    }
}

trait ExtraDiagnostic<FileId> {
    fn with_label(&mut self, iter: impl IntoIterator<Item = Label<FileId>>);
    fn with_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
    fn with_help(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
    fn with_compiler_bug_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
    fn with_runtime_bug_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
    fn no_debug_info(&mut self);
}

impl<FileId> ExtraDiagnostic<FileId> for Message<FileId> {
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
