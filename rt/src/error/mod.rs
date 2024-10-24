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

use gc::Root;

use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::gc::{DisplayWith, Heap};
use crate::runtime::Interned;
use crate::value::{CoreTypes, Strong, StrongValue, Types, Value};

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

pub type RtError<Ty> = RuntimeError<Value<Strong, Ty>>;

pub enum RuntimeError<Value> {
    Value(ValueError<Value>),
    Borrow(BorrowError),
    AlreadyDropped(AlreadyDroppedError),
    Immutable(ImmutableCacheError),
    Diagnostic(Box<Diagnostic>),
    MissingChunk(MissingChunk),
    MissingFunction(MissingFunction),
    OutOfBoundsStack(OutOfBoundsStack),
    UpvalueCountMismatch(UpvalueCountMismatch),
    OpCode(OpCodeError),
}

impl<Rf, Ty> RuntimeError<Value<Rf, Ty>>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::String<Ty::String>: From<Root<Interned<Ty::String>>>,
{
    pub fn from_msg(msg: Root<Interned<Ty::String>>) -> Self {
        RuntimeError::Value(ValueError(Value::String(msg.into())))
    }
}

impl<Value> RuntimeError<Value> {
    pub fn from_value(value: Value) -> Self {
        RuntimeError::Value(ValueError(value))
    }

    fn chunk_id(&self) -> Option<ChunkId> {
        if let RuntimeError::OpCode(err) = self {
            Some(err.fn_ptr.chunk_id)
        } else {
            None
        }
    }
}

impl<Ty> RuntimeError<StrongValue<Ty>>
where
    Ty: CoreTypes,
{
    pub fn into_diagnostic(self, heap: &Heap<Ty>, chunk_cache: &dyn ChunkCache) -> Diagnostic
    where
        Ty::String: AsRef<[u8]>,
        StrongValue<Ty>: DisplayWith<Heap<Ty>>,
    {
        use codespan_reporting::files::SimpleFile;

        let chunk_id = self.chunk_id();

        let message = match self {
            RuntimeError::Value(err) => err.into_diagnostic(heap),
            RuntimeError::Borrow(err) => err.into_diagnostic(),
            RuntimeError::AlreadyDropped(err) => err.into_diagnostic(),
            RuntimeError::Immutable(err) => err.into_diagnostic(),
            RuntimeError::Diagnostic(diag) => return *diag,
            RuntimeError::MissingChunk(err) => err.into_diagnostic(),
            RuntimeError::MissingFunction(err) => err.into_diagnostic(),
            RuntimeError::OutOfBoundsStack(err) => err.into_diagnostic(),
            RuntimeError::UpvalueCountMismatch(err) => err.into_diagnostic(),
            RuntimeError::OpCode(err) => err.into_diagnostic((), chunk_cache),
        };

        let source = chunk_id
            .and_then(|id| chunk_cache.source(id))
            .unwrap_or_default();
        let name = chunk_id
            .and_then(|id| chunk_cache.location(id).map(|location| location.file))
            .unwrap_or_else(|| "<unnamed>".to_string());

        let files = SimpleFile::new(name, source);

        Diagnostic { files, message }
    }
}

impl<Value> From<BorrowError> for RuntimeError<Value> {
    fn from(value: BorrowError) -> Self {
        Self::Borrow(value)
    }
}

impl<Value> From<AlreadyDroppedError> for RuntimeError<Value> {
    fn from(value: AlreadyDroppedError) -> Self {
        Self::AlreadyDropped(value)
    }
}

impl<Value> From<MissingChunk> for RuntimeError<Value> {
    fn from(value: MissingChunk) -> Self {
        RuntimeError::MissingChunk(value)
    }
}

impl<Value> From<MissingFunction> for RuntimeError<Value> {
    fn from(value: MissingFunction) -> Self {
        RuntimeError::MissingFunction(value)
    }
}

impl<Value> From<OutOfBoundsStack> for RuntimeError<Value> {
    fn from(value: OutOfBoundsStack) -> Self {
        RuntimeError::OutOfBoundsStack(value)
    }
}

impl<Value> From<UpvalueCountMismatch> for RuntimeError<Value> {
    fn from(value: UpvalueCountMismatch) -> Self {
        RuntimeError::UpvalueCountMismatch(value)
    }
}

impl<Value> From<ValueError<Value>> for RuntimeError<Value> {
    fn from(value: ValueError<Value>) -> Self {
        RuntimeError::Value(value)
    }
}

impl<Value> From<OpCodeError> for RuntimeError<Value> {
    fn from(value: OpCodeError) -> Self {
        RuntimeError::OpCode(value)
    }
}

impl<Value> Debug for RuntimeError<Value>
where
    Value: Debug,
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

impl<Value> Display for RuntimeError<Value> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "runtime error")
    }
}

impl<Value> Error for RuntimeError<Value> where Self: Debug + Display {}

#[derive(Debug)]
pub enum RefAccessError {
    Dropped(AlreadyDroppedError),
    Borrowed(BorrowError),
}

impl Display for RefAccessError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dropped(err) => write!(f, "{err}"),
            Self::Borrowed(err) => write!(f, "{err}"),
        }
    }
}

impl Error for RefAccessError {}

impl From<AlreadyDroppedError> for RefAccessError {
    fn from(value: AlreadyDroppedError) -> Self {
        RefAccessError::Dropped(value)
    }
}

impl From<BorrowError> for RefAccessError {
    fn from(value: BorrowError) -> Self {
        RefAccessError::Borrowed(value)
    }
}

impl<Value> From<RefAccessError> for RuntimeError<Value> {
    fn from(value: RefAccessError) -> Self {
        match value {
            RefAccessError::Dropped(err) => err.into(),
            RefAccessError::Borrowed(err) => err.into(),
        }
    }
}

pub(crate) trait ExtraDiagnostic<FileId> {
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
