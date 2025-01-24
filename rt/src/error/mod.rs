pub mod already_dropped;
pub mod borrow;
mod closure;
pub mod diagnostic;
pub mod invalid_key;
pub mod not_callable;
pub mod not_text;
pub mod opcode;
pub mod out_of_bounds_stack;
pub mod signature;
pub mod thread;
pub mod value;

use codespan_reporting::diagnostic::{Diagnostic as Message, Label};
use std::error::Error;
use std::fmt::{Debug, Display};

use gc::{Interned, Root};

use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::gc::Heap;
use crate::value::{StrongValue, Types, Value};

pub use crate::chunk_cache::ImmutableCacheError;
pub use already_dropped::{AlreadyDroppedError, AlreadyDroppedOr};
pub use borrow::BorrowError;
pub use closure::{CapturesMismatch, MalformedClosureError, MissingChunk, MissingFunction};
pub use diagnostic::Diagnostic;
pub use invalid_key::InvalidKeyError;
pub use not_callable::NotCallableError;
pub use not_text::NotTextError;
pub use opcode::Error as OpCodeError;
pub use out_of_bounds_stack::OutOfBoundsStack;
pub use signature::SignatureError;
pub use thread::{ReentryFailure, ThreadError, ThreadPanicked};
pub use value::ValueError;

pub type RtError<Ty> = RuntimeError<Ty>;

pub enum RuntimeError<Ty>
where
    Ty: Types,
{
    Value(ValueError<Ty>),
    Borrow(BorrowError),
    AlreadyDropped(AlreadyDroppedError),
    InvalidKey(InvalidKeyError),
    Immutable(ImmutableCacheError),
    Diagnostic(Box<Diagnostic>),
    MissingChunk(MissingChunk),
    MissingFunction(MissingFunction),
    OutOfBoundsStack(OutOfBoundsStack),
    UpvalueCountMismatch(CapturesMismatch),
    Signature(SignatureError),
    NotCallable(NotCallableError<Ty>),
    NotText(NotTextError<Ty::String>),
    Thread(ThreadError),
    OpCode(OpCodeError),
}

impl<Ty> RuntimeError<Ty>
where
    Ty: Types,
{
    pub fn from_msg(msg: Root<Interned<Ty::String>>) -> Self {
        RuntimeError::Value(ValueError(Value::String(msg.into())))
    }

    pub fn from_value(value: StrongValue<Ty>) -> Self {
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

impl<Ty> RuntimeError<Ty>
where
    Ty: Types,
{
    pub fn into_diagnostic(self, heap: &Heap<Ty>, chunk_cache: &dyn ChunkCache) -> Diagnostic {
        use codespan_reporting::files::SimpleFile;

        let chunk_id = self.chunk_id();

        let message = match self {
            RuntimeError::Value(err) => err.into_diagnostic(heap),
            RuntimeError::Borrow(err) => err.into_diagnostic(),
            RuntimeError::AlreadyDropped(err) => err.into_diagnostic(),
            RuntimeError::InvalidKey(err) => err.into_diagnostic(),
            RuntimeError::Immutable(err) => err.into_diagnostic(),
            RuntimeError::Diagnostic(diag) => return *diag,
            RuntimeError::MissingChunk(err) => err.into_diagnostic(),
            RuntimeError::MissingFunction(err) => err.into_diagnostic(),
            RuntimeError::OutOfBoundsStack(err) => err.into_diagnostic(),
            RuntimeError::UpvalueCountMismatch(err) => err.into_diagnostic(),
            RuntimeError::Signature(err) => err.into_diagnostic(),
            RuntimeError::NotCallable(err) => err.into_diagnostic(),
            RuntimeError::NotText(err) => err.into_diagnostic(),
            RuntimeError::Thread(err) => err.into_diagnostic(),
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

impl<Ty> From<AlreadyDroppedError> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: AlreadyDroppedError) -> Self {
        Self::AlreadyDropped(value)
    }
}

impl<Ty, E> From<AlreadyDroppedOr<E>> for RuntimeError<Ty>
where
    Ty: Types,
    E: Into<Self>,
{
    fn from(value: AlreadyDroppedOr<E>) -> Self {
        match value {
            AlreadyDroppedOr::Dropped(err) => RuntimeError::AlreadyDropped(err),
            AlreadyDroppedOr::Other(err) => err.into(),
        }
    }
}

impl<Ty> From<BorrowError> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: BorrowError) -> Self {
        Self::Borrow(value)
    }
}

impl<Ty> From<MissingChunk> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: MissingChunk) -> Self {
        RuntimeError::MissingChunk(value)
    }
}

impl<Ty> From<MissingFunction> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: MissingFunction) -> Self {
        RuntimeError::MissingFunction(value)
    }
}

impl<Ty> From<OutOfBoundsStack> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: OutOfBoundsStack) -> Self {
        RuntimeError::OutOfBoundsStack(value)
    }
}

impl<Ty> From<CapturesMismatch> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: CapturesMismatch) -> Self {
        RuntimeError::UpvalueCountMismatch(value)
    }
}

impl<Ty> From<ValueError<Ty>> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: ValueError<Ty>) -> Self {
        RuntimeError::Value(value)
    }
}

impl<Ty> From<OpCodeError> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: OpCodeError) -> Self {
        RuntimeError::OpCode(value)
    }
}

impl<Ty> From<ThreadError> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: ThreadError) -> Self {
        RuntimeError::Thread(value)
    }
}

impl<Ty> From<SignatureError> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: SignatureError) -> Self {
        RuntimeError::Signature(value)
    }
}

impl<E, Ty> From<crate::ffi::arg_parser::ParseError<E>> for RuntimeError<Ty>
where
    Ty: Types,
    E: Display,
{
    fn from(value: crate::ffi::arg_parser::ParseError<E>) -> Self {
        SignatureError::from(value).into()
    }
}

impl<Ty> From<NotCallableError<Ty>> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: NotCallableError<Ty>) -> Self {
        RuntimeError::NotCallable(value)
    }
}

impl<Ty> From<NotTextError<Ty::String>> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: NotTextError<Ty::String>) -> Self {
        RuntimeError::NotText(value)
    }
}

impl<Ty> From<MalformedClosureError> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: MalformedClosureError) -> Self {
        match value {
            MalformedClosureError::MissingChunk(err) => RuntimeError::MissingChunk(err),
            MalformedClosureError::MissingFunction(err) => RuntimeError::MissingFunction(err),
            MalformedClosureError::CapturesMismatch(err) => RuntimeError::UpvalueCountMismatch(err),
        }
    }
}

impl<Ty> From<InvalidKeyError> for RuntimeError<Ty>
where
    Ty: Types,
{
    fn from(value: InvalidKeyError) -> Self {
        RuntimeError::InvalidKey(value)
    }
}

impl<Ty> Debug for RuntimeError<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(arg0) => f.debug_tuple("Value").field(arg0).finish(),
            Self::Borrow(arg0) => f.debug_tuple("Borrow").field(arg0).finish(),
            Self::AlreadyDropped(arg0) => f.debug_tuple("AlreadyDropped").field(arg0).finish(),
            Self::InvalidKey(arg0) => f.debug_tuple("InvalidKey").field(arg0).finish(),
            Self::Immutable(arg0) => f.debug_tuple("Immutable").field(arg0).finish(),
            Self::Diagnostic(arg0) => f.debug_tuple("Diagnostic").field(arg0).finish(),
            Self::MissingChunk(arg0) => f.debug_tuple("MissingChunk").field(arg0).finish(),
            Self::MissingFunction(arg0) => f.debug_tuple("MissingFunction").field(arg0).finish(),
            Self::OutOfBoundsStack(arg0) => f.debug_tuple("OutOfBoundsStack").field(arg0).finish(),
            Self::UpvalueCountMismatch(arg0) => {
                f.debug_tuple("UpvalueCountMismatch").field(arg0).finish()
            }
            Self::Signature(arg0) => f.debug_tuple("Signature").field(arg0).finish(),
            Self::NotCallable(arg0) => f.debug_tuple("NotCallable").field(arg0).finish(),
            Self::NotText(arg0) => f.debug_tuple("NotText").field(arg0).finish(),
            Self::Thread(arg0) => f.debug_tuple("Thread").field(arg0).finish(),
            Self::OpCode(arg0) => f.debug_tuple("OpCode").field(arg0).finish(),
        }
    }
}

impl<Ty> Display for RuntimeError<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "runtime error")
    }
}

// This impl is autogenerated.
#[expect(clippy::clone_on_copy)]
impl<Ty> Clone for RuntimeError<Ty>
where
    Ty: Types,
{
    fn clone(&self) -> Self {
        match self {
            Self::Value(arg0) => Self::Value(arg0.clone()),
            Self::Borrow(arg0) => Self::Borrow(arg0.clone()),
            Self::AlreadyDropped(arg0) => Self::AlreadyDropped(arg0.clone()),
            Self::InvalidKey(arg0) => Self::InvalidKey(arg0.clone()),
            Self::Immutable(arg0) => Self::Immutable(arg0.clone()),
            Self::Diagnostic(arg0) => Self::Diagnostic(arg0.clone()),
            Self::MissingChunk(arg0) => Self::MissingChunk(arg0.clone()),
            Self::MissingFunction(arg0) => Self::MissingFunction(arg0.clone()),
            Self::OutOfBoundsStack(arg0) => Self::OutOfBoundsStack(arg0.clone()),
            Self::UpvalueCountMismatch(arg0) => Self::UpvalueCountMismatch(arg0.clone()),
            Self::Signature(arg0) => Self::Signature(arg0.clone()),
            Self::NotCallable(arg0) => Self::NotCallable(arg0.clone()),
            Self::NotText(arg0) => Self::NotText(arg0.clone()),
            Self::Thread(arg0) => Self::Thread(arg0.clone()),
            Self::OpCode(arg0) => Self::OpCode(arg0.clone()),
        }
    }
}

impl<Ty> Error for RuntimeError<Ty>
where
    Ty: Types,
    Self: Debug + Display,
{
}

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

impl<Ty> From<RefAccessError> for RuntimeError<Ty>
where
    Ty: Types,
{
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
