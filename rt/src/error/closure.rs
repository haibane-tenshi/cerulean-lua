use codespan_reporting::diagnostic::Diagnostic;

use crate::chunk_cache::ChunkId;
use crate::runtime::FunctionPtr;

#[derive(Debug, Clone, Copy)]
pub struct MissingChunk(pub ChunkId);

impl MissingChunk {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        let MissingChunk(chunk_id) = self;

        Diagnostic::error().with_message(format!(
            "chunk cache does not contain chunk with id={chunk_id}"
        ))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MissingFunction(pub FunctionPtr);

impl MissingFunction {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        let MissingFunction(fn_ptr) = self;

        Diagnostic::error().with_message(format!(
            "chunk [id={}] does not contain function with id={}",
            fn_ptr.chunk_id, fn_ptr.function_id,
        ))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UpvalueCountMismatch {
    pub expected: usize,
    pub closure: usize,
}

impl UpvalueCountMismatch {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        use super::ExtraDiagnostic;

        let UpvalueCountMismatch { expected, closure } = self;

        let mut diag = Diagnostic::error().with_message(format!(
            "function expected {expected} upvalues but closure provided {closure} upvalues instead"
        ));

        diag.with_compiler_bug_note([
            "under normal circumstances closures should be constructed with the same number of upvalues as its target function, but something went horribly wrong",
        ]);

        diag
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MalformedClosureError {
    MissingChunk(MissingChunk),
    MissingFunction(MissingFunction),
    CapturesMismatch(UpvalueCountMismatch),
}

impl From<MissingChunk> for MalformedClosureError {
    fn from(value: MissingChunk) -> Self {
        MalformedClosureError::MissingChunk(value)
    }
}

impl From<MissingFunction> for MalformedClosureError {
    fn from(value: MissingFunction) -> Self {
        MalformedClosureError::MissingFunction(value)
    }
}

impl From<UpvalueCountMismatch> for MalformedClosureError {
    fn from(value: UpvalueCountMismatch) -> Self {
        MalformedClosureError::CapturesMismatch(value)
    }
}
