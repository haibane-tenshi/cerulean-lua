use codespan_reporting::diagnostic::Diagnostic;

use crate::runtime::FunctionPtr;

#[derive(Debug, Clone, Copy)]
pub struct MissingFunction(pub FunctionPtr);

impl MissingFunction {
    pub(super) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        let MissingFunction(fn_ptr) = self;

        Diagnostic::error().with_message(format!(
            "chunk [id={}] does not contain function with id={}",
            fn_ptr.chunk_id, fn_ptr.function_id,
        ))
    }
}
