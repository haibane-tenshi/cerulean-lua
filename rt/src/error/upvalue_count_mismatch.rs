use codespan_reporting::diagnostic::Diagnostic;

#[derive(Debug, Clone, Copy)]
pub struct UpvalueCountMismatch {
    pub expected: usize,
    pub closure: usize,
}

impl UpvalueCountMismatch {
    pub(super) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
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
