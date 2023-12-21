use codespan_reporting::diagnostic::Diagnostic;

#[derive(Debug, Clone, Copy)]
pub struct OutOfBoundsStack;

impl OutOfBoundsStack {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        use super::ExtraDiagnostic;

        let mut diag = Diagnostic::error().with_message("cannot use out of bounds stack space");

        diag.with_note([
            "this happened because something attempted to pass part of the stack which is beyond current stack length to another function",
        ]);

        diag
    }
}
