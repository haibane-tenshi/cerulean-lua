use std::error::Error;
use std::fmt::Display;

use codespan_reporting::diagnostic::Diagnostic;

use super::{ExtraDiagnostic, RuntimeError};
use crate::value::TypeProvider;

#[derive(Debug)]
pub enum BorrowError {
    Ref,
    Mut,
}

impl BorrowError {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        let msg = match self {
            BorrowError::Ref => "value is already mutably borrowed",
            BorrowError::Mut => "value is already borrowed",
        };

        let mut diag = Diagnostic::error().with_message(msg);

        diag.with_help([
            "you see this error because an access pattern violated Rust aliasing rules:\nthere can only exist one mutable reference to a value at a time",
            "this likely happened due to recursive call of mutable Rust closure,\nclosure needs to borrow its contents for the entire call duration",
        ]);

        diag.with_note(["Rust as host language enforces aliasing rules for all values"]);

        diag
    }
}

impl Display for BorrowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "value is already borrowed")
    }
}

impl Error for BorrowError {}
