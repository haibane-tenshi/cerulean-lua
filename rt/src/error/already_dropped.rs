use std::error::Error;
use std::fmt::{Debug, Display};

use codespan_reporting::diagnostic::Diagnostic;

#[derive(Debug)]
pub struct AlreadyDroppedError;

impl AlreadyDroppedError {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        Diagnostic::error().with_message("value is already dropped")
    }
}

impl Display for AlreadyDroppedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "value behind this reference is already dropped")
    }
}

impl Error for AlreadyDroppedError {}
