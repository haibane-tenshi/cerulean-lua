use std::error::Error;
use std::fmt::{Debug, Display};

use codespan_reporting::diagnostic::Diagnostic;

use super::RuntimeError;
use crate::value::TypeProvider;

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

#[derive(Debug)]
pub enum AlreadyDroppedOrError<E> {
    AlreadyDropped(AlreadyDroppedError),
    Other(E),
}

impl<E> Display for AlreadyDroppedOrError<E>
where
    E: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AlreadyDropped(err) => write!(f, "{err}"),
            Self::Other(err) => write!(f, "{err}"),
        }
    }
}

impl<E> Error for AlreadyDroppedOrError<E> where Self: Debug + Display {}

impl<E> From<AlreadyDroppedError> for AlreadyDroppedOrError<E> {
    fn from(value: AlreadyDroppedError) -> Self {
        AlreadyDroppedOrError::AlreadyDropped(value)
    }
}
