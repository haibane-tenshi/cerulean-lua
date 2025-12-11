use std::error::Error;
use std::fmt::{Debug, Display};

use super::Message;

#[derive(Debug, Clone, Copy)]
pub struct AlreadyDroppedError;

impl AlreadyDroppedError {
    pub(crate) fn into_diagnostic<FileId>(self) -> Message<FileId> {
        Message::error().with_message("value is already dropped")
    }
}

impl Display for AlreadyDroppedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "value behind this reference is already dropped")
    }
}

impl Error for AlreadyDroppedError {}

#[derive(Debug, Clone, Copy)]
pub enum AlreadyDroppedOr<E> {
    Dropped(AlreadyDroppedError),
    Other(E),
}

impl<E> AlreadyDroppedOr<E> {
    pub fn map_other<T>(self, f: impl FnOnce(E) -> T) -> AlreadyDroppedOr<T> {
        match self {
            AlreadyDroppedOr::Dropped(t) => AlreadyDroppedOr::Dropped(t),
            AlreadyDroppedOr::Other(t) => AlreadyDroppedOr::Other(f(t)),
        }
    }
}

impl<E> Display for AlreadyDroppedOr<E>
where
    E: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AlreadyDroppedOr::Dropped(err) => write!(f, "{}", err),
            AlreadyDroppedOr::Other(err) => write!(f, "{}", err),
        }
    }
}

impl<E> Error for AlreadyDroppedOr<E> where E: Debug + Display {}

impl<E> From<AlreadyDroppedError> for AlreadyDroppedOr<E> {
    fn from(value: AlreadyDroppedError) -> Self {
        AlreadyDroppedOr::Dropped(value)
    }
}
