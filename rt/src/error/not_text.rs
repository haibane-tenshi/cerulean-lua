use std::error::Error;
use std::fmt::{Debug, Display};

use gc::Root;

use super::Message;

pub struct NotTextError<T>(pub Root<T>);

impl<T> NotTextError<T> {
    pub(crate) fn into_diagnostic<FileId>(self) -> Message<FileId> {
        Message::error().with_message(self.to_string())
    }
}

impl<T> Debug for NotTextError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NotTextError").field(&self.0).finish()
    }
}

impl<T> Display for NotTextError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "string at <{:p}> cannot be interpreted as text",
            self.0.location()
        )
    }
}

impl<T> Clone for NotTextError<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Error for NotTextError<T> where Self: Debug + Display {}
