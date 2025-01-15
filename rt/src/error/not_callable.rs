use std::error::Error;
use std::fmt::{Debug, Display};

use super::{Message, RuntimeError};
use crate::value::{StrongValue, Types};

#[derive(Debug, Clone, Copy)]
pub struct NotCallableError<Value>(pub Value);

impl<Ty> NotCallableError<StrongValue<Ty>>
where
    Ty: Types,
{
    pub(crate) fn into_diagnostic<FileId>(self) -> Message<FileId> {
        Message::error()
            .with_message(format!("value {:#?} cannot be called", self.0))
            .with_notes(vec![
                "runtime only permits invoking values of `function` type".to_string(),
                "besides functions Lua considers any value with `__call` metamethod to be callable, however, `__call` metavalue is subject to the same consideration making lookup recursive; regardless, this lookup must terminate in actual function object, which seem to have failed for this value".to_string(),
            ])
    }
}

impl<Ty> Display for NotCallableError<StrongValue<Ty>>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "value {self:?} cannot be called")
    }
}

impl<Value> Error for NotCallableError<Value> where Self: Debug + Display {}

impl<Value> From<NotCallableError<Value>> for RuntimeError<Value> {
    fn from(value: NotCallableError<Value>) -> Self {
        RuntimeError::NotCallable(value)
    }
}
