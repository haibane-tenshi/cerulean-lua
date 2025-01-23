use std::error::Error;
use std::fmt::{Debug, Display};

use super::Message;
use crate::value::{StrongValue, Types};

pub struct NotCallableError<Ty>(pub StrongValue<Ty>)
where
    Ty: Types;

impl<Ty> NotCallableError<Ty>
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

impl<Ty> Debug for NotCallableError<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NotCallableError").field(&self.0).finish()
    }
}

impl<Ty> Display for NotCallableError<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "value {self:?} cannot be called")
    }
}

impl<Ty> Clone for NotCallableError<Ty>
where
    Ty: Types,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Ty> Error for NotCallableError<Ty>
where
    Ty: Types,
    Self: Debug + Display,
{
}
