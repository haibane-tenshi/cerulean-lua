use std::fmt::Display;

use super::{Refs, Type, Value};
use crate::ffi::arg_parser::TypeMismatchError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct Nil;

impl Nil {
    pub fn to_bool(&self) -> bool {
        false
    }

    pub fn type_(&self) -> Type {
        Type::Nil
    }
}

impl Display for Nil {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "nil")
    }
}

impl<Rf> TryFrom<Value<Rf>> for Nil
where
    Rf: Refs,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Rf>) -> Result<Self, Self::Error> {
        match value {
            Value::Nil => Ok(Nil),
            value => {
                let err = TypeMismatchError {
                    expected: Type::Nil,
                    found: value.type_(),
                };

                Err(err)
            }
        }
    }
}

impl<Rf> From<Nil> for Value<Rf>
where
    Rf: Refs,
{
    fn from(Nil: Nil) -> Self {
        Value::Nil
    }
}
