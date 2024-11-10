use std::fmt::Display;

use super::{Refs, Type, Types, Value};
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

impl<Rf, Ty> TryFrom<Value<Rf, Ty>> for Nil
where
    Rf: Refs,
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Rf, Ty>) -> Result<Self, Self::Error> {
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

impl<Rf, Ty> From<Nil> for Value<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    fn from(Nil: Nil) -> Self {
        Value::Nil
    }
}
