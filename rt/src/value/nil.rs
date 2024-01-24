use std::fmt::Display;

use super::{Type, TypeMismatchError, Value};

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

impl<C> TryFrom<Value<C>> for Nil {
    type Error = TypeMismatchError;

    fn try_from(value: Value<C>) -> Result<Self, Self::Error> {
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

impl<C> From<Nil> for Value<C> {
    fn from(Nil: Nil) -> Self {
        Value::Nil
    }
}
