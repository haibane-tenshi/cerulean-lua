use std::fmt::Display;

use super::{Type, TypeMismatchError, TypeProvider, Value};

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

impl<Types: TypeProvider> TryFrom<Value<Types>> for Nil {
    type Error = TypeMismatchError;

    fn try_from(value: Value<Types>) -> Result<Self, Self::Error> {
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

impl<Types: TypeProvider> From<Nil> for Value<Types> {
    fn from(Nil: Nil) -> Self {
        Value::Nil
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum NilOr<T> {
    #[default]
    Nil,
    Some(T),
}

impl<T> NilOr<T> {
    pub fn into_option(self) -> Option<T> {
        self.into()
    }
}

impl<T> From<Option<T>> for NilOr<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(t) => NilOr::Some(t),
            None => NilOr::Nil,
        }
    }
}

impl<T> From<NilOr<T>> for Option<T> {
    fn from(value: NilOr<T>) -> Self {
        match value {
            NilOr::Nil => None,
            NilOr::Some(t) => Some(t),
        }
    }
}

impl<T, Types: TypeProvider> TryFrom<Value<Types>> for NilOr<T>
where
    T: TryFrom<Value<Types>>,
{
    type Error = <T as TryFrom<Value<Types>>>::Error;

    fn try_from(value: Value<Types>) -> Result<Self, Self::Error> {
        match value {
            Value::Nil => Ok(Self::Nil),
            value => Ok(Self::Some(value.try_into()?)),
        }
    }
}

impl<T> From<Nil> for NilOr<T> {
    fn from(Nil: Nil) -> Self {
        NilOr::Nil
    }
}

impl<T, Types: TypeProvider> From<NilOr<T>> for Value<Types>
where
    T: Into<Value<Types>>,
{
    fn from(value: NilOr<T>) -> Self {
        match value {
            NilOr::Nil => Value::Nil,
            NilOr::Some(value) => value.into(),
        }
    }
}
