use std::fmt::Display;

use super::{Type, TypeMismatchError, Types, Value};
use crate::gc::{TryFromWithGc, TryIntoWithGc};

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

impl<Gc: Types> TryFrom<Value<Gc>> for Nil {
    type Error = TypeMismatchError;

    fn try_from(value: Value<Gc>) -> Result<Self, Self::Error> {
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

impl<Gc: Types> From<Nil> for Value<Gc> {
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

impl<T, Ty, Gc> TryFromWithGc<Value<Ty>, Gc> for NilOr<T>
where
    Ty: Types,
    Value<Ty>: TryIntoWithGc<T, Gc>,
{
    type Error = <Value<Ty> as TryIntoWithGc<T, Gc>>::Error;

    fn try_from_with_gc(value: Value<Ty>, gc: &mut Gc) -> Result<NilOr<T>, Self::Error> {
        match value {
            Value::Nil => Ok(NilOr::Nil),
            value => Ok(NilOr::Some(value.try_into_with_gc(gc)?)),
        }
    }
}

impl<T> From<Nil> for NilOr<T> {
    fn from(Nil: Nil) -> Self {
        NilOr::Nil
    }
}

impl<T, Gc: Types> From<NilOr<T>> for Value<Gc>
where
    T: Into<Value<Gc>>,
{
    fn from(value: NilOr<T>) -> Self {
        match value {
            NilOr::Nil => Value::Nil,
            NilOr::Some(value) => value.into(),
        }
    }
}
