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

impl<Gc: TypeProvider> TryFrom<Value<Gc>> for Nil {
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

impl<Gc: TypeProvider> From<Nil> for Value<Gc> {
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

impl<T, Gc> TryInto<NilOr<T>> for Value<Gc>
where
    Gc: TypeProvider,
    Value<Gc>: TryInto<T>,
{
    type Error = <Value<Gc> as TryInto<T>>::Error;

    fn try_into(self) -> Result<NilOr<T>, Self::Error> {
        match self {
            Value::Nil => Ok(NilOr::Nil),
            value => Ok(NilOr::Some(value.try_into()?)),
        }
    }
}

impl<T> From<Nil> for NilOr<T> {
    fn from(Nil: Nil) -> Self {
        NilOr::Nil
    }
}

impl<T, Gc: TypeProvider> From<NilOr<T>> for Value<Gc>
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
