use std::fmt::Display;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};

use super::{CoreTypes, Type, Types, Value};
use crate::ffi::arg_parser::TypeMismatchError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct Boolean(pub bool);

impl Boolean {
    pub fn to_bool(&self) -> bool {
        self.0
    }

    pub fn type_(&self) -> Type {
        Type::Bool
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Not for Boolean {
    type Output = Self;

    fn not(self) -> Self::Output {
        Boolean(!self.0)
    }
}

impl BitAnd for Boolean {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Boolean(self.0 & rhs.0)
    }
}

impl BitAndAssign for Boolean {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl BitOr for Boolean {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Boolean(self.0 | rhs.0)
    }
}

impl BitOrAssign for Boolean {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl BitXor for Boolean {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Boolean(self.0 ^ rhs.0)
    }
}

impl BitXorAssign for Boolean {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0
    }
}

impl<Rf, Ty> TryFrom<Value<Rf, Ty>> for Boolean
where
    Rf: Types,
    Ty: CoreTypes,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Rf, Ty>) -> Result<Self, Self::Error> {
        match value {
            Value::Bool(t) => Ok(Boolean(t)),
            value => {
                let err = TypeMismatchError {
                    expected: Type::Bool,
                    found: value.type_(),
                };

                Err(err)
            }
        }
    }
}

impl<Rf, Ty> From<Boolean> for Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
{
    fn from(value: Boolean) -> Self {
        let Boolean(value) = value;

        Value::Bool(value)
    }
}
