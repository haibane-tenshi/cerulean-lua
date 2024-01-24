use std::fmt::Display;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};

use super::{Type, TypeMismatchError, Value};

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

impl<C> TryFrom<Value<C>> for Boolean {
    type Error = TypeMismatchError;

    fn try_from(value: Value<C>) -> Result<Self, Self::Error> {
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

impl<C> From<Boolean> for Value<C> {
    fn from(value: Boolean) -> Self {
        let Boolean(value) = value;

        Value::Bool(value)
    }
}
