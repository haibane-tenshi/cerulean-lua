use std::cmp::Ordering;
use std::fmt::Display;
use std::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div, DivAssign,
    Mul, MulAssign, Neg, Not, Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
};

use super::{Float, Type, TypeMismatchError, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Default, Hash)]
pub struct Int(pub i64);

impl Int {
    pub fn type_(&self) -> Type {
        Type::Int
    }

    pub fn to_bool(&self) -> bool {
        true
    }

    pub fn floor_div(self, rhs: Self) -> Self {
        self / rhs
    }

    pub fn exp(self, rhs: Self) -> Option<Self> {
        rhs.0.try_into().ok().map(|rhs| self.0.pow(rhs)).map(Int)
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{}_i64", self.0)
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl TryFrom<Float> for Int {
    type Error = NotExactIntError;

    /// Attempt to convert float to integer in Lua sense.
    ///
    /// Convertion succeeds under two conditions:
    /// * float contains exactly integer (e.g. there is no fractional part)
    /// * resulting integer is representable
    fn try_from(value: Float) -> Result<Self, Self::Error> {
        let Float(value) = value;

        // Integer to float casts produce exact representation if possible,
        // otherwise result is rounded towards 0.0.
        // https://doc.rust-lang.org/stable/reference/expressions/operator-expr.html#numeric-cast
        // This is good for us:
        // after rounding we get (inclusive) boundaries where
        // exact in-range integer representation is possible by float type.
        const MIN: f64 = i64::MIN as f64;
        const MAX: f64 = i64::MAX as f64;

        if (MIN..=MAX).contains(&value) && value == value.trunc() {
            Ok(Int(value as i64))
        } else {
            Err(NotExactIntError)
        }
    }
}

pub struct NotExactIntError;

impl PartialEq<Float> for Int {
    /// Compare two values according to mathematical value they represent.
    ///
    /// Lua spec prescribes that two [numbers should be compared according to their methematical values][lua_ref#3.4.4]
    /// regardless of underlying type.
    /// The function provides implementation of such comparison between integers and floats.
    ///
    /// [lua_ref#3.4.4]: https://www.lua.org/manual/5.4/manual.html#3.4.4
    fn eq(&self, other: &Float) -> bool {
        if let Ok(rhs) = Int::try_from(*other) {
            *self == rhs
        } else {
            false
        }
    }
}

impl PartialOrd<Float> for Int {
    /// Compare two values according to mathematical value they represent.
    ///
    /// Lua spec prescribes that two [numbers should be compared according to their methematical values][lua_ref#3.4.4]
    /// regardless of underlying type.
    /// The function provides implementation of such comparison between integers and floats.
    ///
    /// [lua_ref#3.4.4]: https://www.lua.org/manual/5.4/manual.html#3.4.4
    fn partial_cmp(&self, other: &Float) -> Option<std::cmp::Ordering> {
        if let Ok(rhs) = Int::try_from(*other) {
            // Float represents exact integer.
            PartialOrd::partial_cmp(self, &rhs)
        } else if let Ok(rhs) = Int::try_from(Float(other.0.floor())) {
            // The actual rhs value has fractional part,
            // therefore equality breaks into less.
            match PartialOrd::partial_cmp(self, &rhs) {
                Some(Ordering::Equal) => Some(Ordering::Less),
                ord => ord,
            }

            // Otherwise we are outside `i64::MIN..=i64::MAX`
        } else if other.0 > 0.0 {
            Some(Ordering::Less)
        } else {
            Some(Ordering::Greater)
        }
    }
}

impl Neg for Int {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Int(-self.0)
    }
}

impl Add for Int {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Int(self.0.wrapping_add(rhs.0))
    }
}

impl AddAssign for Int {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

impl Sub for Int {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Int(self.0.wrapping_sub(rhs.0))
    }
}

impl SubAssign for Int {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs
    }
}

impl Mul for Int {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Int(self.0.wrapping_mul(rhs.0))
    }
}

impl MulAssign for Int {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl Div for Int {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Int(self.0 / rhs.0)
    }
}

impl DivAssign for Int {
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs;
    }
}

impl Rem for Int {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        Int(self.0.rem_euclid(rhs.0))
    }
}

impl RemAssign for Int {
    fn rem_assign(&mut self, rhs: Self) {
        *self = *self % rhs;
    }
}

impl Not for Int {
    type Output = Self;

    fn not(self) -> Self::Output {
        Int(!self.0)
    }
}

impl BitAnd for Int {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Int(self.0 & rhs.0)
    }
}

impl BitAndAssign for Int {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl BitOr for Int {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Int(self.0 | rhs.0)
    }
}

impl BitOrAssign for Int {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitXor for Int {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Int(self.0 ^ rhs.0)
    }
}

impl BitXorAssign for Int {
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs;
    }
}

impl Shl for Int {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        let Int(lhs) = self;
        let Int(rhs) = rhs;

        let r = if let Ok(rhs) = rhs.try_into() {
            lhs.checked_shl(rhs).unwrap_or_default()
        } else if let Ok(rhs) = (-rhs).try_into() {
            let lhs = lhs as u64;
            lhs.checked_shr(rhs).unwrap_or_default() as i64
        } else {
            0
        };

        Int(r)
    }
}

impl ShlAssign for Int {
    fn shl_assign(&mut self, rhs: Self) {
        *self = *self << rhs;
    }
}

impl Shr for Int {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        let Int(lhs) = self;
        let Int(rhs) = rhs;

        let r = if let Ok(rhs) = rhs.try_into() {
            let lhs = lhs as u64;
            lhs.checked_shr(rhs).unwrap_or_default() as i64
        } else if let Ok(rhs) = (-rhs).try_into() {
            lhs.checked_shl(rhs).unwrap_or_default()
        } else {
            0
        };

        Int(r)
    }
}

impl ShrAssign for Int {
    fn shr_assign(&mut self, rhs: Self) {
        *self = *self >> rhs;
    }
}

impl<C> TryFrom<Value<C>> for Int {
    type Error = TypeMismatchError;

    fn try_from(value: Value<C>) -> Result<Self, Self::Error> {
        match value {
            Value::Int(value) => Ok(Int(value)),
            value => {
                let err = TypeMismatchError {
                    expected: Type::Int,
                    found: value.type_(),
                };

                Err(err)
            }
        }
    }
}

impl<C> From<Int> for Value<C> {
    fn from(value: Int) -> Self {
        let Int(value) = value;
        Value::Int(value)
    }
}
