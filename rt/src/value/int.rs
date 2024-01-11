use std::fmt::Display;
use std::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div, DivAssign,
    Mul, MulAssign, Neg, Not, Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
};

use super::Type;

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
