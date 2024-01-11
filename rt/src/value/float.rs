use std::fmt::Display;
use std::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Rem, RemAssign, Sub, SubAssign,
};

use super::Type;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Default)]
pub struct Float(pub f64);

impl Float {
    pub fn type_(&self) -> Type {
        Type::Float
    }

    pub fn to_bool(&self) -> bool {
        true
    }

    pub fn floor_div(self, rhs: Self) -> Self {
        let Float(lhs) = self;
        let Float(rhs) = rhs;

        Float((lhs / rhs).floor())
    }

    pub fn exp(self, rhs: Self) -> Self {
        Float(self.0.powf(rhs.0))
    }
}

impl Display for Float {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{}_f64", self.0)
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl Neg for Float {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Float(-self.0)
    }
}

impl Add for Float {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Float(self.0 + rhs.0)
    }
}

impl AddAssign for Float {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Sub for Float {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Float(self.0 - rhs.0)
    }
}

impl SubAssign for Float {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl Mul for Float {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Float(self.0 * rhs.0)
    }
}

impl MulAssign for Float {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl Div for Float {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Float(self.0 / rhs.0)
    }
}

impl DivAssign for Float {
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs;
    }
}

impl Rem for Float {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        let Float(lhs) = self;
        let Float(rhs) = rhs;

        let r = lhs - rhs * (lhs / rhs).floor();
        Float(r)
    }
}

impl RemAssign for Float {
    fn rem_assign(&mut self, rhs: Self) {
        *self = *self % rhs;
    }
}
