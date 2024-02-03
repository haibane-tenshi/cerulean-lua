use std::cmp::Ordering;
use std::fmt::Display;
use std::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Rem, RemAssign, Sub, SubAssign,
};

use super::{Int, Type, TypeMismatchError, TypeProvider, Value};

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

impl From<Int> for Float {
    /// Convert integer to float in Lua sense.
    ///
    /// [Lua specifies][lua#3.4.3] that convertion results in
    /// * exact float representation if possible
    /// * otherwise nearest lower *or* nearest higher representable value
    ///
    /// # Note
    /// Specification seems to be ambiguous in this case,
    /// it doesn't specify *the nearest* but *either of*.
    /// We take liberty in implementation and give it the same sematics
    /// as Rust's [int-to-float casts][rust_ref#numeric-cast].
    ///
    /// [lua#3.4.3]: https://www.lua.org/manual/5.4/manual.html#3.4.3
    /// [rust_ref#numeric-cast]: https://doc.rust-lang.org/stable/reference/expressions/operator-expr.html#numeric-cast
    fn from(value: Int) -> Self {
        Float(value.0 as f64)
    }
}

impl PartialEq<Int> for Float {
    /// Compare two values according to mathematical value they represent.
    ///
    /// Lua spec prescribes that two [numbers should be compared according to their methematical values][lua_ref#3.4.4]
    /// regardless of underlying type.
    /// The function provides implementation of such comparison between integers and floats.
    ///
    /// [lua_ref#3.4.4]: https://www.lua.org/manual/5.4/manual.html#3.4.4
    fn eq(&self, other: &Int) -> bool {
        PartialEq::eq(other, self)
    }
}

impl PartialOrd<Int> for Float {
    /// Compare two values according to mathematical value they represent.
    ///
    /// Lua spec prescribes that two [numbers should be compared according to their methematical values][lua_ref#3.4.4]
    /// regardless of underlying type.
    /// The function provides implementation of such comparison between integers and floats.
    ///
    /// [lua_ref#3.4.4]: https://www.lua.org/manual/5.4/manual.html#3.4.4
    fn partial_cmp(&self, other: &Int) -> Option<std::cmp::Ordering> {
        PartialOrd::partial_cmp(other, self).map(Ordering::reverse)
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

impl<Types: TypeProvider> TryFrom<Value<Types>> for Float {
    type Error = TypeMismatchError;

    fn try_from(value: Value<Types>) -> Result<Self, Self::Error> {
        match value {
            Value::Float(value) => Ok(Float(value)),
            value => {
                let err = TypeMismatchError {
                    expected: Type::Float,
                    found: value.type_(),
                };

                Err(err)
            }
        }
    }
}

impl<Types: TypeProvider> From<Float> for Value<Types> {
    fn from(value: Float) -> Self {
        let Float(value) = value;
        Value::Float(value)
    }
}
