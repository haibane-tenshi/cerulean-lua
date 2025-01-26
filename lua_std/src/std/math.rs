//! Mathematical functions provided by [Lua `std`'s math library][lua#6.7]
//!
//! Note that all items from this module will insert directly into parent table when `.include`d ,
//! they *will not* create an intermediate `math` table!
//! This module exists primarily for ease of organization.
//!
//! To correctly namespace APIs you need to use [`Math`](crate::lib::Math) module:
//!
//! ```
//! use lua_std::std;
//! use lua_std::lib::{Std, Math};
//!
//! let global_env = Std::empty()
//!     .include(
//!         Math::empty()
//!             .include(std::math::abs)
//!             .include(std::math::floor)
//!     );
//! ```
//!
//! See documentation of [`lib`](crate::lib) module for more details.
//!
//! [lua#6.7]: https://www.lua.org/manual/5.4/manual.html#6.7

use gc::RootCell;
use rt::ffi::{boxed, DLuaFfi};
use rt::gc::LuaPtr;
use rt::runtime::Core;
use rt::value::{Callable, KeyValue, TableIndex, Types, Value};

use crate::traits::{RootTable, TableEntry};

/// Compute absolute value of a number.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int) -> int`
/// * `(x: float) -> float`
///
/// Returns the maximum value between `x` and `-x`. (integer/float)
///
/// # Implementation-specific behavior
///
/// * See [`i64::abs`] for details on behavior on overflow.
#[expect(non_camel_case_types)]
pub struct abs;

impl<Ty> TableEntry<Ty> for abs
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::abs();
        let key = core.gc.intern("abs".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Compute arccosine of a number.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> float`
///
/// Returns the arc cosine of `x` (in radians).
///
/// # Implementation-specific behavior
///
/// * Integer inputs will be [coerced to floats](self#integer-to-float-coercions).
/// *   Implementation is delegated to [`f64::acos`].
///     
///     Return value is in radians in the range [0, pi] or NaN if the number is outside the range [-1, 1].
///
#[expect(non_camel_case_types)]
pub struct acos;

impl<Ty> TableEntry<Ty> for acos
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::acos();
        let key = core.gc.intern("acos".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Compute arcsine of a number.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> float`
///
/// Returns the arc sine of `x` (in radians).
///
/// # Implementation-specific behavior
///
/// * Integer inputs will be [coerced to floats](self#integer-to-float-coercions).
/// *   Implementation is delegated to [`f64::asin`].
///     
///     Return value is in radians in the range [-pi/2, pi/2] or NaN if the number is outside the range [-1, 1].
#[expect(non_camel_case_types)]
pub struct asin;

impl<Ty> TableEntry<Ty> for asin
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::asin();
        let key = core.gc.intern("asin".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Compute arctangent of two numbers.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(y: int | float [, x: int | float]) -> float`
///
/// Returns the arc tangent of `y`/`x` (in radians), using the signs of both arguments to find the quadrant of the result.
/// It also handles correctly the case of `x` being zero.
///
/// The default value for `x` is 1, so that the call `math.atan(y)` returns the arc tangent of `y`.
///
/// # Implementation-specific behavior
///
/// * Integer inputs will be [coerced to floats](self#integer-to-float-coercions).
/// *   Implementation is delegated to [`f64::atan2`].
///     
///     Return value is in radians in the range (-pi, pi].
#[expect(non_camel_case_types)]
pub struct atan;

impl<Ty> TableEntry<Ty> for atan
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::atan();
        let key = core.gc.intern("atan".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Returns the smallest integral value greater than or equal to a number.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int) -> int`
/// * `(x: float) -> int | float`
///
/// Returns the smallest integral value greater than or equal to `x`.
///
/// The rounding functions `math.ceil`, `math.floor`, and `math.modf` return an integer when the result fits in the range of an integer, or a float otherwise.
///
/// # Implementation-specific behavior
///
/// * Implementation is delegated to [`f64::ceil`].
#[expect(non_camel_case_types)]
pub struct ceil;

impl<Ty> TableEntry<Ty> for ceil
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::ceil();
        let key = core.gc.intern("ceil".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Compute cosine of a number (in radians).
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> float`
///
/// Returns the cosine of `x` (assumed to be in radians).
///
/// # Implementation-specific behavior
///
/// * Integer inputs will be [coerced to floats](self#integer-to-float-coercions).
/// * Implementation is delegated to [`f64::cos`].
#[expect(non_camel_case_types)]
pub struct cos;

impl<Ty> TableEntry<Ty> for cos
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::cos();
        let key = core.gc.intern("cos".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Convert radians to degrees.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> float`
///
/// Converts the angle `x` from radians to degrees.
///
/// # Implementation-specific behavior
///
/// * Integer inputs will be [coerced to floats](self#integer-to-float-coercions).
/// * Implementation is delegated to [`f64::to_degrees`].
#[expect(non_camel_case_types)]
pub struct deg;

impl<Ty> TableEntry<Ty> for deg
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::deg();
        let key = core.gc.intern("deg".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Compute exponential function.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> float`
///
/// Returns the value `e^x` (where `e` is the base of natural logarithms).
///
/// # Implementation-specific behavior
///
/// * Integer inputs will be [coerced to floats](self#integer-to-float-coercions).
/// * Implementation is delegated to [`f64::exp`].
#[expect(non_camel_case_types)]
pub struct exp;

impl<Ty> TableEntry<Ty> for exp
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::exp();
        let key = core.gc.intern("exp".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Calculate the largest integral value less than or equal to a number.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int) -> int`
/// * `(x: float) -> int | float`
///
/// Returns the largest integral value less than or equal to `x`.
///
/// The rounding functions `math.ceil`, `math.floor`, and `math.modf` return an integer when the result fits in the range of an integer, or a float otherwise.
///
/// # Implementation-specific behavior
///
/// * Implementation is delegated to [`f64::floor`].
#[expect(non_camel_case_types)]
pub struct floor;

impl<Ty> TableEntry<Ty> for floor
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::floor();
        let key = core.gc.intern("floor".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Calculate remainder of division between two numbers rounding quotient towards zero.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int, y: int) -> int`
/// * `(x: float, y: float) -> float`
///
/// Returns the remainder of the division of `x` by `y` that rounds the quotient towards zero. (integer/float)
///
/// # Implementation-specific behavior
///
/// * Passing arguments of mixed numeric types will result in Lua panic.
/// * Operation on integers is checked resulting in Lua panic if `y == 0` or operation results in overflow.
///
/// # Notes
///
/// * Rounding quotient towards zero is default behavior of integer division in Rust.
#[expect(non_camel_case_types)]
pub struct fmod;

impl<Ty> TableEntry<Ty> for fmod
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::fmod();
        let key = core.gc.intern("fmod".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Floating point positive infinity.
///
/// # From Lua documentation
///
/// The float value `HUGE_VAL`, a value greater than any other numeric value.
///
/// # Implementation-specific details
///
/// This value is evaluated to [`f64::INFINITY`].
#[expect(non_camel_case_types)]
pub struct huge;

impl<Ty> TableEntry<Ty> for huge
where
    Ty: Types,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let key = core.gc.intern("huge".into());
        let value = f64::INFINITY;

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Float(value),
        );
    }
}

/// Compute logarithm of a number with respect to an arbitrary base.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float, [base: int | float]) -> float`
///
/// Returns the logarithm of `x` in the given base.
/// The default for base is `e` (so that the function returns the natural logarithm of `x`).
///
/// # Implementation-specific behavior
///
/// * Integer inputs will be [coerced to floats](self#integer-to-float-coercions).
/// * Implementation is delegated to [`f64::ln`] or [`f64::log`] respectively.
#[expect(non_camel_case_types)]
pub struct log;

impl<Ty> TableEntry<Ty> for log
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::log();
        let key = core.gc.intern("log".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Find maximum in a list of values.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(arg0: any, _: any...) -> any`
///
/// Returns the argument with the maximum value, according to the Lua operator `<`.
///
/// # Implementation-specific behavior
///
/// * This function will respect coercion policy set in [`Core`](rt::runtime::Core).
#[expect(non_camel_case_types)]
pub struct max;

impl<Ty> TableEntry<Ty> for max
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::max();
        let key = core.gc.intern("max".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Largest representable integer value.
///
/// # From Lua documentation
///
/// An integer with the maximum value for an integer.
///
/// # Implementation-specific behavior
///
/// This constant evaluates to [`i64::MAX`].
#[expect(non_camel_case_types)]
pub struct maxinteger;

impl<Ty> TableEntry<Ty> for maxinteger
where
    Ty: Types,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let key = core.gc.intern("maxinteger".into());
        let value = i64::MAX;

        core.gc[table].set(KeyValue::String(LuaPtr(key.downgrade())), Value::Int(value));
    }
}

/// Find minimum in a list of values.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(arg0: any, _: any...) -> any`
///
/// Returns the argument with the minimum value, according to the Lua operator `<`.
///
/// # Implementation-specific behavior
///
/// * This function will respect coercion policy set in [`Core`](rt::runtime::Core).
#[expect(non_camel_case_types)]
pub struct min;

impl<Ty> TableEntry<Ty> for min
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::min();
        let key = core.gc.intern("min".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Smallest representable integer value.
///
/// # From Lua documentation
///
/// An integer with the minimum value for an integer.
///
/// # Implementation-specific behavior
///
/// This constant evaluates to [`i64::MIN`].
#[expect(non_camel_case_types)]
pub struct mininteger;

impl<Ty> TableEntry<Ty> for mininteger
where
    Ty: Types,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let key = core.gc.intern("mininteger".into());
        let value = i64::MIN;

        core.gc[table].set(KeyValue::String(LuaPtr(key.downgrade())), Value::Int(value));
    }
}

/// Return integral and fractional part of a number.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int) -> (int, float)`
/// * `(x: float) -> (int | float, float)`
///
/// Returns the integral part of `x` and the fractional part of `x`.
/// Its second result is always a float.
///
/// The rounding functions `math.ceil`, `math.floor`, and `math.modf` return an integer when the result fits in the range of an integer, or a float otherwise.
///
/// # Notes
///
/// * Fractional part is a number in range `[0; 1)`.
#[expect(non_camel_case_types)]
pub struct modf;

impl<Ty> TableEntry<Ty> for modf
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::modf();
        let key = core.gc.intern("modf".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// The value of *pi*.
///
/// # From Lua documentation
///
/// The value of *π*.
///
/// # Implementation-specific behavior
///
/// This constant evaluates to [`std::f64::consts::PI`].
#[expect(non_camel_case_types)]
pub struct pi;

impl<Ty> TableEntry<Ty> for pi
where
    Ty: Types,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let key = core.gc.intern("pi".into());
        let value = std::f64::consts::PI;

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Float(value),
        );
    }
}

/// Convert number from degrees to radians.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> float`
///
/// Converts the angle `x` from degrees to radians.
///
/// # Implementation-specific behavior
///
/// * Integer inputs will be [coerced to floats](self#integer-to-float-coercions).
/// * Implementation is delegated to [`f64::to_radians`].
#[expect(non_camel_case_types)]
pub struct rad;

impl<Ty> TableEntry<Ty> for rad
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::rad();
        let key = core.gc.intern("rad".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Generate a pseudo-random number.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `() -> float`
/// * `(n: int) -> int`
/// * `(m: int, n: int) -> int`
///
/// When called without arguments, returns a pseudo-random float with uniform distribution in the range `[0; 1)`.
/// When called with two integers `m` and `n`, `math.random` returns a pseudo-random integer with uniform distribution in the range `[m; n]`.
/// The call `math.random(n)`, for a positive `n`, is equivalent to `math.random(1,n)`.
/// The call `math.random(0)` produces an integer with all bits (pseudo)random.
///
/// This function uses the xoshiro256** algorithm to produce pseudo-random 64-bit integers,
/// which are the results of calls with argument 0.
/// Other results (ranges and floats) are unbiased extracted from these integers.
///
/// Lua initializes its pseudo-random generator with the equivalent of a call to `math.randomseed` with no arguments,
/// so that `math.random` should generate different sequences of results each time the program runs.
///
/// # Implementation-specific behavior
///
/// *   Lua comments on **algorithm and seeding do not apply**.
///     Read below for details.
///
/// *   This function will use provided `rng_state` as RNG.
///     It is expected that state is shared with [`randomseed`] function.
///
/// *   All values are generated using uniform distribution.
///
/// *   This function can operate on any RNG implementing [`rand::Rng`] trait.
///     It is up to you to choose and properly initialize it.
///
/// *   Default setup provided by this library also uses `xoshiro256**` algorithm.
///     It is a known general purpose algorithm providing both high quality and performance.
///     However, it is **not cryptographically secure** and should not be used as such.
///
///     Default state will be seeded using [system entropy](rand::SeedableRng::from_entropy).
///
///     See [`MathRand`] documentation for more information about defaults and pointers for custom configuration.
#[expect(non_camel_case_types)]
pub struct random<R>(RootCell<R>);

impl<R> random<R> {
    /// Use provided RNG state.
    ///
    /// The state is expected to be shared with [`randomseed`].
    pub fn with_state(rng_state: RootCell<R>) -> Self {
        Self(rng_state)
    }
}

impl<Ty, R> TableEntry<Ty> for random<R>
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    R: rand::Rng + 'static,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let random(rng_state) = self;

        let fn_body = crate::ffi::math::random(rng_state);
        let key = core.gc.intern("random".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

#[expect(non_camel_case_types)]
pub struct randomseed<R>(RootCell<R>);

impl<R> randomseed<R> {
    /// Use provided RNG state.
    ///
    /// The state is expected to be shared with [`random`].
    pub fn with_state(rng_state: RootCell<R>) -> Self {
        Self(rng_state)
    }
}

impl<Ty, R> TableEntry<Ty> for randomseed<R>
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    R: rand::SeedableRng + 'static,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let randomseed(rng_state) = self;

        let fn_body = crate::ffi::math::randomseed(rng_state);
        let key = core.gc.intern("randomseed".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Compute sine of a number (in radians).
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> float`
///
/// Returns the sine of `x` (assumed to be in radians).
///
/// # Implementation-specific behavior
///
/// * Integer inputs will be [coerced to floats](self#integer-to-float-coercions).
/// * Implementation is delegated to [`f64::sin`].
#[expect(non_camel_case_types)]
pub struct sin;

impl<Ty> TableEntry<Ty> for sin
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::sin();
        let key = core.gc.intern("sin".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Compute square root of a number.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> float`
///
/// Returns the square root of `x`.
/// (You can also use the expression `x^0.5` to compute this value.)
///
/// # Implementation-specific behavior
///
/// * Integer inputs will be [coerced to floats](self#integer-to-float-coercions).
/// * Implementation is delegated to [`f64::sqrt`].
#[expect(non_camel_case_types)]
pub struct sqrt;

impl<Ty> TableEntry<Ty> for sqrt
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::sqrt();
        let key = core.gc.intern("sqrt".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Compute tangent of a number (in radians).
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> float`
///
/// Returns the tangent of `x` (assumed to be in radians).
///
/// # Implementation-specific behavior
///
/// * Integer inputs will be [coerced to floats](self#integer-to-float-coercions).
/// * Implementation is delegated to [`f64::tan`].
#[expect(non_camel_case_types)]
pub struct tan;

impl<Ty> TableEntry<Ty> for tan
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::tan();
        let key = core.gc.intern("tan".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Convert a number to integer.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> int | fail`
///
/// If the value `x` is convertible to an integer, returns that integer.
/// Otherwise, returns **fail**.
///
/// # Implementation-specific behavior
///
/// *   This function will not attempt to coerce strings.
///     Use `tonumber` as appropriate if such behavior is desired.
#[expect(non_camel_case_types)]
pub struct tointeger;

impl<Ty> TableEntry<Ty> for tointeger
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::tointeger();
        let key = core.gc.intern("tointeger".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Discriminate between integers and floats.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> string | fail`
///
/// Returns "integer" if `x` is an integer, "float" if it is a float, or **fail** if `x` is not a number.
#[expect(non_camel_case_types)]
pub struct type_;

impl<Ty> TableEntry<Ty> for type_
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::type_();
        let key = core.gc.intern("type_".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Compare two integers as unsigned integers.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int, y: int) -> bool`
///
/// Returns a boolean, `true` if and only if integer `m` is below integer `n` when they are compared as unsigned integers.
///
/// # Implementation-specific behavior
///
/// *  It isn't exactly clear what Lua means here.
///    Implementation will coerce integers to unsigned counterparts [in Rust's sense][rust_ref#numeric-cast]
///    (which simply reinterprets the bits) and compare result with `<`.
/// * This function will panic if inputs are not integers.
///
/// [rust_ref#numeric-cast]: https://doc.rust-lang.org/reference/expressions/operator-expr.html#numeric-cast
#[expect(non_camel_case_types)]
pub struct ult;

impl<Ty> TableEntry<Ty> for ult
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::math::ult();
        let key = core.gc.intern("ult".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}
