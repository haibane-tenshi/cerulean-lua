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
use rt::ffi::{self, DLuaFfi};
use rt::gc::LuaPtr;
use rt::runtime::Core;
use rt::value::{KeyValue, TableIndex, Types, Value};

use crate::lib::set_func;
use crate::traits::{RootTable, TableEntry, TableEntryEx};

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
        let fn_body = ffi::from_fn(crate::ffi::math::abs, "lua_std::std::math::abs", ());
        set_func("abs", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::acos, "lua_std::std::math::acos", ());
        set_func("acos", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::asin, "lua_std::std::math::asin", ());
        set_func("asin", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::atan, "lua_std::std::math::atan", ());
        set_func("atan", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::ceil, "lua_std::std::math::ceil", ());
        set_func("ceil", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::cos, "lua_std::std::math::cos", ());
        set_func("cos", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::deg, "lua_std::std::math::deg", ());
        set_func("deg", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::exp, "lua_std::std::math::exp", ());
        set_func("exp", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::floor, "lua_std::std::math::floor", ());
        set_func("floor", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::fmod, "lua_std::std::math::fmod", ());
        set_func("fmod", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::log, "lua_std::std::math::log", ());
        set_func("log", fn_body).build(table, core);
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
/// * This function will respect coercion policy set in [`Core`].
#[expect(non_camel_case_types)]
pub struct max;

impl<Ty> TableEntry<Ty> for max
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::math::max, "lua_std::std::math::max", ());
        set_func("max", fn_body).build(table, core);
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
/// * This function will respect coercion policy set in [`Core`].
#[expect(non_camel_case_types)]
pub struct min;

impl<Ty> TableEntry<Ty> for min
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::math::min, "lua_std::std::math::min", ());
        set_func("min", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::modf, "lua_std::std::math::modf", ());
        set_func("modf", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::rad, "lua_std::std::math::rad", ());
        set_func("rad", fn_body).build(table, core);
    }
}

/// Generate a pseudo-random number.
///
/// Implementation of `random` expects to be provided with RNG state.
/// Under normal circumstances it will be configured by [`MathRand`](crate::lib::MathRand)
/// and passed in through an extra argument in [`TableEntryEx::build`] method.
///
/// However to get included from other libraries you need to provide state directly via [`with_state`](Self::with_state) method.
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
///     See [`MathRand`](crate::lib::MathRand) documentation for more information about defaults and pointers for custom configuration.
#[expect(non_camel_case_types)]
pub struct random;

impl random {
    /// Use provided RNG state.
    ///
    /// The state is expected to be shared with [`randomseed`].
    pub fn with_state<R, Ty>(rng_state: RootCell<R>) -> impl TableEntry<Ty>
    where
        Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
        R: rand::Rng + 'static,
    {
        struct RandomWith<R>(RootCell<R>);

        impl<Ty, R> TableEntry<Ty> for RandomWith<R>
        where
            Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
            R: rand::Rng + 'static,
        {
            fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
                let RandomWith(state) = self;
                let fn_body = ffi::from_fn(
                    move || crate::ffi::math::random(state.clone()),
                    "lua_std::std::math::random",
                    (),
                );
                set_func("random", fn_body).build(table, core);
            }
        }

        RandomWith(rng_state)
    }
}

impl<Ty, R> TableEntryEx<Ty, RootCell<R>> for random
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    R: rand::Rng + 'static,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>, rng_state: &mut RootCell<R>) {
        let state = rng_state.clone();
        let fn_body = ffi::from_fn(
            move || crate::ffi::math::random(state.clone()),
            "lua_std::std::math::random",
            (),
        );
        set_func("random", fn_body).build(table, core);
    }
}

/// Seed pseudo-random generator used by `random`.
///
/// Implementation of `randomseed` expects to be provided with RNG state.
/// Under normal circumstances it will be configured by [`MathRand`](crate::lib::MathRand)
/// and passed in through an extra argument in [`TableEntryEx::build`] method.
///
/// However to get included from other libraries you need to provide state directly via [`with_state`](Self::with_state) method.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `([x: int, [y: int]]) -> ()`
///
/// When called with at least one argument,
/// the integer parameters `x` and `y` are joined into a 128-bit seed that is used to reinitialize the pseudo-random generator;
/// equal seeds produce equal sequences of numbers.
/// The default for `y` is zero.
///
/// When called with no arguments, Lua generates a seed with a weak attempt for randomness.
///
/// This function returns the two seed components that were effectively used, so that setting them again repeats the sequence.
///
/// To ensure a required level of randomness to the initial state
/// (or contrarily, to have a deterministic sequence, for instance when debugging a program),
/// you should call `math.randomseed` with explicit arguments.
///
/// # Implementation-specific behavior
///
/// *   Lua comments **on seeding do not apply**.
///     Read below for more details.
///
/// *   This function will use provided `rng_state` as RNG.
///     It is expected that state is shared with [`random`] function.
///
/// *   When no arguments are provided, the state will be seeded from [system entropy](rand::SeedableRng::from_entropy).
///
/// *   When 1 integer is provided, it will be used to seed state directly using [`SeedableRng::seed_from_u64`](rand::SeedableRng::seed_from_u64).
///     It should be reproducible and deterministic; default implementation makes an attempt to convert those into reasonably good seeds.
///     See documentation of the method for more details.
///
///     This is **not suitable for cryptography**, as should be clear given that the input size is only 64 bits.
///
/// *   When 2 integers are provided, they will be concatenated into single 128-bit seed which is used to initialize an [HC-128](rand_hc::Hc128Rng) PRNG,
///     output of which is then used to seed `rng_state`.
///
///     HC-128 is a cryptographically secure algorithm and at the time of writing has no known weaknesses.
///     It should provide reasonably good output which doesn't compromise security in case the real PRNG you use is cryptographically secure.
///
///     We have to go through an indirection since most practical algorithms use 256 bit seeds (which includes `xoshiro256**` used as default by this crate).
///     As a bonus current setup allows to initialize PRNGs with arbitrarily-sized state.
///
///     Note that while this is serviceable, it is still recommended to use 256 bits of entropy to initialize PRNG for cryptographic purposes.
///
/// *   When invoking this function with parameters you need to make sure to provide reasonably good seeds.
///
///     From [`SeedableRng::from_seed`](rand::SeedableRng::from_seed) documentation:
///
///     *PRNG implementations are allowed to assume that bits in the seed are well distributed.
///     That means usually that the number of one and zero bits are roughly equal, and values like 0, 1 and (size - 1) are unlikely.
///     Note that many non-cryptographic PRNGs will show poor quality output if this is not adhered to.*
///
///     This also applies to `xoshiro256**` algorithm which is used in the default configuration of [`MathRand`](crate::lib::MathRand).
#[expect(non_camel_case_types)]
pub struct randomseed;

impl randomseed {
    /// Use provided RNG state.
    ///
    /// The state is expected to be shared with [`random`].
    pub fn with_state<R, Ty>(rng_state: RootCell<R>) -> impl TableEntry<Ty>
    where
        Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
        R: rand::SeedableRng + 'static,
    {
        struct RandomseedWith<R>(RootCell<R>);

        impl<Ty, R> TableEntry<Ty> for RandomseedWith<R>
        where
            Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
            R: rand::SeedableRng + 'static,
        {
            fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
                let RandomseedWith(state) = self;
                let fn_body = ffi::from_fn(
                    move || crate::ffi::math::randomseed(state.clone()),
                    "lua_std::std::math::randomseed",
                    (),
                );
                set_func("randomseed", fn_body).build(table, core);
            }
        }

        RandomseedWith(rng_state)
    }
}

impl<Ty, R> TableEntryEx<Ty, RootCell<R>> for randomseed
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    R: rand::SeedableRng + 'static,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>, rng_state: &mut RootCell<R>) {
        let state = rng_state.clone();
        let fn_body = ffi::from_fn(
            move || crate::ffi::math::randomseed(state.clone()),
            "lua_std::std::math::randomseed",
            (),
        );
        set_func("randomseed", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::sin, "lua_std::std::math::sin", ());
        set_func("sin", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::sqrt, "lua_std::std::math::sqrt", ());
        set_func("sqrt", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::tan, "lua_std::std::math::tan", ());
        set_func("tan", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(
            crate::ffi::math::tointeger,
            "lua_std::std::math::tointeger",
            (),
        );
        set_func("tointeger", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::type_, "lua_std::std::math::type", ());
        set_func("type", fn_body).build(table, core);
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
        let fn_body = ffi::from_fn(crate::ffi::math::ult, "lua_std::std::math::ult", ());
        set_func("ult", fn_body).build(table, core);
    }
}
