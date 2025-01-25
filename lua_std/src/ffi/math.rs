//! Math library
//!
//! # From Lua documentation
//!
//! This library provides basic mathematical functions.
//! It provides all its functions and constants inside the table `math`.
//! Functions with the annotation "integer/float" give integer results for integer arguments and float results for non-integer arguments.
//! The rounding functions `math.ceil`, `math.floor`, and `math.modf` return an integer when the result fits in the range of an integer, or a float otherwise.
//!
//! # Implementation-specific behavior
//!
//! All functions inside this module will accept either integers or floats (just like Lua expects it to be)
//! except [`ult`] which accepts only integer inputs.
//! Integers will get coerced to floats when necessary.
//! See documentation of individual methods to see when coercions are applied.
//!
//! ## Integer-to-float coercions
//!
//! Integers are cast to floats following [Lua convention](rt::value::Float#impl-From<Int>-for-Float).
//! Currently it's implemented exactly the same way as [Rust's numeric int-to-float casts][rust_ref#numeric-cast].
//! Regardless, you should be aware that for big integers it may silently lose precision.
//!
//! [rust_ref#numeric-cast]: https://doc.rust-lang.org/reference/expressions/operator-expr.html#numeric-cast

use rt::ffi::{self, delegate, LuaFfi};
use rt::value::Types;

/// Compute absolute value of a number.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int) -> int`
/// * `(x: float) -> float`
///
/// Returns the maximum value between `x` and `-x`. (integer/float)
pub fn abs<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};

    let body = || {
        delegate::from_mut(|mut rt| {
            let n: Number = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let res = match n {
                Number::Int(v) => Number::Int(v.abs()),
                Number::Float(v) => Number::Float(v.abs()),
            };

            rt.stack.transient().push(res.into());
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::abs", ())
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
pub fn acos<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Float, Number, ParseArgs};
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let n: Number = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let Float(n) = n.to_float();

            let res = n.acos();

            rt.stack.transient().push(Value::Float(res));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::acos", ())
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
pub fn asin<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Float, Number, ParseArgs};
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let n: Number = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let Float(n) = n.to_float();

            let res = n.asin();

            rt.stack.transient().push(Value::Float(res));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::asin", ())
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
pub fn atan<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, Opts, ParseArgs, Split};
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let (y, x): (Number, Opts<(Number,)>) = rt.stack.parse(&mut rt.core.gc)?;
            let (x,) = x.split();
            let y = y.to_float().0;
            let x = x.map(|t| t.to_float().0).unwrap_or(1.0);
            rt.stack.clear();

            let res = y.atan2(x);

            rt.stack.transient().push(Value::Float(res));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::atan", ())
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
pub fn ceil<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Float, Int, Number, ParseArgs};

    let body = || {
        delegate::from_mut(|mut rt| {
            let x: Number = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let res = match x {
                Number::Int(x) => Number::Int(x),
                Number::Float(x) => {
                    let x = x.ceil();
                    match Float(x).try_into() {
                        Ok(Int(x)) => Number::Int(x),
                        Err(_) => Number::Float(x),
                    }
                }
            };

            rt.stack.transient().push(res.into());
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::ceil", ())
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
pub fn cos<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let x: Number = rt.stack.parse(&mut rt.core.gc)?;
            let x = x.to_float().0;
            rt.stack.clear();

            let res = x.cos();

            rt.stack.transient().push(Value::Float(res));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::cos", ())
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
pub fn deg<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let x: Number = rt.stack.parse(&mut rt.core.gc)?;
            let x = x.to_float().0;
            rt.stack.clear();

            let res = x.to_degrees();

            rt.stack.transient().push(Value::Float(res));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::deg", ())
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
pub fn exp<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let x: Number = rt.stack.parse(&mut rt.core.gc)?;
            let x = x.to_float().0;
            rt.stack.clear();

            let res = x.exp();

            rt.stack.transient().push(Value::Float(res));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::exp", ())
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
pub fn floor<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Float, Int, Number, ParseArgs};

    let body = || {
        delegate::from_mut(|mut rt| {
            let x: Number = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let res = match x {
                Number::Int(x) => Number::Int(x),
                Number::Float(x) => {
                    let x = x.floor();
                    match Float(x).try_into() {
                        Ok(Int(x)) => Number::Int(x),
                        Err(_) => Number::Float(x),
                    }
                }
            };

            rt.stack.transient().push(res.into());
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::floor", ())
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
pub fn fmod<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::error::SignatureError;
    use rt::ffi::arg_parser::NotNumberError;
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let slice = rt.stack.as_slice();
            match slice {
                &[Value::Int(x), Value::Int(y)] => {
                    rt.stack.clear();

                    let res = x % y;

                    rt.stack.transient().push(Value::Int(res));
                    Ok(())
                }
                &[Value::Float(x), Value::Float(y)] => {
                    rt.stack.clear();

                    let res = x - (x / y).trunc() * y;

                    rt.stack.transient().push(Value::Float(res));
                    Ok(())
                }
                [Value::Int(_), y] => {
                    let err = SignatureError::ConversionFailure {
                        index: 1,
                        msg: format!("expected integer, found {}", y.type_()),
                    };
                    Err(err.into())
                }
                [Value::Float(_), y] => {
                    let err = SignatureError::ConversionFailure {
                        index: 1,
                        msg: format!("expected float, found {}", y.type_()),
                    };
                    Err(err.into())
                }
                [x, _] => {
                    let err = SignatureError::ConversionFailure {
                        index: 0,
                        msg: NotNumberError(x.type_()).to_string(),
                    };
                    Err(err.into())
                }
                [] | [_] => {
                    let err = SignatureError::TooFewArgs { found: slice.len() };
                    Err(err.into())
                }
                _ => {
                    let err = SignatureError::TooManyArgs {
                        found: slice.len(),
                        expected: 2,
                    };
                    Err(err.into())
                }
            }
        })
    };

    ffi::from_fn(body, "lua_std::math::fmod", ())
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
pub fn log<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, Opts, ParseArgs, Split};
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let (x, base): (Number, Opts<(Number,)>) = rt.stack.parse(&mut rt.core.gc)?;
            let x = x.to_float().0;
            let (base,) = base.split();
            let base = base.map(|x| x.to_float().0);
            rt.stack.clear();

            let res = if let Some(base) = base {
                x.log(base)
            } else {
                x.ln()
            };

            rt.stack.transient().push(Value::Float(res));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::log", ())
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
pub fn modf<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Float, FormatReturns, Int, Number, ParseArgs};

    let body = || {
        delegate::from_mut(|mut rt| {
            let x: Number = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let res = match x {
                Number::Int(n) => (Number::Int(n), Float(0.0)),
                Number::Float(x) => {
                    let integral = x.floor();
                    let fract = (x - integral).clamp(0.0, 1.0);
                    let integral = match Float(integral).try_into() {
                        Ok(Int(x)) => Number::Int(x),
                        Err(_) => Number::Float(x),
                    };

                    (integral, Float(fract))
                }
            };

            rt.stack.transient().format(res);
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::modf", ())
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
pub fn rad<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let x: Number = rt.stack.parse(&mut rt.core.gc)?;
            let x = x.to_float().0;
            rt.stack.clear();

            let res = x.to_radians();

            rt.stack.transient().push(Value::Float(res));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::rad", ())
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
pub fn sin<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let x: Number = rt.stack.parse(&mut rt.core.gc)?;
            let x = x.to_float().0;
            rt.stack.clear();

            let res = x.sin();

            rt.stack.transient().push(Value::Float(res));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::sin", ())
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
pub fn sqrt<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let x: Number = rt.stack.parse(&mut rt.core.gc)?;
            let x = x.to_float().0;
            rt.stack.clear();

            let res = x.sqrt();

            rt.stack.transient().push(Value::Float(res));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::sqrt", ())
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
pub fn tan<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let x: Number = rt.stack.parse(&mut rt.core.gc)?;
            let x = x.to_float().0;
            rt.stack.clear();

            let res = x.tan();

            rt.stack.transient().push(Value::Float(res));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::tan", ())
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
pub fn tointeger<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Float, FormatReturns, Int, NilOr, Number, ParseArgs};

    let body = || {
        delegate::from_mut(|mut rt| {
            let x: Number = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let res = match x {
                Number::Int(n) => NilOr::Some(Int(n)),
                Number::Float(x) => match Float(x).try_into() {
                    Ok(n) => NilOr::Some(n),
                    Err(_) => NilOr::Nil,
                },
            };

            rt.stack.transient().format(res);
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::tointeger", ())
}

/// Discriminate between integers and floats.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> string | fail`
///
/// Returns "integer" if `x` is an integer, "float" if it is a float, or **fail** if `x` is not a number.
pub fn type_<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{FormatReturns, LuaString, NilOr, ParseArgs};
    use rt::gc::LuaPtr;
    use rt::value::{Type, Value};

    let body = || {
        delegate::from_mut(|mut rt| {
            let x: Value<_, _> = rt.stack.as_slice().parse(&mut rt.core.gc)?;

            rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                stack.clear();

                let res = match x.type_() {
                    Type::Int => {
                        let s = heap.intern("integer".into());
                        NilOr::Some(LuaString(LuaPtr(s.downgrade())))
                    }
                    Type::Float => {
                        let s = heap.intern("float".into());
                        NilOr::Some(LuaString(LuaPtr(s.downgrade())))
                    }
                    _ => NilOr::Nil,
                };

                stack.format(res);
                Ok(())
            })
        })
    };

    ffi::from_fn(body, "lua_std::math::type", ())
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
pub fn ult<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Int, ParseArgs};
    use rt::value::Value;

    let body = || {
        delegate::from_mut(|mut rt| {
            let (Int(x), Int(y)) = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let res = (x as u64) < (y as u64);

            rt.stack.transient().push(Value::Bool(res));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::math::ult", ())
}
