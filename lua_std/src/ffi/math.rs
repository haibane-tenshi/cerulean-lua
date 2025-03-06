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

use gc::RootCell;
use rt::ffi::delegate::{self, Delegate};
use rt::ffi::DLuaFfi;
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
///
/// # Implementation-specific behavior
///
/// * See [`i64::abs`] for details on behavior on overflow.
pub fn abs<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};

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
pub fn acos<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Float, Number, ParseArgs};
    use rt::value::Value;

    delegate::from_mut(|mut rt| {
        let n: Number = rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();

        let Float(n) = n.to_float();

        let res = n.acos();

        rt.stack.transient().push(Value::Float(res));
        Ok(())
    })
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
pub fn asin<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Float, Number, ParseArgs};
    use rt::value::Value;

    delegate::from_mut(|mut rt| {
        let n: Number = rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();

        let Float(n) = n.to_float();

        let res = n.asin();

        rt.stack.transient().push(Value::Float(res));
        Ok(())
    })
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
pub fn atan<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, Opts, ParseArgs, Split};
    use rt::value::Value;

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
pub fn ceil<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Float, Int, Number, ParseArgs};

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
pub fn cos<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    delegate::from_mut(|mut rt| {
        let x: Number = rt.stack.parse(&mut rt.core.gc)?;
        let x = x.to_float().0;
        rt.stack.clear();

        let res = x.cos();

        rt.stack.transient().push(Value::Float(res));
        Ok(())
    })
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
pub fn deg<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    delegate::from_mut(|mut rt| {
        let x: Number = rt.stack.parse(&mut rt.core.gc)?;
        let x = x.to_float().0;
        rt.stack.clear();

        let res = x.to_degrees();

        rt.stack.transient().push(Value::Float(res));
        Ok(())
    })
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
pub fn exp<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    delegate::from_mut(|mut rt| {
        let x: Number = rt.stack.parse(&mut rt.core.gc)?;
        let x = x.to_float().0;
        rt.stack.clear();

        let res = x.exp();

        rt.stack.transient().push(Value::Float(res));
        Ok(())
    })
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
pub fn floor<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Float, Int, Number, ParseArgs};

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
pub fn fmod<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::error::SignatureError;
    use rt::ffi::arg_parser::NotNumberError;
    use rt::value::Value;

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
pub fn log<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, Opts, ParseArgs, Split};
    use rt::value::Value;

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
pub fn modf<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Float, FormatReturns, Int, Number, ParseArgs};

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
pub fn rad<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    delegate::from_mut(|mut rt| {
        let x: Number = rt.stack.parse(&mut rt.core.gc)?;
        let x = x.to_float().0;
        rt.stack.clear();

        let res = x.to_radians();

        rt.stack.transient().push(Value::Float(res));
        Ok(())
    })
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
pub fn sin<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    delegate::from_mut(|mut rt| {
        let x: Number = rt.stack.parse(&mut rt.core.gc)?;
        let x = x.to_float().0;
        rt.stack.clear();

        let res = x.sin();

        rt.stack.transient().push(Value::Float(res));
        Ok(())
    })
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
pub fn sqrt<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    delegate::from_mut(|mut rt| {
        let x: Number = rt.stack.parse(&mut rt.core.gc)?;
        let x = x.to_float().0;
        rt.stack.clear();

        let res = x.sqrt();

        rt.stack.transient().push(Value::Float(res));
        Ok(())
    })
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
pub fn tan<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Number, ParseArgs};
    use rt::value::Value;

    delegate::from_mut(|mut rt| {
        let x: Number = rt.stack.parse(&mut rt.core.gc)?;
        let x = x.to_float().0;
        rt.stack.clear();

        let res = x.tan();

        rt.stack.transient().push(Value::Float(res));
        Ok(())
    })
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
pub fn tointeger<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Float, FormatReturns, Int, NilOr, Number, ParseArgs};

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
}

/// Discriminate between integers and floats.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(x: int | float) -> string | fail`
///
/// Returns "integer" if `x` is an integer, "float" if it is a float, or **fail** if `x` is not a number.
pub fn type_<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{FormatReturns, LuaString, NilOr, ParseArgs};
    use rt::gc::LuaPtr;
    use rt::value::{Type, Value};

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
pub fn ult<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::{Int, ParseArgs};
    use rt::value::Value;

    delegate::from_mut(|mut rt| {
        let (Int(x), Int(y)) = rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();

        let res = (x as u64) < (y as u64);

        rt.stack.transient().push(Value::Bool(res));
        Ok(())
    })
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
pub fn min<Ty>() -> impl Delegate<Ty>
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    use rt::builtins::coerce::CoerceArgs;
    use rt::builtins::raw::{lt, MetamethodRequired};
    use rt::ffi::arg_parser::FormatReturns;
    use rt::ffi::delegate::{Request, StackSlot};
    use rt::gc::{Downgrade, Upgrade};
    use rt::value::{Callable, Strong, Value};
    use std::ops::ControlFlow;

    enum State<Ty>
    where
        Ty: Types,
    {
        Started,
        CalledLt {
            current: Value<Strong, Ty>,
            lt_callable: Callable<Strong, Ty>,
            count: StackSlot,
        },
        Finished,
    }

    let mut state = State::Started;
    delegate::try_repeat(move |mut rt| {
        let current = std::mem::replace(&mut state, State::Finished);
        match current {
            State::Started => rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                match stack.as_slice() {
                    [] => {
                        use rt::error::SignatureError;

                        let err = SignatureError::TooFewArgs { found: 0 };
                        Err(err.into())
                    }
                    [_] => Ok(delegate::State::Complete(())),
                    [arg0, rest @ ..] => {
                        let mut current = *arg0;
                        let cmp_int_flt = CoerceArgs::<Ty>::cmp_float_and_int(&rt.core.dialect);

                        for (i, next) in rest.iter().copied().enumerate() {
                            let cmp = match lt([next, current], heap, cmp_int_flt)? {
                                ControlFlow::Break(MetamethodRequired) => {
                                    // +1 to account for first arg which is not part of `rest`.
                                    // This leaves rhs in first stack slot.
                                    let _ = stack.drain(..StackSlot(i + 1));
                                    let count = stack.top();

                                    stack.format([next, current]);

                                    let current = current.try_upgrade(heap)?;
                                    let lt_callable = {
                                        use rt::builtins::full::lt as lt_ffi;
                                        use rt::ffi::{self, boxed};
                                        use rt::gc::LuaPtr;
                                        use rt::value::Callable;

                                        let f = ffi::from_fn(lt_ffi::<Ty>, "rt::builtins::lt", ());
                                        let f = heap.alloc_cell(boxed(f));
                                        Callable::Rust(LuaPtr(f))
                                    };

                                    let request = Request::Invoke {
                                        callable: lt_callable.clone(),
                                        start: count,
                                    };

                                    state = State::CalledLt {
                                        current,
                                        lt_callable,
                                        count,
                                    };
                                    return Ok(delegate::State::Yielded(request));
                                }
                                ControlFlow::Continue(cmp) => cmp,
                            };

                            current = if cmp { next } else { current };
                        }

                        stack.clear();
                        stack.push(current);

                        Ok(delegate::State::Complete(()))
                    }
                }
            }),
            State::CalledLt {
                current,
                lt_callable,
                count,
            } => rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                assert_eq!(
                    stack.len(),
                    count.0 + 1,
                    "lt should produce a single boolean"
                );

                let Some(Value::Bool(cmp)) = stack.pop() else {
                    unreachable!("lt should produce a single boolean");
                };

                let mut current = if cmp {
                    *stack.first().unwrap()
                } else {
                    current.downgrade()
                };

                let cmp_int_flt = CoerceArgs::<Ty>::cmp_float_and_int(&rt.core.dialect);

                for (i, next) in stack[1..].iter().copied().enumerate() {
                    let cmp = match lt([next, current], heap, cmp_int_flt)? {
                        ControlFlow::Break(MetamethodRequired) => {
                            // +1 to account for first arg which is not part of `rest`.
                            // This leaves rhs in first stack slot.
                            let _ = stack.drain(..StackSlot(i + 1));
                            let count = stack.top();

                            stack.format([next, current]);

                            let current = current.try_upgrade(heap)?;
                            let request = Request::Invoke {
                                callable: lt_callable.clone(),
                                start: count,
                            };

                            state = State::CalledLt {
                                current,
                                lt_callable,
                                count,
                            };
                            return Ok(delegate::State::Yielded(request));
                        }
                        ControlFlow::Continue(cmp) => cmp,
                    };

                    current = if cmp { next } else { current };
                }

                stack.clear();
                stack.push(current);

                Ok(delegate::State::Complete(()))
            }),
            State::Finished => Ok(delegate::State::Complete(())),
        }
    })
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
pub fn max<Ty>() -> impl Delegate<Ty>
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    use rt::builtins::coerce::CoerceArgs;
    use rt::builtins::raw::{gt, MetamethodRequired};
    use rt::ffi::arg_parser::FormatReturns;
    use rt::ffi::delegate::{Request, StackSlot};
    use rt::gc::{Downgrade, Upgrade};
    use rt::value::{Callable, Strong, Value};
    use std::ops::ControlFlow;

    enum State<Ty>
    where
        Ty: Types,
    {
        Started,
        CalledGt {
            current: Value<Strong, Ty>,
            gt_callable: Callable<Strong, Ty>,
            count: StackSlot,
        },
        Finished,
    }

    let mut state = State::Started;
    delegate::try_repeat(move |mut rt| {
        let current = std::mem::replace(&mut state, State::Finished);
        match current {
            State::Started => rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                match stack.as_slice() {
                    [] => {
                        use rt::error::SignatureError;

                        let err = SignatureError::TooFewArgs { found: 0 };
                        Err(err.into())
                    }
                    [_] => Ok(delegate::State::Complete(())),
                    [arg0, rest @ ..] => {
                        let mut current = *arg0;
                        let cmp_int_flt = CoerceArgs::<Ty>::cmp_float_and_int(&rt.core.dialect);

                        for (i, next) in rest.iter().copied().enumerate() {
                            let cmp = match gt([next, current], heap, cmp_int_flt)? {
                                ControlFlow::Break(MetamethodRequired) => {
                                    // +1 to account for first arg which is not part of `rest`.
                                    // This leaves rhs in first stack slot.
                                    let _ = stack.drain(..StackSlot(i + 1));
                                    let count = stack.top();

                                    stack.format([next, current]);

                                    let current = current.try_upgrade(heap)?;
                                    let gt_callable = {
                                        use rt::builtins::full::gt as gt_ffi;
                                        use rt::ffi::{self, boxed};
                                        use rt::gc::LuaPtr;
                                        use rt::value::Callable;

                                        let f = ffi::from_fn(gt_ffi::<Ty>, "rt::builtins::gt", ());
                                        let f = heap.alloc_cell(boxed(f));
                                        Callable::Rust(LuaPtr(f))
                                    };

                                    let request = Request::Invoke {
                                        callable: gt_callable.clone(),
                                        start: count,
                                    };

                                    state = State::CalledGt {
                                        current,
                                        gt_callable,
                                        count,
                                    };
                                    return Ok(delegate::State::Yielded(request));
                                }
                                ControlFlow::Continue(cmp) => cmp,
                            };

                            current = if cmp { next } else { current };
                        }

                        stack.clear();
                        stack.push(current);

                        Ok(delegate::State::Complete(()))
                    }
                }
            }),
            State::CalledGt {
                current,
                gt_callable,
                count,
            } => rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                assert_eq!(
                    stack.len(),
                    count.0 + 1,
                    "lt should produce a single boolean"
                );

                let Some(Value::Bool(cmp)) = stack.pop() else {
                    unreachable!("lt should produce a single boolean");
                };

                let mut current = if cmp {
                    *stack.first().unwrap()
                } else {
                    current.downgrade()
                };

                let cmp_int_flt = CoerceArgs::<Ty>::cmp_float_and_int(&rt.core.dialect);

                for (i, next) in stack[1..].iter().copied().enumerate() {
                    let cmp = match gt([next, current], heap, cmp_int_flt)? {
                        ControlFlow::Break(MetamethodRequired) => {
                            // +1 to account for first arg which is not part of `rest`.
                            // This leaves rhs in first stack slot.
                            let _ = stack.drain(..StackSlot(i + 1));
                            let count = stack.top();

                            stack.format([next, current]);

                            let current = current.try_upgrade(heap)?;
                            let request = Request::Invoke {
                                callable: gt_callable.clone(),
                                start: count,
                            };

                            state = State::CalledGt {
                                current,
                                gt_callable,
                                count,
                            };
                            return Ok(delegate::State::Yielded(request));
                        }
                        ControlFlow::Continue(cmp) => cmp,
                    };

                    current = if cmp { next } else { current };
                }

                stack.clear();
                stack.push(current);

                Ok(delegate::State::Complete(()))
            }),
            State::Finished => Ok(delegate::State::Complete(())),
        }
    })
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
///     See [`MathRand`](crate::lib::MathRand) documentation for more information about defaults and pointers for custom configuration.
pub fn random<Ty, R>(rng_state: RootCell<R>) -> impl Delegate<Ty>
where
    Ty: Types,
    R: rand::Rng + 'static,
{
    use rt::ffi::arg_parser::{Int, Number, Opts, ParseArgs, Split};

    delegate::from_mut(move |mut rt| {
        let args: Opts<(Int, Int)> = rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();

        let rng = rt.core.gc.get_root_mut(&rng_state);

        let res = match args.split() {
            (None, None) => Number::Float(rng.gen()),
            (Some(Int(m)), Some(Int(n))) => Number::Int(rng.gen_range(m..=n)),
            (Some(Int(0)), None) => Number::Int(rng.gen()),
            (Some(Int(n)), None) if n > 0 => Number::Int(rng.gen_range(1..=n)),
            (Some(_), None) => {
                use rt::error::SignatureError;

                let err = SignatureError::ConversionFailure {
                    index: 0,
                    msg: String::from(
                        "random expects an non-zero integer when invoked with single argument",
                    ),
                };

                return Err(err.into());
            }
            (None, Some(_)) => unreachable!(),
        };

        rt.stack.transient().push(res.into());
        Ok(())
    })
}

/// Seed pseudo-random generator used by `random`.
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
pub fn randomseed<Ty, R>(rng_state: RootCell<R>) -> impl Delegate<Ty>
where
    Ty: Types,
    R: rand::SeedableRng + 'static,
{
    use rand::SeedableRng;
    use rt::ffi::arg_parser::{Int, Opts, ParseArgs, Split};

    delegate::from_mut(move |mut rt| {
        let args: Opts<(Int, Int)> = rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();

        let new_rng = match args.split() {
            (None, None) => R::from_entropy(),
            (Some(Int(x)), None) => R::seed_from_u64(x as u64),
            (Some(Int(x)), Some(Int(y))) => {
                use rand_hc::Hc128Rng;

                let seed = {
                    let mut r = [0; 32];
                    r[0..16].copy_from_slice(&x.to_be_bytes());
                    r[16..32].copy_from_slice(&y.to_be_bytes());

                    r
                };

                let seeder = Hc128Rng::from_seed(seed);
                R::from_rng(seeder).map_err(|err| rt.core.alloc_error_msg(err.to_string()))?
            }
            (None, Some(_)) => unreachable!(),
        };

        let rng = rt.core.gc.get_root_mut(&rng_state);
        *rng = new_rng;

        Ok(())
    })
}
