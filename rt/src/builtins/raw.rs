use crate::value::{Refs, Types, Value};

#[derive(Debug)]
pub struct IncompatibleTypesError;

/// Perform addition of two values.
///
/// Arithmetic builtins are defined only on numeric types, so only the following type pairing are permitted:
///
/// * `Int` + `Int`
/// * `Float` + `Float`
///
/// Consider performing a coercion step prior in case you want to perform mixed-type operation.
pub fn add<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, IncompatibleTypesError>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) + Int(rhs)).into()),
        [Value::Float(lhs), Value::Float(rhs)] => Ok((Float(lhs) + Float(rhs)).into()),
        _ => Err(IncompatibleTypesError),
    }
}

/// Perform subtraction between two values.
///
/// Arithmetic builtins are defined only on numeric types, so only the following type pairing are permitted:
///
/// * `Int` + `Int`
/// * `Float` + `Float`
///
/// Consider performing a coercion step prior in case you want to perform mixed-type operation.
pub fn sub<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, IncompatibleTypesError>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) - Int(rhs)).into()),
        [Value::Float(lhs), Value::Float(rhs)] => Ok((Float(lhs) - Float(rhs)).into()),
        _ => Err(IncompatibleTypesError),
    }
}

/// Perform multiplication of two values.
///
/// Arithmetic builtins are defined only on numeric types, so only the following type pairing are permitted:
///
/// * `Int` + `Int`
/// * `Float` + `Float`
///
/// Consider performing a coercion step prior in case you want to perform mixed-type operation.
pub fn mul<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, IncompatibleTypesError>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) * Int(rhs)).into()),
        [Value::Float(lhs), Value::Float(rhs)] => Ok((Float(lhs) * Float(rhs)).into()),
        _ => Err(IncompatibleTypesError),
    }
}

/// Perform division between two values.
///
/// Arithmetic builtins are defined only on numeric types, so only the following type pairing are permitted:
///
/// * `Int` + `Int`
/// * `Float` + `Float`
///
/// Consider performing a coercion step prior in case you want to perform mixed-type operation.
///
/// # Behavior
///
/// This function does not perform argument coercions, as such division between two integers will result in an integer.
/// This can be considered divergent from Lua spec which states that integers are always converted to floats before division.
///
/// Native behavior can be recovered by performing a coercion step prior to calling this function.
/// Raw builtins explicitly are not responsible for argument coercion, so it is up to the user to enforce correct/expected behavior.
///
/// # Division by 0
///
/// The operation between integers is *checked*, that is if `rhs` is 0 `None` will be returned.
///
/// The operation between floats is unchecked and will result in NaN.
pub fn div<Rf, Ty>(
    args: [Value<Rf, Ty>; 2],
) -> Result<Option<Value<Rf, Ty>>, IncompatibleTypesError>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(_), Value::Int(0)] => Ok(None),
        [Value::Int(lhs), Value::Int(rhs)] => Ok(Some((Int(lhs) / Int(rhs)).into())),
        [Value::Float(lhs), Value::Float(rhs)] => Ok(Some((Float(lhs) / Float(rhs)).into())),
        _ => Err(IncompatibleTypesError),
    }
}

/// Perform flooring division between two values.
///
/// Arithmetic builtins are defined only on numeric types, so only the following type pairing are permitted:
///
/// * `Int` + `Int`
/// * `Float` + `Float`
///
/// Consider performing a coercion step prior in case you want to perform mixed-type operation.
///
/// # Division by 0
///
/// The operation between integers is *checked*, that is if `rhs` is 0 `None` will be returned.
///
/// The operation between floats is unchecked and will result in NaN.
pub fn floor_div<Rf, Ty>(
    args: [Value<Rf, Ty>; 2],
) -> Result<Option<Value<Rf, Ty>>, IncompatibleTypesError>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(_), Value::Int(0)] => Ok(None),
        [Value::Int(lhs), Value::Int(rhs)] => Ok(Some((Int(lhs).floor_div(Int(rhs))).into())),
        [Value::Float(lhs), Value::Float(rhs)] => {
            Ok(Some((Float(lhs).floor_div(Float(rhs))).into()))
        }
        _ => Err(IncompatibleTypesError),
    }
}

/// Find reminder of division between two values.
///
/// Arithmetic builtins are defined only on numeric types, so only the following type pairing are permitted:
///
/// * `Int` + `Int`
/// * `Float` + `Float`
///
/// Consider performing a coercion step prior in case you want to perform mixed-type operation.
///
/// # Behavior
///
/// This function does not perform argument coercions, as such division between two integers will result in an integer.
/// This can be considered divergent from Lua spec which states that integers are always converted to floats before division.
///
/// Native behavior can be recovered by performing a coercion step prior to calling this function.
/// Raw builtins explicitly are not responsible for argument coercion, so it is up to the user to enforce correct/expected behavior.
///
/// In Lua reminders are always non-negative, in Rust this is an equivalent of [`i64::rem_euclid`] and [`f64::rem_euclid`].
///
/// # Division by 0
///
/// The operation between integers is *checked*, that is if `rhs` is 0 `None` will be returned.
///
/// The operation between floats is unchecked and will result in NaN.
pub fn rem<Rf, Ty>(
    args: [Value<Rf, Ty>; 2],
) -> Result<Option<Value<Rf, Ty>>, IncompatibleTypesError>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(_), Value::Int(0)] => Ok(None),
        [Value::Int(lhs), Value::Int(rhs)] => Ok(Some((Int(lhs) % Int(rhs)).into())),
        [Value::Float(lhs), Value::Float(rhs)] => Ok(Some((Float(lhs) % Float(rhs)).into())),
        _ => Err(IncompatibleTypesError),
    }
}

/// Perform division between two values.
///
/// Arithmetic builtins are defined only on numeric types, so only the following type pairing are permitted:
///
/// * `Int` + `Int`
/// * `Float` + `Float`
///
/// Consider performing a coercion step prior in case you want to perform mixed-type operation.
///
/// # Behavior
///
/// This function does not perform argument coercions, as such exponentiation between two integers will result in an integer.
/// This can be considered divergent from Lua spec which states that integers are always converted to floats before exponentiation.
///
/// Native behavior can be recovered by performing a coercion step prior to calling this function.
/// Raw builtins explicitly are not responsible for argument coercion, so it is up to the user to enforce correct/expected behavior.
///
/// # Power of 0
///
/// 0^0 is defined as 1 both for integers and floats.
///
/// # Returns
///
/// Currently this function will return a `None` if the power argument is too large (bigger that `u32::MAX`).
/// This restriction will likely be lifted in the future.
pub fn pow<Rf, Ty>(
    args: [Value<Rf, Ty>; 2],
) -> Result<Option<Value<Rf, Ty>>, IncompatibleTypesError>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs).exp(Int(rhs))).map(Into::into)),
        [Value::Float(lhs), Value::Float(rhs)] => Ok(Some((Float(lhs).exp(Float(rhs))).into())),
        _ => Err(IncompatibleTypesError),
    }
}

pub fn bit_and<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, IncompatibleTypesError>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) & Int(rhs)).into()),
        _ => Err(IncompatibleTypesError),
    }
}

pub fn bit_or<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, IncompatibleTypesError>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) | Int(rhs)).into()),
        _ => Err(IncompatibleTypesError),
    }
}

pub fn bit_xor<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, IncompatibleTypesError>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) ^ Int(rhs)).into()),
        _ => Err(IncompatibleTypesError),
    }
}

pub fn shl<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, IncompatibleTypesError>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) << Int(rhs)).into()),
        _ => Err(IncompatibleTypesError),
    }
}

pub fn shr<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, IncompatibleTypesError>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) >> Int(rhs)).into()),
        _ => Err(IncompatibleTypesError),
    }
}
