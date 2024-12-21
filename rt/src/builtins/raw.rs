use std::cmp::Ordering;

use crate::error::DroppedOr;
use crate::gc::Heap;
use crate::value::{Refs, Types, Value, WeakValue};

#[derive(Debug)]
pub struct MetamethodRequired;

/// Perform addition of two values.
///
/// Arithmetic builtins are defined only on numeric types, so only the following type pairing are permitted:
///
/// * `Int` + `Int`
/// * `Float` + `Float`
///
/// Consider performing a coercion step prior in case you want to perform mixed-type operation.
pub fn add<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) + Int(rhs)).into()),
        [Value::Float(lhs), Value::Float(rhs)] => Ok((Float(lhs) + Float(rhs)).into()),
        _ => Err(MetamethodRequired),
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
pub fn sub<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) - Int(rhs)).into()),
        [Value::Float(lhs), Value::Float(rhs)] => Ok((Float(lhs) - Float(rhs)).into()),
        _ => Err(MetamethodRequired),
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
pub fn mul<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) * Int(rhs)).into()),
        [Value::Float(lhs), Value::Float(rhs)] => Ok((Float(lhs) * Float(rhs)).into()),
        _ => Err(MetamethodRequired),
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
pub fn div<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Option<Value<Rf, Ty>>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(_), Value::Int(0)] => Ok(None),
        [Value::Int(lhs), Value::Int(rhs)] => Ok(Some((Int(lhs) / Int(rhs)).into())),
        [Value::Float(lhs), Value::Float(rhs)] => Ok(Some((Float(lhs) / Float(rhs)).into())),
        _ => Err(MetamethodRequired),
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
) -> Result<Option<Value<Rf, Ty>>, MetamethodRequired>
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
        _ => Err(MetamethodRequired),
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
pub fn rem<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Option<Value<Rf, Ty>>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(_), Value::Int(0)] => Ok(None),
        [Value::Int(lhs), Value::Int(rhs)] => Ok(Some((Int(lhs) % Int(rhs)).into())),
        [Value::Float(lhs), Value::Float(rhs)] => Ok(Some((Float(lhs) % Float(rhs)).into())),
        _ => Err(MetamethodRequired),
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
pub fn pow<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Option<Value<Rf, Ty>>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs).exp(Int(rhs))).map(Into::into)),
        [Value::Float(lhs), Value::Float(rhs)] => Ok(Some((Float(lhs).exp(Float(rhs))).into())),
        _ => Err(MetamethodRequired),
    }
}

pub fn bit_and<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) & Int(rhs)).into()),
        _ => Err(MetamethodRequired),
    }
}

pub fn bit_or<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) | Int(rhs)).into()),
        _ => Err(MetamethodRequired),
    }
}

pub fn bit_xor<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) ^ Int(rhs)).into()),
        _ => Err(MetamethodRequired),
    }
}

pub fn shl<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) << Int(rhs)).into()),
        _ => Err(MetamethodRequired),
    }
}

pub fn shr<Rf, Ty>(args: [Value<Rf, Ty>; 2]) -> Result<Value<Rf, Ty>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Ok((Int(lhs) >> Int(rhs)).into()),
        _ => Err(MetamethodRequired),
    }
}

pub fn eq<Ty>(args: [WeakValue<Ty>; 2], cmp_int_flt: bool) -> Result<bool, MetamethodRequired>
where
    Ty: Types,
{
    use crate::value::{Float, Int};

    if args[0] == args[1] {
        return Ok(true);
    }

    match args {
        [Value::Int(lhs), Value::Float(rhs)] if cmp_int_flt => {
            let r = PartialEq::eq(&Int(lhs), &Float(rhs));
            Ok(r)
        }
        [Value::Float(lhs), Value::Int(rhs)] if cmp_int_flt => {
            let r = PartialEq::eq(&Float(lhs), &Int(rhs));
            Ok(r)
        }
        [Value::Table(_), Value::Table(_)] | [Value::Userdata(_), Value::Userdata(_)] => {
            Err(MetamethodRequired)
        }
        _ => Ok(false),
    }
}

pub fn ne<Ty>(args: [WeakValue<Ty>; 2], cmp_int_flt: bool) -> Result<bool, MetamethodRequired>
where
    Ty: Types,
{
    eq(args, cmp_int_flt).map(|value| !value)
}

pub fn partial_cmp<Ty>(
    args: [WeakValue<Ty>; 2],
    heap: &Heap<Ty>,
    cmp_int_flt: bool,
) -> Result<Option<Ordering>, DroppedOr<MetamethodRequired>>
where
    Ty: Types,
{
    use crate::error::AlreadyDroppedError;
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => {
            let r = PartialOrd::partial_cmp(&Int(lhs), &Int(rhs));
            Ok(r)
        }
        [Value::Float(lhs), Value::Float(rhs)] => {
            let r = PartialOrd::partial_cmp(&Float(lhs), &Float(rhs));
            Ok(r)
        }
        [Value::Int(lhs), Value::Float(rhs)] if cmp_int_flt => {
            let r = PartialOrd::partial_cmp(&Int(lhs), &Float(rhs));
            Ok(r)
        }
        [Value::Float(lhs), Value::Int(rhs)] if cmp_int_flt => {
            let r = PartialOrd::partial_cmp(&Float(lhs), &Int(rhs));
            Ok(r)
        }
        [Value::String(lhs), Value::String(rhs)] => {
            let lhs = heap.get(lhs.0).ok_or(AlreadyDroppedError)?;
            let rhs = heap.get(rhs.0).ok_or(AlreadyDroppedError)?;
            let r = PartialOrd::partial_cmp(lhs, rhs);
            Ok(r)
        }
        _ => Err(DroppedOr::Other(MetamethodRequired)),
    }
}

pub fn lt<Ty>(
    args: [WeakValue<Ty>; 2],
    heap: &Heap<Ty>,
    cmp_int_flt: bool,
) -> Result<bool, DroppedOr<MetamethodRequired>>
where
    Ty: Types,
{
    partial_cmp(args, heap, cmp_int_flt).map(|ord| matches!(ord, Some(Ordering::Less)))
}

pub fn gt<Ty>(
    args: [WeakValue<Ty>; 2],
    heap: &Heap<Ty>,
    cmp_int_flt: bool,
) -> Result<bool, DroppedOr<MetamethodRequired>>
where
    Ty: Types,
{
    partial_cmp(args, heap, cmp_int_flt).map(|ord| matches!(ord, Some(Ordering::Greater)))
}

pub fn lt_eq<Ty>(
    args: [WeakValue<Ty>; 2],
    heap: &Heap<Ty>,
    cmp_int_flt: bool,
) -> Result<bool, DroppedOr<MetamethodRequired>>
where
    Ty: Types,
{
    partial_cmp(args, heap, cmp_int_flt)
        .map(|ord| matches!(ord, Some(Ordering::Less | Ordering::Equal)))
}

pub fn gt_eq<Ty>(
    args: [WeakValue<Ty>; 2],
    heap: &Heap<Ty>,
    cmp_int_flt: bool,
) -> Result<bool, DroppedOr<MetamethodRequired>>
where
    Ty: Types,
{
    partial_cmp(args, heap, cmp_int_flt)
        .map(|ord| matches!(ord, Some(Ordering::Less | Ordering::Equal)))
}

pub fn concat<Ty>(
    args: [WeakValue<Ty>; 2],
    heap: &mut Heap<Ty>,
) -> Result<WeakValue<Ty>, DroppedOr<MetamethodRequired>>
where
    Ty: Types,
{
    use crate::error::AlreadyDroppedError;
    use crate::gc::LuaPtr;
    use crate::value::{Concat, Len};

    match args {
        [Value::String(lhs), Value::String(rhs)] => {
            let lhs_value = heap.get(lhs.0).ok_or(AlreadyDroppedError)?.as_inner();
            let rhs_value = heap.get(rhs.0).ok_or(AlreadyDroppedError)?.as_inner();

            if rhs_value.is_empty() {
                Ok(Value::String(lhs))
            } else if lhs_value.is_empty() {
                Ok(Value::String(rhs))
            } else {
                let mut value = lhs_value.clone();
                value.concat(rhs_value);
                let ptr = heap.intern(value);
                Ok(Value::String(LuaPtr(ptr.downgrade())))
            }
        }
        _ => Err(DroppedOr::Other(MetamethodRequired)),
    }
}

pub fn neg<Rf, Ty>(args: [Value<Rf, Ty>; 1]) -> Result<Value<Rf, Ty>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(val)] => {
            let Int(r) = -Int(val);
            Ok(Value::Int(r))
        }
        [Value::Float(val)] => {
            let Float(r) = -Float(val);
            Ok(Value::Float(r))
        }
        _ => Err(MetamethodRequired),
    }
}

pub fn bit_not<Rf, Ty>(args: [Value<Rf, Ty>; 1]) -> Result<Value<Rf, Ty>, MetamethodRequired>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(val)] => {
            let Int(r) = !Int(val);
            Ok(Value::Int(r))
        }
        _ => Err(MetamethodRequired),
    }
}

pub fn len<Ty>(
    args: [WeakValue<Ty>; 1],
    heap: &Heap<Ty>,
) -> Result<usize, DroppedOr<MetamethodRequired>>
where
    Ty: Types,
{
    use crate::error::AlreadyDroppedError;
    use crate::value::Len;

    match args {
        [Value::String(val)] => {
            let val = heap.get(val.0).ok_or(AlreadyDroppedError)?;
            Ok(val.len())
        }
        _ => Err(DroppedOr::Other(MetamethodRequired)),
    }
}
