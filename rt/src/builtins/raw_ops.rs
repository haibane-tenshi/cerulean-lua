//! Raw operations on Lua [`Value`]s.
//!
//! Lua behavior when evaluating basic ops can be separated into three steps:
//!
//! 1. coercion
//! 2. **raw builtins** *<- you are here*
//! 3. metamethod call
//!
//! Methods within this module correspond to the second step.
//!
//! Generally speaking there are only two reasons for you to reach to this module.
//!
//! First, you may want to implement Lua ops behavior from basic bits.
//! Actually, APIs provided here is exactly what backs runtime behavior.
//!
//! Second, you may want to *test whether op can be performed without calling a metamethod*.
//! Using metamethods is an involved process:
//! you need to inspect relevant metatables of involved inputs,
//! resolve entry using correct metamethod name, arrange call stack,
//! perform actual request to runtime,
//! and when it is evaluated clean up return values according to op semantics.
//! Normally it is recommended that you delegate all this work to methods from [`builtins::full`](crate::builtins::full) module.
//!
//! However, those methods themselves need to be invoked inside runtime.
//! Sometimes this behavior can be superfluous, especially if you know that op is unlikely to require metamethod.
//! This is when functions inside this module can become handy.
//! Their return type contains `ControlFlow<MetamethodRequired, _>`,
//! so when you receive `Continue(_)` you can be assured that it is exactly what the op is supposed to evaluate to.

use std::cmp::Ordering;
use std::hash::Hash;
use std::ops::ControlFlow;
use ControlFlow::{Break, Continue};

use gc::{Interned, Root, Trace};
use repr::opcode::{BinOp as BinaryOp, UnaOp as UnaryOp};

use crate::error::AlreadyDroppedError;
use crate::gc::Heap;
use crate::value::{Int, Refs, Types, Value, WeakValue};

/// Operation cannot be resolved using raw builtin behavior and require a metamethod call.
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
pub fn add<Ty>(args: [WeakValue<Ty>; 2]) -> ControlFlow<MetamethodRequired, WeakValue<Ty>>
where
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Continue((Int(lhs) + Int(rhs)).into()),
        [Value::Float(lhs), Value::Float(rhs)] => Continue((Float(lhs) + Float(rhs)).into()),
        _ => Break(MetamethodRequired),
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
pub fn sub<Ty>(args: [WeakValue<Ty>; 2]) -> ControlFlow<MetamethodRequired, WeakValue<Ty>>
where
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Continue((Int(lhs) - Int(rhs)).into()),
        [Value::Float(lhs), Value::Float(rhs)] => Continue((Float(lhs) - Float(rhs)).into()),
        _ => Break(MetamethodRequired),
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
pub fn mul<Ty>(args: [WeakValue<Ty>; 2]) -> ControlFlow<MetamethodRequired, WeakValue<Ty>>
where
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Continue((Int(lhs) * Int(rhs)).into()),
        [Value::Float(lhs), Value::Float(rhs)] => Continue((Float(lhs) * Float(rhs)).into()),
        _ => Break(MetamethodRequired),
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
pub fn div<Ty>(args: [WeakValue<Ty>; 2]) -> ControlFlow<MetamethodRequired, Option<WeakValue<Ty>>>
where
    Ty: Types,
{
    use crate::value::ops::CheckedDiv;
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => {
            Continue(Int(lhs).checked_div(Int(rhs)).map(Into::into))
        }
        [Value::Float(lhs), Value::Float(rhs)] => Continue(Some((Float(lhs) / Float(rhs)).into())),
        _ => Break(MetamethodRequired),
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
pub fn floor_div<Ty>(
    args: [WeakValue<Ty>; 2],
) -> ControlFlow<MetamethodRequired, Option<WeakValue<Ty>>>
where
    Ty: Types,
{
    use crate::value::ops::{CheckedFloorDiv, FloorDiv};
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => {
            Continue(Int(lhs).checked_floor_div(Int(rhs)).map(Into::into))
        }
        [Value::Float(lhs), Value::Float(rhs)] => {
            Continue(Some((Float(lhs).floor_div(Float(rhs))).into()))
        }
        _ => Break(MetamethodRequired),
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
pub fn rem<Ty>(args: [WeakValue<Ty>; 2]) -> ControlFlow<MetamethodRequired, Option<WeakValue<Ty>>>
where
    Ty: Types,
{
    use crate::value::ops::CheckedRem;
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => {
            Continue(Int(lhs).checked_rem(Int(rhs)).map(Into::into))
        }
        [Value::Float(lhs), Value::Float(rhs)] => Continue(Some((Float(lhs) % Float(rhs)).into())),
        _ => Break(MetamethodRequired),
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
pub fn pow<Ty>(args: [WeakValue<Ty>; 2]) -> ControlFlow<MetamethodRequired, Option<WeakValue<Ty>>>
where
    Ty: Types,
{
    use crate::value::ops::{CheckedPow, Pow};
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => {
            Continue((Int(lhs).checked_pow(Int(rhs))).map(Into::into))
        }
        [Value::Float(lhs), Value::Float(rhs)] => {
            Continue(Some((Float(lhs).pow(Float(rhs))).into()))
        }
        _ => Break(MetamethodRequired),
    }
}

pub fn bit_and<Ty>(args: [WeakValue<Ty>; 2]) -> ControlFlow<MetamethodRequired, WeakValue<Ty>>
where
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Continue((Int(lhs) & Int(rhs)).into()),
        _ => Break(MetamethodRequired),
    }
}

pub fn bit_or<Ty>(args: [WeakValue<Ty>; 2]) -> ControlFlow<MetamethodRequired, WeakValue<Ty>>
where
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Continue((Int(lhs) | Int(rhs)).into()),
        _ => Break(MetamethodRequired),
    }
}

pub fn bit_xor<Ty>(args: [WeakValue<Ty>; 2]) -> ControlFlow<MetamethodRequired, WeakValue<Ty>>
where
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Continue((Int(lhs) ^ Int(rhs)).into()),
        _ => Break(MetamethodRequired),
    }
}

pub fn bit_shl<Ty>(args: [WeakValue<Ty>; 2]) -> ControlFlow<MetamethodRequired, WeakValue<Ty>>
where
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Continue((Int(lhs) << Int(rhs)).into()),
        _ => Break(MetamethodRequired),
    }
}

pub fn bit_shr<Ty>(args: [WeakValue<Ty>; 2]) -> ControlFlow<MetamethodRequired, WeakValue<Ty>>
where
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => Continue((Int(lhs) >> Int(rhs)).into()),
        _ => Break(MetamethodRequired),
    }
}

pub fn eq<Ty>(args: [WeakValue<Ty>; 2], cmp_int_flt: bool) -> ControlFlow<MetamethodRequired, bool>
where
    Ty: Types,
{
    use crate::value::{Float, Int};

    if args[0] == args[1] {
        return Continue(true);
    }

    match args {
        [Value::Int(lhs), Value::Float(rhs)] if cmp_int_flt => {
            let r = PartialEq::eq(&Int(lhs), &Float(rhs));
            Continue(r)
        }
        [Value::Float(lhs), Value::Int(rhs)] if cmp_int_flt => {
            let r = PartialEq::eq(&Float(lhs), &Int(rhs));
            Continue(r)
        }
        [Value::Table(_), Value::Table(_)] | [Value::Userdata(_), Value::Userdata(_)] => {
            Break(MetamethodRequired)
        }
        _ => Continue(false),
    }
}

pub fn ne<Ty>(args: [WeakValue<Ty>; 2], cmp_int_flt: bool) -> ControlFlow<MetamethodRequired, bool>
where
    Ty: Types,
{
    eq(args, cmp_int_flt).map_continue(|value| !value)
}

pub fn partial_cmp<Ty>(
    args: [WeakValue<Ty>; 2],
    heap: &Heap<Ty>,
    cmp_int_flt: bool,
) -> Result<ControlFlow<MetamethodRequired, Option<Ordering>>, AlreadyDroppedError>
where
    Ty: Types,
{
    use crate::error::AlreadyDroppedError;
    use crate::value::{Float, Int};

    match args {
        [Value::Int(lhs), Value::Int(rhs)] => {
            let r = PartialOrd::partial_cmp(&Int(lhs), &Int(rhs));
            Ok(Continue(r))
        }
        [Value::Float(lhs), Value::Float(rhs)] => {
            let r = PartialOrd::partial_cmp(&Float(lhs), &Float(rhs));
            Ok(Continue(r))
        }
        [Value::Int(lhs), Value::Float(rhs)] if cmp_int_flt => {
            let r = PartialOrd::partial_cmp(&Int(lhs), &Float(rhs));
            Ok(Continue(r))
        }
        [Value::Float(lhs), Value::Int(rhs)] if cmp_int_flt => {
            let r = PartialOrd::partial_cmp(&Float(lhs), &Int(rhs));
            Ok(Continue(r))
        }
        [Value::String(lhs), Value::String(rhs)] => {
            let lhs = heap.get(lhs.0).ok_or(AlreadyDroppedError)?;
            let rhs = heap.get(rhs.0).ok_or(AlreadyDroppedError)?;
            let r = PartialOrd::partial_cmp(lhs, rhs);
            Ok(Continue(r))
        }
        _ => Ok(Break(MetamethodRequired)),
    }
}

pub fn lt<Ty>(
    args: [WeakValue<Ty>; 2],
    heap: &Heap<Ty>,
    cmp_int_flt: bool,
) -> Result<ControlFlow<MetamethodRequired, bool>, AlreadyDroppedError>
where
    Ty: Types,
{
    partial_cmp(args, heap, cmp_int_flt)
        .map(|ctrl| ctrl.map_continue(|ord| matches!(ord, Some(Ordering::Less))))
}

pub fn gt<Ty>(
    args: [WeakValue<Ty>; 2],
    heap: &Heap<Ty>,
    cmp_int_flt: bool,
) -> Result<ControlFlow<MetamethodRequired, bool>, AlreadyDroppedError>
where
    Ty: Types,
{
    partial_cmp(args, heap, cmp_int_flt)
        .map(|ctrl| ctrl.map_continue(|ord| matches!(ord, Some(Ordering::Greater))))
}

pub fn lt_eq<Ty>(
    args: [WeakValue<Ty>; 2],
    heap: &Heap<Ty>,
    cmp_int_flt: bool,
) -> Result<ControlFlow<MetamethodRequired, bool>, AlreadyDroppedError>
where
    Ty: Types,
{
    partial_cmp(args, heap, cmp_int_flt)
        .map(|ctrl| ctrl.map_continue(|ord| matches!(ord, Some(Ordering::Less | Ordering::Equal))))
}

pub fn gt_eq<Ty>(
    args: [WeakValue<Ty>; 2],
    heap: &Heap<Ty>,
    cmp_int_flt: bool,
) -> Result<ControlFlow<MetamethodRequired, bool>, AlreadyDroppedError>
where
    Ty: Types,
{
    partial_cmp(args, heap, cmp_int_flt)
        .map(|ctrl| ctrl.map_continue(|ord| matches!(ord, Some(Ordering::Less | Ordering::Equal))))
}

pub(crate) fn inner_concat<Ty, Heap>(
    args: [WeakValue<Ty>; 2],
    heap: &mut Heap,
) -> Result<ControlFlow<MetamethodRequired, WeakValue<Ty>>, AlreadyDroppedError>
where
    Ty: Types,
    Heap: AsHeap<Ty> + Intern<Ty::String>,
{
    use crate::error::AlreadyDroppedError;
    use crate::gc::LuaPtr;
    use crate::value::{Concat, Len};

    match args {
        [Value::String(lhs), Value::String(rhs)] => {
            let lhs_value = heap
                .as_heap()
                .get(lhs.0)
                .ok_or(AlreadyDroppedError)?
                .as_inner();
            let rhs_value = heap
                .as_heap()
                .get(rhs.0)
                .ok_or(AlreadyDroppedError)?
                .as_inner();

            if rhs_value.is_empty() {
                Ok(Continue(Value::String(lhs)))
            } else if lhs_value.is_empty() {
                Ok(Continue(Value::String(rhs)))
            } else {
                let mut value = lhs_value.clone();
                value.concat(rhs_value);
                let ptr = heap.intern(value);
                Ok(Continue(Value::String(LuaPtr(ptr.downgrade()))))
            }
        }
        _ => Ok(Break(MetamethodRequired)),
    }
}

pub fn concat<Ty>(
    args: [WeakValue<Ty>; 2],
    heap: &mut Heap<Ty>,
) -> Result<ControlFlow<MetamethodRequired, WeakValue<Ty>>, AlreadyDroppedError>
where
    Ty: Types,
{
    inner_concat(args, heap)
}

pub(crate) fn inner_binary_op<Ty, Heap>(
    op: BinaryOp,
    args: [WeakValue<Ty>; 2],
    cmp_int_flt: bool,
    heap: &mut Heap,
) -> Result<ControlFlow<MetamethodRequired, Option<WeakValue<Ty>>>, AlreadyDroppedError>
where
    Ty: Types,
    Heap: AsHeap<Ty> + Intern<Ty::String>,
{
    use repr::opcode::{AriBinOp, BitBinOp, EqBinOp, RelBinOp, StrBinOp};
    use AriBinOp::*;
    use BitBinOp::*;
    use EqBinOp::*;
    use RelBinOp::*;
    use StrBinOp::*;

    match op {
        BinaryOp::Ari(Add) => Ok(add(args).map_continue(Some)),
        BinaryOp::Ari(Sub) => Ok(sub(args).map_continue(Some)),
        BinaryOp::Ari(Mul) => Ok(mul(args).map_continue(Some)),
        BinaryOp::Ari(Div) => Ok(div(args)),
        BinaryOp::Ari(FloorDiv) => Ok(floor_div(args)),
        BinaryOp::Ari(Rem) => Ok(rem(args)),
        BinaryOp::Ari(Pow) => Ok(pow(args)),
        BinaryOp::Bit(And) => Ok(bit_and(args).map_continue(Some)),
        BinaryOp::Bit(Or) => Ok(bit_or(args).map_continue(Some)),
        BinaryOp::Bit(Xor) => Ok(bit_xor(args).map_continue(Some)),
        BinaryOp::Bit(ShL) => Ok(bit_shl(args).map_continue(Some)),
        BinaryOp::Bit(ShR) => Ok(bit_shr(args).map_continue(Some)),
        BinaryOp::Str(Concat) => inner_concat(args, heap).map(|ctrl| ctrl.map_continue(Some)),
        BinaryOp::Eq(Eq) => Ok(eq(args, cmp_int_flt).map_continue(|t| Some(Value::Bool(t)))),
        BinaryOp::Eq(Neq) => Ok(ne(args, cmp_int_flt).map_continue(|t| Some(Value::Bool(t)))),
        BinaryOp::Rel(Lt) => lt(args, heap.as_heap(), cmp_int_flt)
            .map(|ctrl| ctrl.map_continue(|t| Some(Value::Bool(t)))),
        BinaryOp::Rel(Gt) => gt(args, heap.as_heap(), cmp_int_flt)
            .map(|ctrl| ctrl.map_continue(|t| Some(Value::Bool(t)))),
        BinaryOp::Rel(LtEq) => lt_eq(args, heap.as_heap(), cmp_int_flt)
            .map(|ctrl| ctrl.map_continue(|t| Some(Value::Bool(t)))),
        BinaryOp::Rel(GtEq) => gt_eq(args, heap.as_heap(), cmp_int_flt)
            .map(|ctrl| ctrl.map_continue(|t| Some(Value::Bool(t)))),
    }
}

pub fn binary_op<Ty>(
    op: BinaryOp,
    args: [WeakValue<Ty>; 2],
    cmp_int_flt: bool,
    heap: &mut Heap<Ty>,
) -> Result<ControlFlow<MetamethodRequired, Option<WeakValue<Ty>>>, AlreadyDroppedError>
where
    Ty: Types,
{
    inner_binary_op(op, args, cmp_int_flt, heap)
}

pub fn neg<Rf, Ty>(args: [Value<Rf, Ty>; 1]) -> ControlFlow<MetamethodRequired, WeakValue<Ty>>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::{Float, Int};

    match args {
        [Value::Int(val)] => {
            let Int(r) = -Int(val);
            Continue(Value::Int(r))
        }
        [Value::Float(val)] => {
            let Float(r) = -Float(val);
            Continue(Value::Float(r))
        }
        _ => Break(MetamethodRequired),
    }
}

pub fn bit_not<Rf, Ty>(args: [Value<Rf, Ty>; 1]) -> ControlFlow<MetamethodRequired, WeakValue<Ty>>
where
    Rf: Refs,
    Ty: Types,
{
    use crate::value::Int;

    match args {
        [Value::Int(val)] => {
            let Int(r) = !Int(val);
            Continue(Value::Int(r))
        }
        _ => Break(MetamethodRequired),
    }
}

pub fn len<Ty>(
    args: [WeakValue<Ty>; 1],
    heap: &Heap<Ty>,
) -> Result<ControlFlow<MetamethodRequired, Int>, AlreadyDroppedError>
where
    Ty: Types,
{
    use crate::gc::{LuaPtr, TryGet};
    use crate::value::{Key, Len, Metatable, TableIndex};

    match args {
        [Value::String(LuaPtr(ptr))] => {
            let res = heap.try_get(ptr)?;
            Ok(Continue(res.len()))
        }
        [Value::Table(LuaPtr(ptr))] => {
            use crate::runtime::thread::frame::BuiltinMetamethod;

            // Check if table has __len metamethod.

            let table = heap.try_get(ptr)?;
            let has_len = if let Some(meta) = table.metatable() {
                let meta = heap.try_get(*meta)?;
                let key: Ty::String = BuiltinMetamethod::Len.to_str().into();
                if let Some(key) = heap.find_interned(&key) {
                    let key = Key::String(LuaPtr(key.downgrade()));
                    meta.contains_key(&key)
                } else {
                    false
                }
            } else {
                false
            };

            if has_len {
                Ok(Break(MetamethodRequired))
            } else {
                Ok(Continue(table.len()))
            }
        }
        _ => Ok(Break(MetamethodRequired)),
    }
}

pub fn log_not<Ty>(args: [WeakValue<Ty>; 1]) -> bool
where
    Ty: Types,
{
    let [val] = args;
    !val.to_bool()
}

pub fn unary_op<Ty>(
    op: UnaryOp,
    args: [WeakValue<Ty>; 1],
    heap: &Heap<Ty>,
) -> Result<ControlFlow<MetamethodRequired, WeakValue<Ty>>, AlreadyDroppedError>
where
    Ty: Types,
{
    use UnaryOp::*;

    match op {
        AriNeg => Ok(neg(args)),
        BitNot => Ok(bit_not(args)),
        StrLen => len(args, heap).map(|ctrl| ctrl.map_continue(Into::into)),
        LogNot => Ok(Continue(Value::Bool(log_not(args)))),
    }
}

pub(crate) trait AsHeap<Ty>
where
    Ty: Types,
{
    fn as_heap(&self) -> &Heap<Ty>;
}

pub(crate) trait Intern<T> {
    fn intern(&mut self, value: T) -> Root<Interned<T>>;
}

impl<Ty> AsHeap<Ty> for Heap<Ty>
where
    Ty: Types,
{
    fn as_heap(&self) -> &Heap<Ty> {
        self
    }
}

impl<T, Ty> Intern<T> for Heap<Ty>
where
    Ty: Types,
    T: Trace + Hash + Eq,
{
    fn intern(&mut self, value: T) -> Root<Interned<T>> {
        Heap::intern(self, value)
    }
}
