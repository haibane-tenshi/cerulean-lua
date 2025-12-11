use std::ops::ControlFlow;

use repr::opcode::{AriBinOp, BinOp, BitBinOp, EqBinOp, RelBinOp, StrBinOp, UnaOp};

use crate::builtins::coerce::CoerceArgs;
use crate::builtins::raw_ops::MetamethodRequired;
use crate::builtins::{find_metavalue, prepare_invoke};
use crate::error::{AlreadyDroppedError, AlreadyDroppedOr, RtError};
use crate::ffi::arg_parser::ParseArgs;
use crate::ffi::delegate::{
    try_repeat, Delegate, Request, RuntimeView, StackSlot, State as CoState,
};
use crate::gc::LuaPtr;
use crate::runtime::thread::frame::Event;
use crate::value::{Key, Types, Value};

fn binary_op<Ty>(op: BinOp) -> impl Delegate<Ty>
where
    Ty: Types,
{
    #[derive(Debug, Clone, Copy)]
    enum State {
        Started,
        MetamethodCall,
    }

    let mut state = State::Started;

    try_repeat(move |mut rt: RuntimeView<'_, _>| match state {
        State::Started => {
            use crate::builtins::raw_ops::binary_op;

            let heap = &mut rt.core.gc;
            let mut stack = rt.stack.transient();
            let args = stack.parse(heap)?;
            // Keep values on the stack.
            // In case metamethod is used, we can just pass current stack state as is.
            // If builtin is resolved it clears the stack before writing.

            let args = rt
                .core
                .dialect
                .binary_op(op, args, |value| heap.intern(value));
            let cmp_flt_int = CoerceArgs::<Ty>::cmp_float_and_int(&rt.core.dialect);
            match binary_op(op, args, cmp_flt_int, heap)? {
                ControlFlow::Continue(Some(value)) => {
                    stack.clear();
                    stack.push(value);
                    return Ok(CoState::Complete(()));
                }
                ControlFlow::Continue(None) => {
                    let msg = heap.intern("op failed to produce value".into());
                    return Err(RtError::from_msg(msg));
                }
                ControlFlow::Break(MetamethodRequired) => (),
            }

            let event: Event = op.into();
            let s = heap
                .intern(event.to_metamethod().to_str().into())
                .downgrade();
            let key = Key::String(LuaPtr(s));
            let metavalue = find_metavalue(args, key, heap, &rt.core.metatable_registry)?;

            match metavalue {
                Value::Nil => {
                    let [lhs, rhs] = args;
                    let fallback = match op {
                        BinOp::Eq(EqBinOp::Eq) => Some(Value::Bool(lhs == rhs)),
                        BinOp::Eq(EqBinOp::Neq) => Some(Value::Bool(lhs != rhs)),
                        _ => None,
                    };

                    match fallback {
                        Some(value) => {
                            stack.clear();
                            stack.push(value);
                            Ok(CoState::Complete(()))
                        }
                        None => {
                            let msg = heap.intern(
                                format!(
                                    "cannot perform `{}` op between values of types {} and {}",
                                    op,
                                    args[0].type_(),
                                    args[1].type_()
                                )
                                .into(),
                            );
                            Err(RtError::from_msg(msg))
                        }
                    }
                }
                metavalue => {
                    let callable =
                        prepare_invoke(metavalue, stack, heap, &rt.core.metatable_registry)
                            .map_err(|err| match err {
                                AlreadyDroppedOr::Dropped(err) => err.into(),
                                AlreadyDroppedOr::Other(_) => {
                                    let msg = heap.intern("metavalue cannot be called".into());
                                    RtError::from_msg(msg)
                                }
                            })?;
                    let request = Request::Invoke {
                        callable,
                        start: StackSlot(0),
                    };

                    state = State::MetamethodCall;
                    Ok(CoState::Yielded(request))
                }
            }
        }
        State::MetamethodCall => {
            rt.stack.adjust_height(StackSlot(1));

            state = State::Started;
            Ok(CoState::Complete(()))
        }
    })
}

pub fn add<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Ari(AriBinOp::Add))
}

pub fn sub<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Ari(AriBinOp::Sub))
}

pub fn mul<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Ari(AriBinOp::Mul))
}

pub fn div<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Ari(AriBinOp::Div))
}

pub fn floor_div<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Ari(AriBinOp::FloorDiv))
}

pub fn rem<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Ari(AriBinOp::Rem))
}

pub fn pow<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Ari(AriBinOp::Pow))
}

pub fn bit_and<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Bit(BitBinOp::And))
}

pub fn bit_or<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Bit(BitBinOp::Or))
}

pub fn bit_xor<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Bit(BitBinOp::Xor))
}

pub fn bit_shl<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Bit(BitBinOp::ShL))
}

pub fn bit_shr<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Bit(BitBinOp::ShR))
}

pub fn concat<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Str(StrBinOp::Concat))
}

pub fn lt<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Rel(RelBinOp::Lt))
}

pub fn lt_eq<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Rel(RelBinOp::LtEq))
}

pub fn gt<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Rel(RelBinOp::Gt))
}

pub fn gt_eq<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Rel(RelBinOp::GtEq))
}

pub fn eq<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Eq(EqBinOp::Eq))
}

pub fn neq<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    binary_op(BinOp::Eq(EqBinOp::Neq))
}

fn unary_op<Ty>(op: UnaOp) -> impl Delegate<Ty>
where
    Ty: Types,
{
    #[derive(Debug, Clone, Copy)]
    enum State {
        Started,
        MetamethodCall,
    }

    let mut state = State::Started;

    try_repeat(move |mut rt: RuntimeView<'_, _>| match state {
        State::Started => {
            use crate::builtins::raw_ops::unary_op;

            let heap = &mut rt.core.gc;
            let mut stack = rt.stack.transient();
            let args = stack.parse(heap)?;
            // Keep values on the stack.
            // In case metamethod is used, we can just pass current stack state as is.
            // If builtin is resolved it clears the stack before writing.

            let args = rt.core.dialect.unary_op(op, args);
            let eval = match (op, args) {
                (UnaOp::StrLen, [Value::Table(_)]) => ControlFlow::Break(MetamethodRequired),
                _ => unary_op(op, args, heap)?,
            };
            match eval {
                ControlFlow::Continue(value) => {
                    stack.clear();
                    stack.push(value);
                    return Ok(CoState::Complete(()));
                }
                ControlFlow::Break(MetamethodRequired) => (),
            }

            let event: Event = match op {
                UnaOp::AriNeg => Event::Neg,
                UnaOp::BitNot => Event::BitNot,
                UnaOp::StrLen => Event::Len,
                UnaOp::LogNot => unreachable!("logical not should always produce a value"),
            };
            let s = heap
                .intern(event.to_metamethod().to_str().into())
                .downgrade();
            let key = Key::String(LuaPtr(s));
            let metavalue = find_metavalue(args, key, heap, &rt.core.metatable_registry)?;

            match metavalue {
                Value::Nil => {
                    let [value] = args;
                    let fallback = match (op, value) {
                        (UnaOp::StrLen, Value::Table(LuaPtr(ptr))) => {
                            use crate::value::TableIndex;

                            let table: &Ty::Table = heap.get(ptr).ok_or(AlreadyDroppedError)?;
                            let border = table.border();
                            Some(Value::Int(border))
                        }
                        _ => None,
                    };

                    match fallback {
                        Some(value) => {
                            stack.clear();
                            stack.push(value);
                            Ok(CoState::Complete(()))
                        }
                        None => {
                            let msg = heap.intern(
                                format!(
                                    "cannot perform `{}` op on value of types {}",
                                    op,
                                    value.type_(),
                                )
                                .into(),
                            );
                            Err(RtError::from_msg(msg))
                        }
                    }
                }
                metavalue => {
                    let callable =
                        prepare_invoke(metavalue, stack, heap, &rt.core.metatable_registry)
                            .map_err(|err| match err {
                                AlreadyDroppedOr::Dropped(err) => err.into(),
                                AlreadyDroppedOr::Other(_) => {
                                    let msg = heap.intern("metavalue cannot be called".into());
                                    RtError::from_msg(msg)
                                }
                            })?;
                    let request = Request::Invoke {
                        callable,
                        start: StackSlot(0),
                    };

                    state = State::MetamethodCall;
                    Ok(CoState::Yielded(request))
                }
            }
        }
        State::MetamethodCall => {
            rt.stack.adjust_height(StackSlot(1));

            state = State::Started;
            Ok(CoState::Complete(()))
        }
    })
}

pub fn neg<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    unary_op(UnaOp::AriNeg)
}

pub fn bit_not<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    unary_op(UnaOp::BitNot)
}

pub fn len<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    unary_op(UnaOp::StrLen)
}
