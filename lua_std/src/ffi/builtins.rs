use rt::error::RuntimeError;
use rt::ffi::delegate::{Request, Response, RuntimeView, StackSlot, State};
use rt::value::{Key, Types, Weak, WeakValue};

pub(crate) fn len<Ty>(value: WeakValue<Ty>) -> Len<Ty>
where
    Ty: Types,
{
    Len::Started(value)
}

pub(crate) enum Len<Ty>
where
    Ty: Types,
{
    Started(WeakValue<Ty>),
    CalledMeta(StackSlot),
    Finished,
}

impl<Ty> Len<Ty>
where
    Ty: Types,
{
    pub(crate) fn resume(
        &mut self,
        mut rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<WeakValue<Ty>, RuntimeError<Ty>>> {
        use rt::builtins::raw_ops::{self, MetamethodRequired};
        use rt::builtins::{find_metavalue, prepare_invoke};
        use rt::ffi::delegate::rearrange;
        use rt::gc::{Heap, LuaPtr, TryGet};
        use rt::value::{Key, Value};
        use std::ops::ControlFlow;

        let body = move || {
            let err_msg = |heap: &mut Heap<_>, ty| {
                RuntimeError::from_msg(
                    heap.intern(format!("cannot perform len op on value of type {ty}").into()),
                )
            };

            let current = std::mem::replace(self, Len::Finished);
            match (current, response) {
                (Len::Started(value), Response::Resume) => {
                    match raw_ops::len([value], &rt.core.gc)? {
                        ControlFlow::Break(MetamethodRequired) => {
                            let key = {
                                let name = rt
                                    .core
                                    .gc
                                    .find_interned(&Ty::String::from("__len"))
                                    .ok_or_else(|| err_msg(&mut rt.core.gc, value.type_()))?;
                                Key::String(LuaPtr(name.downgrade()))
                            };
                            let metamethod = find_metavalue(
                                [value],
                                key,
                                &rt.core.gc,
                                &rt.core.metatable_registry,
                            )?;

                            if metamethod == Value::Nil {
                                // Fallback.
                                if let Value::Table(LuaPtr(ptr)) = value {
                                    use rt::value::ops::Len;

                                    let table: &Ty::Table = rt.core.gc.try_get(ptr)?;
                                    return Ok(State::Complete(table.len().into()));
                                } else {
                                    return Err(err_msg(&mut rt.core.gc, value.type_()));
                                }
                            }

                            let start = rt.stack.top();
                            let mut stack = rt.stack.guard_at(start).unwrap();
                            let mut stack = stack.transient();
                            stack.push(value);

                            let callable = prepare_invoke(
                                metamethod,
                                stack,
                                &rt.core.gc,
                                &rt.core.metatable_registry,
                            )?;

                            let request = Request::Invoke { callable, start };

                            *self = Len::CalledMeta(start);
                            Ok(State::Yielded(request))
                        }
                        ControlFlow::Continue(res) => Ok(State::Complete(res.into())),
                    }
                }
                (Len::CalledMeta(start), Response::Evaluated(res)) => {
                    res?;

                    rt.stack.adjust_height(StackSlot(start.0 + 1));
                    let value = rt.stack.pop().unwrap();

                    Ok(State::Complete(value))
                }
                (Len::Finished, _) => unreachable!("resumed completed coroutine"),
                _ => unreachable!("invalid runtime response"),
            }
        };

        rearrange(body())
    }
}

pub(crate) fn get_index<Ty>(table: WeakValue<Ty>, key: Key<Weak, Ty>) -> GetIndex<Ty>
where
    Ty: Types,
{
    GetIndex::Started { target: table, key }
}

pub(crate) enum GetIndex<Ty>
where
    Ty: Types,
{
    Started {
        target: WeakValue<Ty>,
        key: Key<Weak, Ty>,
    },
    CalledMethod {
        start: StackSlot,
    },
    Finished,
}

impl<Ty> GetIndex<Ty>
where
    Ty: Types,
{
    pub(crate) fn resume(
        &mut self,
        mut rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<WeakValue<Ty>, RuntimeError<Ty>>>
    where
        Ty: Types,
    {
        use rt::builtins::coerce::CoerceArgs;
        use rt::builtins::table::{get_index, CallRequired};
        use rt::error::AlreadyDroppedOr;
        use rt::ffi::delegate::{self, rearrange};
        use std::ops::ControlFlow;

        let body = || {
            let current = std::mem::replace(self, GetIndex::Finished);
            match (current, response) {
                (GetIndex::Started { target, key }, Response::Resume) => {
                    let index = rt.core.dialect.tab_get(key.into());
                    let index = index.try_into().unwrap();

                    let call =
                        match get_index(target, index, &rt.core.gc, &rt.core.metatable_registry) {
                            Ok(ControlFlow::Continue(call)) => call,
                            Ok(ControlFlow::Break(value)) => {
                                return Ok(delegate::State::Complete(value))
                            }
                            Err(AlreadyDroppedOr::Dropped(err)) => return Err(err.into()),
                            Err(AlreadyDroppedOr::Other(err)) => {
                                let msg = rt.core.gc.intern(err.to_string().into());
                                let err = RuntimeError::from_msg(msg);
                                return Err(err);
                            }
                        };

                    let CallRequired { func, target } = call;

                    let start = rt.stack.top();
                    let mut stack = rt.stack.guard_at(start).unwrap();
                    let mut stack = stack.transient();

                    stack.push(target);
                    stack.push(key.into());

                    let request = Request::Invoke {
                        callable: func,
                        start,
                    };

                    *self = GetIndex::CalledMethod { start };
                    Ok(delegate::State::Yielded(request))
                }
                (GetIndex::CalledMethod { start }, Response::Evaluated(res)) => {
                    res?;

                    rt.stack.adjust_height(StackSlot(start.0 + 1));
                    let value = rt.stack.pop().unwrap();

                    Ok(delegate::State::Complete(value))
                }
                (GetIndex::Finished, _) => unreachable!("resumed completed coroutine"),
                _ => unreachable!("invalid runtime response"),
            }
        };

        rearrange(body())
    }
}

pub(crate) fn set_index<Ty>(
    table: WeakValue<Ty>,
    key: Key<Weak, Ty>,
    value: WeakValue<Ty>,
) -> SetIndex<Ty>
where
    Ty: Types,
{
    SetIndex::Started {
        target: table,
        key,
        value,
    }
}

pub(crate) enum SetIndex<Ty>
where
    Ty: Types,
{
    Started {
        target: WeakValue<Ty>,
        key: Key<Weak, Ty>,
        value: WeakValue<Ty>,
    },
    CalledMethod {
        start: StackSlot,
    },
    Finished,
}

impl<Ty> SetIndex<Ty>
where
    Ty: Types,
{
    pub(crate) fn resume(
        &mut self,
        mut rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<(), RuntimeError<Ty>>>
    where
        Ty: Types,
    {
        use rt::builtins::coerce::CoerceArgs;
        use rt::builtins::table::{set_index, CallRequired};
        use rt::error::AlreadyDroppedOr;
        use rt::ffi::delegate::{self, rearrange};
        use std::ops::ControlFlow;

        let body = || {
            let current = std::mem::replace(self, SetIndex::Finished);
            match (current, response) {
                (SetIndex::Started { target, key, value }, Response::Resume) => {
                    let index = rt.core.dialect.tab_get(key.into());
                    let index = index.try_into().unwrap();

                    let call = match set_index(
                        target,
                        index,
                        value,
                        &mut rt.core.gc,
                        &rt.core.metatable_registry,
                    ) {
                        Ok(ControlFlow::Continue(call)) => call,
                        Ok(ControlFlow::Break(value)) => {
                            return Ok(delegate::State::Complete(value))
                        }
                        Err(AlreadyDroppedOr::Dropped(err)) => return Err(err.into()),
                        Err(AlreadyDroppedOr::Other(err)) => {
                            let msg = rt.core.gc.intern(err.to_string().into());
                            let err = RuntimeError::from_msg(msg);
                            return Err(err);
                        }
                    };

                    let CallRequired { func, target } = call;

                    let start = rt.stack.top();
                    let mut stack = rt.stack.guard_at(start).unwrap();
                    let mut stack = stack.transient();

                    stack.push(target);
                    stack.push(key.into());
                    stack.push(value);

                    let request = Request::Invoke {
                        callable: func,
                        start,
                    };

                    *self = SetIndex::CalledMethod { start };
                    Ok(delegate::State::Yielded(request))
                }
                (SetIndex::CalledMethod { start }, Response::Evaluated(res)) => {
                    res?;

                    rt.stack.adjust_height(start);

                    Ok(delegate::State::Complete(()))
                }
                (SetIndex::Finished, _) => unreachable!("resumed completed coroutine"),
                _ => unreachable!("invalid runtime response"),
            }
        };

        rearrange(body())
    }
}
