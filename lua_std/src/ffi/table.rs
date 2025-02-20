//! Table manipulation library.
//!
//! # From Lua documentation
//!
//! This library provides generic functions for table manipulation.
//! It provides all its functions inside the table `table`.
//!
//! Remember that, whenever an operation needs the length of a table, all caveats about the length operator apply (see §3.4.7).
//! All functions ignore non-numeric keys in the tables given as arguments.

use rt::ffi::{self, LuaFfi};
use rt::value::Types;

/// Stringify and concatenate table elements.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(list: table, [sep: string, [i: int, [j: int]]]) -> string`
///
/// Given a list where all elements are strings or numbers, returns the string `list[i]..sep..list[i+1] ··· sep..list[j]`.
/// The default value for `sep` is the empty string, the default for `i` is 1, and the default for `j` is `#list`.
/// If `i` is greater than `j`, returns the empty string.
///
/// Remember that, whenever an operation needs the length of a table, all caveats about the length operator apply (see §3.4.7).
/// All functions ignore non-numeric keys in the tables given as arguments.
///
/// # Implementation-specific behavior
///
/// *  Operations performed by this function are *regular* that is it may invoke metamethods.
///    This includes getting length and values out of the table.
///    
///    This replicates behavior of vanilla implementation.
///
/// *  Numeric entries will always get coerced to strings, irrespective of runtime configuration.
///    This is due to that fact that method documentation explicitly states that it can process integer and float elements.
///
///    Usual caveats about int/float-to-string conversion apply:
///    exact representation details are implementation-specific and are subject to change, however result is promised to be human-readable.
///    In particular it is not guaranteed that numbers can be round-tripped.
///    Render numbers manually to have better control over output.
///
///    Note that raw concatenation between strings always takes priority over metamethod even if one is configured.
pub fn concat<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
    Ty::String: Unpin,
{
    use super::builtins::{get_index, len, GetIndex, Len};
    use gc::{Gc, Interned};
    use rt::error::RuntimeError;
    use rt::ffi::delegate::{self, rearrange, Delegate, Request, Response, RuntimeView};
    use rt::value::WeakValue;
    use std::pin::Pin;

    enum State<Ty>
    where
        Ty: Types,
    {
        Started,
        CalledLen {
            co: Len<Ty>,
            i: i64,
        },
        CalledFirstGetIndex {
            co: GetIndex<Ty>,
            i: i64,
            j: i64,
        },
        CalledGetIndex {
            co: GetIndex<Ty>,
            current: i64,
            j: i64,
            res: Ty::String,
        },
        Finished,
    }

    impl<Ty> Delegate<Ty> for State<Ty>
    where
        Ty: Types,
        Ty::String: Unpin,
    {
        fn resume(
            self: Pin<&mut Self>,
            mut rt: RuntimeView<'_, Ty>,
            response: Response<Ty>,
        ) -> delegate::State<Request<Ty>, Result<(), RuntimeError<Ty>>> {
            use rt::ffi::arg_parser::{LuaString, NilOr, ParseArgs};

            let this = self.get_mut();
            let current = std::mem::replace(this, State::Finished);
            match (current, response) {
                (State::Started, Response::Resume) => rearrange(this.started(rt)),
                (State::CalledLen { mut co, i }, response) => {
                    let len = match co.resume(rt.reborrow(), response) {
                        delegate::State::Complete(Ok(len)) => len,
                        delegate::State::Complete(Err(err)) => {
                            return delegate::State::Complete(Err(err))
                        }
                        delegate::State::Yielded(request) => {
                            *this = State::CalledLen { co, i };
                            return delegate::State::Yielded(request);
                        }
                    };

                    let (list, sep): (WeakValue<Ty>, NilOr<LuaString<_>>) = rt
                        .stack
                        .parse(&mut rt.core.gc)
                        .expect("internal stack is invalid");
                    let sep = sep.into_option().map(|t| t.0 .0);

                    rearrange(this.after_len(rt, list, sep, i, len))
                }
                (State::CalledFirstGetIndex { mut co, i, j }, response) => {
                    let first = match co.resume(rt.reborrow(), response) {
                        delegate::State::Complete(Ok(t)) => t,
                        delegate::State::Complete(Err(err)) => {
                            return delegate::State::Complete(Err(err))
                        }
                        delegate::State::Yielded(request) => {
                            *this = State::CalledFirstGetIndex { co, i, j };
                            return delegate::State::Yielded(request);
                        }
                    };

                    let (list, sep): (WeakValue<Ty>, NilOr<LuaString<_>>) = rt
                        .stack
                        .parse(&mut rt.core.gc)
                        .expect("internal stack is invalid");
                    let sep = sep.into_option().map(|t| t.0 .0);

                    rearrange(this.after_first(rt, list, sep, i, j, first))
                }
                (
                    State::CalledGetIndex {
                        mut co,
                        current,
                        j,
                        res,
                    },
                    response,
                ) => {
                    let value = match co.resume(rt.reborrow(), response) {
                        delegate::State::Complete(Ok(t)) => t,
                        delegate::State::Complete(Err(err)) => {
                            return delegate::State::Complete(Err(err))
                        }
                        delegate::State::Yielded(request) => {
                            *this = State::CalledGetIndex {
                                co,
                                current,
                                j,
                                res,
                            };
                            return delegate::State::Yielded(request);
                        }
                    };

                    let (list, sep): (WeakValue<Ty>, NilOr<LuaString<_>>) = rt
                        .stack
                        .parse(&mut rt.core.gc)
                        .expect("internal stack is invalid");
                    let sep = sep.into_option().map(|t| t.0 .0);

                    rearrange(this.iter_after_get_index(rt, list, sep, current, j, res, value))
                }
                (State::Finished, _) => unreachable!("resumed completed coroutine"),
                _ => unreachable!("invalid runtime response"),
            }
        }
    }

    impl<Ty> State<Ty>
    where
        Ty: Types,
    {
        fn started(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
        ) -> Result<delegate::State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::ffi::arg_parser::{Int, LuaString, LuaTable, NilOr, Opts, ParseArgs, Split};
            use rt::value::Value;

            let (list, args): (LuaTable<_>, Opts<(LuaString<_>, Int, Int)>) =
                rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let (sep, i, j) = args.split();

            rt.stack.transient_in(&mut rt.core.gc, |mut stack, _| {
                stack.push(list.into());
                stack.push(sep.map(NilOr::Some).unwrap_or_default().into());
            });

            let sep = sep.map(|t| t.0 .0);

            let i = i.map(|t| t.0).unwrap_or(1);
            let len = match j.map(|t| t.0) {
                Some(j) => Value::Int(j),
                None => {
                    let mut co = len(list.into());

                    match co.resume(rt.reborrow(), Response::Resume) {
                        delegate::State::Complete(res) => res?,
                        delegate::State::Yielded(request) => {
                            *self = State::CalledLen { co, i };
                            return Ok(delegate::State::Yielded(request));
                        }
                    }
                }
            };

            self.after_len(rt, list.into(), sep, i, len)
        }

        fn after_len(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
            list: WeakValue<Ty>,
            sep: Option<Gc<Interned<Ty::String>>>,
            i: i64,
            len: WeakValue<Ty>,
        ) -> Result<delegate::State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::gc::LuaPtr;
            use rt::value::{KeyValue as Key, Value};

            let j = match len {
                Value::Int(j) => j,
                value => {
                    let err = rt.core.alloc_error_msg(format!(
                        "table length must be an integer (metamethod produced {})",
                        value.type_()
                    ));
                    return Err(err);
                }
            };

            if i > j {
                rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                    stack.clear();

                    let r = Value::String(LuaPtr(heap.intern("".into()).downgrade()));
                    stack.push(r);
                });

                return Ok(delegate::State::Complete(()));
            }

            let mut co = get_index(list, Key::Int(i));

            let first = match co.resume(rt.reborrow(), Response::Resume) {
                delegate::State::Complete(res) => res?,
                delegate::State::Yielded(request) => {
                    *self = State::CalledFirstGetIndex { co, i, j };
                    return Ok(delegate::State::Yielded(request));
                }
            };

            self.after_first(rt, list, sep, i, j, first)
        }

        fn after_first(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
            list: WeakValue<Ty>,
            sep: Option<Gc<Interned<Ty::String>>>,
            i: i64,
            j: i64,
            first: WeakValue<Ty>,
        ) -> Result<delegate::State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::builtins::coerce;
            use rt::builtins::table::GetIndexCache;
            use rt::ffi::arg_parser::{Float, Int};
            use rt::gc::{LuaPtr, TryGet};
            use rt::value::traits::Concat;
            use rt::value::{KeyValue as Key, Value};
            use std::borrow::Cow;
            use std::ops::ControlFlow;

            let mut res = match first {
                Value::String(LuaPtr(ptr)) => {
                    let s = rt.core.gc.try_get(ptr)?;
                    s.as_inner().clone()
                }
                Value::Int(n) => coerce::int_to_str(Int(n)).into(),
                Value::Float(n) => coerce::flt_to_str(Float(n)).into(),
                value => {
                    let err = rt.core.alloc_error_msg(format!("value under index {i} has type {}; only strings, integers or floats permitted", value.type_()));
                    return Err(err);
                }
            };
            let sep = sep
                .map(|ptr| rt.core.gc.try_get(ptr))
                .transpose()?
                .map(AsRef::as_ref);

            let getter = GetIndexCache::new(list, &rt.core.gc, &rt.core.metatable_registry)?;

            for current in (i..=j).skip(1) {
                if let Some(sep) = sep {
                    res.concat(sep);
                }

                let value = match getter.get(Key::Int(current), &rt.core.gc) {
                    Ok(ControlFlow::Break(value)) => value,
                    Ok(ControlFlow::Continue(_)) => {
                        let mut co = get_index(list, Key::Int(current));

                        match co.resume(rt.reborrow(), Response::Resume) {
                            delegate::State::Complete(_) => {
                                unreachable!("get_index should not immediately resolve")
                            }
                            delegate::State::Yielded(request) => {
                                *self = State::CalledGetIndex {
                                    co,
                                    current,
                                    j,
                                    res,
                                };
                                return Ok(delegate::State::Yielded(request));
                            }
                        }
                    }
                    Err(err) => {
                        let err = rt.core.alloc_error_msg(err.to_string());
                        return Err(err);
                    }
                };

                let value = match value {
                    Value::String(LuaPtr(ptr)) => {
                        let s = rt.core.gc.try_get(ptr)?;
                        Cow::Borrowed(s.as_inner())
                    }
                    Value::Int(n) => Cow::Owned(coerce::int_to_str(Int(n)).into()),
                    Value::Float(n) => Cow::Owned(coerce::flt_to_str(Float(n)).into()),
                    value => {
                        let err = rt.core.alloc_error_msg(format!("value under index {current} has type {}; only strings, integers or floats permitted", value.type_()));
                        return Err(err);
                    }
                };

                res.concat(value.as_ref());
            }

            rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                stack.clear();

                let r = Value::String(LuaPtr(heap.intern(res).downgrade()));
                stack.push(r);
            });

            Ok(delegate::State::Complete(()))
        }

        #[expect(clippy::too_many_arguments)]
        fn iter_after_get_index(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
            list: WeakValue<Ty>,
            sep: Option<Gc<Interned<Ty::String>>>,
            current: i64,
            j: i64,
            mut res: Ty::String,
            value: WeakValue<Ty>,
        ) -> Result<delegate::State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::builtins::coerce;
            use rt::ffi::arg_parser::{Float, Int};
            use rt::gc::{LuaPtr, TryGet};
            use rt::value::traits::Concat;
            use rt::value::{KeyValue as Key, Value};
            use std::borrow::Cow;

            let value = match value {
                Value::String(LuaPtr(ptr)) => {
                    let s = rt.core.gc.try_get(ptr)?;
                    Cow::Borrowed(s.as_inner())
                }
                Value::Int(n) => Cow::Owned(coerce::int_to_str(Int(n)).into()),
                Value::Float(n) => Cow::Owned(coerce::flt_to_str(Float(n)).into()),
                value => {
                    let err = rt.core.alloc_error_msg(format!("value under index {current} has type {}; only strings, integers or floats permitted", value.type_()));
                    return Err(err);
                }
            };

            res.concat(value.as_ref());

            for current in (current..=j).skip(1) {
                if let Some(sep) = sep {
                    let sep = rt.core.gc.try_get(sep)?;
                    res.concat(sep);
                }

                let mut co = get_index(list, Key::Int(current));

                let value = match co.resume(rt.reborrow(), Response::Resume) {
                    delegate::State::Complete(res) => res?,
                    delegate::State::Yielded(request) => {
                        *self = State::CalledGetIndex {
                            co,
                            current,
                            j,
                            res,
                        };
                        return Ok(delegate::State::Yielded(request));
                    }
                };

                let value = match value {
                    Value::String(LuaPtr(ptr)) => {
                        let s = rt.core.gc.try_get(ptr)?;
                        Cow::Borrowed(s.as_inner())
                    }
                    Value::Int(n) => Cow::Owned(coerce::int_to_str(Int(n)).into()),
                    Value::Float(n) => Cow::Owned(coerce::flt_to_str(Float(n)).into()),
                    value => {
                        let err = rt.core.alloc_error_msg(format!("value under index {current} has type {}; only strings, integers or floats permitted", value.type_()));
                        return Err(err);
                    }
                };

                res.concat(value.as_ref());
            }

            rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                stack.clear();

                let r = Value::String(LuaPtr(heap.intern(res).downgrade()));
                stack.push(r);
            });

            Ok(delegate::State::Complete(()))
        }
    }

    ffi::from_fn(|| State::Started, "lua_std::table::concat", ())
}

/// Insert value into table under integer key.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(list: table, [pos: int,] value: any) -> ()`
///
/// Inserts element `value` at position `pos` in list, shifting up the elements `list[pos]`, `list[pos+1]`, ···, `list[#list]`.
/// The default value for `pos` is `#list+1`, so that a call `table.insert(t,x)` inserts `x` at the end of the list `t`.
///
/// Remember that, whenever an operation needs the length of a table, all caveats about the length operator apply (see §3.4.7).
/// All functions ignore non-numeric keys in the tables given as arguments.
///
/// # Implementation-specific behavior
///
/// *  Operations performed by this function are *regular* that is it may invoke metamethods.
///    This includes calculating length as well as getting and setting values on the table.
///    
///    This replicates behavior of vanilla implementation.
///
/// *  In essence this function will perform assignments of the form `list[n+1] = list[n]` in order to shift values.
///
///    Order of operations is implementation-specific (including between getters and setters).
pub fn insert<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use super::builtins::{get_index, len, set_index, GetIndex, Len, SetIndex};
    use rt::error::RuntimeError;
    use rt::ffi::delegate::{self, Delegate, Request, Response, RuntimeView};
    use rt::value::WeakValue;
    use std::pin::Pin;

    enum State<Ty>
    where
        Ty: Types,
    {
        Started,
        CalledLen {
            co: Len<Ty>,
            pos: Option<i64>,
        },
        CalledGetIndex {
            co: GetIndex<Ty>,
            pos: i64,
            current: i64,
        },
        CalledSetIndex {
            co: SetIndex<Ty>,
            pos: i64,
            current: i64,
        },
        CalledSetPos {
            co: SetIndex<Ty>,
        },
        Finished,
    }

    impl<Ty> Delegate<Ty> for State<Ty>
    where
        Ty: Types,
    {
        fn resume(
            self: Pin<&mut Self>,
            mut rt: RuntimeView<'_, Ty>,
            response: Response<Ty>,
        ) -> delegate::State<Request<Ty>, Result<(), RuntimeError<Ty>>> {
            use rt::ffi::delegate::rearrange;

            let this = self.get_mut();
            let current = std::mem::replace(this, State::Finished);
            match (current, response) {
                (State::Started, Response::Resume) => rearrange(this.started(rt)),
                (State::CalledLen { mut co, pos }, response) => {
                    let len = match co.resume(rt.reborrow(), response) {
                        delegate::State::Complete(Err(err)) => {
                            return delegate::State::Complete(Err(err))
                        }
                        delegate::State::Yielded(request) => {
                            *this = State::CalledLen { co, pos };
                            return delegate::State::Yielded(request);
                        }
                        delegate::State::Complete(Ok(len)) => len,
                    };

                    use rt::ffi::arg_parser::ParseArgs;

                    let [list, value] = rt
                        .stack
                        .parse(&mut rt.core.gc)
                        .expect("internal stack state is invalid");
                    rearrange(this.after_len(rt, list, pos, len, value))
                }
                (
                    State::CalledGetIndex {
                        mut co,
                        pos,
                        current,
                    },
                    response,
                ) => {
                    let value = match co.resume(rt.reborrow(), response) {
                        delegate::State::Complete(Err(err)) => {
                            return delegate::State::Complete(Err(err))
                        }
                        delegate::State::Yielded(request) => {
                            *this = State::CalledGetIndex { co, pos, current };
                            return delegate::State::Yielded(request);
                        }
                        delegate::State::Complete(Ok(value)) => value,
                    };

                    use rt::value::KeyValue as Key;

                    let mut co = set_index(rt.stack[0], Key::Int(current + 1), value);

                    match co.resume(rt.reborrow(), Response::Resume) {
                        delegate::State::Complete(Ok(())) => (),
                        delegate::State::Complete(Err(err)) => {
                            return delegate::State::Complete(Err(err))
                        }
                        delegate::State::Yielded(request) => {
                            *this = State::CalledSetIndex { co, pos, current };
                            return delegate::State::Yielded(request);
                        }
                    }

                    rearrange(this.iter_indices(rt, pos, current))
                }
                (
                    State::CalledSetIndex {
                        mut co,
                        pos,
                        current,
                    },
                    response,
                ) => {
                    match co.resume(rt.reborrow(), response) {
                        delegate::State::Complete(Err(err)) => {
                            return delegate::State::Complete(Err(err))
                        }
                        delegate::State::Complete(Ok(())) => (),
                        delegate::State::Yielded(request) => {
                            *this = State::CalledSetIndex { co, pos, current };
                            return delegate::State::Yielded(request);
                        }
                    }

                    rearrange(this.iter_indices(rt, pos, current))
                }
                (State::CalledSetPos { mut co }, response) => {
                    match co.resume(rt.reborrow(), response) {
                        delegate::State::Complete(Ok(())) => (),
                        delegate::State::Complete(Err(err)) => {
                            return delegate::State::Complete(Err(err))
                        }
                        delegate::State::Yielded(request) => {
                            *this = State::CalledSetPos { co };
                            return delegate::State::Yielded(request);
                        }
                    };

                    rt.stack.clear();
                    delegate::State::Complete(Ok(()))
                }
                (State::Finished, _) => unreachable!("resumed completed coroutine"),
                _ => unreachable!("invalid runtime response"),
            }
        }
    }

    impl<Ty> State<Ty>
    where
        Ty: Types,
    {
        fn started(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
        ) -> Result<delegate::State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::error::SignatureError;
            use rt::ffi::arg_parser::{LuaTable, TypeMismatchError};
            use rt::ffi::delegate::{self};
            use rt::value::{Type, Value};

            let (list, pos, value) = match rt.stack.as_slice() {
                [Value::Table(list), value] => (LuaTable(*list), None, *value),
                [Value::Table(list), Value::Int(pos), value] => {
                    (LuaTable(*list), Some(*pos), *value)
                }
                [] | [_] => {
                    let err = SignatureError::TooFewArgs {
                        found: rt.stack.len(),
                    };
                    return Err(err.into());
                }
                [Value::Table(_), arg1, _] => {
                    let msg = TypeMismatchError {
                        expected: Type::Int,
                        found: arg1.type_(),
                    };
                    let err = SignatureError::ConversionFailure {
                        index: 1,
                        msg: msg.to_string(),
                    };
                    return Err(err.into());
                }
                [arg0, _] | [arg0, _, _] => {
                    let msg = TypeMismatchError {
                        expected: Type::Table,
                        found: arg0.type_(),
                    };
                    let err = SignatureError::ConversionFailure {
                        index: 0,
                        msg: msg.to_string(),
                    };
                    return Err(err.into());
                }
                _ => {
                    let err = SignatureError::TooManyArgs {
                        found: rt.stack.len(),
                        expected: 3,
                    };
                    return Err(err.into());
                }
            };
            rt.stack.clear();
            rt.stack.transient_in(&mut rt.core.gc, |mut stack, _| {
                stack.push(list.into());
                stack.push(value);
            });

            let mut co = len(list.into());

            let len = match co.resume(rt.reborrow(), Response::Resume) {
                delegate::State::Complete(res) => res?,
                delegate::State::Yielded(request) => {
                    *self = State::CalledLen { co, pos };
                    return Ok(delegate::State::Yielded(request));
                }
            };

            self.after_len(rt, list.into(), pos, len, value)
        }

        fn after_len(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
            list: WeakValue<Ty>,
            pos: Option<i64>,
            len: WeakValue<Ty>,
            pos_value: WeakValue<Ty>,
        ) -> Result<delegate::State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::builtins::table::{GetIndexCache, SetIndexCache};
            use rt::error::RuntimeError;
            use rt::ffi::delegate::{self};
            use rt::value::{KeyValue as Key, Value};
            use std::ops::ControlFlow;

            let Value::Int(len) = len else {
                let err = rt.core.alloc_error_msg(format!(
                    "table length must be an integer (metamethod produced {})",
                    len.type_()
                ));
                return Err(err);
            };

            let upper_bound = len.checked_add(1).ok_or_else(|| {
                rt.core
                    .alloc_error_msg("attempt to insert at position past i64::MAX")
            })?;
            let pos = pos.unwrap_or(upper_bound);

            let getter = GetIndexCache::new(list, &rt.core.gc, &rt.core.metatable_registry)?;
            let setter = SetIndexCache::new(list, &rt.core.gc, &rt.core.metatable_registry)?;

            for current in (pos..upper_bound).rev() {
                let value = match getter.get(Key::Int(current), &rt.core.gc) {
                    Ok(ControlFlow::Break(value)) => value,
                    Ok(ControlFlow::Continue(_)) => {
                        let mut co = get_index(list, Key::Int(current));

                        match co.resume(rt.reborrow(), Response::Resume) {
                            delegate::State::Complete(res) => res?,
                            delegate::State::Yielded(request) => {
                                *self = State::CalledGetIndex { co, pos, current };
                                return Ok(delegate::State::Yielded(request));
                            }
                        }
                    }
                    Err(err) => {
                        let msg = rt.core.gc.intern(err.to_string().into());
                        let err = RuntimeError::from_msg(msg);
                        return Err(err);
                    }
                };

                let next = current + 1;
                match setter.set(Key::Int(next), value, &mut rt.core.gc) {
                    Ok(ControlFlow::Break(())) => (),
                    Ok(ControlFlow::Continue(_)) => {
                        let mut co = set_index(list, Key::Int(next), value);

                        match co.resume(rt.reborrow(), Response::Resume) {
                            delegate::State::Complete(res) => res?,
                            delegate::State::Yielded(request) => {
                                *self = State::CalledSetIndex { co, pos, current };
                                return Ok(delegate::State::Yielded(request));
                            }
                        }
                    }
                    Err(err) => {
                        let msg = rt.core.gc.intern(err.to_string().into());
                        let err = RuntimeError::from_msg(msg);
                        return Err(err);
                    }
                }
            }

            match setter.set(Key::Int(pos), pos_value, &mut rt.core.gc) {
                Ok(ControlFlow::Break(())) => (),
                Ok(ControlFlow::Continue(_)) => {
                    let mut co = set_index(list, Key::Int(pos), pos_value);

                    match co.resume(rt.reborrow(), Response::Resume) {
                        delegate::State::Complete(res) => res?,
                        delegate::State::Yielded(request) => {
                            *self = State::CalledSetPos { co };
                            return Ok(delegate::State::Yielded(request));
                        }
                    }
                }
                Err(err) => {
                    let msg = rt.core.gc.intern(err.to_string().into());
                    let err = RuntimeError::from_msg(msg);
                    return Err(err);
                }
            }

            rt.stack.clear();
            Ok(delegate::State::Complete(()))
        }

        fn iter_indices(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
            pos: i64,
            current: i64,
        ) -> Result<delegate::State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::ffi::arg_parser::ParseArgs;
            use rt::ffi::delegate;
            use rt::value::KeyValue as Key;

            let list = rt.stack[0];

            for current in (pos..current).rev() {
                let mut co = get_index(list, Key::Int(current));

                let value = match co.resume(rt.reborrow(), Response::Resume) {
                    delegate::State::Complete(res) => res?,
                    delegate::State::Yielded(request) => {
                        *self = State::CalledGetIndex { co, pos, current };
                        return Ok(delegate::State::Yielded(request));
                    }
                };

                let next = current + 1;
                let mut co = set_index(list, Key::Int(next), value);

                match co.resume(rt.reborrow(), Response::Resume) {
                    delegate::State::Complete(res) => res?,
                    delegate::State::Yielded(request) => {
                        *self = State::CalledSetIndex { co, pos, current };
                        return Ok(delegate::State::Yielded(request));
                    }
                }
            }

            let [list, value] = rt
                .stack
                .parse(&mut rt.core.gc)
                .expect("internal stack state is invalid");

            let mut co = set_index(list, Key::Int(pos), value);

            match co.resume(rt.reborrow(), Response::Resume) {
                delegate::State::Complete(res) => res?,
                delegate::State::Yielded(request) => {
                    *self = State::CalledSetPos { co };
                    return Ok(delegate::State::Yielded(request));
                }
            }

            rt.stack.clear();
            Ok(delegate::State::Complete(()))
        }
    }

    ffi::from_fn(|| State::Started, "lua_std::table::insert", ())
}

/// Copy range of values from one table into another.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(a1: table, f: int, e: int, t: int, [a2: table]) -> table`
///
/// Moves elements from the table `a1` to the table `a2`, performing the equivalent to the following multiple assignment: `a2[t],··· = a1[f],···,a1[e]`.
/// The default for `a2` is `a1`.
/// The destination range can overlap with the source range.
/// The number of elements to be moved must fit in a Lua integer.
///
/// Returns the destination table `a2`.
///
/// # Implementation-specific behavior
///
/// *  Operations performed by this function are *regular* that is it may invoke metamethods.
///    This includes both getting and setting values on tables.
///    
///    This replicates behavior of vanilla implementation.
///
/// *  Order of operations is undefined.
pub fn move_<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use super::builtins::{get_index, set_index, GetIndex, SetIndex};
    use rt::error::RuntimeError;
    use rt::ffi::delegate::{Delegate, Request, Response, RuntimeView, State};
    use rt::value::WeakValue;
    use std::ops::RangeInclusive;
    use std::pin::Pin;

    enum Coro<Ty>
    where
        Ty: Types,
    {
        Started,
        CalledGetIndex {
            co: GetIndex<Ty>,
            source_range: RangeInclusive<i64>,
            target_range: RangeInclusive<i64>,
        },
        CalledSetIndex {
            co: SetIndex<Ty>,
            target_range: RangeInclusive<i64>,
        },
        Finished,
    }

    impl<Ty> Delegate<Ty> for Coro<Ty>
    where
        Ty: Types,
    {
        fn resume(
            self: Pin<&mut Self>,
            mut rt: RuntimeView<'_, Ty>,
            response: Response<Ty>,
        ) -> State<Request<Ty>, Result<(), RuntimeError<Ty>>> {
            use rt::ffi::delegate::rearrange;

            let this = self.get_mut();
            let current = std::mem::replace(this, Coro::Finished);
            match (current, response) {
                (Coro::Started, Response::Resume) => rearrange(this.started(rt)),
                (
                    Coro::CalledGetIndex {
                        mut co,
                        source_range,
                        target_range,
                    },
                    response,
                ) => {
                    let value = match co.resume(rt.reborrow(), response) {
                        State::Complete(Ok(value)) => value,
                        State::Complete(Err(err)) => return State::Complete(Err(err)),
                        State::Yielded(request) => {
                            *this = Coro::CalledGetIndex {
                                co,
                                source_range,
                                target_range,
                            };
                            return State::Yielded(request);
                        }
                    };

                    let target = rt.stack[0];
                    let source = rt.stack[1];

                    rearrange(this.iter_get_index(
                        rt,
                        source,
                        target,
                        source_range,
                        target_range,
                        value,
                    ))
                }
                (
                    Coro::CalledSetIndex {
                        mut co,
                        target_range,
                    },
                    response,
                ) => {
                    match co.resume(rt.reborrow(), response) {
                        State::Complete(Ok(())) => (),
                        State::Complete(Err(err)) => return State::Complete(Err(err)),
                        State::Yielded(request) => {
                            *this = Coro::CalledSetIndex { co, target_range };
                            return State::Yielded(request);
                        }
                    };

                    let target = rt.stack[0];

                    rearrange(this.iter_set_index(rt, target, target_range))
                }
                (Coro::Finished, _) => unreachable!("resumed finished coroutine"),
                _ => unreachable!("invalid runtime response"),
            }
        }
    }

    impl<Ty> Coro<Ty>
    where
        Ty: Types,
    {
        fn started(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
        ) -> Result<State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::builtins::table::{GetIndexCache, SetIndexCache};
            use rt::ffi::arg_parser::{Int, LuaTable, Opts, ParseArgs, Split};
            use rt::ffi::delegate::StackSlot;
            use rt::value::{KeyValue as Key, WeakValue};
            use std::ops::ControlFlow;

            let (a1, f, e, t, a2): (LuaTable<_>, Int, Int, Int, Opts<(LuaTable<_>,)>) =
                rt.stack.parse(&mut rt.core.gc)?;
            let (a2,) = a2.split();
            let target = a2.unwrap_or(a1);
            let source = a1;
            let source_start = f.0;
            let source_end = e.0;
            let target_start = t.0;

            rt.stack.clear();

            // If the source range is empty simply exit.
            // Original Lua implementation doesn't error on this case.
            if (source_start..=source_end).is_empty() {
                rt.stack.transient_in(&mut rt.core.gc, |mut stack, _| {
                    stack.push(target.into());
                });

                return Ok(State::Complete(()));
            }

            let len = match source_end.checked_sub(source_start) {
                Some(t) => t,
                None => {
                    let err = rt
                        .core
                        .alloc_error_msg("range length does not fit into integer");
                    return Err(err);
                }
            };

            debug_assert!(len >= 0);

            let target_end = match target_start.checked_add(len) {
                Some(t) => t,
                None => {
                    let err = rt
                        .core
                        .alloc_error_msg("end of target range does not fit into integer");
                    return Err(err);
                }
            };

            enum SuspendPoint<Ty>
            where
                Ty: Types,
            {
                GetIndex(i64),
                SetIndex(i64, WeakValue<Ty>),
                Complete,
            }

            let point = rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                // Ensure that target is placed into first slot.
                // It is expected to be the return value of this function.
                stack.push(target.into());
                stack.push(source.into());

                let getter = GetIndexCache::new(source.into(), heap, &rt.core.metatable_registry)?;

                for i in source_start..=source_end {
                    let value = match getter.get(Key::Int(i), heap) {
                        Ok(ControlFlow::Break(value)) => value,
                        Ok(ControlFlow::Continue(_)) => return Ok(SuspendPoint::GetIndex(i)),
                        Err(err) => {
                            let msg = heap.intern(err.to_string().into());
                            return Err(RuntimeError::from_msg(msg));
                        }
                    };

                    stack.push(value)
                }

                let setter = SetIndexCache::new(target.into(), heap, &rt.core.metatable_registry)?;

                for i in (target_start..=target_end).rev() {
                    let value = stack.pop().unwrap();

                    match setter.set(Key::Int(i), value, heap) {
                        Ok(ControlFlow::Break(())) => (),
                        Ok(ControlFlow::Continue(_)) => {
                            return Ok(SuspendPoint::SetIndex(i, value))
                        }
                        Err(err) => {
                            let msg = heap.intern(err.to_string().into());
                            return Err(RuntimeError::from_msg(msg));
                        }
                    }
                }

                Ok(SuspendPoint::Complete)
            })?;

            match point {
                SuspendPoint::GetIndex(i) => {
                    let mut co = get_index(source.into(), Key::Int(i));

                    match co.resume(rt.reborrow(), Response::Resume) {
                        State::Complete(_) => unreachable!(),
                        State::Yielded(request) => {
                            #[expect(
                                clippy::reversed_empty_ranges,
                                reason = "we deliberately construct empty inclusive range on arithmetic overflow"
                            )]
                            let source_range = i
                                .checked_add(1)
                                .map(|start| start..=source_end)
                                .unwrap_or(1..=0);

                            *self = Coro::CalledGetIndex {
                                co,
                                source_range,
                                target_range: target_start..=target_end,
                            };
                            Ok(State::Yielded(request))
                        }
                    }
                }
                SuspendPoint::SetIndex(i, value) => {
                    let mut co = set_index(target.into(), Key::Int(i), value);

                    match co.resume(rt.reborrow(), Response::Resume) {
                        State::Complete(_) => unreachable!(),
                        State::Yielded(request) => {
                            #[expect(
                                clippy::reversed_empty_ranges,
                                reason = "we deliberately construct empty inclusive range on arithmetic overflow"
                            )]
                            let target_range = i
                                .checked_sub(1)
                                .map(|end| target_start..=end)
                                .unwrap_or(1..=0);

                            *self = Coro::CalledSetIndex { co, target_range };
                            Ok(State::Yielded(request))
                        }
                    }
                }
                SuspendPoint::Complete => {
                    rt.stack.adjust_height(StackSlot(1));
                    Ok(State::Complete(()))
                }
            }
        }

        fn iter_get_index(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
            source: WeakValue<Ty>,
            target: WeakValue<Ty>,
            source_range: RangeInclusive<i64>,
            target_range: RangeInclusive<i64>,
            value: WeakValue<Ty>,
        ) -> Result<State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::value::KeyValue as Key;

            rt.stack.transient().push(value);

            for i in source_range.clone() {
                let mut co = get_index(source, Key::Int(i));

                let value = match co.resume(rt.reborrow(), Response::Resume) {
                    State::Complete(res) => res?,
                    State::Yielded(request) => {
                        #[expect(
                            clippy::reversed_empty_ranges,
                            reason = "we deliberately construct empty inclusive range on arithmetic overflow"
                        )]
                        let source_range = i
                            .checked_add(1)
                            .map(|start| start..=*source_range.end())
                            .unwrap_or(1..=0);

                        *self = Coro::CalledGetIndex {
                            co,
                            source_range,
                            target_range,
                        };
                        rt.stack.sync(&mut rt.core.gc);
                        return Ok(State::Yielded(request));
                    }
                };

                rt.stack.transient().push(value);
            }

            self.iter_set_index(rt, target, target_range)
        }

        fn iter_set_index(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
            target: WeakValue<Ty>,
            target_range: RangeInclusive<i64>,
        ) -> Result<State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::ffi::delegate::StackSlot;
            use rt::value::KeyValue as Key;

            for i in target_range.clone().rev() {
                let value = rt.stack.pop().unwrap();

                let mut co = set_index(target, Key::Int(i), value);

                match co.resume(rt.reborrow(), Response::Resume) {
                    State::Complete(res) => res?,
                    State::Yielded(request) => {
                        #[expect(
                            clippy::reversed_empty_ranges,
                            reason = "we deliberately construct empty inclusive range on arithmetic overflow"
                        )]
                        let target_range = i
                            .checked_sub(1)
                            .map(|end| *target_range.start()..=end)
                            .unwrap_or(1..=0);

                        *self = Coro::CalledSetIndex { co, target_range };
                        return Ok(State::Yielded(request));
                    }
                }
            }

            rt.stack.adjust_height(StackSlot(1));
            rt.stack.sync(&mut rt.core.gc);
            Ok(State::Complete(()))
        }
    }

    ffi::from_fn(|| Coro::Started, "lua_std::table::move", ())
}

/// Pack all arguments into a new table.
///
/// # From Lua documentation
///
/// Returns a new table with all arguments stored into keys 1, 2, etc. and with a field **"n"** with the total number of arguments.
/// Note that the resulting table may not be a sequence, if some arguments are `nil`.
pub fn pack<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    let body = || {
        ffi::delegate::from_mut(|mut rt| {
            use rt::gc::LuaPtr;
            use rt::value::{KeyValue as Key, TableIndex, Value};

            let mut table = Ty::Table::default();

            let len = rt.stack.len();
            for (i, value) in (1..).zip(rt.stack.drain(..)) {
                table.set(Key::Int(i), value);
            }

            let n = rt.core.gc.intern("n".into());
            let n = Key::String(LuaPtr(n.downgrade()));

            let count = Value::Int(len.try_into().unwrap());

            table.set(n, count);

            let table = rt.core.gc.alloc_cell(table);
            let table = Value::Table(LuaPtr(table.downgrade()));

            rt.stack.synchronized(&mut rt.core.gc).push(table);
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::table::pack", ())
}
