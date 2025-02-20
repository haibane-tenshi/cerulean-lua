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

/// Stringify and concatenate arguments.
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
