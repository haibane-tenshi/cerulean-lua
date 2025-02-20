//! Table manipulation library.
//!
//! # From Lua documentation
//!
//! This library provides generic functions for table manipulation.
//! It provides all its functions inside the table `table`.
//!
//! Remember that, whenever an operation needs the length of a table, all caveats about the length operator apply (see §3.4.7).
//! All functions ignore non-numeric keys in the tables given as arguments.

use rt::ffi::delegate::{self};
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
/// *   Numeric arguments will always get coerced to strings, irrespective of runtime configuration.
///     Usual caveats about int/float string representation apply:
///     no particular format is promised besides it being human-readable.
///     Render numbers manually to have control over output.
pub fn concat<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    let body = || {
        delegate::from_mut(|mut rt| {
            use rt::builtins::coerce;
            use rt::ffi::arg_parser::{Float, Int, LuaString, LuaTable, Opts, ParseArgs, Split};
            use rt::gc::{LuaPtr, TryGet};
            use rt::value::traits::Concat;
            use rt::value::{KeyValue, TableIndex, Value};

            let (list, args): (LuaTable<_>, Opts<(LuaString<_>, Int, Int)>) =
                rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let (sep, i, j) = args.split();

            let sep = sep.map(|sep| rt.core.gc.try_get(sep.0 .0)).transpose()?;

            let table: &Ty::Table = rt.core.gc.try_get(list.0 .0)?;
            let i = i.map(|t| t.0).unwrap_or(1);
            let j = j.map(|t| t.0).unwrap_or_else(|| table.border());

            if i > j {
                rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                    let r = Value::String(LuaPtr(heap.intern("".into()).downgrade()));
                    stack.push(r);
                });

                return Ok(());
            }

            let mut res = match table.get(&KeyValue::Int(i)) {
                Value::String(LuaPtr(ptr)) => {
                    let value = rt.core.gc.try_get(ptr)?.as_inner();
                    value.clone()
                }
                Value::Int(n) => coerce::int_to_str(Int(n)).into(),
                Value::Float(n) => coerce::flt_to_str(Float(n)).into(),
                value => {
                    let err = rt.core.alloc_error_msg(format!("value under index {i} has type {}; only strings, integers or floats permitted", value.type_()));
                    return Err(err);
                }
            };

            if let Some(k) = i.checked_add(1) {
                for key in k..=j {
                    if let Some(sep) = sep {
                        res.concat(sep);
                    }

                    match table.get(&KeyValue::Int(key)) {
                        Value::String(LuaPtr(ptr)) => {
                            let value = rt.core.gc.try_get(ptr)?.as_inner();
                            res.concat(value);
                        }
                        Value::Int(n) => {
                            let value = coerce::int_to_str(Int(n)).into();
                            res.concat(&value);
                        }
                        Value::Float(n) => {
                            let value = coerce::flt_to_str(Float(n)).into();
                            res.concat(&value);
                        }
                        value => {
                            let err = rt.core.alloc_error_msg(format!("value under index {key} has type {}; only strings, integers or floats permitted", value.type_()));
                            return Err(err);
                        }
                    }
                }
            }

            rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                let r = Value::String(LuaPtr(heap.intern(res).downgrade()));
                stack.push(r);
            });

            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::table::concat", ())
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
/// *  Note that all operations performed by this function are *regular* that is it may invoke metamethods.
///    This includes both getting and setting values on the table.
///    
///    This replicates vanilla Lua behavior.
///
/// *  In essence this function will perform assignments of the form `list[n+1] = list[n]` in order to shift values.
///    However no particular promises are made about order of operations (including between getters and setters).
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
                    rt.stack.clear();
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
                    match co.resume(rt, response) {
                        delegate::State::Complete(Ok(())) => (),
                        delegate::State::Complete(Err(err)) => {
                            return delegate::State::Complete(Err(err))
                        }
                        delegate::State::Yielded(request) => {
                            *this = State::CalledSetPos { co };
                            return delegate::State::Yielded(request);
                        }
                    };

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

            let mut co = len(list.into());

            match co.resume(rt.reborrow(), Response::Resume) {
                delegate::State::Complete(res) => self.after_len(rt, list.into(), pos, res?, value),
                delegate::State::Yielded(request) => {
                    rt.stack.transient_in(&mut rt.core.gc, |mut stack, _| {
                        stack.push(list.into());
                        stack.push(value);
                    });

                    *self = State::CalledLen { co, pos };
                    Ok(delegate::State::Yielded(request))
                }
            }
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
                                rt.stack.transient_in(&mut rt.core.gc, |mut stack, _| {
                                    stack.push(list);
                                    stack.push(pos_value);
                                });

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
                                rt.stack.transient_in(&mut rt.core.gc, |mut stack, _| {
                                    stack.push(list);
                                    stack.push(pos_value);
                                });

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
            rt.stack.clear();

            let mut co = set_index(list, Key::Int(pos), value);

            match co.resume(rt.reborrow(), Response::Resume) {
                delegate::State::Complete(res) => res?,
                delegate::State::Yielded(request) => {
                    *self = State::CalledSetPos { co };
                    return Ok(delegate::State::Yielded(request));
                }
            }

            Ok(delegate::State::Complete(()))
        }
    }

    ffi::from_fn(|| State::Started, "lua_std::table::insert", ())
}
