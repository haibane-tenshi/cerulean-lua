//! Table manipulation library.
//!
//! # From Lua documentation
//!
//! This library provides generic functions for table manipulation.
//! It provides all its functions inside the table `table`.
//!
//! Remember that, whenever an operation needs the length of a table, all caveats about the length operator apply (see §3.4.7).
//! All functions ignore non-numeric keys in the tables given as arguments.

use rt::ffi::{self, delegate, LuaFfi};
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
