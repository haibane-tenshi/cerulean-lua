use repr::index::StackSlot;
use std::path::Path;

use crate::chunk_cache::{ChunkCache, KeyedChunkCache};
use crate::ffi::{IntoLuaFfi, LuaFfi, WithName};
use crate::runtime::RuntimeView;
use crate::value::{Callable, Value};

pub fn assert<C>() -> impl LuaFfi<C> + 'static {
    (|rt: RuntimeView<'_, C>| {
        let Some(cond) = rt.stack.get(StackSlot(0)) else {
            return Err(Value::String("assert expects at least one argument".to_string()).into());
        };

        if cond.to_bool() {
            Ok(())
        } else {
            let err = rt
                .stack
                .get(StackSlot(1))
                .cloned()
                .unwrap_or_else(|| Value::String("assertion failed!".to_string()));
            Err(err.into())
        }
    })
    .with_name("lua_std::assert")
}

pub fn print<C>() -> impl LuaFfi<C> + 'static {
    (|rt: RuntimeView<'_, C>| {
        for value in rt.stack.iter() {
            print!("{value}");
        }

        Ok(())
    })
    .with_name("lua_std::print")
}

pub fn pcall<C>() -> impl LuaFfi<C> + 'static
where
    C: ChunkCache,
{
    (|mut rt: RuntimeView<'_, C>| {
        let Some(value) = rt.stack.get_mut(StackSlot(0)) else {
            return Err(Value::String("pcall expects at least one argument".to_string()).into());
        };

        let Value::Function(func) = value.take() else {
            return Err(Value::String(
                "pcall expects the first argument to be a function".to_string(),
            )
            .into());
        };

        match rt.invoke_at(func, StackSlot(1)) {
            Ok(()) => {
                let place = rt.stack.get_mut(StackSlot(0)).unwrap();
                *place = Value::Bool(true);
            }
            Err(err) => {
                use codespan_reporting::term::termcolor::NoColor;
                use std::io::Write;

                // Writing to buffer basically never fails, just ignore errors.
                let mut s = Vec::new();
                let _ = rt.backtrace().emit(&mut s);
                let _ = writeln!(&mut s);
                let _ = rt
                    .into_diagnostic(err)
                    .emit(&mut NoColor::new(&mut s), &Default::default());
                let string = String::from_utf8_lossy(&s).to_string();

                rt.reset();
                rt.stack.push(Value::Bool(false));
                rt.stack.push(Value::String(string))
            }
        }

        Ok(())
    })
    .with_name("lua_std::pcall")
}

pub fn load<C>() -> impl LuaFfi<C> + 'static
where
    C: ChunkCache,
{
    (|mut rt: RuntimeView<'_, C>| {
        use crate::runtime::{ClosureRef, FunctionPtr};
            use repr::index::FunctionId;

            let source = match rt.stack.get_mut(StackSlot(0)).map(Value::take) {
                Some(Value::String(s)) => s,
                Some(value) => {
                    return Err(Value::String(format!(
                        "load expects string as the first argument, but it has type {}",
                        value.type_().to_lua_name()
                    ))
                    .into())
                }
                None => {
                    return Err(
                        Value::String("load expects at least one argument".to_string()).into(),
                    )
                }
            };

            let _name = rt
                .stack
                .get_mut(StackSlot(1))
                .map(|value| match value {
                    Value::String(s) => Ok(s),
                    value => Err(Value::String(format!(
                        "load expects chunk name to be a string, but it has type {}",
                        value.type_().to_lua_name()
                    ))),
                })
                .transpose()?;

            let _mode = rt.stack.get_mut(StackSlot(1)).map(|value| {
                    match value.take() {
                        Value::String(s) => match s.as_str() {
                            "t" | "b" | "bt" => Ok(s),
                            _ => Err(Value::String(format!("load: unrecognized mode '{s}' (expected either 't', 'b' or 'bt')")))
                        }
                        value => Err(Value::String(format!("load expects string containing opening mode as the second argument, but it has type {}", value.type_().to_lua_name()))),
                    }
                }).transpose()?;

            let env = rt.stack.get_mut(StackSlot(2)).map(Value::take);

            match rt.load(source, None) {
                Ok(chunk_id) => {
                    let ptr = FunctionPtr {
                        chunk_id,
                        function_id: FunctionId(0),
                    };
                    let env = env.unwrap_or_else(|| rt.global_env.clone());
                    let closure = rt.construct_closure(ptr, [env])?;
                    let closure_ref = Callable::Lua(ClosureRef::new(closure));

                    rt.stack.clear();
                    rt.stack.push(Value::Function(closure_ref));
                }
                Err(err) => {
                    let message = rt.into_diagnostic(err.into()).emit_to_string();

                    rt.stack.clear();
                    rt.stack.push(Value::Nil);
                    rt.stack.push(Value::String(message));
                }
            }

            Ok(())
    }).with_name("lua_std::load")
}

pub fn loadfile<C>() -> impl LuaFfi<C> + 'static
where
    C: ChunkCache + KeyedChunkCache<Path>,
{
    (|mut rt: RuntimeView<'_, C>| {
        use crate::runtime::{ClosureRef, FunctionPtr};
                use repr::index::FunctionId;

                let filename = match rt.stack.get_mut(StackSlot(0)).map(Value::take) {
                    Some(Value::String(s)) => s,
                    Some(value) => {
                        return Err(Value::String(format!(
                            "loadfile expects file name as the first argument, but it has type {}",
                            value.type_().to_lua_name()
                        ))
                        .into())
                    }
                    None => {
                        return Err(Value::String(
                            "loadfile does not support loading chunks from standard input yet"
                                .to_string(),
                        )
                        .into())
                    }
                };

                let _mode = rt.stack.get_mut(StackSlot(1)).map(|value| {
                    match value.take() {
                        Value::String(s) => match s.as_str() {
                            "t" | "b" | "bt" => Ok(s),
                            _ => Err(Value::String(format!("loadfile: unrecognized mode '{s}' (expected either 't', 'b' or 'bt')")))
                        }
                        value => Err(Value::String(format!("loadfile expects string containing opening mode as the second argument, but it has type {}", value.type_().to_lua_name()))),
                    }
                }).transpose()?;

                let env = rt.stack.get_mut(StackSlot(2)).map(Value::take);

                match rt.load_from_file(&filename) {
                    Ok(chunk_id) => {
                        let ptr = FunctionPtr {
                            chunk_id,
                            function_id: FunctionId(0),
                        };
                        let env = env.unwrap_or_else(|| rt.global_env.clone());
                        let closure = rt.construct_closure(ptr, [env])?;
                        let closure_ref = Callable::Lua(ClosureRef::new(closure));

                        rt.stack.clear();
                        rt.stack.push(Value::Function(closure_ref));
                    }
                    Err(err) => {
                        let message = rt.into_diagnostic(err).emit_to_string();

                        rt.stack.clear();
                        rt.stack.push(Value::Nil);
                        rt.stack.push(Value::String(message));
                    }
                }

                Ok(())
    }).with_name("lua_std::loadfile")
}
