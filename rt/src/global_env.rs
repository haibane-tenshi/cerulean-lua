use std::path::Path;

use repr::chunk::{ChunkExtension, ClosureRecipe, Function};
use repr::index::StackSlot;
use repr::literal::Literal;

use crate::chunk_builder::{ChunkBuilder, ChunkPart, ChunkRange};
use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::runtime::RuntimeView;
use crate::value::callable::Callable;
use crate::value::table::KeyValue;
use crate::value::Value;

use crate::error::RuntimeError;

pub fn empty<C>() -> ChunkBuilder<
    impl for<'rt> FnOnce(RuntimeView<'rt, C>, ChunkId, &mut Value<C>) -> Result<(), RuntimeError<C>>,
> {
    use crate::chunk_builder;
    use crate::value::table::TableRef;

    let chunk_part = ChunkPart {
        chunk_ext: ChunkExtension::empty(),
        builder: |mut rt: RuntimeView<C>, _, value: &mut Value<C>| {
            *value = Value::Table(TableRef::new());
            rt.stack.clear();

            Ok(())
        },
    };

    chunk_builder::builder().add(chunk_part)
}

pub fn assert<C>() -> ChunkPart<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, &mut Value<C>) -> Result<(), RuntimeError<C>>,
>
where
    C: ChunkCache,
{
    use crate::value::callable::RustClosureRef;

    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, value: &mut Value<C>| {
        let Value::Table(table) = value else {
            return Err(
                Value::String("global env value is expected to be table".to_string()).into(),
            );
        };

        let fn_assert = RustClosureRef::with_name("lua_std::assert", |rt: RuntimeView<_>| {
            let Some(cond) = rt.stack.get(StackSlot(0)) else {
                return Err(
                    Value::String("assert expects at least one argument".to_string()).into(),
                );
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
        });

        table
            .borrow_mut()
            .map_err(|_| Value::String("failed to borrow global env table".to_string()))?
            .set(
                KeyValue::String("assert".into()),
                Value::Function(Callable::RustClosure(fn_assert)),
            );

        Ok(())
    };

    ChunkPart { chunk_ext, builder }
}

pub fn pcall<C>() -> ChunkPart<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, &mut Value<C>) -> Result<(), RuntimeError<C>>,
>
where
    C: ChunkCache,
{
    use crate::value::callable::RustClosureRef;

    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, value: &mut Value<C>| {
        let Value::Table(table) = value else {
            return Err(
                Value::String("global env value is expected to be table".to_string()).into(),
            );
        };

        let fn_pcall = RustClosureRef::with_name("lua_std::pcall", |mut rt: RuntimeView<_>| {
            let Some(value) = rt.stack.get_mut(StackSlot(0)) else {
                return Err(
                    Value::String("pcall expects at least one argument".to_string()).into(),
                );
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
        });

        table
            .borrow_mut()
            .map_err(|_| Value::String("failed to borrow global env table".to_string()))?
            .set(
                KeyValue::String("pcall".into()),
                Value::Function(Callable::RustClosure(fn_pcall)),
            );

        Ok(())
    };

    ChunkPart { chunk_ext, builder }
}

pub fn print<C>() -> ChunkPart<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, &mut Value<C>) -> Result<(), RuntimeError<C>>,
>
where
    C: ChunkCache,
{
    use crate::value::callable::RustClosureRef;

    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, value: &mut Value<C>| {
        let Value::Table(table) = value else {
            return Err(
                Value::String("global env value is expected to be table".to_string()).into(),
            );
        };

        let fn_print = RustClosureRef::with_name("lua_std::print", |rt: RuntimeView<_>| {
            for value in rt.stack.iter() {
                print!("{value}");
            }

            Ok(())
        });

        table
            .borrow_mut()
            .map_err(|_| Value::String("failed to borrow global env table".to_string()))?
            .set(
                KeyValue::String("print".into()),
                Value::Function(Callable::RustClosure(fn_print)),
            );

        Ok(())
    };

    ChunkPart { chunk_ext, builder }
}

pub fn loadfile<C>() -> ChunkPart<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, &mut Value<C>) -> Result<(), RuntimeError<C>>,
>
where
    C: ChunkCache + KeyedChunkCache<Path>,
{
    use crate::value::callable::RustClosureRef;

    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, value: &mut Value<C>| {
        let Value::Table(table) = value else {
            return Err(
                Value::String("global env value is expected to be table".to_string()).into(),
            );
        };

        let fn_loadfile = RustClosureRef::with_name(
            "lua_std::loadfile",
            |mut rt: RuntimeView<_>| {
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

                let chunk_id = rt.load_from_file(&filename).map_err(|err| {
                    use crate::runtime::LoadWithError::*;

                    match err {
                        Immutable(_) => {
                            Value::String("runtime does not support loading new chunks".to_string())
                        }
                        Error(err) => {
                            Value::String(format!("failed to load file {filename}: {err}"))
                        }
                        CompilationFailure(diag) => {
                            use codespan_reporting::term::termcolor::NoColor;

                            let mut s = Vec::new();
                            let _ = diag.emit(&mut NoColor::new(&mut s), &Default::default());
                            let string = String::from_utf8_lossy(&s).to_string();

                            Value::String(string)
                        }
                    }
                })?;

                let ptr = FunctionPtr {
                    chunk_id,
                    function_id: FunctionId(0),
                };
                let env = env.unwrap_or_else(|| rt.global_env.clone());
                let closure = rt.construct_closure(ptr, [env])?;
                let closure_ref = Callable::LuaClosure(ClosureRef::new(closure));

                rt.stack.clear();
                rt.stack.push(Value::Function(closure_ref));

                Ok(())
            },
        );

        table
            .borrow_mut()
            .map_err(|_| Value::String("failed to borrow global env table".to_string()))?
            .set(
                KeyValue::String("loadfile".into()),
                Value::Function(Callable::RustClosure(fn_loadfile)),
            );

        Ok(())
    };

    ChunkPart { chunk_ext, builder }
}
