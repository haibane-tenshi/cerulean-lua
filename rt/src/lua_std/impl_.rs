use repr::index::StackSlot;
use std::error::Error;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::path::Path;

use crate::chunk_cache::{ChunkCache, KeyedChunkCache};
use crate::error::RuntimeError;
use crate::ffi::{self, LuaFfi, LuaFfiOnce, Maybe, Opts, WithName};
use crate::runtime::{ClosureRef, RuntimeView};
use crate::value::table::KeyValue;
use crate::value::{
    Callable, NilOr, TableRef, TypeMismatchError, TypeMismatchOrError, TypeProvider, Value,
};

pub fn assert<Types, C>() -> impl LuaFfi<Types, C> + 'static
where
    Types: TypeProvider,
    Types::String: From<&'static str>,
    Value<Types>: Clone,
{
    (|rt: RuntimeView<'_, Types, C>| {
        let Some(cond) = rt.stack.get(StackSlot(0)) else {
            return Err(Value::String("assert expects at least one argument".into()).into());
        };

        if cond.to_bool() {
            Ok(())
        } else {
            let err = rt
                .stack
                .get(StackSlot(1))
                .cloned()
                .unwrap_or_else(|| Value::String("assertion failed!".into()));
            Err(err.into())
        }
    })
    .with_name("lua_std::assert")
}

pub fn print<Types, C>() -> impl LuaFfi<Types, C> + 'static
where
    Types: TypeProvider,
    Value<Types>: Clone + Display,
{
    (|mut rt: RuntimeView<'_, Types, C>| {
        for value in rt.stack.iter() {
            print!("{value}");
        }

        rt.stack.clear();

        Ok(())
    })
    .with_name("lua_std::print")
}

pub fn pcall<Types, C>() -> impl LuaFfi<Types, C> + 'static
where
    Types: TypeProvider<String = String>,
    // Types::RustCallable: LuaFfiOnce<Types, C>,
    Value<Types>: Clone + Display,
    Callable<Types::RustCallable>: LuaFfiOnce<Types, C>,
    C: ChunkCache,
{
    (|mut rt: RuntimeView<'_, Types, C>| {
        let Some(value) = rt.stack.get_mut(StackSlot(0)) else {
            return Err(Value::String("pcall expects at least one argument".into()).into());
        };

        let Value::Function(func) = value.take() else {
            return Err(
                Value::String("pcall expects the first argument to be a function".into()).into(),
            );
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
                rt.stack.push(Value::String(string.into()))
            }
        }

        Ok(())
    })
    .with_name("lua_std::pcall")
}

#[derive(Default)]
enum Mode {
    Binary,
    Text,
    #[default]
    BinaryOrText,
}

#[derive(Debug)]
struct ModeError;

impl Display for ModeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unknown loading mode, expected 't', 'b' or 'bt'")
    }
}

impl Error for ModeError {}

impl<Types> TryFrom<Value<Types>> for Mode
where
    Types: TypeProvider,
    Types::String: AsRef<str>,
{
    type Error = TypeMismatchOrError<ModeError>;

    fn try_from(value: Value<Types>) -> Result<Self, Self::Error> {
        use crate::value::Type;

        let s = match value {
            Value::String(s) => s,
            value => {
                let err = TypeMismatchError {
                    expected: Type::String,
                    found: value.type_(),
                };

                return Err(TypeMismatchOrError::TypeMismatch(err));
            }
        };

        let r = match s.as_ref() {
            "t" => Mode::Text,
            "b" => Mode::Binary,
            "bt" => Mode::BinaryOrText,
            _ => return Err(TypeMismatchOrError::Other(ModeError)),
        };

        Ok(r)
    }
}

pub fn load<Types, C>() -> impl LuaFfi<Types, C> + 'static
where
    C: ChunkCache,
    Types: TypeProvider<String = String, Table = TableRef<Types>>,
    Types::RustCallable: LuaFfiOnce<Types, C>,
    Value<Types>: Clone + PartialEq + Debug + Display,
    KeyValue<Types>: Hash + Eq,
{
    use crate::value::Type;

    enum ChunkSource<Types: TypeProvider> {
        String(Types::String),
        Function(Callable<Types::RustCallable>),
    }

    #[derive(Debug)]
    struct ChunkSourceError {
        found: Type,
    }

    impl Display for ChunkSourceError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let Self { found } = self;

            write!(
                f,
                "expected value of type `{}` or `{}`, found `{found}`",
                Type::String,
                Type::Function
            )
        }
    }

    impl Error for ChunkSourceError {}

    impl<Types> TryFrom<Value<Types>> for ChunkSource<Types>
    where
        Types: TypeProvider,
    {
        type Error = ChunkSourceError;

        fn try_from(value: Value<Types>) -> Result<Self, Self::Error> {
            match value {
                Value::Function(t) => Ok(ChunkSource::Function(t)),
                Value::String(t) => Ok(ChunkSource::String(t)),
                value => {
                    let err = ChunkSourceError {
                        found: value.type_(),
                    };

                    Err(err)
                }
            }
        }
    }

    let f = |rt: RuntimeView<'_, Types, C>| {
        ffi::try_invoke_with_rt(
            rt,
            |mut rt: RuntimeView<'_, Types, C>,
             source: ChunkSource<Types>,
             opts: Opts<(String, Mode, Value<Types>)>|
             -> Result<_, RuntimeError<Types>> {
                use crate::ffi::Split;
                use crate::runtime::FunctionPtr;
                use repr::index::FunctionId;

                let (_name, mode, env) = opts.split();

                let _mode = mode.unwrap_or_default();

                let source = match source {
                    ChunkSource::String(s) => s,
                    ChunkSource::Function(_) => {
                        return Err(
                            Value::String("source functions are not yet supported".into()).into(),
                        )
                    }
                };

                match rt.load(source, None) {
                    Ok(chunk_id) => {
                        let ptr = FunctionPtr {
                            chunk_id,
                            function_id: FunctionId(0),
                        };
                        let env = env.unwrap_or_else(|| rt.core.global_env.clone());
                        let closure = rt.construct_closure(ptr, [env])?;
                        let closure_ref = ClosureRef::new(closure);

                        Ok((NilOr::Some(closure_ref), Maybe::None))
                    }
                    Err(err) => {
                        let message = rt.into_diagnostic(err.into()).emit_to_string();

                        Ok((NilOr::Nil, Maybe::Some(message)))
                    }
                }
            },
        )
    };

    f.with_name("lua_std::load")
}

pub fn loadfile<Types, C>() -> impl LuaFfi<Types, C> + 'static
where
    C: ChunkCache + KeyedChunkCache<Path>,
    Types: TypeProvider<String = String, Table = TableRef<Types>>,
    Types::RustCallable: LuaFfiOnce<Types, C>,
    Value<Types>: Clone + PartialEq + Debug + Display,
    KeyValue<Types>: Hash + Eq,
{
    let f = |rt: RuntimeView<'_, Types, C>| {
        ffi::try_invoke_with_rt(
            rt,
            |mut rt: RuntimeView<'_, Types, C>,
             opts: Opts<(String, Mode, Value<Types>)>|
             -> Result<_, RuntimeError<Types>> {
                use crate::ffi::Split;
                use crate::runtime::FunctionPtr;
                use repr::index::FunctionId;

                let (filename, mode, env) = opts.split();
                let _mode = mode.unwrap_or_default();

                let Some(filename) = filename else {
                    return Err(Value::String(
                        "loadfile doesn't yet support loading chunks from stdin".to_string(),
                    )
                    .into());
                };

                match rt.load_from_file(&filename) {
                    Ok(chunk_id) => {
                        let ptr = FunctionPtr {
                            chunk_id,
                            function_id: FunctionId(0),
                        };
                        let env = env.unwrap_or_else(|| rt.core.global_env.clone());
                        let closure = rt.construct_closure(ptr, [env])?;
                        let closure_ref = ClosureRef::new(closure);

                        Ok((NilOr::Some(closure_ref), Maybe::None))
                    }
                    Err(err) => {
                        let message = rt.into_diagnostic(err).emit_to_string();

                        Ok((NilOr::Nil, Maybe::Some(message)))
                    }
                }
            },
        )
    };

    f.with_name("lua_std::loadfile")
}

pub fn getmetatable<Types, C>() -> impl LuaFfi<Types, C> + 'static
where
    Types: TypeProvider<String = String, Table = TableRef<Types>>,
    KeyValue<Types>: Hash + Eq,
    Value<Types>: Clone + Debug + Display,
{
    let f = |rt: RuntimeView<'_, Types, C>| {
        ffi::invoke_with_rt(rt, |rt: RuntimeView<'_, Types, C>, value: Value<Types>| {
            value
                .metatable(&rt.core.primitive_metatables)
                .map(|metatable| {
                    use crate::value::table::KeyValue;

                    let __metatable = metatable
                        .borrow()
                        .unwrap()
                        .get(&KeyValue::String("__metatable".into()));

                    if let Value::Nil = __metatable {
                        Value::Table(metatable)
                    } else {
                        __metatable
                    }
                })
                .unwrap_or_default()
        })
    };

    f.with_name("lua_std::getmetatable")
}

pub fn setmetatable<Types, C>() -> impl LuaFfi<Types, C> + 'static
where
    Types: TypeProvider<String = String, Table = TableRef<Types>>,
    KeyValue<Types>: Hash + Eq,
    Value<Types>: Clone,
{
    let f = |rt: RuntimeView<'_, Types, C>| {
        use crate::value::table::{KeyValue, TableRef};

        ffi::try_invoke(
            rt,
            |table: TableRef<Types>, metatable: NilOr<TableRef<Types>>| {
                let mut t = table.borrow_mut().unwrap();

                let has_meta = t.metatable().is_some_and(|metatable| {
                    metatable
                        .borrow()
                        .unwrap()
                        .contains_key(&KeyValue::String("__metatable".to_string()))
                });

                if has_meta {
                    return Err(Value::String(
                        "table already has metatable with '__metatable' field".to_string(),
                    )
                    .into());
                }

                t.set_metatable(metatable.into());
                drop(t);

                Ok(Value::Table(table))
            },
        )
    };

    f.with_name("lua_std::getmetatable")
}
