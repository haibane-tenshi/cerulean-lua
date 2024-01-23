use repr::index::StackSlot;
use std::error::Error;
use std::fmt::{Debug, Display};
use std::path::Path;

use crate::chunk_cache::{ChunkCache, KeyedChunkCache};
use crate::error::RuntimeError;
use crate::ffi::{self, LuaFfi, Maybe, NilOr, Opts, WithName};
use crate::runtime::{ClosureRef, RuntimeView};
use crate::value::{Callable, TypeMismatchError, TypeMismatchOrError, Value};

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
    (|mut rt: RuntimeView<'_, C>| {
        for value in rt.stack.iter() {
            print!("{value}");
        }

        rt.stack.clear();

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

impl<C> TryFrom<Value<C>> for Mode {
    type Error = TypeMismatchOrError<ModeError>;

    fn try_from(value: Value<C>) -> Result<Self, Self::Error> {
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

        let r = match s.as_str() {
            "t" => Mode::Text,
            "b" => Mode::Binary,
            "bt" => Mode::BinaryOrText,
            _ => return Err(TypeMismatchOrError::Other(ModeError)),
        };

        Ok(r)
    }
}

pub fn load<C>() -> impl LuaFfi<C> + 'static
where
    C: ChunkCache,
{
    use crate::value::Type;

    enum ChunkSource<C> {
        String(String),
        Function(Callable<C>),
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

    impl<C> TryFrom<Value<C>> for ChunkSource<C> {
        type Error = ChunkSourceError;

        fn try_from(value: Value<C>) -> Result<Self, Self::Error> {
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

    let f = |rt: RuntimeView<'_, C>| {
        ffi::try_invoke_with_rt(
            rt,
            |mut rt: RuntimeView<'_, C>,
             source: ChunkSource<C>,
             opts: Opts<(String, Mode, Value<C>)>|
             -> Result<_, RuntimeError<C>> {
                use crate::runtime::FunctionPtr;
                use repr::index::FunctionId;

                let (_name, _mode, env) = match opts {
                    Maybe::None => (None, Default::default(), None),
                    Maybe::Some((name, Maybe::None)) => {
                        (name.into_option(), Default::default(), None)
                    }
                    Maybe::Some((name, Maybe::Some((mode, env)))) => (
                        name.into_option(),
                        mode.into_option().unwrap_or_default(),
                        env.flatten_into_option(),
                    ),
                };

                let source = match source {
                    ChunkSource::String(s) => s,
                    ChunkSource::Function(_) => {
                        return Err(Value::String(
                            "source functions are not yet supported".to_string(),
                        )
                        .into())
                    }
                };

                match rt.load(source, None) {
                    Ok(chunk_id) => {
                        let ptr = FunctionPtr {
                            chunk_id,
                            function_id: FunctionId(0),
                        };
                        let env = env.unwrap_or_else(|| rt.global_env.clone());
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

pub fn loadfile<C>() -> impl LuaFfi<C> + 'static
where
    C: ChunkCache + KeyedChunkCache<Path>,
{
    let f = |rt: RuntimeView<'_, C>| {
        ffi::try_invoke_with_rt(
            rt,
            |mut rt: RuntimeView<'_, C>,
             opts: Opts<(String, Mode, Value<C>)>|
             -> Result<_, RuntimeError<C>> {
                use crate::runtime::FunctionPtr;
                use repr::index::FunctionId;

                let (filename, _mode, env) = match opts {
                    Maybe::None => (None, Default::default(), None),
                    Maybe::Some((filename, Maybe::None)) => {
                        (filename.into_option(), Default::default(), None)
                    }
                    Maybe::Some((filename, Maybe::Some((mode, env)))) => (
                        filename.into_option(),
                        mode.into_option().unwrap_or_default(),
                        env.flatten_into_option(),
                    ),
                };

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
                        let env = env.unwrap_or_else(|| rt.global_env.clone());
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
