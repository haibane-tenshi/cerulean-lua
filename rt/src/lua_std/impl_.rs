use repr::index::StackSlot;
use std::error::Error;
use std::fmt::{Debug, Display};
use std::path::PathBuf;

use crate::error::{RefAccessError, RuntimeError};
use crate::ffi::{self, LuaFfiFnPtr, LuaFfiOnce, Maybe, Opts};
use crate::gc::Gc as GarbageCollector;
use crate::runtime::{ClosureRef, RuntimeView};
use crate::value::table::KeyValue;
use crate::value::{Callable, LuaString, LuaTable, NilOr, TypeMismatchError, TypeProvider, Value};

pub fn assert<Gc>() -> LuaFfiFnPtr<Gc>
where
    Gc: GarbageCollector,
{
    let f = |rt: RuntimeView<'_, Gc>| {
        let Some(cond) = rt.stack.get(StackSlot(0)) else {
            let msg = rt
                .core
                .gc
                .alloc_string("assert expects at least one argument".into());
            return Err(Value::String(msg).into());
        };

        if cond.to_bool() {
            Ok(())
        } else {
            let err = rt.stack.get(StackSlot(1)).cloned().unwrap_or_else(|| {
                let msg = rt.core.gc.alloc_string("assertion failed!".into());
                Value::String(msg)
            });
            Err(err.into())
        }
    };

    LuaFfiFnPtr::new(f, "lua_std::assert")
}

pub fn print<Gc>() -> LuaFfiFnPtr<Gc>
where
    Gc: TypeProvider,
    Value<Gc>: Display,
{
    let f = |mut rt: RuntimeView<'_, Gc>| {
        for value in rt.stack.iter() {
            print!("{value}");
        }

        rt.stack.clear();

        Ok(())
    };

    LuaFfiFnPtr::new(f, "lua_std::print")
}

pub fn pcall<Gc>() -> LuaFfiFnPtr<Gc>
where
    Gc: GarbageCollector,
    Gc::String: AsRef<[u8]>,
    Gc::RustCallable: LuaFfiOnce<Gc>,
    Gc::Table: for<'a> crate::gc::Visit<Gc::Sweeper<'a>>,
    Value<Gc>: Debug + Display,
{
    let f = |mut rt: RuntimeView<'_, Gc>| {
        let Some(value) = rt.stack.get_mut(StackSlot(0)) else {
            let msg = rt
                .core
                .gc
                .alloc_string("pcall expects at least one argument".into());
            return Err(Value::String(msg).into());
        };

        let Value::Function(func) = value.take() else {
            let msg = rt
                .core
                .gc
                .alloc_string("pcall expects the first argument to be a function".into());
            return Err(Value::String(msg).into());
        };

        match rt.invoke_at(func, StackSlot(1)) {
            Ok(()) => {
                let place = rt
                    .stack
                    .get_mut(StackSlot(0))
                    .expect("stack space below invocation point should be untouched");
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
                let msg = rt.core.gc.alloc_string(string.into());

                rt.reset();
                rt.stack.push(Value::Bool(false));
                rt.stack.push(Value::String(msg));
            }
        }

        Ok(())
    };

    LuaFfiFnPtr::new(f, "lua_std::pcall")
}

#[derive(Default)]
enum Mode {
    Binary,
    Text,
    #[default]
    BinaryOrText,
}

#[derive(Debug)]
struct InvalidModeError;

impl Display for InvalidModeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unknown loading mode, expected 't', 'b' or 'bt'")
    }
}

impl Error for InvalidModeError {}

impl<Gc> TryFrom<Value<Gc>> for Mode
where
    Gc: TypeProvider,
    Gc::String: AsRef<[u8]>,
{
    type Error = ModeError;

    fn try_from(value: Value<Gc>) -> Result<Self, Self::Error> {
        use crate::value::{Borrow, Type};

        let s = match value {
            Value::String(s) => s,
            value => {
                let err = TypeMismatchError {
                    expected: Type::String,
                    found: value.type_(),
                };

                return Err(err.into());
            }
        };

        let r = s.with_ref(|s| match s.as_ref() {
            b"t" => Ok(Mode::Text),
            b"b" => Ok(Mode::Binary),
            b"bt" => Ok(Mode::BinaryOrText),
            _ => Err(InvalidModeError),
        })??;

        Ok(r)
    }
}

#[derive(Debug)]
enum ModeError {
    Invalid(InvalidModeError),
    DroppedOrBorrowed(RefAccessError),
    TypeMismatch(TypeMismatchError),
}

impl Display for ModeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid(err) => write!(f, "{err}"),
            Self::DroppedOrBorrowed(err) => write!(f, "{err}"),
            Self::TypeMismatch(err) => write!(f, "{err}"),
        }
    }
}

impl Error for ModeError {}

impl From<InvalidModeError> for ModeError {
    fn from(value: InvalidModeError) -> Self {
        ModeError::Invalid(value)
    }
}

impl From<RefAccessError> for ModeError {
    fn from(value: RefAccessError) -> Self {
        ModeError::DroppedOrBorrowed(value)
    }
}

impl From<TypeMismatchError> for ModeError {
    fn from(value: TypeMismatchError) -> Self {
        ModeError::TypeMismatch(value)
    }
}

pub fn load<Gc>() -> LuaFfiFnPtr<Gc>
where
    Gc: GarbageCollector,
    Gc::String: AsRef<[u8]>,
    Gc::RustCallable: LuaFfiOnce<Gc>,
    Value<Gc>: Debug + Display + TryInto<LuaString<String>>,
    <Value<Gc> as TryInto<LuaString<String>>>::Error: Error,
{
    use crate::value::Type;

    enum ChunkSource<Gc: TypeProvider> {
        String(Gc::StringRef),
        Function(Callable<Gc::RustCallable>),
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

    impl<Gc> TryFrom<Value<Gc>> for ChunkSource<Gc>
    where
        Gc: TypeProvider,
    {
        type Error = ChunkSourceError;

        fn try_from(value: Value<Gc>) -> Result<Self, Self::Error> {
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

    let f = |rt: RuntimeView<'_, Gc>| {
        ffi::try_invoke_with_rt(
            rt,
            |mut rt: RuntimeView<'_, Gc>,
             source: ChunkSource<Gc>,
             opts: Opts<(LuaString<String>, Mode, Value<Gc>)>|
             -> Result<_, RuntimeError<Gc>> {
                use crate::ffi::Split;
                use crate::runtime::FunctionPtr;
                use crate::value::Borrow;
                use repr::index::FunctionId;

                let (_name, mode, env) = opts.split();

                let _mode = mode.unwrap_or_default();

                let source = match source {
                    ChunkSource::String(s) => s,
                    ChunkSource::Function(_) => {
                        let msg = rt
                            .core
                            .gc
                            .alloc_string("source functions are not yet supported".into());
                        return Err(Value::String(msg).into());
                    }
                };

                source.with_ref(|source| {
                    let source = std::str::from_utf8(source.as_ref()).map_err(|_| {
                        let msg = rt
                            .core
                            .gc
                            .alloc_string("string does not contain valid utf8".into());
                        Value::String(msg)
                    })?;

                    match rt.load(source.to_string(), None) {
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
                            let msg = rt.into_diagnostic(err.into()).emit_to_string();
                            let msg = rt.core.gc.alloc_string(msg.into());

                            Ok((NilOr::Nil, Maybe::Some(LuaString(msg))))
                        }
                    }
                })?
            },
        )
    };

    LuaFfiFnPtr::new(f, "lua_std::load")
}

pub fn loadfile<Gc>() -> LuaFfiFnPtr<Gc>
where
    Gc: GarbageCollector,
    Gc::String: TryInto<String> + AsRef<[u8]>,
    Gc::RustCallable: LuaFfiOnce<Gc>,
    Value<Gc>: Debug + Display + TryInto<LuaString<PathBuf>>,
    <Value<Gc> as TryInto<LuaString<PathBuf>>>::Error: Error,
{
    let f = |rt: RuntimeView<'_, Gc>| {
        ffi::try_invoke_with_rt(
            rt,
            |mut rt: RuntimeView<'_, Gc>,
             opts: Opts<(LuaString<PathBuf>, Mode, Value<Gc>)>|
             -> Result<_, RuntimeError<Gc>> {
                use crate::ffi::Split;
                use crate::runtime::FunctionPtr;
                use repr::index::FunctionId;

                let (filename, mode, env) = opts.split();
                let _mode = mode.unwrap_or_default();

                let Some(LuaString(filename)) = filename else {
                    let msg = rt.core.gc.alloc_string(
                        "loadfile doesn't yet support loading chunks from stdin".into(),
                    );
                    return Err(Value::String(msg).into());
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
                        let msg = rt.into_diagnostic(err).emit_to_string();
                        let msg = rt.core.gc.alloc_string(msg.into());

                        Ok((NilOr::Nil, Maybe::Some(LuaString(msg))))
                    }
                }
            },
        )
    };

    LuaFfiFnPtr::new(f, "lua_std::loadfile")
}

pub fn getmetatable<Gc>() -> LuaFfiFnPtr<Gc>
where
    Gc: GarbageCollector,
    Value<Gc>: Debug + Display,
{
    let f = |rt: RuntimeView<'_, Gc>| {
        ffi::try_invoke_with_rt(rt, |rt: RuntimeView<'_, Gc>, value: Value<Gc>| {
            let r = value
                .metatable(&rt.core.primitive_metatables)
                .map(|metatable| -> Result<_, RefAccessError> {
                    use crate::value::{Borrow, TableIndex};

                    let __metatable = metatable.with_ref(|mt| {
                        let key = rt.core.gc.alloc_string("__metatable".into());
                        mt.get(&KeyValue::String(key))
                    })?;

                    let r = if let Value::Nil = __metatable {
                        Value::Table(metatable)
                    } else {
                        __metatable
                    };

                    Ok(r)
                })
                .transpose()?
                .unwrap_or_default();

            Ok(r)
        })
    };

    LuaFfiFnPtr::new(f, "lua_std::getmetatable")
}

pub fn setmetatable<Gc>() -> LuaFfiFnPtr<Gc>
where
    Gc: GarbageCollector,
    Value<Gc>: Debug + Display,
{
    let f = |rt: RuntimeView<'_, Gc>| {
        ffi::try_invoke_with_rt(
            rt,
            |rt: RuntimeView<'_, Gc>,
             table: LuaTable<Gc::TableRef>,
             metatable: NilOr<LuaTable<Gc::TableRef>>| {
                use crate::value::{Borrow, Metatable, TableIndex};

                let LuaTable(table) = table;
                let metatable = metatable.into_option().map(|LuaTable(t)| t);

                table.with_mut(|t| -> Result<_, RuntimeError<Gc>> {
                    let has_meta_field = match t.metatable() {
                        Some(metatable) => metatable.with_ref(|mt| {
                            let key = rt.core.gc.alloc_string("__metatable".into());
                            mt.contains_key(&KeyValue::String(key))
                        })?,
                        None => false,
                    };

                    if has_meta_field {
                        let msg = rt.core.gc.alloc_string(
                            "table already has metatable with '__metatable' field".into(),
                        );
                        return Err(Value::String(msg).into());
                    }

                    t.set_metatable(metatable);

                    Ok(())
                })??;

                Ok(LuaTable(table))
            },
        )
    };

    LuaFfiFnPtr::new(f, "lua_std::getmetatable")
}
