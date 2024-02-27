use repr::index::StackSlot;
use std::error::Error;
use std::fmt::{Debug, Display};
use std::path::PathBuf;

use gc::Heap;

use rt::error::{RefAccessError, RuntimeError};
use rt::ffi::{self, LuaFfi, LuaFfiOnce, LuaFfiPtr, Maybe, Opts};
use rt::gc::{StringRef, TryIntoWithGc};
use rt::runtime::RuntimeView;
use rt::value::table::KeyValue;
use rt::value::{
    Callable, CoreTypes, LuaString, LuaTable, NilOr, StrongValue, TypeMismatchError, Types, Value,
    WeakValue,
};

pub fn assert<Ty>() -> LuaFfiPtr<Ty>
where
    Ty: CoreTypes,
{
    let f = |rt: RuntimeView<'_, Ty>| {
        let Some(cond) = rt.stack.get(StackSlot(0)) else {
            let msg = StringRef::new("assert expects at least one argument".into());
            return Err(Value::String(msg).into());
        };

        if cond.to_bool() {
            Ok(())
        } else {
            let err = rt.stack.get(StackSlot(1)).cloned().unwrap_or_else(|| {
                let msg = StringRef::new("assertion failed!".into());
                Value::String(msg)
            });
            Err(err.try_into_with_gc(&mut rt.core.gc)?)
        }
    };

    LuaFfiPtr::new(f, "lua_std::assert")
}

pub fn print<Ty>() -> LuaFfiPtr<Ty>
where
    Ty: CoreTypes,
    WeakValue<Ty>: Display,
{
    let f = |mut rt: RuntimeView<'_, Ty>| {
        for value in rt.stack.iter() {
            print!("{value}");
        }

        rt.stack.clear();

        Ok(())
    };

    LuaFfiPtr::new(f, "lua_std::print")
}

pub fn pcall<Ty>() -> LuaFfiPtr<Ty>
where
    Ty: CoreTypes,
    Ty::String: AsRef<[u8]>,
    Ty::RustClosure: LuaFfi<Ty>,
    WeakValue<Ty>: Display,
{
    let f = |mut rt: RuntimeView<'_, Ty>| {
        let Some(value) = rt.stack.get_mut(StackSlot(0)) else {
            let msg = StringRef::new("pcall expects at least one argument".into());
            return Err(Value::String(msg).into());
        };

        let Value::Function(func) = value.take() else {
            let msg = StringRef::new("pcall expects the first argument to be a function".into());
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
                let msg = StringRef::new(string.into());

                rt.reset();
                // rt.stack.push(Value::Bool(false));
                // rt.stack.push(Value::String(msg));
                todo!()
            }
        }

        Ok(())
    };

    LuaFfiPtr::new(f, "lua_std::pcall")
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

impl<Ty> TryFrom<StrongValue<Ty>> for Mode
where
    Ty: CoreTypes,
    Ty::String: AsRef<[u8]>,
{
    type Error = ModeError;

    fn try_from(value: StrongValue<Ty>) -> Result<Self, Self::Error> {
        use crate::value::Type;

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

        let r = match s.as_ref() {
            b"t" => Ok(Mode::Text),
            b"b" => Ok(Mode::Binary),
            b"bt" => Ok(Mode::BinaryOrText),
            _ => Err(InvalidModeError),
        }?;

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

pub fn load<Ty>() -> LuaFfiPtr<Ty>
where
    Ty: CoreTypes,
    Ty::String: AsRef<[u8]>,
    Ty::RustClosure: LuaFfiOnce<Ty>,
    StrongValue<Ty>: Display + TryIntoWithGc<LuaString<String>, Heap>,
    <StrongValue<Ty> as TryIntoWithGc<LuaString<String>, Heap>>::Error: Error,
{
    use crate::value::{Strong, Type};

    enum ChunkSource<Ty: Types> {
        String(Ty::String),
        Function(Callable<Ty>),
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

    impl<Ty> TryFrom<Value<Ty>> for ChunkSource<Ty>
    where
        Ty: Types,
    {
        type Error = ChunkSourceError;

        fn try_from(value: Value<Ty>) -> Result<Self, Self::Error> {
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

    let f = |rt: RuntimeView<'_, Ty>| {
        ffi::try_invoke_with_rt(
            rt,
            |mut rt: RuntimeView<'_, Ty>,
             source: ChunkSource<Strong<Ty>>,
             opts: Opts<(LuaString<String>, Mode, StrongValue<Ty>)>|
             -> Result<_, RuntimeError<Ty>> {
                use crate::ffi::Split;
                use crate::runtime::FunctionPtr;
                use repr::index::FunctionId;

                let (_name, mode, env) = opts.split();

                let _mode = mode.unwrap_or_default();

                let source = match source {
                    ChunkSource::String(s) => s,
                    ChunkSource::Function(_) => {
                        let msg = StringRef::new("source functions are not yet supported".into());
                        return Err(Value::String(msg).into());
                    }
                };

                {
                    let source = std::str::from_utf8(source.as_ref().as_ref()).map_err(|_| {
                        let msg = StringRef::new("string does not contain valid utf8".into());
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
                            let closure_ref = rt.core.gc.alloc(closure).into();

                            Ok((NilOr::Some(Callable::Lua(closure_ref)), Maybe::None))
                        }
                        Err(err) => {
                            let msg = rt.into_diagnostic(err.into()).emit_to_string();
                            let msg = StringRef::new(msg.into());

                            Ok((NilOr::Nil, Maybe::Some(LuaString(msg))))
                        }
                    }
                }
            },
        )
    };

    LuaFfiPtr::new(f, "lua_std::load")
}

pub fn loadfile<Ty>() -> LuaFfiPtr<Ty>
where
    Ty: CoreTypes,
    Ty::String: TryInto<String> + AsRef<[u8]>,
    Ty::RustClosure: LuaFfiOnce<Ty>,
    StrongValue<Ty>: Display + TryIntoWithGc<LuaString<PathBuf>, Heap>,
    <StrongValue<Ty> as TryIntoWithGc<LuaString<PathBuf>, Heap>>::Error: Error,
{
    let f = |rt: RuntimeView<'_, Ty>| {
        ffi::try_invoke_with_rt(
            rt,
            |mut rt: RuntimeView<'_, Ty>,
             opts: Opts<(LuaString<PathBuf>, Mode, StrongValue<Ty>)>|
             -> Result<_, RuntimeError<Ty>> {
                use crate::ffi::Split;
                use crate::runtime::FunctionPtr;
                use repr::index::FunctionId;

                let (filename, mode, env) = opts.split();
                let _mode = mode.unwrap_or_default();

                let Some(LuaString(filename)) = filename else {
                    let msg = StringRef::new(
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
                        let closure_ref = rt.core.gc.alloc(closure).into();

                        Ok((NilOr::Some(Callable::Lua(closure_ref)), Maybe::None))
                    }
                    Err(err) => {
                        let msg = rt.into_diagnostic(err).emit_to_string();
                        let msg = StringRef::new(msg.into());

                        Ok((NilOr::Nil, Maybe::Some(LuaString(msg))))
                    }
                }
            },
        )
    };

    LuaFfiPtr::new(f, "lua_std::loadfile")
}

pub fn getmetatable<Ty>() -> LuaFfiPtr<Ty>
where
    Ty: CoreTypes,
    // Value<Gc>: Debug + Display,
{
    let f = |rt: RuntimeView<'_, Ty>| {
        ffi::try_invoke_with_rt(rt, |rt: RuntimeView<'_, Ty>, value: StrongValue<Ty>| {
            let r = value
                .metatable(&rt.core.gc, &rt.core.primitive_metatables)
                .map(|metatable| -> Result<StrongValue<Ty>, RefAccessError> {
                    use crate::value::TableIndex;

                    let __metatable = {
                        let key = StringRef::new("__metatable".into());
                        rt.core.gc[&metatable].get(&KeyValue::String(key))
                    };

                    let r = if let Value::Nil = __metatable {
                        Value::Table(metatable)
                    } else {
                        __metatable.try_into_with_gc(&mut rt.core.gc)?
                    };

                    Ok(r)
                })
                .transpose()?
                .unwrap_or_default();

            Ok(r)
        })
    };

    LuaFfiPtr::new(f, "lua_std::getmetatable")
}

pub fn setmetatable<Ty>() -> LuaFfiPtr<Ty>
where
    Ty: CoreTypes,
    // Value<Gc>: Debug + Display,
{
    use crate::gc::RootTable;

    let f = |rt: RuntimeView<'_, Ty>| {
        ffi::try_invoke_with_rt(
            rt,
            |rt: RuntimeView<'_, Ty>,
             table: LuaTable<RootTable<Ty::Table>>,
             metatable: NilOr<LuaTable<RootTable<Ty::Table>>>| {
                use crate::error::AlreadyDroppedError;
                use crate::value::{Metatable, TableIndex};

                let LuaTable(table) = table;
                let metatable = metatable.into_option().map(|LuaTable(t)| t);

                let has_meta_field = match rt.core.gc[&table].metatable() {
                    Some(metatable) => {
                        let key = StringRef::new("__metatable".into());
                        rt.core
                            .gc
                            .get(metatable.into())
                            .ok_or(AlreadyDroppedError)?
                            .contains_key(&KeyValue::String(key))
                    }
                    None => false,
                };

                if has_meta_field {
                    let msg = StringRef::new(
                        "table already has metatable with '__metatable' field".into(),
                    );
                    return Err(Value::String(msg).into());
                }

                rt.core.gc[&table]
                    .set_metatable(metatable.as_ref().map(RootTable::downgrade).map(Into::into));

                Ok(LuaTable(table))
            },
        )
    };

    LuaFfiPtr::new(f, "lua_std::getmetatable")
}
