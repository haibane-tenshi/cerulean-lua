use std::error::Error;
use std::fmt::{Debug, Display};
use std::path::PathBuf;

use gc::GcCell;
use repr::index::StackSlot;
use rt::error::{AlreadyDroppedError, RefAccessError, RuntimeError};
use rt::ffi::{self, LuaFfiOnce, LuaFfiPtr, Maybe, Opts};
use rt::gc::{DisplayWith, Heap, LuaPtr, TryFromWithGc, TryIntoWithGc};
use rt::runtime::{Closure, RuntimeView};
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
        let Some(cond) = rt.stack.get_slot(StackSlot(0)) else {
            let msg = rt
                .core
                .alloc_string("assert expects at least one argument".into());
            return Err(RuntimeError::from_msg(msg));
        };

        if cond.to_bool() {
            Ok(())
        } else {
            let err = rt
                .stack
                .get_slot(StackSlot(1))
                .cloned()
                .map(|err| err.upgrade(&rt.core.gc).ok_or(AlreadyDroppedError))
                .transpose()?
                .unwrap_or_else(|| {
                    let msg = rt.core.alloc_string("assertion failed!".into());
                    Value::String(LuaPtr(msg))
                });
            Err(RuntimeError::from_value(err))
        }
    };

    LuaFfiPtr::new(f, "lua_std::assert")
}

pub fn print<Ty>() -> LuaFfiPtr<Ty>
where
    Ty: CoreTypes,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
{
    let f = |mut rt: RuntimeView<'_, Ty>| {
        for value in rt.stack.iter() {
            print!("{}", value.display(&rt.core.gc));
        }

        rt.stack.clear();

        Ok(())
    };

    LuaFfiPtr::new(f, "lua_std::print")
}

pub fn pcall<Ty>() -> LuaFfiPtr<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::String: AsRef<[u8]> + Display,
    Ty::RustClosure: LuaFfiOnce<Ty>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    let f = |mut rt: RuntimeView<'_, Ty>| {
        let func = {
            let mut stack = rt.stack.frame(&mut rt.core.gc);
            let Some(mut value) = stack.get_mut(StackSlot(0)) else {
                // Drop forces stack to stay alive until end of block (where its drop impl is called).
                // Have to drop it manually on divergent branches.
                drop(stack);
                let msg = rt
                    .core
                    .alloc_string("pcall expects at least one argument".into());
                return Err(RuntimeError::from_msg(msg));
            };

            let Value::Function(func) = value.take() else {
                drop(value);
                drop(stack);
                let msg = rt
                    .core
                    .alloc_string("pcall expects the first argument to be a function".into());
                return Err(RuntimeError::from_msg(msg));
            };

            func
        };

        match rt.invoke_at(func, StackSlot(1)) {
            Ok(()) => {
                let mut stack = rt.stack.frame(&mut rt.core.gc);
                let mut place = stack
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
                let msg = rt.core.alloc_string(string.into());

                rt.reset();
                let mut stack = rt.stack.transient_frame();
                stack.push(Value::Bool(false));
                stack.push(Value::String(LuaPtr(msg.downgrade())));
                stack.sync(&mut rt.core.gc);
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

impl<Ty> TryFromWithGc<WeakValue<Ty>, Heap<Ty>> for Mode
where
    Ty: CoreTypes,
    Ty::String: AsRef<[u8]>,
{
    type Error = ModeError;

    fn try_from_with_gc(value: WeakValue<Ty>, gc: &mut Heap<Ty>) -> Result<Self, Self::Error> {
        use rt::value::Type;

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

        let bytes = gc.get(s.0).ok_or(AlreadyDroppedError)?.as_ref().as_ref();

        let r = match bytes {
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

impl From<AlreadyDroppedError> for ModeError {
    fn from(value: AlreadyDroppedError) -> Self {
        ModeError::DroppedOrBorrowed(value.into())
    }
}

pub fn load<Ty>() -> LuaFfiPtr<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::String: AsRef<[u8]> + Display,
    Ty::RustClosure: LuaFfiOnce<Ty>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>> + TryIntoWithGc<LuaString<String>, Heap<Ty>>,
    <WeakValue<Ty> as TryIntoWithGc<LuaString<String>, Heap<Ty>>>::Error: Error,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    use rt::value::{Type, Weak};

    enum ChunkSource<Rf: Types, Ty: CoreTypes> {
        String(Rf::String<Ty::String>),
        Function(Callable<Rf, Ty>),
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

    impl<Rf, Ty> TryFrom<Value<Rf, Ty>> for ChunkSource<Rf, Ty>
    where
        Rf: Types,
        Ty: CoreTypes,
    {
        type Error = ChunkSourceError;

        fn try_from(value: Value<Rf, Ty>) -> Result<Self, Self::Error> {
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
             source: ChunkSource<Weak, Ty>,
             opts: Opts<(LuaString<String>, Mode, WeakValue<Ty>)>|
             -> Result<_, RuntimeError<StrongValue<Ty>>> {
                use repr::index::FunctionId;
                use rt::ffi::Split;
                use rt::runtime::FunctionPtr;

                let (_name, mode, env) = opts.split();

                let _mode = mode.unwrap_or_default();

                let source = match source {
                    ChunkSource::String(s) => s.0,
                    ChunkSource::Function(_) => {
                        let msg = rt
                            .core
                            .alloc_string("source functions are not yet supported".into());
                        return Err(RuntimeError::from_msg(msg));
                    }
                };

                let source = rt
                    .core
                    .gc
                    .get(source)
                    .ok_or(AlreadyDroppedError)?
                    .as_ref()
                    .as_ref();

                let source = match std::str::from_utf8(source) {
                    Ok(s) => s.to_string(),
                    Err(_) => {
                        let msg = rt
                            .core
                            .alloc_string("string does not contain valid utf8".into());
                        return Err(RuntimeError::from_msg(msg));
                    }
                };

                match rt.load(source, None) {
                    Ok(chunk_id) => {
                        let ptr = FunctionPtr {
                            chunk_id,
                            function_id: FunctionId(0),
                        };
                        let env = env.unwrap_or_else(|| rt.core.global_env.downgrade());
                        let closure = rt.construct_closure(ptr, [env])?.downgrade();

                        Ok((NilOr::Some(Callable::Lua(LuaPtr(closure))), Maybe::None))
                    }
                    Err(err) => {
                        let msg = rt.into_diagnostic(err.into()).emit_to_string();
                        let msg = rt.core.alloc_string(msg.into());

                        Ok((NilOr::Nil, Maybe::Some(StrongValue::String(LuaPtr(msg)))))
                    }
                }
            },
        )
    };

    LuaFfiPtr::new(f, "lua_std::load")
}

pub fn loadfile<Ty>() -> LuaFfiPtr<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::String: TryInto<String> + AsRef<[u8]> + Display,
    Ty::RustClosure: LuaFfiOnce<Ty>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>> + TryIntoWithGc<LuaString<PathBuf>, Heap<Ty>>,
    <WeakValue<Ty> as TryIntoWithGc<LuaString<PathBuf>, Heap<Ty>>>::Error: Error,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    let f = |rt: RuntimeView<'_, Ty>| {
        ffi::try_invoke_with_rt(
            rt,
            |mut rt: RuntimeView<'_, Ty>,
             opts: Opts<(LuaString<PathBuf>, Mode, WeakValue<Ty>)>|
             -> Result<_, RuntimeError<StrongValue<Ty>>> {
                use repr::index::FunctionId;
                use rt::ffi::Split;
                use rt::runtime::FunctionPtr;

                let (filename, mode, env) = opts.split();
                let _mode = mode.unwrap_or_default();

                let Some(LuaString(filename)) = filename else {
                    let msg = rt.core.alloc_string(
                        "loadfile doesn't yet support loading chunks from stdin".into(),
                    );
                    return Err(RuntimeError::from_msg(msg));
                };

                match rt.load_from_file(&filename) {
                    Ok(chunk_id) => {
                        let ptr = FunctionPtr {
                            chunk_id,
                            function_id: FunctionId(0),
                        };
                        let env = env.unwrap_or_else(|| rt.core.global_env.downgrade());
                        let closure = rt.construct_closure(ptr, [env])?.downgrade();

                        Ok((NilOr::Some(Callable::Lua(LuaPtr(closure))), Maybe::None))
                    }
                    Err(err) => {
                        let msg = rt.into_diagnostic(err).emit_to_string();
                        let msg = rt.core.alloc_string(msg.into());

                        Ok((NilOr::Nil, Maybe::Some(StrongValue::String(LuaPtr(msg)))))
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
        ffi::try_invoke_with_rt(rt, |rt: RuntimeView<'_, Ty>, value: WeakValue<Ty>| {
            let r = rt
                .core
                .metatable_of(&value)?
                .map(|metatable| -> Result<WeakValue<Ty>, RefAccessError> {
                    use rt::value::TableIndex;

                    let __metatable = {
                        let key = rt.core.alloc_string("__metatable".into()).downgrade();
                        rt.core
                            .gc
                            .get(metatable)
                            .ok_or(AlreadyDroppedError)?
                            .get(&KeyValue::String(LuaPtr(key)))
                    };

                    let r = if let Value::Nil = __metatable {
                        Value::Table(LuaPtr(metatable))
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

    LuaFfiPtr::new(f, "lua_std::getmetatable")
}

pub fn setmetatable<Ty>() -> LuaFfiPtr<Ty>
where
    Ty: CoreTypes,
    // Value<Gc>: Debug + Display,
{
    let f = |rt: RuntimeView<'_, Ty>| {
        ffi::try_invoke_with_rt(
            rt,
            |rt: RuntimeView<'_, Ty>,
             table: LuaTable<LuaPtr<GcCell<Ty::Table>>>,
             metatable: NilOr<LuaTable<LuaPtr<GcCell<Ty::Table>>>>| {
                use rt::value::{Metatable, TableIndex};

                let LuaTable(LuaPtr(table)) = table;
                let metatable = metatable.into_option().map(|LuaTable(LuaPtr(t))| t);

                let old_metatable = rt
                    .core
                    .gc
                    .get(table)
                    .ok_or(AlreadyDroppedError)?
                    .metatable()
                    .copied();

                let has_meta_field = match old_metatable {
                    Some(metatable) => {
                        let key = rt.core.alloc_string("__metatable".into()).downgrade();
                        rt.core
                            .gc
                            .get(metatable)
                            .ok_or(AlreadyDroppedError)?
                            .contains_key(&KeyValue::String(LuaPtr(key)))
                    }
                    None => false,
                };

                if has_meta_field {
                    let msg = rt.core.alloc_string(
                        "table already has metatable with '__metatable' field".into(),
                    );
                    return Err(RuntimeError::from_msg(msg));
                }

                rt.core
                    .gc
                    .get_mut(table)
                    .ok_or(AlreadyDroppedError)?
                    .set_metatable(metatable);

                Ok(WeakValue::Table(LuaPtr(table)))
            },
        )
    };

    LuaFfiPtr::new(f, "lua_std::getmetatable")
}
