use std::error::Error;
use std::fmt::{Debug, Display};
use std::path::PathBuf;

use repr::index::StackSlot;
use rt::error::{AlreadyDroppedError, RefAccessError, RtError, RuntimeError};
use rt::ffi::arg_parser::{
    FormatReturns, FromLuaString, LuaTable, Maybe, NilOr, Opts, ParseAtom, WeakConvertError,
};
use rt::ffi::delegate::RuntimeView;
use rt::ffi::{self, delegate, LuaFfi};
use rt::gc::{DisplayWith, Heap, LuaPtr};
use rt::runtime::Closure;
use rt::value::table::KeyValue;
use rt::value::{Callable, Refs, StrongValue, Types, Value, WeakValue};

/// Runtime assertion.
///
/// # From Lua documentation
///
/// Signature: `(v [, message]) -> ()`
///
/// Raises an error if the value of its argument `v` is false (i.e., `nil` or `false`); otherwise, returns all its arguments.
/// In case of error, `message` is the error object; when absent, it defaults to "assertion failed!"
pub fn assert<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    ffi::from_fn(
        || {
            delegate::from_mut(|rt| {
                let Some(cond) = rt.stack.get(StackSlot(0)) else {
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
                        .get(StackSlot(1))
                        .copied()
                        .map(|err| err.upgrade(&rt.core.gc).ok_or(AlreadyDroppedError))
                        .transpose()?
                        .unwrap_or_else(|| {
                            let msg = rt.core.alloc_string("assertion failed!".into());
                            Value::String(LuaPtr(msg))
                        });
                    Err(RuntimeError::from_value(err))
                }
            })
        },
        "lua_std::assert",
        (),
    )
}

pub fn print<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
{
    ffi::from_fn(
        || {
            delegate::from_mut(|mut rt| {
                for value in rt.stack.iter() {
                    print!("{}", value.display(&rt.core.gc));
                }

                rt.stack.clear();

                Ok(())
            })
        },
        "lua_std::print",
        (),
    )
}

pub fn pcall<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::String: AsRef<[u8]> + Display,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    use rt::ffi::coroutine::State as CoState;
    use std::pin::Pin;

    #[derive(Clone, Copy, Default)]
    enum State {
        #[default]
        Started,
        Called,
    }

    #[derive(Default)]
    struct Delegate(State);

    impl<Ty> delegate::Delegate<Ty> for Delegate
    where
        Ty: Types,
    {
        fn resume(
            mut self: Pin<&mut Self>,
            mut rt: RuntimeView<'_, Ty>,
            response: delegate::Response<Ty>,
        ) -> CoState<delegate::Request<Ty>, Result<(), RtError<Ty>>> {
            use rt::ffi::delegate::Response;

            match self.0 {
                State::Started => {
                    let callable = {
                        let mut stack = rt.stack.synchronized(&mut rt.core.gc);
                        let Some(mut value) = stack.get_mut(StackSlot(0)) else {
                            // Drop forces stack to stay alive until end of block (where its drop impl is called).
                            // Have to drop it manually on divergent branches.
                            drop(stack);
                            let msg = rt
                                .core
                                .alloc_string("pcall expects at least one argument".into());
                            return CoState::Complete(Err(RuntimeError::from_msg(msg)));
                        };

                        let Value::Function(func) = value.take() else {
                            drop(value);
                            drop(stack);
                            let msg = rt.core.alloc_string(
                                "pcall expects the first argument to be a function".into(),
                            );
                            return CoState::Complete(Err(RuntimeError::from_msg(msg)));
                        };

                        func
                    };

                    match callable.upgrade(&rt.core.gc) {
                        Some(callable) => {
                            let r = delegate::Request::Invoke {
                                callable,
                                start: StackSlot(1),
                            };

                            self.0 = State::Called;
                            CoState::Yielded(r)
                        }
                        None => CoState::Complete(Err(AlreadyDroppedError.into())),
                    }
                }
                State::Called => {
                    match response {
                        Response::Evaluated(Ok(())) => {
                            let mut stack = rt.stack.synchronized(&mut rt.core.gc);
                            let mut place = stack
                                .get_mut(StackSlot(0))
                                .expect("stack space below invocation point should be untouched");

                            *place = Value::Bool(true);
                        }
                        Response::Evaluated(Err(err)) => {
                            use codespan_reporting::term::termcolor::NoColor;
                            use std::io::Write;

                            // Writing to buffer basically never fails, just ignore errors.
                            let mut s = Vec::new();
                            // let _ = rt.backtrace().emit(&mut s);
                            let _ = writeln!(&mut s);
                            let _ = err
                                .into_diagnostic(&rt.core.gc, rt.chunk_cache)
                                .emit(&mut NoColor::new(&mut s), &Default::default());
                            let string = String::from_utf8_lossy(&s).to_string();
                            let msg = rt.core.alloc_string(string.into());

                            let mut stack = rt.stack.transient();
                            stack.push(Value::Bool(false));
                            stack.push(Value::String(LuaPtr(msg.downgrade())));
                            stack.sync(&mut rt.core.gc);
                        }
                        Response::Resume => unreachable!(),
                    }

                    self.0 = State::Started;
                    CoState::Complete(Ok(()))
                }
            }
        }
    }

    ffi::from_fn(Delegate::default, "lua_std::pcall", ())
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

impl<Ty> ParseAtom<WeakValue<Ty>, Heap<Ty>> for Mode
where
    Ty: Types,
{
    type Error = WeakConvertError<InvalidModeError>;

    fn parse_atom(value: WeakValue<Ty>, gc: &mut Heap<Ty>) -> Result<Self, Self::Error> {
        use rt::ffi::arg_parser::LuaString;

        let LuaString(LuaPtr(s)) = value.try_into()?;
        let bytes = gc.get(s).ok_or(AlreadyDroppedError)?.as_ref().as_ref();

        let r = match bytes {
            b"t" => Mode::Text,
            b"b" => Mode::Binary,
            b"bt" => Mode::BinaryOrText,
            _ => return Err(WeakConvertError::Other(InvalidModeError)),
        };

        Ok(r)
    }
}

pub fn load<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::String: TryInto<String> + AsRef<[u8]> + Display,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    use rt::value::{Type, Weak};

    enum ChunkSource<Rf: Refs, Ty: Types> {
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

    impl<Rf, Ty, Gc> ParseAtom<Value<Rf, Ty>, Gc> for ChunkSource<Rf, Ty>
    where
        Rf: Refs,
        Ty: Types,
    {
        type Error = ChunkSourceError;

        fn parse_atom(value: Value<Rf, Ty>, _: &mut Gc) -> Result<Self, Self::Error> {
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

    ffi::from_fn(
        || {
            delegate::from_mut(|mut rt| {
                use repr::index::FunctionId;
                use rt::ffi::arg_parser::{ParseArgs, Split};
                use rt::runtime::FunctionPtr;

                #[expect(clippy::type_complexity)]
                let (source, opts): (
                    ChunkSource<Weak, Ty>,
                    Opts<(FromLuaString<String>, Mode, WeakValue<Ty>)>,
                ) = rt.stack.parse(&mut rt.core.gc).map_err(|err| {
                    let msg = rt.core.alloc_string(err.to_string().into());
                    RtError::from_msg(msg)
                })?;
                rt.stack.clear();

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

                let results = match rt.load(source, None) {
                    Ok(chunk_id) => {
                        let ptr = FunctionPtr {
                            chunk_id,
                            function_id: FunctionId(0),
                        };
                        let env = env.unwrap_or_else(|| rt.core.global_env.downgrade());
                        let closure = rt.construct_closure(ptr, [env])?.downgrade();

                        (NilOr::Some(Callable::Lua(LuaPtr(closure))), Maybe::None)
                    }
                    Err(err) => {
                        let err: RtError<_> = err.into();
                        let msg = err
                            .into_diagnostic(&rt.core.gc, rt.chunk_cache)
                            .emit_to_string();
                        let msg = rt.core.alloc_string(msg.into());

                        (NilOr::Nil, Maybe::Some(StrongValue::String(LuaPtr(msg))))
                    }
                };

                rt.stack.transient().format(results);

                Ok(())
            })
        },
        "lua_std::load",
        (),
    )
}

pub fn loadfile<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::String: TryInto<String> + TryInto<PathBuf> + AsRef<[u8]> + Display,
    // WeakValue<Ty>: DisplayWith<Heap<Ty>> + TryConvertInto<LuaString<PathBuf>, Heap<Ty>>,
    // <WeakValue<Ty> as TryConvertInto<LuaString<PathBuf>, Heap<Ty>>>::Error: Error,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    ffi::from_fn(
        || {
            delegate::from_mut(|mut rt| {
                use repr::index::FunctionId;
                use rt::ffi::arg_parser::{ParseArgs, Split};
                use rt::runtime::FunctionPtr;

                let opts: Opts<(FromLuaString<PathBuf>, Mode, WeakValue<Ty>)> =
                    rt.stack.parse(&mut rt.core.gc).map_err(|err| {
                        let msg = rt.core.alloc_string(err.to_string().into());
                        RtError::from_msg(msg)
                    })?;
                rt.stack.clear();

                let (filename, mode, env) = opts.split();
                let _mode = mode.unwrap_or_default();

                let Some(FromLuaString(filename)) = filename else {
                    let msg = rt.core.alloc_string(
                        "loadfile doesn't yet support loading chunks from stdin".into(),
                    );
                    return Err(RuntimeError::from_msg(msg));
                };

                let results = match rt.load_from_file(&filename) {
                    Ok(chunk_id) => {
                        let ptr = FunctionPtr {
                            chunk_id,
                            function_id: FunctionId(0),
                        };
                        let env = env.unwrap_or_else(|| rt.core.global_env.downgrade());
                        let closure = rt.construct_closure(ptr, [env])?.downgrade();

                        (NilOr::Some(Callable::Lua(LuaPtr(closure))), Maybe::None)
                    }
                    Err(err) => {
                        let msg = err
                            .into_diagnostic(&rt.core.gc, rt.chunk_cache)
                            .emit_to_string();
                        let msg = rt.core.alloc_string(msg.into());

                        (NilOr::Nil, Maybe::Some(StrongValue::String(LuaPtr(msg))))
                    }
                };

                rt.stack.transient().format(results);

                Ok(())
            })
        },
        "lua_std::loadfile",
        (),
    )
}

pub fn getmetatable<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
    // Value<Gc>: Debug + Display,
{
    ffi::from_fn(
        || {
            delegate::from_mut(|mut rt| {
                use rt::ffi::arg_parser::ParseArgs;

                let value: WeakValue<Ty> = rt.stack.parse(&mut rt.core.gc).map_err(|err| {
                    let msg = rt.core.alloc_string(err.to_string().into());
                    RtError::from_msg(msg)
                })?;
                rt.stack.clear();

                let results = rt
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

                rt.stack.transient().format(results);

                Ok(())
            })
        },
        "lua_std::getmetatable",
        (),
    )
}

pub fn setmetatable<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
    // Value<Gc>: Debug + Display,
{
    ffi::from_fn(
        || {
            delegate::from_mut(|mut rt| {
                use gc::GcCell;
                use rt::ffi::arg_parser::ParseArgs;
                use rt::value::{Metatable, TableIndex};

                let (table, metatable): (LuaTable<_>, NilOr<LuaTable<_>>) =
                    rt.stack.parse(&mut rt.core.gc).map_err(|err| {
                        let msg = rt.core.alloc_string(err.to_string().into());
                        RtError::from_msg(msg)
                    })?;
                rt.stack.clear();

                // Type hint.
                // rustc gets confused and loses track of table type inside reference for some reason.
                let table: GcCell<Ty::Table> = table.0 .0;
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

                let results = WeakValue::Table(LuaPtr(table));

                rt.stack.transient().format(results);

                Ok(())
            })
        },
        "lua_std::getmetatable",
        (),
    )
}
