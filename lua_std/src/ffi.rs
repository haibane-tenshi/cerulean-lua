use std::error::Error;
use std::fmt::{Debug, Display};
use std::path::PathBuf;

use repr::index::StackSlot;
use rt::error::{AlreadyDroppedError, AlreadyDroppedOr, RefAccessError, RtError, RuntimeError};
use rt::ffi::arg_parser::{
    FormatReturns, FromLuaString, LuaString, LuaTable, Maybe, NilOr, Opts, ParseArgs, ParseAtom,
    ParseFrom, Split, WeakConvertError,
};
use rt::ffi::delegate::RuntimeView;
use rt::ffi::{self, delegate, LuaFfi};
use rt::gc::{DisplayWith, Heap, LuaPtr};
use rt::runtime::Closure;
use rt::value::string::{into_utf8, AsEncoding};
use rt::value::table::KeyValue;
use rt::value::{Callable, StrongValue, Types, Value, WeakValue};

/// Runtime assertion.
///
/// # From Lua documentation
///
/// Signature: `(v: any [, message: any]) -> ()`
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
                let (cond, msg): (WeakValue<Ty>, Maybe<WeakValue<Ty>>) =
                    rt.stack.parse(&mut rt.core.gc)?;

                if cond.to_bool() {
                    Ok(())
                } else {
                    let err = msg
                        .into_option()
                        .map(|t| t.upgrade(&rt.core.gc).ok_or(AlreadyDroppedError))
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

/// Issue command to garbage collector.
///
/// # From Lua documentation
///
/// Signature: `([opt: string [, arg: any]]) -> [float | boolean]`
///
/// This function is a generic interface to the garbage collector. It performs different functions according to its first argument, `opt`:
///
/// * **"collect"**: Performs a full garbage-collection cycle. This is the default option.
/// * **"stop"**: Stops automatic execution of the garbage collector.
///     The collector will run only when explicitly invoked, until a call to restart it.
/// * **"restart"**: Restarts automatic execution of the garbage collector.
/// * **"count"**: Returns the total memory in use by Lua in Kbytes.
///     The value has a fractional part, so that it multiplied by 1024 gives the exact number of bytes in use by Lua.
/// * **"step"**: Performs a garbage-collection step.
///     The step "size" is controlled by arg.
///     With a zero value, the collector will perform one basic (indivisible) step.
///     For non-zero values, the collector will perform as if that amount of memory (in Kbytes) had been allocated by Lua.
///     Returns `true` if the step finished a collection cycle.
/// * **"isrunning"**: Returns a boolean that tells whether the collector is running (i.e., not stopped).
/// * **"incremental"**: Change the collector mode to incremental.
///     This option can be followed by three numbers: the garbage-collector pause, the step multiplier, and the step size (see §2.5.1).
///     A zero means to not change that value.
/// * **"generational"**: Change the collector mode to generational.
///     This option can be followed by two numbers: the garbage-collector minor multiplier and the major multiplier (see §2.5.2).
///     A zero means to not change that value.
///
/// See §2.5 for more details about garbage collection and some of these options.
///
/// This function should not be called by a finalizer.
///
/// # Implementation-specific behavior
///
/// You should note that our runtime uses custom garbage collector, different from the one used by vanilla Lua implementation.
/// As such not all commands are supported in the same manner:
///
/// * **"count"** - *total memory in use by Lua* is understood as bytes occupied by all alive objects inside heap.
///     Note the nuance:
///
///     * All currently alive objects will be accounted, even those that are soon to be collected as garbage.
///     * Object don't have to be reachable from inside runtime.
///         It is possible for host program to allocate and keep certain objects alive without ever exposing them to runtime.
///     * Memory reserved by heap but not used by any objects is not included.
///     * Memory used by internal auxiliary structures is not included.
///
///     Additionally, resulting value is provided on best-effort basis.
///     Memory management have a lot of nuance such as accounting for padding caused by alignment and partition into individual memory allocations.
///     It is also possible that some internal structures are allocated alongside objects and will be included in the count.
/// * **"step"** - Unsupported, silently ignored.
/// * **"incremental"** - Unsupported, silently ignored.
/// * **"generational"** - Unsupported, silently ignored.
pub fn collectgarbage<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
    Ty::String: AsEncoding + TryInto<String>,
{
    use rt::error::Diagnostic;
    use std::borrow::Cow;
    use std::fmt::Display;

    #[derive(Debug, Clone, Copy, Default)]
    enum Command {
        #[default]
        Collect,
        Stop,
        Restart,
        Count,
        Step,
        IsRunnning,
        Incremental,
        Generational,
    }

    impl Command {
        fn parse_from(value: Cow<str>) -> Result<Command, Error> {
            let r = match value.as_ref() {
                "collect" => Command::Collect,
                "stop" => Command::Stop,
                "restart" => Command::Restart,
                "count" => Command::Count,
                "step" => Command::Step,
                "isrunning" => Command::IsRunnning,
                "incremental" => Command::Incremental,
                "generational" => Command::Generational,
                _ => return Err(Error::Unknown(value)),
            };

            Ok(r)
        }
    }

    impl Display for Command {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let s = match self {
                Command::Collect => "collect",
                Command::Stop => "stop",
                Command::Restart => "restart",
                Command::Count => "count",
                Command::Step => "step",
                Command::IsRunnning => "isrunning",
                Command::Incremental => "incremental",
                Command::Generational => "generational",
            };

            write!(f, "{s}")
        }
    }

    #[derive(Debug)]
    enum Error<'a> {
        InvalidUtf8,
        Unknown(Cow<'a, str>),
    }

    impl Error<'_> {
        fn into_diagnostic(self) -> Diagnostic {
            use rt::error::diagnostic::Message;

            let s = match self {
                Error::InvalidUtf8 => "function recieved invalid command".into(),
                Error::Unknown(s) => format!(r#""{s}" is an unknown command"#),
            };

            let message = Message::error().with_message(s).with_notes(vec![
                "this function expects a command in its first argument".into(),
                format!(
                    r#"recognized commands are: "{}", "{}", "{}", "{}", "{}", "{}", "{}", "{}""#,
                    Command::Collect,
                    Command::Count,
                    Command::Restart,
                    Command::Stop,
                    Command::IsRunnning,
                    Command::Step,
                    Command::Incremental,
                    Command::Generational
                ),
                format!(
                    r#""{}" is used as default when arguments are absent"#,
                    Command::default()
                ),
            ]);

            Diagnostic::with_message(message)
        }
    }

    ffi::from_fn(
        || {
            delegate::from_mut(|mut rt| {
                let args: Opts<(LuaString<_>, WeakValue<Ty>)> = rt.stack.parse(&mut rt.core.gc)?;
                rt.stack.clear();
                let (command, _) = args.split();
                let command = command
                    .map(|t| {
                        let s = rt
                            .core
                            .gc
                            .get(t.0 .0)
                            .ok_or(AlreadyDroppedError)?
                            .as_inner();
                        let s = into_utf8(s)
                            .map_err(|_| AlreadyDroppedOr::Other(Error::InvalidUtf8))?;
                        let command = Command::parse_from(s).map_err(AlreadyDroppedOr::Other)?;

                        Ok(command)
                    })
                    .transpose()
                    .map_err(|err| match err {
                        AlreadyDroppedOr::Dropped(err) => err.into(),
                        AlreadyDroppedOr::Other(err) => {
                            RtError::Diagnostic(Box::new(err.into_diagnostic()))
                        }
                    })?
                    .unwrap_or_default();

                match command {
                    Command::Collect => rt.core.gc.gc(),
                    Command::Restart => rt.core.gc.enable_auto_gc(true),
                    Command::Stop => rt.core.gc.enable_auto_gc(false),
                    Command::IsRunnning => {
                        let value = rt.core.gc.is_auto_gc_enabled();
                        rt.stack.transient().push(Value::Bool(value));
                    }
                    Command::Count => {
                        let info = rt.core.gc.health_check();
                        let value = info.occupied_bytes as f64 / 1024.0;
                        rt.stack.transient().push(Value::Float(value));
                    }
                    // Silently ignore incompatible commands.
                    Command::Step | Command::Generational | Command::Incremental => (),
                }

                Ok(())
            })
        },
        "lua_std::collectgarbage",
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
    Ty::String: AsEncoding + TryInto<String> + Display,
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
        Ty::String: AsEncoding + TryInto<String>,
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
    Ty::String: AsEncoding + TryInto<String>,
{
    type Error = WeakConvertError<InvalidModeError>;

    fn parse_atom(value: WeakValue<Ty>, gc: &mut Heap<Ty>) -> Result<Self, Self::Error> {
        use rt::ffi::arg_parser::LuaString;

        let LuaString(LuaPtr(s)) = value.try_into()?;
        let s = gc.get(s).ok_or(AlreadyDroppedError)?.as_ref();
        let s = into_utf8(s).ok();

        let r = match s.as_ref().map(AsRef::as_ref) {
            Some("t") => Mode::Text,
            Some("b") => Mode::Binary,
            Some("bt") => Mode::BinaryOrText,
            _ => return Err(WeakConvertError::Other(InvalidModeError)),
        };

        Ok(r)
    }
}

pub fn load<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::String: AsEncoding + TryInto<String> + Display,
    String: ParseFrom<Ty::String>,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
    // Temporary bound
    <Ty::String as TryInto<String>>::Error: Debug,
{
    use rt::value::{Type, Weak};

    enum ChunkSource<Ty: Types> {
        String(String),
        Function(Callable<Weak, Ty>),
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

    impl<Ty> ParseAtom<WeakValue<Ty>, Heap<Ty>> for ChunkSource<Ty>
    where
        Ty: Types,
        Ty::String: TryInto<String>,
        // Temporary bound
        <Ty::String as TryInto<String>>::Error: Debug,
    {
        type Error = ChunkSourceError;

        fn parse_atom(value: WeakValue<Ty>, heap: &mut Heap<Ty>) -> Result<Self, Self::Error> {
            match value {
                Value::Function(t) => Ok(ChunkSource::Function(t)),
                Value::String(LuaPtr(t)) => {
                    let s = heap
                        .get(t)
                        .expect("fix this")
                        .as_inner()
                        .clone()
                        .try_into()
                        .expect("fix this");
                    Ok(ChunkSource::String(s))
                }
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
                    ChunkSource<Ty>,
                    Opts<(FromLuaString<String>, Mode, WeakValue<Ty>)>,
                ) = rt.stack.parse(&mut rt.core.gc)?;
                rt.stack.clear();

                let (_name, mode, env) = opts.split();

                let _mode = mode.unwrap_or_default();

                let source = match source {
                    ChunkSource::String(s) => s,
                    ChunkSource::Function(_) => {
                        let msg = rt
                            .core
                            .alloc_string("source functions are not yet supported".into());
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
    Ty::String: AsEncoding + TryInto<String> + Display,
    // WeakValue<Ty>: DisplayWith<Heap<Ty>> + TryConvertInto<LuaString<PathBuf>, Heap<Ty>>,
    // <WeakValue<Ty> as TryConvertInto<LuaString<PathBuf>, Heap<Ty>>>::Error: Error,
    String: ParseFrom<Ty::String>,
    PathBuf: ParseFrom<Ty::String>,
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
