use std::error::Error;
use std::fmt::{Debug, Display};
use std::path::PathBuf;

use repr::index::StackSlot;
use rt::chunk_cache::ChunkId;
use rt::error::{AlreadyDroppedError, AlreadyDroppedOr, RtError, RuntimeError};
use rt::ffi::arg_parser::{
    FormatReturns, FromLuaString, LuaString, LuaTable, Maybe, NilOr, Opts, ParseArgs, ParseAtom,
    ParseFrom, Split, WeakConvertError,
};
use rt::ffi::delegate::{Request, RuntimeView};
use rt::ffi::{self, boxed, delegate, DLuaFfi, LuaFfi};
use rt::gc::{DisplayWith, Heap, LuaPtr};
use rt::runtime::Closure;
use rt::value::string::{into_utf8, AsEncoding};
use rt::value::table::KeyValue;
use rt::value::{Callable, Int, Nil, StrongValue, Types, Value, WeakValue};

/// Runtime assertion.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(v: any [, message: any]) -> ()`
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
/// **Signature:**
/// * `([opt: string [, arg: any]]) -> [float | boolean]`
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
///     * Memory used by heap's internal auxiliary structures is not included.
///     * Memory allocated by runtime in any other way is not included.
///
///     Additionally, resulting value is provided on best-effort basis.
///     Memory management have nuances which makes it difficult to provide *exact* number.
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

fn load_chunk<Ty>(
    rt: &mut RuntimeView<'_, Ty>,
    file_name: Option<PathBuf>,
) -> Result<ChunkId, RtError<Ty>>
where
    Ty: Types,
{
    match file_name {
        Some(path) => rt.load_from_file(path),
        None => {
            use std::io::Read;

            let mut buf = Vec::new();
            std::io::stdin().read_to_end(&mut buf).map_err(|err| {
                rt.core
                    .alloc_error_msg(format!("failed to read from stdin: {err}"))
            })?;

            let source = String::from_utf8(buf).map_err(|err| {
                rt.core
                    .alloc_error_msg(format!("stdin does not contain valid utf8: {err}"))
            })?;

            rt.load(source, None).map_err(Into::into)
        }
    }
}

/// Load file and execute as Lua chunk.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `([filename: string]) -> [any,...]`
///
/// Opens the named file and executes its content as a Lua chunk.
/// When called without arguments, `dofile` executes the content of the standard input (`stdin`).
/// Returns all values returned by the chunk.
/// In case of errors, `dofile` propagates the error to its caller.
/// (That is, `dofile` does not run in protected mode.)
///
/// # Implementation-specific behavior
///
/// * Currently we don't have binary on-disk format, so binary chunks are (yet) unsupported.
/// * Source is expected to be valid utf8.
/// * On Windows platform only valid utf8 sequences can be read from `stdin`.
///     This limitation is imposed by [Rust's implementation](std::io::stdin).
///
/// # Notes
///
/// When reading from `stdin` this function will continue reading until reaching EoF.
///
/// On Linux this can be triggered by typing Ctrl-D in terminal.
///
/// On Windows this can be triggered by typing Ctrl-Z in terminal.
pub fn dofile<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
    PathBuf: ParseFrom<Ty::String>,
{
    let body = || {
        delegate::yield_1(
            |mut rt| {
                let filename: Maybe<FromLuaString<PathBuf>> = rt.stack.parse(&mut rt.core.gc)?;
                let file_name = filename.into_option().map(|t| t.0);
                rt.stack.clear();

                let chunk_id = load_chunk(&mut rt, file_name)?;
                let callable = rt.core.gc.alloc_cell(boxed(ffi::call_chunk(chunk_id)));
                let callable = Callable::Rust(LuaPtr(callable));
                let request = Request::Invoke {
                    callable,
                    start: StackSlot(0),
                };

                Ok(request)
            },
            |_| Ok(()),
        )
    };

    ffi::from_fn(body, "lua_std::dofile", ())
}

/// Iterate over table's integer indices.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(t: table,) -> (function, table, int)`
///
/// Returns three values (an iterator function, the table `t`, and `0`) so that the construction
///
/// ```lua
/// for i,v in ipairs(t) do body end
/// ```
///
/// will iterate over the key–value pairs `(1,t[1])`, `(2,t[2])`, ..., up to the first absent index.
pub fn ipairs<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn iter<Ty>() -> impl LuaFfi<Ty>
    where
        Ty: Types,
    {
        let body = || {
            delegate::from_mut(|mut rt| {
                use rt::value::traits::TableIndex;

                let (LuaTable(LuaPtr(table)), Int(prev)) = rt.stack.parse(&mut rt.core.gc)?;
                rt.stack.clear();

                let table: &Ty::Table = rt.core.gc.get(table).ok_or(AlreadyDroppedError)?;
                match table.next_key(&KeyValue::Int(prev)).copied() {
                    Some(key) => {
                        let value = table.get(&key);
                        rt.stack.transient().format((key, value));
                    }
                    None => rt.stack.transient().format(Nil),
                }

                Ok(())
            })
        };

        ffi::from_fn(body, "lua_std::ipairs::iter", ())
    }

    let body = || {
        delegate::from_mut(|mut rt| {
            let table: LuaTable<_> = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                let iter = heap.alloc_cell(boxed(iter::<Ty>())).downgrade();
                stack.format((Callable::Rust(LuaPtr(iter)), table, Int(0)));
            });

            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::ipairs", ())
}

/// Call another function in protected mode.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(f: any, [args: any...]) -> any...`
///
/// Calls the function `f` with the given arguments in protected mode.
/// This means that any error inside `f` is not propagated; instead, `pcall` catches the error and returns a status code.
/// Its first result is the status code (a boolean), which is `true` if the call succeeds without errors.
/// In such case, `pcall` also returns all results from the call, after this first result.
/// In case of any error, `pcall` returns `false` plus the error object.
/// Note that errors caught by `pcall` do not call a message handler.
pub fn pcall<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::String: AsEncoding + TryInto<String>,
{
    use rt::ffi::coroutine::State as CoState;
    use std::pin::Pin;

    #[derive(Clone, Copy, Default)]
    enum State {
        #[default]
        Started,
        Called,
        Finished,
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
            use rt::error::SignatureError;
            use rt::ffi::delegate::Response;

            let current = std::mem::replace(&mut self.0, State::Finished);
            match current {
                State::Started => {
                    let Some(value) = rt.stack.get(StackSlot(0)) else {
                        let err = SignatureError::TooFewArgs { found: 0 };
                        return CoState::Complete(Err(err.into()));
                    };

                    let Value::Function(callable) = value else {
                        let msg = rt.core.alloc_string(
                            "pcall expects the first argument to be a function".into(),
                        );
                        return CoState::Complete(Err(RuntimeError::from_msg(msg)));
                    };

                    let Some(callable) = callable.upgrade(&rt.core.gc) else {
                        return CoState::Complete(Err(AlreadyDroppedError.into()));
                    };

                    let r = delegate::Request::Invoke {
                        callable,
                        start: StackSlot(1),
                    };

                    self.0 = State::Called;
                    CoState::Yielded(r)
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

                            rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                                let msg = heap.intern(string.into()).downgrade();
                                stack.push(Value::Bool(false));
                                stack.push(Value::String(LuaPtr(msg)));
                            });
                        }
                        Response::Resume => unreachable!(),
                    }

                    CoState::Complete(Ok(()))
                }
                State::Finished => unreachable!(),
            }
        }
    }

    ffi::from_fn(Delegate::default, "lua_std::pcall", ())
}

#[derive(Default, Clone, Copy)]
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

/// Load a chunk.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(chunk: string | function [, chunkname: string [, mode: string [, env: any]]]) -> function | (fail, any)`
///
/// Loads a chunk.
///
/// If `chunk` is a string, the chunk is this string.
/// If `chunk` is a function, `load` calls it repeatedly to get the chunk pieces.
/// Each call to chunk must return a string that concatenates with previous results.
/// A return of an empty string, `nil`, or no value signals the end of the chunk.
///
/// If there are no syntactic errors, `load` returns the compiled chunk as a function;
/// otherwise, it returns **fail** plus the error message.
///
/// When you load a main chunk, the resulting function will always have exactly one upvalue, the `_ENV` variable (see §2.2).
/// However, when you load a binary chunk created from a function (see `string.dump`),
/// the resulting function can have an arbitrary number of upvalues,
/// and there is no guarantee that its first upvalue will be the `_ENV` variable.
/// (A non-main function may not even have an `_ENV` upvalue.)
///
/// Regardless, if the resulting function has any upvalues, its first upvalue is set to the value of `env`,
/// if that parameter is given, or to the value of the global environment.
/// Other upvalues are initialized with `nil`.
/// All upvalues are fresh, that is, they are not shared with any other function.
///
/// `chunkname` is used as the name of the chunk for error messages and debug information (see §4.7).
/// When absent, it defaults to chunk, if chunk is a string, or to **"=(load)"** otherwise.
///
/// The string `mode` controls whether the chunk can be text or binary (that is, a precompiled chunk).
/// It may be the string **"b"** (only binary chunks), **"t"** (only text chunks), or **"bt"** (both binary and text).
/// The default is **"bt"**.
///
/// It is safe to load malformed binary chunks; `load` signals an appropriate error.
/// However, Lua does not check the consistency of the code inside binary chunks;
/// running maliciously crafted bytecode can crash the interpreter.
///
/// # Implementation-specific behavior
///
/// * Strings produced by `chunk` function are concatenated in the same sense as Lua understands.
/// * Currently we don't have binary on-disk format, so binary chunks are (yet) unsupported.
pub fn load<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::String: AsEncoding + TryInto<String>,
    <Ty::String as TryInto<String>>::Error: Display,
    String: ParseFrom<Ty::String>,

    // Temp bound, remove when we are done with unpin.
    Ty::String: Unpin,
{
    use repr::index::FunctionId;
    use rt::ffi::arg_parser::{ParseArgs, Split};
    use rt::runtime::FunctionPtr;
    use rt::value::{Type, Weak};

    enum ChunkSource<Ty: Types> {
        String(String),
        Function(Callable<Weak, Ty>),
    }

    #[derive(Debug)]
    enum ChunkSourceError<E> {
        TypeMismatch(Type),
        AlreadyDropped(AlreadyDroppedError),
        Conversion(E),
    }

    impl<E> Display for ChunkSourceError<E>
    where
        E: Display,
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                ChunkSourceError::TypeMismatch(found) => write!(
                    f,
                    "expected value of type `{}` or `{}`, found `{found}`",
                    Type::String,
                    Type::Function
                ),
                ChunkSourceError::AlreadyDropped(err) => write!(f, "{err}"),
                ChunkSourceError::Conversion(err) => write!(f, "{err}"),
            }
        }
    }

    impl<E> Error for ChunkSourceError<E> where E: Debug + Display {}

    impl<E> From<AlreadyDroppedError> for ChunkSourceError<E> {
        fn from(value: AlreadyDroppedError) -> Self {
            ChunkSourceError::AlreadyDropped(value)
        }
    }

    impl<Ty> ParseAtom<WeakValue<Ty>, Heap<Ty>> for ChunkSource<Ty>
    where
        Ty: Types,
        Ty::String: TryInto<String>,
    {
        type Error = ChunkSourceError<<Ty::String as TryInto<String>>::Error>;

        fn parse_atom(value: WeakValue<Ty>, heap: &mut Heap<Ty>) -> Result<Self, Self::Error> {
            match value {
                Value::Function(t) => Ok(ChunkSource::Function(t)),
                Value::String(LuaPtr(t)) => {
                    let s = heap
                        .get(t)
                        .ok_or(AlreadyDroppedError)?
                        .as_inner()
                        .clone()
                        .try_into()
                        .map_err(ChunkSourceError::Conversion)?;

                    Ok(ChunkSource::String(s))
                }
                value => {
                    let err = ChunkSourceError::TypeMismatch(value.type_());
                    Err(err)
                }
            }
        }
    }

    fn finish<Ty>(
        mut rt: RuntimeView<'_, Ty>,
        source: String,
        name: Option<String>,
        mode: Mode,
        env: Option<WeakValue<Ty>>,
    ) -> Result<(), RtError<Ty>>
    where
        Ty: Types<LuaClosure = Closure<Ty>>,
        Ty::String: AsEncoding + TryInto<String>,
    {
        use rt::backtrace::Location;

        match mode {
            Mode::Text | Mode::BinaryOrText => (),
            Mode::Binary => {
                let err = rt
                    .core
                    .alloc_error_msg("attempt to load text chunk in binary-only mode");
                return Err(err);
            }
        }

        let name = name.unwrap_or_else(|| format!(r#""{source}""#));

        let location = Location {
            file: name,
            line: 0,
            column: 0,
        };

        let results = match rt.load(source, Some(location)) {
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
    }

    let body = || {
        enum State {
            Started,
            CalledChunkSource,
            Finished,
        }

        let mut state = State::Started;
        let mut persist_callable = None;
        let mut persist_source: Option<Ty::String> = None;
        let mut persist_name = None;
        let mut persist_mode = Mode::default();
        let mut persist_env = None;
        delegate::try_repeat(move |mut rt| {
            let current = std::mem::replace(&mut state, State::Finished);
            match current {
                State::Started => {
                    #[expect(clippy::type_complexity)]
                    let (source, opts): (
                        ChunkSource<Ty>,
                        Opts<(FromLuaString<String>, Mode, WeakValue<Ty>)>,
                    ) = rt.stack.parse(&mut rt.core.gc)?;
                    rt.stack.clear();

                    let (name, mode, env) = opts.split();
                    let name = name.map(|t| t.0);
                    let mode = mode.unwrap_or_default();

                    let source = match source {
                        ChunkSource::String(s) => s,
                        ChunkSource::Function(callable) => {
                            let callable =
                                callable.upgrade(&rt.core.gc).ok_or(AlreadyDroppedError)?;

                            persist_callable = Some(callable.clone());
                            persist_name = name;
                            persist_mode = mode;
                            persist_env = env
                                .map(|value| value.upgrade(&rt.core.gc).ok_or(AlreadyDroppedError))
                                .transpose()?;

                            let request = Request::Invoke {
                                callable,
                                start: StackSlot(0),
                            };

                            state = State::CalledChunkSource;
                            return Ok(delegate::State::Yielded(request));
                        }
                    };

                    finish(rt, source, name, mode, env)?;

                    Ok(delegate::State::Complete(()))
                }
                State::CalledChunkSource => {
                    let args: Opts<(LuaString<_>,)> = rt.stack.parse(&mut rt.core.gc)?;
                    rt.stack.clear();

                    let (part,) = args.split();

                    let finalize = match part {
                        Some(LuaString(LuaPtr(ptr))) => {
                            use rt::value::traits::{Concat, Len};

                            let part = rt.core.gc.get(ptr).ok_or(AlreadyDroppedError)?.as_inner();

                            if part.is_empty() {
                                true
                            } else {
                                let source = persist_source
                                    .take()
                                    .map(|mut source| {
                                        source.concat(part);
                                        source
                                    })
                                    .unwrap_or_else(|| part.clone());

                                persist_source = Some(source);
                                false
                            }
                        }
                        None => true,
                    };

                    if finalize {
                        let source = persist_source
                            .take()
                            .map(TryInto::try_into)
                            .transpose()
                            .map_err(|_| {
                                rt.core
                                    .alloc_error_msg("binary chunks are not (yet) supported")
                            })?
                            .unwrap_or_default();
                        let name = persist_name.take();
                        let env = persist_env.as_ref().map(|value| value.downgrade());
                        finish(rt, source, name, persist_mode, env)?;

                        Ok(delegate::State::Complete(()))
                    } else {
                        let request = Request::Invoke {
                            callable: persist_callable.clone().unwrap(),
                            start: StackSlot(0),
                        };

                        state = State::CalledChunkSource;
                        Ok(delegate::State::Yielded(request))
                    }
                }
                State::Finished => Ok(delegate::State::Complete(())),
            }
        })
    };

    ffi::from_fn(body, "lua_std::load", ())
}

/// Load chunk from a file.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `([filename: string [, mode: string [, env: any]]]) -> function | (fail, any)`
///
/// Similar to `load`, but gets the chunk from file `filename` or from the standard input, if no file name is given.
///
/// # Implementation-specific behavior
///
/// * Currently we don't have binary on-disk format, so binary chunks are (yet) unsupported.
/// * Source is expected to be valid utf8.
/// * On Windows platform only valid utf8 sequences can be read from `stdin`.
///     This limitation is imposed by [Rust's implementation](std::io::stdin).
///
/// # Notes
///
/// When reading from `stdin` this function will continue reading until reaching EoF.
///
/// On Linux this can be triggered by typing Ctrl-D in terminal.
///
/// On Windows this can be triggered by typing Ctrl-Z in terminal.
pub fn loadfile<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::String: AsEncoding + TryInto<String>,
    PathBuf: ParseFrom<Ty::String>,
{
    let body = || {
        delegate::from_mut(|mut rt| {
            use repr::index::FunctionId;
            use rt::ffi::arg_parser::{ParseArgs, Split};
            use rt::runtime::FunctionPtr;

            let opts: Opts<(FromLuaString<PathBuf>, Mode, WeakValue<Ty>)> =
                rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let (file_name, mode, env) = opts.split();
            let file_name = file_name.map(|t| t.0);
            let mode = mode.unwrap_or_default();

            match mode {
                Mode::Text | Mode::BinaryOrText => (),
                Mode::Binary => {
                    let err = rt
                        .core
                        .alloc_error_msg("attempt to load text chunk in binary-only mode");
                    return Err(err);
                }
            }

            let results = match load_chunk(&mut rt, file_name) {
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
    };

    ffi::from_fn(body, "lua_std::loadfile", ())
}

/// Query next key/value pair in the table
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(table: table [, index: any]) -> nil | (any, any)`
///
/// Allows a program to traverse all fields of a table.
/// Its first argument is a table and its second argument is an index in this table.
/// A call to `next` returns the next index of the table and its associated value.
/// When called with `nil` as its second argument, `next` returns an initial index and its associated value.
/// When called with the last index, or with `nil` in an empty table, `next` returns `nil`.
/// If the second argument is absent, then it is interpreted as `nil`.
/// In particular, you can use `next(t)` to check whether a table is empty.
///
/// The order in which the indices are enumerated is not specified, *even for numeric indices*.
/// (To traverse a table in numerical order, use a numerical **for**.)
///
/// You should not assign any value to a non-existent field in a table during its traversal.
/// You may however modify existing fields.
/// In particular, you may set existing fields to `nil`.
pub fn next<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    let body = || {
        delegate::from_mut(|mut rt| {
            use rt::value::traits::TableIndex;

            let (table, index): (LuaTable<_>, Maybe<WeakValue<Ty>>) =
                rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let table = rt.core.gc.get(table.0 .0).ok_or(AlreadyDroppedError)?;

            let index = index.into_option().unwrap_or_default();
            let next_key = match index {
                Value::Nil => table.first_key(),
                value => match value.try_into() {
                    Ok(key) => table.next_key(&key),
                    Err(err) => {
                        let err = rt.core.alloc_error_msg(err.to_string());
                        return Err(err);
                    }
                },
            };

            let results = match next_key {
                Some(key) => {
                    let value = table.get(key);
                    let key: WeakValue<_> = (*key).into();
                    (key, Maybe::Some(value))
                }
                None => (Nil.into(), Maybe::None),
            };

            rt.stack.transient().format(results);
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::next", ())
}

/// Iterate over all key/value pairs of table or object.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(t: any,) -> (any, any, any)`
/// * `(t: table,) -> (function, table, nil)`
///
/// If `t` has a metamethod `__pairs`, calls it with `t` as argument and returns the first three results from the call.
///
/// Otherwise, returns three values: the `next` function, the table `t`, and `nil`, so that the construction
///
/// ```lua
/// for k,v in pairs(t) do body end
/// ```
///
/// will iterate over all key–value pairs of table `t`.
///
/// See function next for the caveats of modifying the table during its traversal.
///
/// # Implementation-specific behavior
///
/// After calling `__pairs` metamethod stack will be forcefully adjusted to 3 elements, padding with `nil` if necessary.
pub fn pairs<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    let body = || {
        enum State {
            Started,
            CalledMetamethod,
            Finished,
        }

        let mut state = State::Started;
        delegate::try_repeat(move |mut rt| {
            let current = std::mem::replace(&mut state, State::Finished);
            match current {
                State::Started => {
                    let value: WeakValue<_> = rt.stack.parse(&mut rt.core.gc)?;

                    let key_string = rt.core.alloc_string("__pairs".into());
                    let key = KeyValue::String(LuaPtr(key_string.downgrade()));
                    let metavalue = rt::builtins::find_metavalue(
                        [value],
                        key,
                        &rt.core.gc,
                        &rt.core.metatable_registry,
                    )?;

                    match metavalue {
                        Value::Nil => {
                            rt.stack.clear();
                            rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                                let callback = heap.alloc_cell(boxed(next())).downgrade();
                                let next = Callable::Rust(LuaPtr(callback));
                                stack.format((next, value, Nil))
                            });

                            Ok(delegate::State::Complete(()))
                        }
                        metamethod => {
                            let callable = rt::builtins::prepare_invoke(
                                metamethod,
                                rt.stack.transient(),
                                &mut rt.core.gc,
                                &rt.core.metatable_registry,
                            )?;

                            // Table is still on the stack, but the latter is exactly the state the function should receive.
                            let request = Request::Invoke {
                                callable,
                                start: StackSlot(0),
                            };

                            state = State::CalledMetamethod;
                            Ok(delegate::State::Yielded(request))
                        }
                    }
                }
                State::CalledMetamethod => {
                    rt.stack.adjust_height(StackSlot(3));
                    Ok(delegate::State::Complete(()))
                }
                State::Finished => Ok(delegate::State::Complete(())),
            }
        })
    };

    ffi::from_fn(body, "lua_std::pairs", ())
}

/// Query metatable of an object.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(object: any) -> any`
///
/// If object does not have a metatable, returns `nil`.
/// Otherwise, if the object's metatable has a `__metatable` field, returns the associated value.
/// Otherwise, returns the metatable of the given object.
pub fn getmetatable<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    let body = || {
        delegate::from_mut(|mut rt| {
            let value: WeakValue<Ty> = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let results = rt
                .core
                .metatable_of(&value)?
                .map(|metatable| -> Result<WeakValue<Ty>, AlreadyDroppedError> {
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
    };

    ffi::from_fn(body, "lua_std::getmetatable", ())
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

/// Print all values
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(_: any...) -> ()`
///
/// Receives any number of arguments and prints their values to `stdout``, converting each argument to a string following the same rules of `tostring`.
///
/// The function `print` is not intended for formatted output, but only as a quick way to show a value, for instance for debugging.
/// For complete control over the output, use `string.format` and `io.write`.
///
/// # Implementation-specific behavior
///
/// If `__tostring` metamethod returns something but a string it will be printed raw, without recursively invoking `tostring` on it.
pub fn print<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    use rt::value::Weak;

    fn find_first<Ty>(
        rt: &RuntimeView<'_, Ty>,
        key: KeyValue<Weak, Ty>,
    ) -> Result<usize, AlreadyDroppedError>
    where
        Ty: Types,
    {
        let r = rt
            .stack
            .iter()
            .enumerate()
            .find_map(|(i, value)| {
                let metavalue = rt::builtins::find_metavalue(
                    [*value],
                    key,
                    &rt.core.gc,
                    &rt.core.metatable_registry,
                );
                match metavalue {
                    Ok(Value::Nil) => None,
                    Ok(_) => Some(Ok(i)),
                    Err(err) => Some(Err(err)),
                }
            })
            .transpose()?
            .unwrap_or(rt.stack.len());

        Ok(r)
    }

    let body = || {
        enum State {
            Started,
            CalledToString,
            Finished,
        }

        let mut state = State::Started;
        let mut persist_key = None;
        let mut persist_tostring = None;
        delegate::try_repeat(move |mut rt| {
            use std::io::Write;

            let current = std::mem::replace(&mut state, State::Finished);
            match current {
                State::Started => {
                    let s = rt.core.alloc_string("__tostring".into());
                    let key = KeyValue::String(LuaPtr(s.downgrade()));

                    // The first value that needs metamethod call for conversion.
                    // Everything before it can be safely rendered using default Display impl.
                    // This is much faster compared to repeated `tostring` calls, and also should be very frequent.
                    let last = find_first(&rt, key)?;

                    let mut stdout = std::io::stdout().lock();
                    for value in rt.stack.drain(..StackSlot(last)) {
                        write!(&mut stdout, "{}", value.display(&rt.core.gc))
                            .map_err(|err| rt.core.alloc_error_msg(err.to_string()))?;
                    }

                    let Some(value) = rt.stack.remove(StackSlot(0)) else {
                        return Ok(delegate::State::Complete(()));
                    };

                    let tostring = rt.core.gc.alloc_cell(boxed(tostring()));
                    let tostring = Callable::Rust(LuaPtr(tostring));

                    let start = StackSlot(rt.stack.len());
                    rt.stack.transient().push(value);

                    let request = Request::Invoke {
                        callable: tostring.clone(),
                        start,
                    };

                    persist_key = Some(s);
                    persist_tostring = Some(tostring);
                    state = State::CalledToString;
                    Ok(delegate::State::Yielded(request))
                }
                State::CalledToString => {
                    // Technically, stack after call can be in any state.
                    // However we call a known trusted function that always produces exactly 1 value or errors.
                    let value = rt.stack.pop().unwrap();

                    let mut stdout = std::io::stdout().lock();

                    write!(&mut stdout, "{}", value.display(&rt.core.gc))
                        .map_err(|err| rt.core.alloc_error_msg(err.to_string()))?;

                    let key = KeyValue::String(LuaPtr(persist_key.as_ref().unwrap().downgrade()));
                    let last = find_first(&rt, key)?;

                    for value in rt.stack.drain(..StackSlot(last)) {
                        write!(&mut stdout, "{}", value.display(&rt.core.gc))
                            .map_err(|err| rt.core.alloc_error_msg(err.to_string()))?;
                    }

                    let Some(value) = rt.stack.remove(StackSlot(0)) else {
                        return Ok(delegate::State::Complete(()));
                    };

                    let start = StackSlot(rt.stack.len());
                    rt.stack.transient().push(value);

                    let request = Request::Invoke {
                        callable: persist_tostring.clone().unwrap(),
                        start,
                    };

                    state = State::CalledToString;
                    Ok(delegate::State::Yielded(request))
                }
                State::Finished => Ok(delegate::State::Complete(())),
            }
        })
    };

    ffi::from_fn(body, "lua_std::print", ())
}

/// Convert value to string.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(v: any) -> string`
/// * `(v: any) -> any`
///
/// Receives a value of any type and converts it to a string in a human-readable format.
///
/// If the metatable of `v` has a `__tostring` field, then `tostring` calls the corresponding value with `v` as argument,
/// and uses the result of the call as its result.
/// Otherwise, if the metatable of `v` has a `__name` field with a string value, `tostring` may use that string in its final result.
///
/// For complete control of how numbers are converted, use `string.format`.
///
/// # Implementation-specific behavior
///
/// * Despite its name, this function is not guaranteed to produce a string.
///
///     While default behavior of this function indeed produces a string, the same guarantee does not extend to metamethod.
///     Lua does not specify any behavior for this case, so the result will be propagated as is.
///
/// * You should be cautions with your expectations of how numbers (both ints and floats) are handled.
///     This function renders numbers using Rust's standard formatting, which is different from Lua's number formats.
///
/// * After calling `__tostring` metamethod stack will be adjusted to 1 value.
///  
/// * Currently, `__name` metavalue is unused.
pub fn tostring<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    let body = || {
        enum State {
            Started,
            CalledMetamethod,
            Finished,
        }

        let mut state = State::Started;
        delegate::try_repeat(move |mut rt| {
            let current = std::mem::replace(&mut state, State::Finished);
            match current {
                State::Started => {
                    let value: WeakValue<_> = rt.stack.parse(&mut rt.core.gc)?;

                    let s = rt.core.alloc_string("__tostring".into());
                    let key = KeyValue::String(LuaPtr(s.downgrade()));
                    let metavalue = rt::builtins::find_metavalue(
                        [value],
                        key,
                        &rt.core.gc,
                        &rt.core.metatable_registry,
                    )?;

                    match metavalue {
                        Value::Nil => {
                            let s = format!("{}", value.display(&rt.core.gc));

                            rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                                let s = heap.intern(s.into());
                                stack.clear();
                                stack.push(Value::String(LuaPtr(s.downgrade())));
                            });

                            Ok(delegate::State::Complete(()))
                        }
                        metamethod => {
                            let callable =
                                rt.stack.transient_in(&mut rt.core.gc, |stack, heap| {
                                    rt::builtins::prepare_invoke(
                                        metamethod,
                                        stack,
                                        heap,
                                        &rt.core.metatable_registry,
                                    )
                                })?;

                            let request = Request::Invoke {
                                callable,
                                start: StackSlot(0),
                            };
                            state = State::CalledMetamethod;
                            Ok(delegate::State::Yielded(request))
                        }
                    }
                }
                State::CalledMetamethod => {
                    rt.stack.adjust_height(StackSlot(1));
                    Ok(delegate::State::Complete(()))
                }
                State::Finished => Ok(delegate::State::Complete(())),
            }
        })
    };

    ffi::from_fn(body, "lua_std::tostring", ())
}

/// Directly compare two values for equality.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(v1: any, v2: any) -> bool`
///
/// Checks whether `v1` is equal to `v2`, without invoking the `__eq` metamethod.
/// Returns a boolean.
///
/// # Implementation-specific behavior
///
/// Arguments are compared using [`Value`]'s `Eq` trait impl.
pub fn rawequal<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    let body = || {
        delegate::from_mut(|mut rt| {
            let [lhs, rhs]: [WeakValue<_>; 2] = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            rt.stack.transient().push(Value::Bool(lhs == rhs));

            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::rawequal", ())
}

/// Return raw length of object.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(v: string | table) -> int`
///
/// Returns the length of the object `v`, which must be a table or a string, without invoking the `__len` metamethod.
/// Returns an integer.
pub fn rawlen<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use gc::{Gc, GcCell, Interned};
    use rt::value::Type;

    enum StringOrTable<Ty>
    where
        Ty: Types,
    {
        String(Gc<Interned<Ty::String>>),
        Table(GcCell<Ty::Table>),
    }

    impl<Ty> ParseAtom<WeakValue<Ty>, Heap<Ty>> for StringOrTable<Ty>
    where
        Ty: Types,
    {
        type Error = Error;

        fn parse_atom(value: WeakValue<Ty>, _: &mut Heap<Ty>) -> Result<Self, Self::Error> {
            match value {
                Value::String(LuaPtr(t)) => Ok(StringOrTable::String(t)),
                Value::Table(LuaPtr(t)) => Ok(StringOrTable::Table(t)),
                value => Err(Error(value.type_())),
            }
        }
    }

    struct Error(Type);

    impl Display for Error {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "expected value of type {} or {}, found {}",
                Type::String,
                Type::Table,
                self.0
            )
        }
    }

    let body = || {
        delegate::from_mut(|mut rt| {
            use rt::value::traits::{Len, TableIndex};

            let value: StringOrTable<Ty> = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let r = match value {
                StringOrTable::String(value) => {
                    let s = rt.core.gc.get(value).ok_or(AlreadyDroppedError)?;
                    s.len().try_into().unwrap()
                }
                StringOrTable::Table(value) => {
                    let t = rt.core.gc.get(value).ok_or(AlreadyDroppedError)?;
                    t.border()
                }
            };

            rt.stack.transient().push(Value::Int(r));

            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::rawlen", ())
}
