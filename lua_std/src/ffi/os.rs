//! Operating system facilities
//!
//! # From Lua documentation
//!
//! This library is implemented through table `os`.

use std::fmt::Display;
use std::path::PathBuf;
use std::process::Command;

use gc::RootCell;
use rt::ffi::arg_parser::ParseFrom;
use rt::ffi::{self, LuaFfi};
use rt::value::Types;

/// Return CPU time consumed by the entire process.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `() -> int`
///
/// Returns an approximation of the amount in seconds of CPU time used by the program, as returned by the underlying ISO C function `clock`.
pub fn clock<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    let body = || {
        ffi::delegate::from_mut(|mut rt| {
            use cpu_time::ProcessTime;
            use rt::ffi::arg_parser::ParseArgs;
            use rt::value::Value;

            let () = rt.stack.parse(&mut rt.core.gc)?;

            let dur = ProcessTime::now().as_duration();
            let secs = dur.as_secs();
            let output = secs.try_into().unwrap();

            rt.stack.transient().push(Value::Int(output));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::os::clock", ())
}

/// Return date and time as table or format it to string.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `([format: string, [time: int]]) -> string | table`
///  
/// Returns a string or a table containing date and time, formatted according to the given string format.
///
/// If the `time` argument is present, this is the time to be formatted (see the `os.time` function for a description of this value).
/// Otherwise, `date` formats the current time.
///
/// If format starts with '!', then the date is formatted in Coordinated Universal Time.
/// After this optional character, if format is the string "*t", then date returns a table with the following fields:
/// `year`, `month` (1–12), `day` (1–31), `hour` (0–23), `min` (0–59), `sec` (0–61, due to leap seconds),
/// `wday` (weekday, 1–7, Sunday is 1), `yday` (day of the year, 1–366), and `isdst` (daylight saving flag, a boolean).
/// This last field may be absent if the information is not available.
///
/// If format is not "*t", then date returns the date as a string, formatted according to the same rules as the ISO C function `strftime`.
///
/// If format is absent, it defaults to "%c", which gives a human-readable date and time representation using the current locale.
///
/// On non-POSIX systems, this function may be not thread safe because of its reliance on C function `gmtime` and C function `localtime`.
///
/// # Implementation-specific behavior
///
/// *  Implementation of this function does not rely on C library, therefore comments on thread safety do not apply.
///    Calling this function is always thread-safe.
///
///    Output is locale-independent as is the case with all functions provided by this library.
///
/// *  `time` when provided is expected to be number of seconds since Unix epoch (regardless of platform).
///    This is exactly what is returned by [`time`] API.
///
/// *  String formatting is done through [`chrono::format::strftime`] facility.
///    Technically, its template format is a superset of C's `strftime`.
///
/// *  Currently no information about DST is provided.
pub fn date<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    let body = || {
        ffi::delegate::from_mut(|mut rt| {
            use chrono::{DateTime, Datelike, FixedOffset, Local, Timelike};
            use rt::ffi::arg_parser::{Int, LuaString, Opts, ParseArgs, Split};
            use rt::gc::LuaPtr;
            use rt::value::string::try_gc_to_str;
            use rt::value::{KeyValue as Key, TableIndex, Value};

            let rest: Opts<(LuaString<_>, Int)> = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();
            let (format, time) = rest.split();

            let time = match time {
                Some(time) => {
                    use std::time::Duration;
                    let secs = time.0.try_into().map_err(|_| rt.core.alloc_error_msg(""))?;
                    Duration::from_secs(secs)
                }
                None => {
                    use std::time::{SystemTime, UNIX_EPOCH};
                    SystemTime::now().duration_since(UNIX_EPOCH).unwrap()
                }
            };

            let format = format
                .map(|t| try_gc_to_str(t.0 .0, &rt.core.gc))
                .transpose()?;
            let format = format.as_ref().map(AsRef::as_ref).unwrap_or("%c");

            let (is_utc, format) = if let Some(format) = format.strip_prefix('!') {
                (true, format)
            } else {
                (false, format)
            };

            let utc_time =
                DateTime::from_timestamp(time.as_secs().try_into().unwrap(), time.subsec_nanos())
                    .unwrap();
            let time: DateTime<FixedOffset> = if is_utc {
                utc_time.into()
            } else {
                utc_time.with_timezone(&Local).into()
            };

            if format == "*t" {
                rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                    let year = time.year().into();
                    let month = time.month().into();
                    let day = time.day().into();

                    let hour = time.hour().into();
                    let min = time.minute().into();

                    // Chrono folds leap seconds into nanoseconds.
                    let leap_second = time.nanosecond() >= 1_000_000_000;
                    let sec = (time.second() + if leap_second { 1 } else { 0 }).into();

                    let wday = time.weekday().number_from_monday().into();
                    let yday = time.ordinal().into();

                    let mut alloc_key =
                        |name: &str| Key::String(LuaPtr(heap.intern(name.into()).downgrade()));
                    let mut table = Ty::Table::default();

                    table.set(alloc_key("year"), Value::Int(year));
                    table.set(alloc_key("month"), Value::Int(month));
                    table.set(alloc_key("day"), Value::Int(day));

                    table.set(alloc_key("hour"), Value::Int(hour));
                    table.set(alloc_key("min"), Value::Int(min));
                    table.set(alloc_key("sec"), Value::Int(sec));

                    table.set(alloc_key("wday"), Value::Int(wday));
                    table.set(alloc_key("yday"), Value::Int(yday));

                    let result = Value::Table(LuaPtr(heap.alloc_cell(table).downgrade()));

                    stack.push(result);
                });
            } else {
                use std::fmt::Write;

                let mut output = String::new();
                if let Err(err) = write!(&mut output, "{}", time.format(format)) {
                    let err = rt.core.alloc_error_msg(err.to_string());
                    return Err(err);
                }

                rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                    let s = heap.intern(output.into());
                    let output = Value::String(LuaPtr(s.downgrade()));

                    stack.push(output);
                });
            }

            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::os::date", ())
}

/// Calculate difference between two timestamps.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(t2: int, t1: int) -> int`
///
/// Returns the difference, in seconds, from time `t1` to time `t2` (where the times are values returned by `os.time`).
/// In POSIX, Windows, and some other systems, this value is exactly `t2-t1`.
///
/// # Implementation-specific behavior
///
/// *  `os.time` always returns number of seconds since Unix epoch, so this function simply returns difference between two values.
///    
///    You will get an error on underflow.
pub fn difftime<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    let body = || {
        ffi::delegate::from_mut(|mut rt| {
            use rt::ffi::arg_parser::{Int, ParseArgs};
            use rt::value::Value;

            let [t2, t1]: [Int; 2] = rt.stack.parse(&mut rt.core.gc)?;

            if let Some(output) = t2.0.checked_sub(t1.0) {
                rt.stack.transient().push(Value::Int(output));

                Ok(())
            } else {
                let err = rt
                    .core
                    .alloc_error_msg("difference between timestamps does not fit into integer");
                Err(err)
            }
        })
    };

    ffi::from_fn(body, "lua_std::os::difftime", ())
}

/// Execute command in a shell.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `() -> bool`
/// * `(command: string) -> (bool | fail, string, int)`
///
/// This function is equivalent to the ISO C function `system`.
/// It passes `command` to be executed by an operating system shell.
/// Its first result is `true` if the command terminated successfully, or **fail** otherwise.
/// After this first result the function returns a string plus a number, as follows:
///
/// * **"exit"**: the command terminated normally; the following number is the exit status of the command.
/// * **"signal"**: the command was terminated by a signal; the following number is the signal that terminated the command.
///
/// When called without a command, `os.execute` returns a boolean that is true if a shell is available.
///
/// # Implementation-specific behavior
///
/// *   This function will attempt to spawn a new shell in child process and pipe the command as utf8-encoded string to the shell.
///     Afterwards it **will block entire host process** while waiting for the process to finish.
///     Any errors during this will be converted into Lua panics.
///
/// *   As its appearance suggests, this function **permits executing arbitrary commands** on your machine.
///     You *should not* expose it to untrusted scripts.
///
/// *   The `shell` parameter will be used to construct child process.
///     It is your responsibility to correctly configure it.
///
///     This function will override all stdio configuration associated with it before invocation.
///
/// *   When shell constructor is not set, attempting to execute any command will result in Lua panic.
///     You can call `execute` without parameters to test whether configuration was provided.
///
///     Note that it is impossible to verify beforehand whether constructing the shell is going to succeed or not,
///     so as long as shell constructor is configured `execute` will claim that shell is available.
///
/// *   See [`OsExecute`] for ways to configure shell constructor.
pub fn execute<Ty>(shell: Option<RootCell<Command>>) -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    let body = move || {
        let shell = shell.clone();
        ffi::delegate::from_mut(move |mut rt| {
            use rt::ffi::arg_parser::{LuaString, Opts, ParseArgs, Split};
            use rt::gc::LuaPtr;
            use rt::value::string::try_gc_to_str;
            use rt::value::Value;
            use std::io::Write;
            use std::process::Stdio;

            let rest: Opts<(LuaString<_>,)> = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();
            let (command,) = rest.split();

            let Some(command) = command else {
                rt.stack.transient().push(Value::Bool(shell.is_some()));
                return Ok(());
            };

            let Some(shell) = &shell else {
                let err = rt.core.alloc_error_msg("shell is not available");
                return Err(err);
            };

            let command = try_gc_to_str(command.0 .0, &rt.core.gc)?.into_owned();

            let shell = rt.core.gc.get_root_mut(shell);

            shell
                .stdin(Stdio::piped())
                .stdout(Stdio::null())
                .stderr(Stdio::null());

            let mut child = match shell.spawn() {
                Ok(t) => t,
                Err(err) => {
                    let err = rt.core.alloc_error_msg(err.to_string());
                    return Err(err);
                }
            };

            let mut stdin = child.stdin.take().unwrap();
            if let Err(err) = write!(&mut stdin, "{}", command) {
                let msg = if let Err(kill_err) = child.kill() {
                    format!("{err}; {kill_err}")
                } else {
                    format!("{err}")
                };

                let err = rt.core.alloc_error_msg(msg);
                return Err(err);
            }

            let exit_status = match child.wait() {
                Ok(t) => t,
                Err(err) => {
                    let err = rt.core.alloc_error_msg(err.to_string());
                    return Err(err);
                }
            };

            let status = if exit_status.success() {
                Value::Bool(true)
            } else {
                Value::Nil
            };
            let (tag, code) = match exit_status.code() {
                Some(code) => {
                    let tag = rt.core.gc.intern("exit".into());
                    let tag = Value::String(LuaPtr(tag.downgrade()));

                    let code = Value::Int(code.into());

                    (tag, code)
                }
                None => {
                    #[allow(unused_mut)]
                    let mut signal: Option<i32> = None;
                    #[cfg(unix)]
                    {
                        use std::os::unix::process::ExitStatusExt;
                        signal = exit_status.signal();
                    }

                    // This panic *should* be unreachable.
                    // According to docs only Unix targets may return None as exit code and only when child was signaled.
                    let signal = signal.expect("failed to extract signal code");

                    let tag = rt.core.gc.intern("signal".into());
                    let tag = Value::String(LuaPtr(tag.downgrade()));

                    let code = Value::Int(signal.into());

                    (tag, code)
                }
            };

            rt.stack.transient_in(&mut rt.core.gc, |mut stack, _| {
                stack.push(status);
                stack.push(tag);
                stack.push(code);
            });

            Ok(())
        })
    };

    ffi::from_fn_mut(body, "lua_std::os::execute", ())
}

/// Terminate host program
///
/// # From Lua documentation
///
/// **Signature:**
/// * `([code: bool | int, [close: bool]]) -> !`
///
/// Calls the ISO C function `exit` to terminate the host program.
/// If code is `true`, the returned status is `EXIT_SUCCESS`; if code is `false`, the returned status is `EXIT_FAILURE`;
/// if code is a number, the returned status is this number.
/// The default value for code is `true`.
///
/// If the optional second argument close is `true`, the function closes the Lua state before exiting (see `lua_close`).
///
/// # Implementation-specific details
///
/// *   This function will invoke Rust's [`exit`](std::process::exit) function, see documentation for caveats.
/// *   When `code` is boolean, process will terminate with canonical error codes for success/failure respectively.
///     On Rust side those are provided by [`ExitCode::SUCCESS`](std::process::ExitCode::SUCCESS) and [`ExitCode::FAILURE`](std::process::ExitCode::FAILURE).
/// *   Rust uses `i32` to represent exit codes.
///     Unrepresentable integers will be replaced with `ExitCode::FAILURE`.
pub fn exit<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    use rt::ffi::arg_parser::ParseAtom;
    use rt::gc::Heap;
    use rt::value::{Type, WeakValue};
    use std::fmt::Display;

    enum BoolOrInt {
        Bool(bool),
        Int(i64),
    }

    #[derive(Debug)]
    struct Error(Type);

    impl Display for Error {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "expected bool or int, found {}", self.0)
        }
    }

    impl<Ty> ParseAtom<WeakValue<Ty>, Heap<Ty>> for BoolOrInt
    where
        Ty: Types,
    {
        type Error = Error;

        fn parse_atom(value: WeakValue<Ty>, _: &mut Heap<Ty>) -> Result<Self, Self::Error> {
            match value {
                WeakValue::Bool(t) => Ok(BoolOrInt::Bool(t)),
                WeakValue::Int(t) => Ok(BoolOrInt::Int(t)),
                value => Err(Error(value.type_())),
            }
        }
    }

    let body = || {
        ffi::delegate::from_mut(|mut rt| {
            use rt::ffi::arg_parser::{Boolean, Opts, ParseArgs, Split};
            use std::process::ExitCode;

            let rest: Opts<(BoolOrInt, Boolean)> = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();
            let (code, close) = rest.split();
            let code = code.unwrap_or(BoolOrInt::Bool(true));
            let _close = close.map(|t| t.0).unwrap_or(false);

            debug_assert_eq!(ExitCode::SUCCESS, ExitCode::from(0));
            debug_assert_eq!(ExitCode::FAILURE, ExitCode::from(1));

            let code = match code {
                BoolOrInt::Bool(true) => 0,
                BoolOrInt::Bool(false) => 1,
                BoolOrInt::Int(n) => n.try_into().unwrap_or(1),
            };

            std::process::exit(code)
        })
    };

    ffi::from_fn(body, "lua_std::os::exit", ())
}

/// Read value of environment variable.
///
/// # From Lua documentation
///
/// **Signature:**
/// * (varname: string) -> string | fail
///
/// Returns the value of the process environment variable `varname` or **fail** if the variable is not defined.
///
/// # Implementation-specific behavior
///
/// *   Environment variable is expected to contain valid utf8.
///     This function will return **fail** if that doesn't hold.
pub fn getenv<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    let body = || {
        ffi::delegate::from_mut(|mut rt| {
            use rt::ffi::arg_parser::{LuaString, ParseArgs};
            use rt::gc::LuaPtr;
            use rt::value::string::try_gc_to_str;
            use rt::value::Value;

            let name: LuaString<_> = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();

            let name = try_gc_to_str(name.0 .0, &rt.core.gc)?;

            let Ok(value) = std::env::var(name.as_ref()) else {
                rt.stack.transient().push(Value::Nil);
                return Ok(());
            };

            let value = rt.core.gc.intern(value.into());
            let value = Value::String(LuaPtr(value.downgrade()));

            rt.stack.synchronized(&mut rt.core.gc).push(value);
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::os::getenv", ())
}

/// Delete file or directory.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(filename: string) -> bool`
/// * `(filename: string) -> (fail, string, nil | int)`
///
/// Deletes the file (or empty directory, on POSIX systems) with the given name.
/// If this function fails, it returns **fail** plus a string describing the error and the error code.
/// Otherwise, it returns `true`.
///
/// # Implementation-specific behavior
///
/// *   It is valid to target empty directories on all platforms.
/// *   On failure this will attempt to recover OS-specific error code to provide in last return.
///     This may fail and produce no value.
pub fn remove<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
    PathBuf: ParseFrom<Ty::String>,
    <PathBuf as ParseFrom<Ty::String>>::Error: Display,
{
    let body = || {
        ffi::delegate::from_mut(|mut rt| {
            use rt::ffi::arg_parser::{FromLuaString, ParseArgs};
            use rt::gc::LuaPtr;
            use rt::value::Value;
            use std::path::PathBuf;

            let file_name: FromLuaString<PathBuf> = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();
            let file_name = file_name.0;

            let inner = || {
                let metadata = std::fs::metadata(&file_name)?;
                if metadata.is_dir() {
                    std::fs::remove_dir(&file_name)
                } else {
                    std::fs::remove_file(&file_name)
                }
            };

            match inner() {
                Ok(()) => {
                    rt.stack.transient().push(Value::Bool(true));
                    Ok(())
                }
                Err(err) => rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
                    let code = err
                        .raw_os_error()
                        .map(|n| Value::Int(n.into()))
                        .unwrap_or_default();

                    let msg = heap.intern(err.to_string().into());
                    let msg = Value::String(LuaPtr(msg.downgrade()));

                    stack.push(Value::Nil);
                    stack.push(msg);
                    stack.push(code);

                    Ok(())
                }),
            }
        })
    };

    ffi::from_fn(body, "lua_std::os::remove", ())
}
