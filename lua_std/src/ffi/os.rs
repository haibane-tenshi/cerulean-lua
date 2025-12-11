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
use rt::ffi::delegate::{self, Delegate};
use rt::value::Types;

/// Return CPU time consumed by the entire process.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `() -> int`
///
/// Returns an approximation of the amount in seconds of CPU time used by the program, as returned by the underlying ISO C function `clock`.
pub fn clock<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
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
pub fn date<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use chrono::{DateTime, Datelike, FixedOffset, Local, Timelike};
        use rt::ffi::arg_parser::{Int, LuaString, Opts, ParseArgs, Split};
        use rt::gc::LuaPtr;
        use rt::value::{Key, TableIndex, Value};

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

        let format = format.map(|t| t.to_str(&rt.core.gc)).transpose()?;
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
pub fn difftime<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
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
pub fn execute<Ty>(shell: Option<RootCell<Command>>) -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(move |mut rt| {
        use rt::ffi::arg_parser::{LuaString, Opts, ParseArgs, Split};
        use rt::gc::LuaPtr;
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

        let command = command.to_str(&rt.core.gc)?.into_owned();

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
        drop(stdin);

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
pub fn exit<Ty>() -> impl Delegate<Ty>
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

    delegate::from_mut(|mut rt| {
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
pub fn getenv<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{LuaString, ParseArgs};
        use rt::gc::LuaPtr;
        use rt::value::Value;

        let name: LuaString<_> = rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();

        let name = name.to_str(&rt.core.gc)?;

        let Ok(value) = std::env::var(name.as_ref()) else {
            rt.stack.transient().push(Value::Nil);
            return Ok(());
        };

        let value = rt.core.gc.intern(value.into());
        let value = Value::String(LuaPtr(value.downgrade()));

        rt.stack.synchronized(&mut rt.core.gc).push(value);
        Ok(())
    })
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
pub fn remove<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
    PathBuf: ParseFrom<Ty::String>,
    <PathBuf as ParseFrom<Ty::String>>::Error: Display,
{
    delegate::from_mut(|mut rt| {
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
}

/// Rename file or directory.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(oldname: string, newname: string) -> bool`
/// * `(oldname: string, newname: string) -> (fail, string, nil | int)`
///
/// # Implementation-specific behavior
///
/// *   On failure this will attempt to recover OS-specific error code to provide in last return.
///     This may fail and produce no value.
pub fn rename<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
    PathBuf: ParseFrom<Ty::String>,
    <PathBuf as ParseFrom<Ty::String>>::Error: Display,
{
    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{FromLuaString, ParseArgs};
        use rt::gc::LuaPtr;
        use rt::value::Value;
        use std::path::PathBuf;

        let [old, new]: [FromLuaString<PathBuf>; 2] = rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();
        let old = old.0;
        let new = new.0;

        match std::fs::rename(&old, &new) {
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
}

/// Set current locale.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(locale: nil | string, [category: string]) -> string | fail`
///
/// Sets the current locale of the program.
/// `locale` is a system-dependent string specifying a locale;
/// category is an optional string describing which category to change: "all", "collate", "ctype", "monetary", "numeric", or "time";
/// the default category is "all".
/// The function returns the name of the new locale, or **fail** if the request cannot be honored.
///
/// If `locale` is the empty string, the current locale is set to an implementation-defined native locale.
/// If `locale` is the string "C", the current locale is set to the standard C locale.
///
/// When called with `nil` as the first argument, this function only returns the name of the current locale for the given category.
///
/// This function may be not thread safe because of its reliance on C function `setlocale`.
///
/// # Implementation-specific behavior
///
/// *   Our implementation of Lua std is independent from C locale, therefore this function does nothing (besides validating arguments).
///     It will always return **fail**.
pub fn setlocale<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{LuaString, Opts, ParseArgs, Split};
        use rt::value::Value;

        let (locale, rest): (LuaString<_>, Opts<(LuaString<_>,)>) =
            rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();
        let (category,) = rest.split();

        let _locale = locale.to_str(&rt.core.gc)?;
        if let Some(category) = category {
            let category = category.to_str(&rt.core.gc)?;
            let is_valid = matches!(
                category.as_ref(),
                "all" | "collate" | "ctype" | "monetary" | "numeric" | "time"
            );

            if !is_valid {
                let err = rt.core.alloc_error_msg("invalid category");
                return Err(err);
            }
        }

        rt.stack.transient().push(Value::Nil);
        Ok(())
    })
}

/// Return current timestamp or convert local time to timestamp.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `() -> int`
/// * `(table: table) -> nil | int`
///
/// Returns the current time when called without arguments, or a time representing the local date and time specified by the given table.
/// This table must have fields `year`, `month`, and `day`,
/// and may have fields `hour` (default is 12), `min` (default is 0), `sec` (default is 0), and `isdst` (default is `nil`).
/// Other fields are ignored. For a description of these fields, see the `os.date` function.
///
/// When the function is called, the values in these fields do not need to be inside their valid ranges.
/// For instance, if `sec` is -10, it means 10 seconds before the time specified by the other fields;
/// if `hour` is 1000, it means 1000 hours after the time specified by the other fields.
///
/// The returned value is a number, whose meaning depends on your system.
/// In POSIX, Windows, and some other systems, this number counts the number of seconds since some given start time (the "epoch").
/// In other systems, the meaning is not specified, and the number returned by time can be used only as an argument to `os.date` and `os.difftime`.
///
/// When called with a table, `os.time` also normalizes all the fields documented in the `os.date` function,
/// so that they represent the same time as before the call but with values inside their valid ranges.
///
/// # Implementation-specific behavior
///
/// *  Operations performed by this function are *regular* that is it may invoke metamethods.
///    This includes getting and setting values on the table.
///    
///    This replicates behavior of vanilla implementation.
///
/// *  The function returns number of seconds since Unix epoch on all supported targets.
///
/// *  Datetimes are expected to be in [proleptic Gregorian calendar](https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar).
///    This functions is capable of processing all possible calendar dates from Jan 1, 262145 BCE to Dec 31, 262143 CE.
///
///    However, you need to be careful of offsetting pitfalls.
///    Because Lua permits arbitrary offsets in *all* fields intermediate values do not necessarily fall into this range.
///    This function may invoke Lua panic if the year or any intermediate result become unrepresentable.
///    
///    Additionally, there are limitations on hour/minute/second offsets as well.
///    Implementation permits at most `i64::MAX` *milliseconds* total (including hours, minutes and seconds) time offset,
///    which can be either positive or negative.
///    This should be more than enough for regular use, however exceeding the limit will cause Lua panic.
///
/// *  Implementation will first attempt to construct suitable datetime and then convert it to local timezone.
///    You should note that the last operation is *fallible*.
///
///    This is because local time is not guaranteed to be contiguous.
///    If the time is shifted backwards there will be a *fold* in time, during which the local time is ambiguous,
///    and if the time is shifted forwards there will be a *gap* in time, where the local time doesn't exist.
///    Commonly this happens due to daylight saving time (DST), but it may happen for other reasons as well.
///
///    If the specified datetime cannot be resolved to a single local datetime this function will produce a `nil`.
///    There is no correct result to be produced in this case, so this function will refuse to produce any.
///
///    Fixing APIs to correctly handle time quirks is outside of purview for this implementation.
///
/// *  This function behaves as if leap seconds don't exist.
///    
///    See documentation in `chrono` library for [brief explanation](chrono::NaiveTime#leap-second-handling).
pub fn time<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    use chrono::{DateTime, Local};
    use std::pin::Pin;

    use rt::error::RuntimeError;
    use rt::ffi::delegate::{Request, Response, RuntimeView, StackSlot, State};
    use rt::gc::Downgrade;
    use rt::value::WeakValue;

    use super::builtins::{GetIndex, SetIndex};

    enum Coro<Ty>
    where
        Ty: Types,
    {
        Started,
        ToSlowGetIndex {
            boundary: StackSlot,
            time: TimeTable,
            comp: ReadComp,
        },
        IterGetIndex {
            co: GetIndex<Ty>,
            time: TimeTable,
            comp: ReadComp,
        },
        ToSlowSetIndex {
            boundary: StackSlot,
            date_time: DateTime<Local>,
            comp: WriteComp,
        },
        IterSetIndex {
            co: SetIndex<Ty>,
            date_time: DateTime<Local>,
            comp: WriteComp,
        },
        Finished,
    }

    #[derive(Debug, Default)]
    struct TimeTable {
        year: i64,
        month: i64,
        day: i64,
        hour: i64,
        min: i64,
        sec: i64,
    }

    impl TimeTable {
        fn set<Ty>(&mut self, comp: ReadComp, value: WeakValue<Ty>) -> Result<(), String>
        where
            Ty: Types,
        {
            use rt::value::Value;
            use ReadComp::*;

            let value = match (comp, value) {
                (_, Value::Int(t)) => t,
                (ReadComp::Hour, Value::Nil) => 12,
                (ReadComp::Min | ReadComp::Sec, Value::Nil) => 0,
                (ReadComp::Year | ReadComp::Month | ReadComp::Day, Value::Nil) => {
                    let err = format!("table must have field \"{}\"", comp.name());
                    return Err(err);
                }
                (_, value) => {
                    let err = format!(
                        "field {} is expected to contain int, found {}",
                        comp.name(),
                        value.type_()
                    );
                    return Err(err);
                }
            };

            let place = match comp {
                Year => &mut self.year,
                Month => &mut self.month,
                Day => &mut self.day,
                Hour => &mut self.hour,
                Min => &mut self.min,
                Sec => &mut self.sec,
            };

            *place = value;
            Ok(())
        }

        fn into_datetime(self) -> Result<DateTime<Local>, Error> {
            use chrono::{Days, LocalResult, Months, NaiveDate, TimeDelta};

            let TimeTable {
                year,
                month,
                day,
                hour,
                min,
                sec,
            } = self;

            // So, much to our headache, Lua effectively permits to have *any* number in any position.
            // We have to build result piecewise using checked ops.
            let year = year.try_into().map_err(|_| Error::OutOfBounds)?;

            // First, we build naive (tz-unaware) date using ymd.
            // This can fail only due to numeric overflows.
            let build_date = || {
                let date = NaiveDate::from_ymd_opt(year, 0, 0)?;

                let date = if let Ok(months) = month.try_into() {
                    date.checked_add_months(Months::new(months))?
                } else if let Ok(months) = month.checked_neg()?.try_into() {
                    date.checked_sub_months(Months::new(months))?
                } else {
                    return None;
                };

                if let Ok(days) = day.try_into() {
                    date.checked_add_days(Days::new(days))
                } else if let Ok(days) = day.checked_neg()?.try_into() {
                    date.checked_sub_days(Days::new(days))
                } else {
                    None
                }
            };
            let date = build_date().ok_or(Error::OutOfBounds)?;

            // Next, build a time delta from hms.
            // This also can fail only due to numeric overflows.
            let build_delta = || {
                let hours = TimeDelta::try_hours(hour)?;
                let minutes = TimeDelta::try_minutes(min)?;
                let seconds = TimeDelta::try_seconds(sec)?;

                hours.checked_add(&minutes)?.checked_add(&seconds)
            };
            let delta = build_delta().ok_or(Error::OutOfBounds)?;

            // Then, try to offset naive date by calculated delta.
            let date_time = date.and_time(Default::default());
            let date_time = date_time
                .checked_add_signed(delta)
                .ok_or(Error::OutOfBounds)?;

            // Finally, attempt to convert it to local tz.
            // This may fail if this time falls into discontinuity.
            match date_time.and_local_timezone(Local) {
                LocalResult::Single(t) => Ok(t),
                LocalResult::None => Err(Error::None),
                LocalResult::Ambiguous(..) => Err(Error::Ambiguous),
            }
        }
    }

    enum Error {
        OutOfBounds,
        Ambiguous,
        None,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum ReadComp {
        Year,
        Month,
        Day,
        Hour,
        Min,
        Sec,
    }

    impl ReadComp {
        fn name(&self) -> &'static str {
            use ReadComp::*;

            match self {
                Year => "year",
                Month => "month",
                Day => "day",
                Hour => "hour",
                Min => "min",
                Sec => "sec",
            }
        }

        fn iter_all() -> impl Iterator<Item = ReadComp> {
            use ReadComp::*;

            [Year, Month, Day, Hour, Min, Sec].into_iter()
        }

        fn iter_after(self) -> impl Iterator<Item = ReadComp> {
            Self::iter_all().skip_while(move |c| *c != self).skip(1)
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum WriteComp {
        Year,
        Month,
        Day,
        Hour,
        Min,
        Sec,
        Wday,
        Yday,
    }

    impl WriteComp {
        fn name(&self) -> &'static str {
            use WriteComp::*;

            match self {
                Year => "year",
                Month => "month",
                Day => "day",
                Hour => "hour",
                Min => "min",
                Sec => "sec",
                Yday => "yday",
                Wday => "wday",
            }
        }

        fn get(&self, date_time: &DateTime<Local>) -> i64 {
            use chrono::{Datelike, Timelike};

            match self {
                WriteComp::Year => date_time.year().into(),
                WriteComp::Month => date_time.month().into(),
                WriteComp::Day => date_time.day().into(),
                WriteComp::Hour => date_time.hour().into(),
                WriteComp::Min => date_time.minute().into(),
                WriteComp::Sec => date_time.second().into(),
                WriteComp::Wday => date_time.weekday().number_from_monday().into(),
                WriteComp::Yday => date_time.ordinal().into(),
            }
        }

        fn iter_all() -> impl Iterator<Item = WriteComp> {
            use WriteComp::*;

            [Year, Month, Day, Hour, Min, Sec, Wday, Yday].into_iter()
        }

        fn iter_after(self) -> impl Iterator<Item = WriteComp> {
            Self::iter_all().skip_while(move |c| *c != self).skip(1)
        }
    }

    impl<Ty> Delegate<Ty> for Coro<Ty>
    where
        Ty: Types,
    {
        fn resume(
            self: Pin<&mut Self>,
            mut rt: RuntimeView<'_, Ty>,
            response: Response<Ty>,
        ) -> State<Request<Ty>, Result<(), RuntimeError<Ty>>> {
            use rt::ffi::delegate::rearrange;

            let this = self.get_mut();
            let current = std::mem::replace(this, Coro::Finished);
            match (current, response) {
                (Coro::Started, Response::Resume) => rearrange(this.started(rt)),
                (
                    Coro::ToSlowGetIndex {
                        boundary,
                        time,
                        comp,
                    },
                    Response::Evaluated(Ok(())),
                ) => {
                    rt.stack.adjust_height(boundary + 1);
                    let value = rt.stack.pop().unwrap();

                    let table = rt.stack[0];

                    rearrange(this.iter_get_index(rt, table, time, comp, value))
                }
                (Coro::IterGetIndex { mut co, time, comp }, response) => {
                    let value = match co.resume(rt.reborrow(), response) {
                        State::Complete(Ok(value)) => value,
                        State::Complete(Err(err)) => return State::Complete(Err(err)),
                        State::Yielded(request) => {
                            *this = Coro::IterGetIndex { co, time, comp };
                            return State::Yielded(request);
                        }
                    };

                    let table = rt.stack[0];

                    rearrange(this.iter_get_index(rt, table, time, comp, value))
                }
                (
                    Coro::ToSlowSetIndex {
                        boundary,
                        date_time,
                        comp,
                    },
                    Response::Evaluated(Ok(())),
                ) => {
                    rt.stack.adjust_height(boundary);

                    let table = rt.stack[0];

                    rearrange(this.iter_set_index(rt, table, date_time, comp))
                }
                (
                    Coro::IterSetIndex {
                        mut co,
                        date_time,
                        comp,
                    },
                    response,
                ) => {
                    match co.resume(rt.reborrow(), response) {
                        State::Complete(Ok(())) => (),
                        State::Complete(Err(err)) => return State::Complete(Err(err)),
                        State::Yielded(request) => {
                            *this = Coro::IterSetIndex {
                                co,
                                date_time,
                                comp,
                            };
                            return State::Yielded(request);
                        }
                    };

                    let table = rt.stack[0];

                    rearrange(this.iter_set_index(rt, table, date_time, comp))
                }
                (
                    Coro::ToSlowGetIndex { .. } | Coro::ToSlowSetIndex { .. },
                    Response::Evaluated(Err(err)),
                ) => State::Complete(Err(err)),
                (Coro::Finished, _) => unreachable!("resumed completed coroutine"),
                _ => unreachable!("invalid runtime response"),
            }
        }
    }

    impl<Ty> Coro<Ty>
    where
        Ty: Types,
    {
        fn started(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
        ) -> Result<State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::builtins::table::GetIndexCache;
            use rt::ffi::arg_parser::{LuaTable, Opts, ParseArgs, Split};
            use rt::gc::AllocExt;
            use rt::value::Value;
            use std::ops::ControlFlow;

            let rest: Opts<(LuaTable<_>,)> = rt.stack.parse(&mut rt.core.gc)?;
            let (table,) = rest.split();

            let Some(table) = table else {
                use std::time::{SystemTime, UNIX_EPOCH};
                let time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
                let secs = time.as_secs();

                let secs = secs.try_into().map_err(|_| {
                    rt.core
                        .gc
                        .alloc_error_msg("timestamp does not fit into integer")
                })?;

                rt.stack.transient().push(Value::Int(secs));
                return Ok(State::Complete(()));
            };

            // Keep table in 0-th slot.

            let mut time = TimeTable::default();
            let getter =
                GetIndexCache::new(table.into(), &rt.core.gc, &rt.core.metatable_registry)?;
            for comp in ReadComp::iter_all() {
                let key = rt.core.gc.alloc_str_key(comp.name());
                let value = match getter.get(key.downgrade(), &rt.core.gc) {
                    Ok(ControlFlow::Break(t)) => t,
                    Ok(ControlFlow::Continue(call)) => {
                        let boundary = rt.stack.top();
                        rt.stack.transient_in(&mut rt.core.gc, |mut stack, _| {
                            stack.push(call.target);
                            stack.push(key.downgrade().into());
                        });

                        let request = Request::Invoke {
                            callable: call.func,
                            start: boundary,
                        };
                        *self = Coro::ToSlowGetIndex {
                            boundary,
                            time,
                            comp,
                        };
                        return Ok(State::Yielded(request));
                    }
                    Err(err) => {
                        let err = rt.core.gc.alloc_error_msg(err.to_string());
                        return Err(err);
                    }
                };

                time.set(comp, value)
                    .map_err(|msg| rt.core.gc.alloc_error_msg(msg))?;
            }

            self.after_extract(rt, table.into(), time)
        }

        fn iter_get_index(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
            table: WeakValue<Ty>,
            mut time: TimeTable,
            comp: ReadComp,
            value: WeakValue<Ty>,
        ) -> Result<State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::gc::AllocExt;

            use super::builtins::get_index;

            time.set(comp, value)
                .map_err(|msg| rt.core.gc.alloc_error_msg(msg))?;

            for comp in comp.iter_after() {
                let key = rt.core.gc.alloc_str_key(comp.name());
                let mut co = get_index(table, key.downgrade());
                let value = match co.resume(rt.reborrow(), Response::Resume) {
                    State::Complete(res) => res?,
                    State::Yielded(request) => {
                        *self = Coro::IterGetIndex { co, time, comp };
                        return Ok(State::Yielded(request));
                    }
                };

                time.set(comp, value)
                    .map_err(|msg| rt.core.gc.alloc_error_msg(msg))?;
            }

            self.after_extract(rt, table, time)
        }

        fn after_extract(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
            table: WeakValue<Ty>,
            time: TimeTable,
        ) -> Result<State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::builtins::table::SetIndexCache;
            use rt::gc::AllocExt;
            use rt::value::Value;
            use std::ops::ControlFlow;

            let date_time = match time.into_datetime() {
                Ok(t) => t,
                Err(Error::OutOfBounds) => {
                    let err = rt
                        .core
                        .gc
                        .alloc_error_msg("specified datetime is out of bounds");
                    return Err(err);
                }
                Err(Error::None | Error::Ambiguous) => {
                    rt.stack.clear();
                    rt.stack.transient().push(Value::Nil);
                    return Ok(State::Complete(()));
                }
            };

            let setter = SetIndexCache::new(table, &rt.core.gc, &rt.core.metatable_registry)?;
            for comp in WriteComp::iter_all() {
                let value = Value::Int(comp.get(&date_time));

                let key = rt.core.gc.alloc_str_key(comp.name());
                match setter.set(key.downgrade(), value, &mut rt.core.gc) {
                    Ok(ControlFlow::Break(())) => (),
                    Ok(ControlFlow::Continue(call)) => {
                        let boundary = rt.stack.top();
                        rt.stack.transient_in(&mut rt.core.gc, |mut stack, _| {
                            stack.push(call.target);
                            stack.push(key.downgrade().into());
                            stack.push(value);
                        });

                        let request = Request::Invoke {
                            callable: call.func,
                            start: boundary,
                        };
                        *self = Coro::ToSlowSetIndex {
                            boundary,
                            date_time,
                            comp,
                        };
                        return Ok(State::Yielded(request));
                    }
                    Err(err) => {
                        let err = rt.core.gc.alloc_error_msg(err.to_string());
                        return Err(err);
                    }
                }
            }

            self.cleanup(rt, date_time)
        }

        fn iter_set_index(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
            table: WeakValue<Ty>,
            date_time: DateTime<Local>,
            comp: WriteComp,
        ) -> Result<State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::gc::AllocExt;
            use rt::value::Value;

            use super::builtins::set_index;

            for comp in comp.iter_after() {
                let value = Value::Int(comp.get(&date_time));
                let key = rt.core.gc.alloc_str_key(comp.name());

                let mut co = set_index(table, key.downgrade(), value);
                match co.resume(rt.reborrow(), Response::Resume) {
                    State::Complete(res) => res?,
                    State::Yielded(request) => {
                        *self = Coro::IterSetIndex {
                            co,
                            date_time,
                            comp,
                        };
                        return Ok(State::Yielded(request));
                    }
                }
            }

            self.cleanup(rt, date_time)
        }

        fn cleanup(
            &mut self,
            mut rt: RuntimeView<'_, Ty>,
            date_time: DateTime<Local>,
        ) -> Result<State<Request<Ty>, ()>, RuntimeError<Ty>> {
            use rt::value::Value;

            let timestamp = date_time.timestamp();

            rt.stack.clear();
            rt.stack.transient().push(Value::Int(timestamp));
            Ok(State::Complete(()))
        }
    }

    Coro::Started
}

/// Generate a named temporary file.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `() -> string`
///
/// Returns a string with a file name that can be used for a temporary file.
/// The file must be explicitly opened before its use and explicitly removed when no longer needed.
///
/// In POSIX systems, this function also creates a file with that name, to avoid security risks.
/// (Someone else might create the file with wrong permissions in the time between getting the name and creating the file.)
/// You still have to open the file to use it and to remove it (even if you do not use it).
///
/// When possible, you may prefer to use `io.tmpfile`, which automatically removes the file when the program ends.
///
/// # Implementation-specific behavior
///
/// *   This function will always create a new empty file.
///
/// *   The file will be created in default temp directory.
///
/// *   The file will not be marked as temporary in filesystem.
///     You are responsible for removing it afterwards.
///
/// *   You should consider accessing the file as **insecure**.
///     As is common when working with filesystem it is a subject to TOCTOU and DoS attacks.
///
///     There are some mitigations in place (randomized file name, file is marked as private),
///     but none of those are perfect.
///     If security is of concern use `io.tmpfile` instead (which should generally be secure) or
///     manually manage files in location private to your program.
pub fn tmpname<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use tempfile::Builder;

        use rt::ffi::arg_parser::ParseArgs;
        use rt::gc::{AllocExt, Downgrade};

        let () = rt.stack.parse(&mut rt.core.gc)?;

        let file = Builder::new()
            .suffix(".tmp")
            .rand_bytes(10)
            .keep(true)
            .tempfile()
            .map_err(|err| rt.core.gc.alloc_error_msg(err.to_string()))?;

        let path = file.into_temp_path();
        let name = path
            .to_str()
            .expect("temporary file name is not valid utf8");

        let file_name = rt.core.gc.alloc_str(name);

        rt.stack.transient_in(&mut rt.core.gc, |mut stack, _| {
            stack.push(file_name.downgrade());
        });

        Ok(())
    })
}
