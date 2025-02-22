/// Operating system facilities
///
/// # From Lua documentation
///
/// This library is implemented through table `os`.
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
