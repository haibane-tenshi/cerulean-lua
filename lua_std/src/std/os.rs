use std::fmt::Display;
use std::path::PathBuf;
use std::process::Command;

use gc::RootCell;
use rt::ffi::arg_parser::ParseFrom;
use rt::ffi::{self, DLuaFfi};
use rt::runtime::Core;
use rt::value::Types;

use crate::lib::set_func;
use crate::traits::{RootTable, TableEntry, TableEntryEx};

/// Return CPU time consumed by the entire process.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `() -> int`
///
/// Returns an approximation of the amount in seconds of CPU time used by the program, as returned by the underlying ISO C function `clock`.
#[expect(non_camel_case_types)]
pub struct clock;

impl<Ty> TableEntry<Ty> for clock
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::os::clock, "lua_std::std::os::clock", ());
        set_func("clock", fn_body).build(table, core);
    }
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
#[expect(non_camel_case_types)]
pub struct date;

impl<Ty> TableEntry<Ty> for date
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::os::date, "lua_std::std::os::date", ());
        set_func("date", fn_body).build(table, core);
    }
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
#[expect(non_camel_case_types)]
pub struct difftime;

impl<Ty> TableEntry<Ty> for difftime
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::os::difftime, "lua_std::std::os::difftime", ());
        set_func("difftime", fn_body).build(table, core);
    }
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
///     You can provide shell constructor either in extra parameter to [`TableEntryEx::build`] or
///     set it directly with [`execute::with_shell`].
///
/// *   When shell constructor is not set, attempting to execute any command will result in Lua panic.
///     You can call `execute` without parameters to test whether configuration was provided.
///
///     Note that it is impossible to verify beforehand whether constructing the shell is going to succeed or not,
///     so as long as shell constructor is configured `execute` will claim that shell is available.
///
/// *   When included in [`OsExecute`] library module it will inherit its shell constructor.
///
///     Default constructor is configured by [`default_os_shell`](crate::lib::default_os_shell) function.
///     Produced shell is OS-specific, so you should consider inputs to this function to be non-portable.
#[expect(non_camel_case_types)]
pub struct execute;

impl execute {
    /// Directly set shell constructor.
    pub fn with_shell<Ty>(shell: Option<RootCell<Command>>) -> impl TableEntry<Ty>
    where
        Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    {
        struct ExecuteWith(Option<RootCell<Command>>);

        impl<Ty> TableEntry<Ty> for ExecuteWith
        where
            Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
        {
            fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
                let ExecuteWith(shell) = self;

                let fn_body = ffi::from_fn(
                    move || crate::ffi::os::execute(shell.clone()),
                    "lua_std::std::os::execute",
                    (),
                );
                set_func("execute", fn_body).build(table, core);
            }
        }

        ExecuteWith(shell)
    }
}

impl<Ty> TableEntryEx<Ty, Option<RootCell<Command>>> for execute
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(
        self,
        table: &RootTable<Ty>,
        core: &mut Core<Ty>,
        shell: &mut Option<RootCell<Command>>,
    ) {
        let shell = shell.clone();
        let fn_body = ffi::from_fn(
            move || crate::ffi::os::execute(shell.clone()),
            "lua_std::std::os::execute",
            (),
        );
        set_func("execute", fn_body).build(table, core);
    }
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
#[expect(non_camel_case_types)]
pub struct exit;

impl<Ty> TableEntry<Ty> for exit
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::os::exit, "lua_std::std::os::exit", ());
        set_func("exit", fn_body).build(table, core);
    }
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
#[expect(non_camel_case_types)]
pub struct getenv;

impl<Ty> TableEntry<Ty> for getenv
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::os::getenv, "lua_std::std::os::getenv", ());
        set_func("getenv", fn_body).build(table, core);
    }
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
#[expect(non_camel_case_types)]
pub struct remove;

impl<Ty> TableEntry<Ty> for remove
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    PathBuf: ParseFrom<Ty::String>,
    <PathBuf as ParseFrom<Ty::String>>::Error: Display,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::os::remove, "lua_std::std::os::remove", ());
        set_func("remove", fn_body).build(table, core);
    }
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
#[expect(non_camel_case_types)]
pub struct rename;

impl<Ty> TableEntry<Ty> for rename
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    PathBuf: ParseFrom<Ty::String>,
    <PathBuf as ParseFrom<Ty::String>>::Error: Display,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::os::rename, "lua_std::std::os::rename", ());
        set_func("rename", fn_body).build(table, core);
    }
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
#[expect(non_camel_case_types)]
pub struct setlocale;

impl<Ty> TableEntry<Ty> for setlocale
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    PathBuf: ParseFrom<Ty::String>,
    <PathBuf as ParseFrom<Ty::String>>::Error: Display,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::os::setlocale, "lua_std::std::os::setlocale", ());
        set_func("setlocale", fn_body).build(table, core);
    }
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
#[expect(non_camel_case_types)]
pub struct time;

impl<Ty> TableEntry<Ty> for time
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::os::time, "lua_std::std::os::time", ());
        set_func("time", fn_body).build(table, core);
    }
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
#[expect(non_camel_case_types)]
pub struct tmpname;

impl<Ty> TableEntry<Ty> for tmpname
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::os::tmpname, "lua_std::std::os::tmpname", ());
        set_func("tmpname", fn_body).build(table, core);
    }
}
