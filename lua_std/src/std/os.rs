use gc::RootCell;
use rt::ffi::{boxed, DLuaFfi};
use rt::gc::LuaPtr;
use rt::runtime::Core;
use rt::value::{Callable, KeyValue, TableIndex, Types, Value};
use std::process::Command;

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
        let fn_body = crate::ffi::os::clock();
        let key = core.alloc_string("clock".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
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
        let fn_body = crate::ffi::os::date();
        let key = core.alloc_string("date".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
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
        let fn_body = crate::ffi::os::difftime();
        let key = core.alloc_string("difftime".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
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

                let fn_body = crate::ffi::os::execute(shell);
                let key = core.alloc_string("execute".into());
                let callback = core.gc.alloc_cell(boxed(fn_body));

                core.gc[table].set(
                    KeyValue::String(LuaPtr(key.downgrade())),
                    Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
                );
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
        let fn_body = crate::ffi::os::execute(shell.clone());
        let key = core.alloc_string("execute".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}
