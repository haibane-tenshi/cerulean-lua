pub mod math;
pub mod os;
pub mod table;

use std::fmt::Display;
use std::path::PathBuf;

use rt::ffi::arg_parser::ParseFrom;
use rt::ffi::{boxed, DLuaFfi};
use rt::gc::LuaPtr;
use rt::runtime::{Closure, Core};
use rt::value::{Callable, KeyValue, TableIndex, Types, Value};

use crate::traits::{RootTable, TableEntry};

/// Runtime assertion.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(v: any [, message: any]) -> ()`
///
/// Raises an error if the value of its argument `v` is false (i.e., `nil` or `false`); otherwise, returns all its arguments.
/// In case of error, `message` is the error object; when absent, it defaults to "assertion failed!"
#[expect(non_camel_case_types)]
pub struct assert;

impl<Ty> TableEntry<Ty> for assert
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::assert();
        let key = core.alloc_string("assert".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Trigger runtime error.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(message: any [, level: int]) -> !`
///
/// Raises an error (see §2.3) with message as the error object.
/// This function never returns.
///
/// Usually, error adds some information about the error position at the beginning of the message, if the message is a string.
/// The level argument specifies how to get the error position.
/// With level 1 (the default), the error position is where the error function was called.
/// Level 2 points the error to where the function that called error was called; and so on.
/// Passing a level 0 avoids the addition of error position information to the message.
///
/// # Implementation-specific behavior
///
/// Levels are currently unsupported and ignored.
#[expect(non_camel_case_types)]
pub struct error;

impl<Ty> TableEntry<Ty> for error
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::error();
        let key = core.alloc_string("error".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
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
#[expect(non_camel_case_types)]
pub struct collectgarbage;

impl<Ty> TableEntry<Ty> for collectgarbage
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::collectgarbage();
        let key = core.alloc_string("collectgarbage".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
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
#[expect(non_camel_case_types)]
pub struct dofile;

impl<Ty> TableEntry<Ty> for dofile
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
    PathBuf: ParseFrom<Ty::String>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::dofile();
        let key = core.alloc_string("dofile".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Global variable holding global env.
///
/// # From Lua documentation
///
/// A global variable (not a function) that holds the global environment (see §2.2).
/// Lua itself does not use this variable; changing its value does not affect any environment, nor vice versa.
pub struct _G;

impl<Ty> TableEntry<Ty> for _G
where
    Ty: Types,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let key = core.alloc_string("_G".into());

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Table(LuaPtr(value.downgrade())),
        );
    }
}

/// Global variable containing Lua version as a string.
///
/// # From Lua documentation
///
/// A global variable (not a function) that holds a string containing the running Lua version.
/// The current value of this variable is "Lua 5.4".
///
/// # Implementation-specific behavior
///
/// Variable will always have string "Lua 5.4", currently this is the only Lua version supported by runtime.
pub struct _VERSION;

impl<Ty> TableEntry<Ty> for _VERSION
where
    Ty: Types,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let key = core.alloc_string("_VERSION".into());
        let version = core.alloc_string("Lua 5.4".into());

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::String(LuaPtr(version.downgrade())),
        );
    }
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
#[expect(non_camel_case_types)]
pub struct ipairs;

impl<Ty> TableEntry<Ty> for ipairs
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::ipairs();
        let key = core.alloc_string("ipairs".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
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
///
/// # Implementation-specific behavior
///
/// Note that in our evaluation model `pcall` doesn't do anything special.
/// During unwinding every rust-backed frame receives runtime error to process in order,
/// `pcall` simply doesn't propagate it which indicates that Lua panic was handled.
/// See [delegate contract](rt::ffi::delegate#error-handling) for more details.
#[expect(non_camel_case_types)]
pub struct pcall;

impl<Ty> TableEntry<Ty> for pcall
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::pcall();
        let key = core.alloc_string("pcall".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Call another function in protected mode, setting a message handler.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(f: any, msgh: function, [args: any...]) -> any...`
///
/// This function is similar to pcall, except that it sets a new message handler msgh.
///
/// # Implementation-specific behavior
///
/// Currently, message handlers are not supported, so this function is equivalent to `pcall`.
#[expect(non_camel_case_types)]
pub struct xpcall;

impl<Ty> TableEntry<Ty> for xpcall
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::xpcall();
        let key = core.alloc_string("xpcall".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
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
#[expect(non_camel_case_types)]
pub struct load;

impl<Ty> TableEntry<Ty> for load
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
    String: ParseFrom<Ty::String>,

    // Temp bound, remove when we are done with unpin.
    Ty::String: Unpin,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::load();
        let key = core.alloc_string("load".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
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
#[expect(non_camel_case_types)]
pub struct loadfile;

impl<Ty> TableEntry<Ty> for loadfile
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
    PathBuf: ParseFrom<Ty::String>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::loadfile();
        let key = core.alloc_string("loadfile".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
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
#[expect(non_camel_case_types)]
pub struct next;

impl<Ty> TableEntry<Ty> for next
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::next();
        let key = core.alloc_string("next".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
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
/// See function `next` for the caveats of modifying the table during its traversal.
///
/// # Implementation-specific behavior
///
/// After calling `__pairs` metamethod stack will be forcefully adjusted to 3 elements, padding with `nil` if necessary.
#[expect(non_camel_case_types)]
pub struct pairs;

impl<Ty> TableEntry<Ty> for pairs
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::pairs();
        let key = core.alloc_string("pairs".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Set metatable on table.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(table: table, metatable: nil | table) -> table`
///
/// Sets the metatable for the given table.
/// If `metatable` is `nil`, removes the metatable of the given table.
/// If the original metatable has a `__metatable` field, raises an error.
///
/// This function returns `table`.
///
/// To change the metatable of other types from Lua code, you must use the debug library (§6.10).
#[expect(non_camel_case_types)]
pub struct setmetatable;

impl<Ty> TableEntry<Ty> for setmetatable
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::setmetatable();
        let key = core.alloc_string("setmetatable".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
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
#[expect(non_camel_case_types)]
pub struct getmetatable;

impl<Ty> TableEntry<Ty> for getmetatable
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::getmetatable();
        let key = core.alloc_string("getmetatable".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Print all values
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(_: any...) -> ()`
///
/// Receives any number of arguments and prints their values to `stdout`, converting each argument to a string following the same rules of `tostring`.
///
/// The function `print` is not intended for formatted output, but only as a quick way to show a value, for instance for debugging.
/// For complete control over the output, use `string.format` and `io.write`.
///
/// # Implementation-specific behavior
///
/// If `__tostring` metamethod returns something but a string it will be printed raw, without recursively invoking `tostring` on it.
#[expect(non_camel_case_types)]
pub struct print;

impl<Ty> TableEntry<Ty> for print
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::print();
        let key = core.alloc_string("print".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
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
#[expect(non_camel_case_types)]
pub struct tostring;

impl<Ty> TableEntry<Ty> for tostring
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::tostring();
        let key = core.alloc_string("tostring".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Convert value to number
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(e: any,) -> int | float | fail`
/// * `(e: string, base: int) -> int | fail`
///
/// When called with no `base`, `tonumber` tries to convert its argument to a number.
/// If the argument is already a number or a string convertible to a number, then `tonumber` returns this number; otherwise, it returns **fail**.
///
/// The conversion of strings can result in integers or floats, according to the lexical conventions of Lua (see §3.1).
/// The string may have leading and trailing spaces and a sign.
///
/// When called with base, then `e` must be a string to be interpreted as an integer numeral in that base.
/// The base may be any integer between 2 and 36, inclusive.
/// In bases above 10, the letter 'A' (in either upper or lower case) represents 10, 'B' represents 11, and so forth, with 'Z' representing 35.
/// If the string `e` is not a valid numeral in the given base, the function returns fail.
#[expect(non_camel_case_types)]
pub struct tonumber;

impl<Ty> TableEntry<Ty> for tonumber
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    String: ParseFrom<Ty::String>,
    <String as ParseFrom<Ty::String>>::Error: Display,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::tonumber();
        let key = core.alloc_string("tonumber".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
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
#[expect(non_camel_case_types)]
pub struct rawequal;

impl<Ty> TableEntry<Ty> for rawequal
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::rawequal();
        let key = core.alloc_string("rawequal".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
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
#[expect(non_camel_case_types)]
pub struct rawlen;

impl<Ty> TableEntry<Ty> for rawlen
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::rawlen();
        let key = core.alloc_string("rawlen".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Get value directly out of table.
///
/// # From Lua documentation
///
/// **Singature:**
/// * `(table: table, index: any) -> any`
///
/// Gets the real value of `table[index]`, without using the `__index` metavalue.
/// `table` must be a table; `index` may be any value.
///
/// # Implementation-specific behavior
///
/// * The lookup is still subject to usual rules about table indices,
///     `nil` and NaN are not permitted and will cause Lua panic.
/// * This function will never perform index coercions.
///     In particular floats containing exact integer values will not get coerced.
///     This is of importance because the runtime (and consequently tables) considers ints and floats to be distinct.
#[expect(non_camel_case_types)]
pub struct rawget;

impl<Ty> TableEntry<Ty> for rawget
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::rawget();
        let key = core.alloc_string("rawget".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Set value directly into of table.
///
/// # From Lua documentation
///
/// **Singature:**
/// * `(table: table, index: any, value: any) -> table`
///
/// Sets the real value of `table[index]` to `value`, without using the `__newindex` metavalue.
/// `table` must be a table, `index` any value different from `nil` and `NaN`, and `value` any Lua value.
///
/// This function returns `table`.
///
/// # Implementation-specific behavior
///
/// * The lookup is still subject to usual rules about table indices,
///     `nil` and `NaN` are not permitted and will cause Lua panic.
/// * This function will never perform index coercions.
///     In particular floats containing exact integer values will not get coerced.
///     This is of importance because the runtime (and consequently tables) considers ints and floats to be distinct.
#[expect(non_camel_case_types)]
pub struct rawset;

impl<Ty> TableEntry<Ty> for rawset
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::rawset();
        let key = core.alloc_string("rawset".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Select argument out of arg list or their total count.
///
/// # From Lua documentation
///
/// **Singature:**
/// * `(index: int | string, args: any...) -> any`
///
/// If `index` is a number, returns all arguments after argument number `index`;
/// a negative number indexes from the end (-1 is the last argument).
/// Otherwise, `index` must be the string "#", and `select` returns the total number of extra arguments it received.
///
/// # Implementation-specific behavior
///
/// When `index` is integer it is treated as offset into argument list (from the beginning when non-negative, from the end when negative).
/// Offsetting outside of `[0; len]` range (where `len` is total number of args) will result in Lua panic.
///
/// Note that Lua customarily start enumeration from 1, not 0 how Rust does it.
/// For example, Lua considers `index` an argument with index 1,
/// so `select(1, ...)` will choose all arguments excluding the index itself.
/// This behavior coincides with notion of Rustic offsets for the purposes of this function.
#[expect(non_camel_case_types)]
pub struct select;

impl<Ty> TableEntry<Ty> for select
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::select();
        let key = core.alloc_string("select".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Produce string with name of value's type.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(v: any) -> string`
///
/// Returns the type of its only argument, coded as a string.
/// The possible results of this function are "nil" (a string, not the value `nil`),
/// "number", "string", "boolean", "table", "function", "thread", and "userdata".
#[expect(non_camel_case_types)]
pub struct type_;

impl<Ty> TableEntry<Ty> for type_
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::type_();
        let key = core.alloc_string("type".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Emit a warning message.
///
/// # From Lua documentation
///
///
/// **Signature:**
/// * `(msg1: string, _: string...) -> ()`
///
/// Emits a warning with a message composed by the concatenation of all its arguments (which should be strings).
///
/// By convention, a one-piece message starting with '@' is intended to be a control message,
/// which is a message to the warning system itself.
/// In particular, the standard warning function in Lua recognizes the control messages "@off", to stop the emission of warnings,
/// and "@on", to (re)start the emission; it ignores unknown control messages.
///
/// # Implementation-specific behavior
///
/// * Output will be written into `stderr`.
/// * Arguments are required to be text, Lua strings containing binary data will cause runtime error.
/// *   Currently runtime contains no specific state related to warning system.
///     Any control messages (including `@on` and `@off`) will be ignored.
#[expect(non_camel_case_types)]
pub struct warn;

impl<Ty> TableEntry<Ty> for warn
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::warn();
        let key = core.alloc_string("warn".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}
