use std::fmt::Display;
use std::path::PathBuf;

use rt::ffi::arg_parser::ParseFrom;
use rt::ffi::{boxed, DLuaFfi};
use rt::gc::{DisplayWith, Heap, LuaPtr};
use rt::runtime::{Closure, Core};
use rt::value::string::AsEncoding;
use rt::value::{Callable, KeyValue, StrongValue, TableIndex, Types, Value, WeakValue};

use crate::plugin::{RootTable, StdPlugin};

/// Runtime assertion.
///
/// # From Lua documentation
///
/// Signature: `(v [, message]) -> ()`
///
/// Raises an error if the value of its argument `v` is false (i.e., `nil` or `false`); otherwise, returns all its arguments.
/// In case of error, `message` is the error object; when absent, it defaults to "assertion failed!"
#[expect(non_camel_case_types)]
pub struct assert;

impl<Ty> StdPlugin<Ty> for assert
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_assert = crate::ffi::assert();
        let key = core.alloc_string("assert".into());
        let callback = core.gc.alloc_cell(boxed(fn_assert));

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

impl<Ty> StdPlugin<Ty> for collectgarbage
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    Ty::String: AsEncoding + TryInto<String>,
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
/// Signature: `([filename: string]) -> [any,...]`
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

impl<Ty> StdPlugin<Ty> for dofile
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

impl<Ty> StdPlugin<Ty> for _G
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

/// Iterate over table's integer indices.
///
/// # From Lua documentation
///
/// Signature: `(t: table,) -> (function, table, int)`
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

impl<Ty> StdPlugin<Ty> for ipairs
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

#[expect(non_camel_case_types)]
pub struct pcall;

impl<Ty> StdPlugin<Ty> for pcall
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
    Ty::String: AsEncoding + TryInto<String>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_pcall = crate::ffi::pcall();
        let key = core.alloc_string("pcall".into());
        let callback = core.gc.alloc_cell(boxed(fn_pcall));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

#[expect(non_camel_case_types)]
pub struct print;

impl<Ty> StdPlugin<Ty> for print
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    Ty::String: TryInto<String> + From<&'static str>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_print = crate::ffi::print();
        let key = core.alloc_string("print".into());
        let callback = core.gc.alloc_cell(boxed(fn_print));

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
/// Signature: `(chunk: string | function [, chunkname: string [, mode: string [, env: any]]]) -> function | (fail, any)`
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

impl<Ty> StdPlugin<Ty> for load
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
    Ty::String: AsEncoding + TryInto<String>,
    <Ty::String as TryInto<String>>::Error: Display,
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
/// Signature: `([filename: string [, mode: string [, env: any]]]) -> function | (fail, any)`
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

impl<Ty> StdPlugin<Ty> for loadfile
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
    Ty::String: AsEncoding + TryInto<String>,
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
/// Signature: `(table: table [, index: any]) -> nil | (any, any)`
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

impl<Ty> StdPlugin<Ty> for next
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
/// Signature:
/// * `(t: any,) -> (any, any, any)`
/// * `(t: table,) -> (function, table, nil)`
///
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
#[expect(non_camel_case_types)]
pub struct pairs;

impl<Ty> StdPlugin<Ty> for pairs
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

#[expect(non_camel_case_types)]
pub struct setmetatable;

impl<Ty> StdPlugin<Ty> for setmetatable
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let f = crate::ffi::setmetatable();
        let key = core.alloc_string("setmetatable".into());
        let callback = core.gc.alloc_cell(boxed(f));

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
/// Signature: `(object: any) -> any`
///
/// If object does not have a metatable, returns `nil`.
/// Otherwise, if the object's metatable has a `__metatable` field, returns the associated value.
/// Otherwise, returns the metatable of the given object.
#[expect(non_camel_case_types)]
pub struct getmetatable;

impl<Ty> StdPlugin<Ty> for getmetatable
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
