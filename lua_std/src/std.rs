use std::fmt::{Debug, Display};
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
///     * Memory used by internal auxiliary structures is not included.
///
///     Additionally, resulting value is provided on best-effort basis.
///     Memory management have a lot of nuance such as accounting for padding caused by alignment and partition into individual memory allocations.
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

#[expect(non_camel_case_types)]
pub struct load;

impl<Ty> StdPlugin<Ty> for load
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
    Ty::String: AsEncoding + TryInto<String> + Display,
    String: ParseFrom<Ty::String>,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,

    // Temporary bound
    <Ty::String as TryInto<String>>::Error: Debug,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_load = crate::ffi::load();
        let key = core.alloc_string("load".into());
        let callback = core.gc.alloc_cell(boxed(fn_load));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

#[expect(non_camel_case_types)]
pub struct loadfile;

impl<Ty> StdPlugin<Ty> for loadfile
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
    Ty::String: AsEncoding + TryInto<String> + Display,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
    String: ParseFrom<Ty::String>,
    PathBuf: ParseFrom<Ty::String>,

    // Temporary bound
    <Ty::String as TryInto<String>>::Error: Debug,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_loadfile = crate::ffi::loadfile();
        let key = core.alloc_string("loadfile".into());
        let callback = core.gc.alloc_cell(boxed(fn_loadfile));

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

#[expect(non_camel_case_types)]
pub struct getmetatable;

impl<Ty> StdPlugin<Ty> for getmetatable
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let f = crate::ffi::getmetatable();
        let key = core.alloc_string("getmetatable".into());
        let callback = core.gc.alloc_cell(boxed(f));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}
