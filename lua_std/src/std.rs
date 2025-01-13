use std::fmt::Display;
use std::path::PathBuf;

use rt::ffi::{boxed, DLuaFfi};
use rt::gc::{DisplayWith, Heap, LuaPtr};
use rt::runtime::{Closure, Core};
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

#[expect(non_camel_case_types)]
pub struct pcall;

impl<Ty> StdPlugin<Ty> for pcall
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
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
    Ty::String: TryInto<String> + AsRef<[u8]> + From<&'static str> + Display,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
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
    Ty::String: TryInto<String> + TryInto<PathBuf> + Display,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
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
