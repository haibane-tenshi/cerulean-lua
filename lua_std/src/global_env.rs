use std::fmt::Display;
use std::path::PathBuf;

use gc::{RootCell, Trace};

use rt::ffi::{boxed, DLuaFfi};
use rt::gc::{DisplayWith, Heap, LuaPtr};
use rt::runtime::{Closure, Core};
use rt::value::{Callable, KeyValue, StrongValue, TableIndex, Types, Value, Weak, WeakValue};

type RootTable<Ty> = RootCell<<Ty as Types>::Table>;

pub struct Builder<'a, Ty>
where
    Ty: Types,
{
    value: RootTable<Ty>,
    core: &'a mut Core<Ty>,
}

impl<'a, Ty> Builder<'a, Ty>
where
    Ty: Types,
{
    pub fn empty(core: &'a mut Core<Ty>) -> Self {
        let value = core.gc.alloc_cell(Default::default());

        Builder { value, core }
    }

    pub fn include(self, f: impl FnOnce(&RootTable<Ty>, &mut Core<Ty>)) -> Self {
        f(&self.value, self.core);
        self
    }

    pub fn finish(self) -> RootTable<Ty> {
        self.value
    }
}

pub fn assert<Ty>() -> impl FnOnce(&RootTable<Ty>, &mut Core<Ty>)
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    |value, core| {
        let fn_assert = crate::impl_::assert();
        let key = core.alloc_string("assert".into());
        let callback = core.gc.alloc_cell(boxed(fn_assert));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

pub fn pcall<Ty>() -> impl FnOnce(&RootTable<Ty>, &mut Core<Ty>)
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    |value, core| {
        let fn_pcall = crate::impl_::pcall();
        let key = core.alloc_string("pcall".into());
        let callback = core.gc.alloc_cell(boxed(fn_pcall));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

pub fn print<Ty>() -> impl FnOnce(&RootTable<Ty>, &mut Core<Ty>)
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    Ty::String: TryInto<String> + From<&'static str>,
    Ty::Table: Trace + TableIndex<Weak, Ty>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
{
    |value, core| {
        let fn_print = crate::impl_::print();
        let key = core.alloc_string("print".into());
        let callback = core.gc.alloc_cell(boxed(fn_print));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

pub fn load<Ty>() -> impl FnOnce(&RootTable<Ty>, &mut Core<Ty>)
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
    Ty::String: TryInto<String> + AsRef<[u8]> + From<&'static str> + Display,
    Ty::Table: Trace + TableIndex<Weak, Ty>,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    |value, core| {
        let fn_load = crate::impl_::load();
        let key = core.alloc_string("load".into());
        let callback = core.gc.alloc_cell(boxed(fn_load));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

pub fn loadfile<Ty>() -> impl FnOnce(&RootTable<Ty>, &mut Core<Ty>)
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
    Ty::String: TryInto<String> + TryInto<PathBuf> + Display,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    |value, core| {
        let fn_loadfile = crate::impl_::loadfile();
        let key = core.alloc_string("loadfile".into());
        let callback = core.gc.alloc_cell(boxed(fn_loadfile));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

pub fn setmetatable<Ty>() -> impl FnOnce(&RootTable<Ty>, &mut Core<Ty>)
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    |value, core| {
        let f = crate::impl_::setmetatable();
        let key = core.alloc_string("setmetatable".into());
        let callback = core.gc.alloc_cell(boxed(f));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

pub fn getmetatable<Ty>() -> impl FnOnce(&RootTable<Ty>, &mut Core<Ty>)
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    |value, core| {
        let f = crate::impl_::getmetatable();
        let key = core.alloc_string("getmetatable".into());
        let callback = core.gc.alloc_cell(boxed(f));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}
