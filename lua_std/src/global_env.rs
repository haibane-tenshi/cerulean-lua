use std::fmt::Display;
use std::path::PathBuf;

use gc::RootCell;

use rt::ffi::{boxed, DLuaFfi};
use rt::gc::{DisplayWith, Heap, LuaPtr};
use rt::plugin::Plugin;
use rt::runtime::{Closure, Core};
use rt::value::{Callable, KeyValue, StrongValue, TableIndex, Types, Value, WeakValue};

type RootTable<Ty> = RootCell<<Ty as Types>::Table>;

pub trait StdPlugin<Ty>
where
    Ty: Types,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>);
}

impl<Ty> StdPlugin<Ty> for ()
where
    Ty: Types,
{
    fn build(self, _value: &RootTable<Ty>, _core: &mut Core<Ty>) {}
}

impl<Ty, A, B> StdPlugin<Ty> for (A, B)
where
    Ty: Types,
    A: StdPlugin<Ty>,
    B: StdPlugin<Ty>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let (a, b) = self;
        a.build(value, core);
        b.build(value, core);
    }
}

pub struct Std<P>(P);

impl Std<()> {
    pub fn empty() -> Self {
        Std(())
    }
}

impl<P> Std<P> {
    pub fn with<T>(self, part: T) -> Std<(P, T)> {
        let Std(p) = self;
        Std((p, part))
    }
}

impl<Ty, C, T> Plugin<Ty, C> for Std<T>
where
    Ty: Types,
    T: StdPlugin<Ty>,
{
    fn build(self, rt: &mut rt::runtime::Runtime<Ty, C>) {
        let value = rt.core.gc.alloc_cell(Default::default());
        let Std(builder) = self;
        builder.build(&value, &mut rt.core);

        rt.core.global_env = Value::Table(LuaPtr(value));
    }
}

#[expect(non_camel_case_types)]
pub struct assert;

impl<Ty> StdPlugin<Ty> for assert
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_assert = crate::impl_::assert();
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
        let fn_pcall = crate::impl_::pcall();
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
        let fn_print = crate::impl_::print();
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
        let fn_load = crate::impl_::load();
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
        let fn_loadfile = crate::impl_::loadfile();
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
        let f = crate::impl_::setmetatable();
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
        let f = crate::impl_::getmetatable();
        let key = core.alloc_string("getmetatable".into());
        let callback = core.gc.alloc_cell(boxed(f));

        core.gc[value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}
