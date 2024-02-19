use std::error::Error;
use std::fmt::Display;
use std::path::PathBuf;

use gc::{Heap, Trace};
use repr::chunk::{ChunkExtension, ClosureRecipe, Function};
use repr::literal::Literal;

use crate::chunk_cache::ChunkId;
use crate::ffi::{LuaFfiFnPtr, LuaFfiOnce};
use crate::gc::TryIntoWithGc;
use crate::runtime::RuntimeView;
use crate::value::{
    Callable, CoreTypes, KeyValue, LuaString, RootValue, Strong, TableIndex, Types, Value, Weak,
};
use crate::value_builder::{ChunkRange, Part, ValueBuilder};

use crate::error::RuntimeError;

type RootTable<Ty> = <Strong<Ty> as Types>::Table;

pub fn empty<Ty>() -> ValueBuilder<
    impl for<'rt> FnOnce(RuntimeView<'rt, Ty>, ChunkId, ()) -> Result<RootTable<Ty>, RuntimeError<Ty>>,
>
where
    Ty: CoreTypes,
    RootValue<Ty>: Clone,
    Ty::Table: Default + Trace,
{
    use crate::value_builder;

    let chunk_part = Part {
        chunk_ext: ChunkExtension::empty(),
        builder: |mut rt: RuntimeView<'_, Ty>, _, _| -> Result<RootTable<Ty>, RuntimeError<Ty>> {
            use crate::gc::RootOrd;

            rt.stack.clear();

            let value = rt.core.gc.alloc(Default::default());

            Ok(RootOrd(value))
        },
    };

    value_builder::builder().include(chunk_part)
}

pub fn assert<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Ty>, ChunkRange, RootTable<Ty>) -> Result<RootTable<Ty>, RuntimeError<Ty>>,
>
where
    Ty: CoreTypes,
    Ty::String: From<&'static str>,
    Ty::Table: Trace + TableIndex<Weak<Ty>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_assert = crate::lua_std::impl_::assert();
        let key = std::rc::Rc::new("assert".into());

        rt.core.gc[&value].set(
            KeyValue::String(key),
            Value::Function(Callable::Rust(fn_assert.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn pcall<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Ty>, ChunkRange, RootTable<Ty>) -> Result<RootTable<Ty>, RuntimeError<Ty>>,
>
where
    Ty: CoreTypes,
    Ty::String: AsRef<[u8]> + From<&'static str>,
    Ty::RustCallable: LuaFfiOnce<Ty>,
    Ty::Table: Trace + TableIndex<Weak<Ty>>,
    RootValue<Ty>: Display,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_pcall = crate::lua_std::impl_::pcall();
        let key = std::rc::Rc::new("pcall".into());

        rt.core.gc[&value].set(
            KeyValue::String(key),
            Value::Function(Callable::Rust(fn_pcall.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn print<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Ty>, ChunkRange, RootTable<Ty>) -> Result<RootTable<Ty>, RuntimeError<Ty>>,
>
where
    Ty: CoreTypes,
    Ty::String: TryInto<String> + From<&'static str>,
    Ty::Table: Trace + TableIndex<Weak<Ty>>,
    RootValue<Ty>: Display,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_print = crate::lua_std::impl_::print();
        let key = std::rc::Rc::new("print".into());

        rt.core.gc[&value].set(
            KeyValue::String(key),
            Value::Function(Callable::Rust(fn_print.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn load<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Ty>, ChunkRange, RootTable<Ty>) -> Result<RootTable<Ty>, RuntimeError<Ty>>,
>
where
    Ty: CoreTypes,
    Ty::String: AsRef<[u8]> + From<&'static str>,
    Ty::RustCallable: From<LuaFfiFnPtr<Ty>> + LuaFfiOnce<Ty>,
    Ty::Table: Trace + TableIndex<Weak<Ty>>,
    RootValue<Ty>: Display + TryIntoWithGc<LuaString<String>, Heap>,
    <RootValue<Ty> as TryIntoWithGc<LuaString<String>, Heap>>::Error: Error,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_load = crate::lua_std::impl_::load();
        let key = std::rc::Rc::new("load".into());

        rt.core.gc[&value].set(
            KeyValue::String(key),
            Value::Function(Callable::Rust(fn_load.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn loadfile<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Ty>, ChunkRange, RootTable<Ty>) -> Result<RootTable<Ty>, RuntimeError<Ty>>,
>
where
    Ty: CoreTypes,
    Ty::String: TryInto<String> + AsRef<[u8]> + From<&'static str>,
    Ty::RustCallable: From<LuaFfiFnPtr<Ty>> + LuaFfiOnce<Ty>,
    Ty::Table: Trace + TableIndex<Weak<Ty>>,
    RootValue<Ty>: Display + TryIntoWithGc<LuaString<PathBuf>, Heap>,
    <RootValue<Ty> as TryIntoWithGc<LuaString<PathBuf>, Heap>>::Error: Error,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_loadfile = crate::lua_std::impl_::loadfile();
        let key = std::rc::Rc::new("loadfile".into());

        rt.core.gc[&value].set(
            KeyValue::String(key),
            Value::Function(Callable::Rust(fn_loadfile.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn setmetatable<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Ty>, ChunkRange, RootTable<Ty>) -> Result<RootTable<Ty>, RuntimeError<Ty>>,
>
where
    Ty: CoreTypes,
    Ty::String: From<&'static str>,
    Ty::Table: Trace + TableIndex<Weak<Ty>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let f = crate::lua_std::impl_::setmetatable();
        let key = std::rc::Rc::new("setmetatable".into());

        rt.core.gc[&value].set(
            KeyValue::String(key),
            Value::Function(Callable::Rust(f.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn getmetatable<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Ty>, ChunkRange, RootTable<Ty>) -> Result<RootTable<Ty>, RuntimeError<Ty>>,
>
where
    Ty: CoreTypes,
    Ty::String: From<&'static str>,
    Ty::Table: Trace + TableIndex<Weak<Ty>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let f = crate::lua_std::impl_::getmetatable();
        let key = std::rc::Rc::new("getmetatable".into());

        rt.core.gc[&value].set(
            KeyValue::String(key),
            Value::Function(Callable::Rust(f.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}
