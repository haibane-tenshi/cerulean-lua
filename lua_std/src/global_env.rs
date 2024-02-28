use std::error::Error;
use std::fmt::Display;
use std::path::PathBuf;

use gc::{Heap, Trace};
use repr::chunk::{ChunkExtension, ClosureRecipe, Function};
use repr::literal::Literal;

use rt::chunk_cache::ChunkId;
use rt::error::RuntimeError;
use rt::ffi::{LuaFfi, LuaFfiOnce};
use rt::gc::{StringRef, TryIntoWithGc};
use rt::runtime::RuntimeView;
use rt::value::{
    Callable, CoreTypes, KeyValue, LuaString, Strong, StrongValue, TableIndex, Types, Value, Weak,
    WeakValue,
};
use rt::value_builder::{ChunkRange, Part, ValueBuilder};

type RootTable<Ty> = <Strong<Ty> as Types>::Table;

pub fn empty<Ty>() -> ValueBuilder<
    impl for<'rt> FnOnce(RuntimeView<'rt, Ty>, ChunkId, ()) -> Result<RootTable<Ty>, RuntimeError<Ty>>,
>
where
    Ty: CoreTypes,
    StrongValue<Ty>: Clone,
    Ty::Table: Default + Trace,
{
    use rt::value_builder;

    let chunk_part = Part {
        chunk_ext: ChunkExtension::empty(),
        builder: |mut rt: RuntimeView<'_, Ty>, _, _| -> Result<RootTable<Ty>, RuntimeError<Ty>> {
            use rt::gc::RootTable;

            rt.stack.clear();

            let value = rt.core.gc.alloc_cell(Default::default());

            Ok(RootTable(value))
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
        let fn_assert = crate::impl_::assert();
        let key = StringRef::new("assert".into());

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
    Ty::String: AsRef<[u8]> + From<&'static str> + Display,
    Ty::RustClosure: LuaFfi<Ty>,
    Ty::Table: Trace + TableIndex<Weak<Ty>>,
    WeakValue<Ty>: Display,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_pcall = crate::impl_::pcall();
        let key = StringRef::new("pcall".into());

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
    WeakValue<Ty>: Display,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_print = crate::impl_::print();
        let key = StringRef::new("print".into());

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
    Ty::String: AsRef<[u8]> + From<&'static str> + Display,
    Ty::RustClosure: LuaFfiOnce<Ty>,
    Ty::Table: Trace + TableIndex<Weak<Ty>>,
    WeakValue<Ty>: Display + TryIntoWithGc<LuaString<String>, Heap>,
    <WeakValue<Ty> as TryIntoWithGc<LuaString<String>, Heap>>::Error: Error,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_load = crate::impl_::load();
        let key = StringRef::new("load".into());

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
    Ty::String: TryInto<String> + AsRef<[u8]> + From<&'static str> + Display,
    Ty::RustClosure: LuaFfiOnce<Ty>,
    Ty::Table: Trace + TableIndex<Weak<Ty>>,
    WeakValue<Ty>: Display + TryIntoWithGc<LuaString<PathBuf>, Heap>,
    <WeakValue<Ty> as TryIntoWithGc<LuaString<PathBuf>, Heap>>::Error: Error,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_loadfile = crate::impl_::loadfile();
        let key = StringRef::new("loadfile".into());

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
        let f = crate::impl_::setmetatable();
        let key = StringRef::new("setmetatable".into());

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
        let f = crate::impl_::getmetatable();
        let key = StringRef::new("getmetatable".into());

        rt.core.gc[&value].set(
            KeyValue::String(key),
            Value::Function(Callable::Rust(f.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}
