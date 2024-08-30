use std::error::Error;
use std::fmt::Display;
use std::path::PathBuf;

use gc::{RootCell, Trace};
use repr::chunk::{ChunkExtension, ClosureRecipe, Function};
use repr::literal::Literal;

use rt::chunk_cache::ChunkId;
use rt::error::RuntimeError;
use rt::ffi::{LuaFfi, LuaFfiOnce, LuaFfiPtr};
use rt::gc::{DisplayWith, Heap, LuaPtr, TryIntoWithGc};
use rt::runtime::Closure;
use rt::runtime::RuntimeView;
use rt::value::{
    Callable, CoreTypes, KeyValue, LuaString, StrongValue, TableIndex, Value, Weak, WeakValue,
};
use rt::value_builder::{ChunkRange, Part, ValueBuilder};

type RootTable<Ty> = RootCell<<Ty as CoreTypes>::Table>;

pub fn empty<Ty>() -> ValueBuilder<
    impl for<'rt> FnOnce(
        RuntimeView<'rt, Ty>,
        ChunkId,
        (),
    ) -> Result<RootTable<Ty>, RuntimeError<StrongValue<Ty>>>,
>
where
    Ty: CoreTypes,
    StrongValue<Ty>: Clone,
    Ty::Table: Default + Trace,
{
    use rt::value_builder;

    let chunk_part = Part {
        chunk_ext: ChunkExtension::empty(),
        builder: |mut rt: RuntimeView<'_, Ty>, _, _| -> Result<RootTable<Ty>, _> {
            rt.stack.clear();

            let value = rt.core.gc.alloc_cell(Default::default());

            Ok(value)
        },
    };

    value_builder::builder().include(chunk_part)
}

pub fn assert<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Ty>,
        ChunkRange,
        RootTable<Ty>,
    ) -> Result<RootTable<Ty>, RuntimeError<StrongValue<Ty>>>,
>
where
    Ty: CoreTypes,
    Ty::RustClosure: From<LuaFfiPtr<Ty>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_assert = crate::impl_::assert();
        let key = rt.core.alloc_string("assert".into());
        let callback = rt.core.gc.alloc_cell(fn_assert.into());

        rt.core.gc[&value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn pcall<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Ty>,
        ChunkRange,
        RootTable<Ty>,
    ) -> Result<RootTable<Ty>, RuntimeError<StrongValue<Ty>>>,
>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: LuaFfi<Ty> + From<LuaFfiPtr<Ty>>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_pcall = crate::impl_::pcall();
        let key = rt.core.alloc_string("pcall".into());
        let callback = rt.core.gc.alloc_cell(fn_pcall.into());

        rt.core.gc[&value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn print<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Ty>,
        ChunkRange,
        RootTable<Ty>,
    ) -> Result<RootTable<Ty>, RuntimeError<StrongValue<Ty>>>,
>
where
    Ty: CoreTypes,
    Ty::String: TryInto<String> + From<&'static str>,
    Ty::Table: Trace + TableIndex<Weak, Ty>,
    Ty::RustClosure: From<LuaFfiPtr<Ty>>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_print = crate::impl_::print();
        let key = rt.core.alloc_string("print".into());
        let callback = rt.core.gc.alloc_cell(fn_print.into());

        rt.core.gc[&value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn load<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Ty>,
        ChunkRange,
        RootTable<Ty>,
    ) -> Result<RootTable<Ty>, RuntimeError<StrongValue<Ty>>>,
>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::String: AsRef<[u8]> + From<&'static str> + Display,
    Ty::RustClosure: LuaFfiOnce<Ty> + From<LuaFfiPtr<Ty>>,
    Ty::Table: Trace + TableIndex<Weak, Ty>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>> + TryIntoWithGc<LuaString<String>, Heap<Ty>>,
    <WeakValue<Ty> as TryIntoWithGc<LuaString<String>, Heap<Ty>>>::Error: Error,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_load = crate::impl_::load();
        let key = rt.core.alloc_string("load".into());
        let callback = rt.core.gc.alloc_cell(fn_load.into());

        rt.core.gc[&value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn loadfile<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Ty>,
        ChunkRange,
        RootTable<Ty>,
    ) -> Result<RootTable<Ty>, RuntimeError<StrongValue<Ty>>>,
>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::String: TryInto<String> + Display,
    Ty::RustClosure: LuaFfiOnce<Ty> + From<LuaFfiPtr<Ty>>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>> + TryIntoWithGc<LuaString<PathBuf>, Heap<Ty>>,
    <WeakValue<Ty> as TryIntoWithGc<LuaString<PathBuf>, Heap<Ty>>>::Error: Error,
    StrongValue<Ty>: DisplayWith<Heap<Ty>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let fn_loadfile = crate::impl_::loadfile();
        let key = rt.core.alloc_string("loadfile".into());
        let callback = rt.core.gc.alloc_cell(fn_loadfile.into());

        rt.core.gc[&value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn setmetatable<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Ty>,
        ChunkRange,
        RootTable<Ty>,
    ) -> Result<RootTable<Ty>, RuntimeError<StrongValue<Ty>>>,
>
where
    Ty: CoreTypes,
    Ty::RustClosure: From<LuaFfiPtr<Ty>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let f = crate::impl_::setmetatable();
        let key = rt.core.alloc_string("setmetatable".into());
        let callback = rt.core.gc.alloc_cell(f.into());

        rt.core.gc[&value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn getmetatable<Ty>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Ty>,
        ChunkRange,
        RootTable<Ty>,
    ) -> Result<RootTable<Ty>, RuntimeError<StrongValue<Ty>>>,
>
where
    Ty: CoreTypes,
    Ty::RustClosure: From<LuaFfiPtr<Ty>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |rt: RuntimeView<Ty>, _: ChunkRange, value: RootTable<Ty>| {
        let f = crate::impl_::getmetatable();
        let key = rt.core.alloc_string("getmetatable".into());
        let callback = rt.core.gc.alloc_cell(f.into());

        rt.core.gc[&value].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}
