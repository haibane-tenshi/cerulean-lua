use std::error::Error;
use std::fmt::{Debug, Display};
use std::path::{Path, PathBuf};

use repr::chunk::{ChunkExtension, ClosureRecipe, Function};
use repr::literal::Literal;

use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::ffi::LuaFfiOnce;
use crate::gc::Gc as GarbageCollector;
use crate::runtime::RuntimeView;
use crate::value::callable::RustClosureRef;
use crate::value::{Callable, KeyValue, LuaString, TableIndex, TypeProvider, Value};
use crate::value_builder::{ChunkRange, Part, ValueBuilder};

use crate::error::RuntimeError;

pub fn empty<Gc, C>() -> ValueBuilder<
    impl for<'rt> FnOnce(RuntimeView<'rt, Gc, C>, ChunkId, ()) -> Result<Gc::Table, RuntimeError<Gc>>,
>
where
    Gc: TypeProvider,
    Value<Gc>: Display,
{
    use crate::value_builder;

    let chunk_part = Part {
        chunk_ext: ChunkExtension::empty(),
        builder: |mut rt: RuntimeView<'_, Gc, C>, _, _| -> Result<Gc::Table, RuntimeError<Gc>> {
            rt.stack.clear();

            let value = Default::default();

            Ok(value)
        },
    };

    value_builder::builder().include(chunk_part)
}

pub fn assert<Gc, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Gc, C>, ChunkRange, Gc::Table) -> Result<Gc::Table, RuntimeError<Gc>>,
>
where
    C: ChunkCache,
    Gc: TypeProvider,
    Gc::RustCallable: From<RustClosureRef<Gc, C>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Gc, C>, _: ChunkRange, mut value: Gc::Table| {
        let fn_assert = RustClosureRef::new(crate::lua_std::impl_::assert());

        value.set(
            KeyValue::String("assert".into()),
            Value::Function(Callable::Rust(fn_assert.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn pcall<Gc, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Gc, C>, ChunkRange, Gc::Table) -> Result<Gc::Table, RuntimeError<Gc>>,
>
where
    C: ChunkCache,
    Gc: GarbageCollector,
    Gc::String: TryInto<String>,
    Gc::RustCallable: From<RustClosureRef<Gc, C>> + crate::ffi::LuaFfiOnce<Gc, C>,
    Value<Gc>: Debug + Display,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Gc, C>, _: ChunkRange, mut value: Gc::Table| {
        let fn_pcall = RustClosureRef::new(crate::lua_std::impl_::pcall());

        value.set(
            KeyValue::String("pcall".into()),
            Value::Function(Callable::Rust(fn_pcall.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn print<Gc, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Gc, C>, ChunkRange, Gc::Table) -> Result<Gc::Table, RuntimeError<Gc>>,
>
where
    C: ChunkCache,
    Gc: TypeProvider,
    Gc::String: TryInto<String>,
    Gc::RustCallable: From<RustClosureRef<Gc, C>>,
    Value<Gc>: Display,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Gc, C>, _: ChunkRange, mut value: Gc::Table| {
        let fn_print = RustClosureRef::new(crate::lua_std::impl_::print());

        value.set(
            KeyValue::String("print".into()),
            Value::Function(Callable::Rust(fn_print.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn load<Gc, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Gc, C>, ChunkRange, Gc::Table) -> Result<Gc::Table, RuntimeError<Gc>>,
>
where
    C: ChunkCache,
    Gc: GarbageCollector,
    Gc::String: TryInto<String> + AsRef<[u8]>,
    Gc::RustCallable: From<RustClosureRef<Gc, C>> + LuaFfiOnce<Gc, C>,
    Value<Gc>: Debug + Display + TryInto<LuaString<String>>,
    <Value<Gc> as TryInto<LuaString<String>>>::Error: Error,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Gc, C>, _: ChunkRange, mut value: Gc::Table| {
        let fn_load = RustClosureRef::new(crate::lua_std::impl_::load());

        value.set(
            KeyValue::String("load".into()),
            Value::Function(Callable::Rust(fn_load.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn loadfile<Gc, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Gc, C>, ChunkRange, Gc::Table) -> Result<Gc::Table, RuntimeError<Gc>>,
>
where
    C: ChunkCache + KeyedChunkCache<Path>,
    Gc: GarbageCollector,
    Gc::String: TryInto<String> + AsRef<[u8]>,
    Gc::RustCallable: From<RustClosureRef<Gc, C>> + LuaFfiOnce<Gc, C>,
    Value<Gc>: Debug + Display + TryInto<LuaString<PathBuf>>,
    <Value<Gc> as TryInto<LuaString<PathBuf>>>::Error: Error,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Gc, C>, _: ChunkRange, mut value: Gc::Table| {
        let fn_loadfile = RustClosureRef::new(crate::lua_std::impl_::loadfile());

        value.set(
            KeyValue::String("loadfile".into()),
            Value::Function(Callable::Rust(fn_loadfile.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn setmetatable<Gc, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Gc, C>, ChunkRange, Gc::Table) -> Result<Gc::Table, RuntimeError<Gc>>,
>
where
    Gc: TypeProvider,
    Gc::RustCallable: From<RustClosureRef<Gc, C>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Gc, C>, _: ChunkRange, mut value: Gc::Table| {
        let f = RustClosureRef::new(crate::lua_std::impl_::setmetatable());

        value.set(
            KeyValue::String("setmetatable".into()),
            Value::Function(Callable::Rust(f.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn getmetatable<Gc, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<Gc, C>, ChunkRange, Gc::Table) -> Result<Gc::Table, RuntimeError<Gc>>,
>
where
    Gc: TypeProvider,
    Gc::RustCallable: From<RustClosureRef<Gc, C>>,
    Value<Gc>: Debug + Display,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Gc, C>, _: ChunkRange, mut value: Gc::Table| {
        let f = RustClosureRef::new(crate::lua_std::impl_::getmetatable());

        value.set(
            KeyValue::String("getmetatable".into()),
            Value::Function(Callable::Rust(f.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}
