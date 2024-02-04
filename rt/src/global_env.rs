use std::error::Error;
use std::fmt::{Debug, Display};
use std::path::{Path, PathBuf};

use repr::chunk::{ChunkExtension, ClosureRecipe, Function};
use repr::literal::Literal;

use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::ffi::LuaFfiOnce;
use crate::runtime::RuntimeView;
use crate::value::callable::RustClosureRef;
use crate::value::{Callable, KeyValue, LuaString, TableIndex, TypeProvider, Value};
use crate::value_builder::{ChunkRange, Part, ValueBuilder};

use crate::error::RuntimeError;

pub fn empty<Types, C>() -> ValueBuilder<
    impl for<'rt> FnOnce(
        RuntimeView<'rt, Types, C>,
        ChunkId,
        (),
    ) -> Result<Types::Table, RuntimeError<Types>>,
>
where
    Types: TypeProvider,
    Value<Types>: Display,
{
    use crate::value_builder;

    let chunk_part = Part {
        chunk_ext: ChunkExtension::empty(),
        builder: |mut rt: RuntimeView<'_, Types, C>,
                  _,
                  _|
         -> Result<Types::Table, RuntimeError<Types>> {
            rt.stack.clear();

            let value = Default::default();

            Ok(value)
        },
    };

    value_builder::builder().include(chunk_part)
}

pub fn assert<Types, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Types, C>,
        ChunkRange,
        Types::Table,
    ) -> Result<Types::Table, RuntimeError<Types>>,
>
where
    C: ChunkCache,
    Types: TypeProvider,
    Types::RustCallable: From<RustClosureRef<Types, C>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Types::Table| {
        let fn_assert = RustClosureRef::new(crate::lua_std::impl_::assert());

        value.set(
            KeyValue::String("assert".into()),
            Value::Function(Callable::Rust(fn_assert.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn pcall<Types, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Types, C>,
        ChunkRange,
        Types::Table,
    ) -> Result<Types::Table, RuntimeError<Types>>,
>
where
    C: ChunkCache,
    Types: TypeProvider,
    Types::String: TryInto<String>,
    Types::RustCallable: From<RustClosureRef<Types, C>> + crate::ffi::LuaFfiOnce<Types, C>,
    Value<Types>: Debug + Display,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Types::Table| {
        let fn_pcall = RustClosureRef::new(crate::lua_std::impl_::pcall());

        value.set(
            KeyValue::String("pcall".into()),
            Value::Function(Callable::Rust(fn_pcall.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn print<Types, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Types, C>,
        ChunkRange,
        Types::Table,
    ) -> Result<Types::Table, RuntimeError<Types>>,
>
where
    C: ChunkCache,
    Types: TypeProvider,
    Types::String: TryInto<String>,
    Types::RustCallable: From<RustClosureRef<Types, C>>,
    Value<Types>: Display,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Types::Table| {
        let fn_print = RustClosureRef::new(crate::lua_std::impl_::print());

        value.set(
            KeyValue::String("print".into()),
            Value::Function(Callable::Rust(fn_print.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn load<Types, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Types, C>,
        ChunkRange,
        Types::Table,
    ) -> Result<Types::Table, RuntimeError<Types>>,
>
where
    C: ChunkCache,
    Types: TypeProvider,
    Types::String: TryInto<String> + AsRef<[u8]>,
    Types::RustCallable: From<RustClosureRef<Types, C>> + LuaFfiOnce<Types, C>,
    Value<Types>: Debug + Display + TryInto<LuaString<String>>,
    <Value<Types> as TryInto<LuaString<String>>>::Error: Error,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Types::Table| {
        let fn_load = RustClosureRef::new(crate::lua_std::impl_::load());

        value.set(
            KeyValue::String("load".into()),
            Value::Function(Callable::Rust(fn_load.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn loadfile<Types, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Types, C>,
        ChunkRange,
        Types::Table,
    ) -> Result<Types::Table, RuntimeError<Types>>,
>
where
    C: ChunkCache + KeyedChunkCache<Path>,
    Types: TypeProvider,
    Types::String: TryInto<String> + AsRef<[u8]>,
    Types::RustCallable: From<RustClosureRef<Types, C>> + LuaFfiOnce<Types, C>,
    Value<Types>: Debug + Display + TryInto<LuaString<PathBuf>>,
    <Value<Types> as TryInto<LuaString<PathBuf>>>::Error: Error,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Types::Table| {
        let fn_loadfile = RustClosureRef::new(crate::lua_std::impl_::loadfile());

        value.set(
            KeyValue::String("loadfile".into()),
            Value::Function(Callable::Rust(fn_loadfile.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn setmetatable<Types, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Types, C>,
        ChunkRange,
        Types::Table,
    ) -> Result<Types::Table, RuntimeError<Types>>,
>
where
    Types: TypeProvider,
    Types::RustCallable: From<RustClosureRef<Types, C>>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Types::Table| {
        let f = RustClosureRef::new(crate::lua_std::impl_::setmetatable());

        value.set(
            KeyValue::String("setmetatable".into()),
            Value::Function(Callable::Rust(f.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn getmetatable<Types, C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(
        RuntimeView<Types, C>,
        ChunkRange,
        Types::Table,
    ) -> Result<Types::Table, RuntimeError<Types>>,
>
where
    Types: TypeProvider,
    Types::RustCallable: From<RustClosureRef<Types, C>>,
    Value<Types>: Debug + Display,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Types::Table| {
        let f = RustClosureRef::new(crate::lua_std::impl_::getmetatable());

        value.set(
            KeyValue::String("getmetatable".into()),
            Value::Function(Callable::Rust(f.into())),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}
