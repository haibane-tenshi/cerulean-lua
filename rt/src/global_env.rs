use std::path::Path;

use repr::chunk::{ChunkExtension, ClosureRecipe, Function};
use repr::literal::Literal;

use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::runtime::RuntimeView;
use crate::value::callable::RustClosureRef;
use crate::value::table::{KeyValue, Table};
use crate::value::Value;
use crate::value_builder::{ChunkRange, Part, ValueBuilder};

use crate::error::RuntimeError;

pub fn empty<C>() -> ValueBuilder<
    impl for<'rt> FnOnce(RuntimeView<'rt, C>, ChunkId, ()) -> Result<Table<C>, RuntimeError<C>>,
> {
    use crate::value_builder;

    let chunk_part = Part {
        chunk_ext: ChunkExtension::empty(),
        builder: |mut rt: RuntimeView<C>, _, _| {
            rt.stack.clear();

            let value = Table::default();

            Ok(value)
        },
    };

    value_builder::builder().include(chunk_part)
}

pub fn assert<C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, Table<C>) -> Result<Table<C>, RuntimeError<C>>,
>
where
    C: ChunkCache,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, mut value: Table<C>| {
        let fn_assert = RustClosureRef::new(crate::lua_std::impl_::assert());

        value.set(
            KeyValue::String("assert".into()),
            Value::Function(fn_assert.into()),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn pcall<C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, Table<C>) -> Result<Table<C>, RuntimeError<C>>,
>
where
    C: ChunkCache,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, mut value: Table<C>| {
        let fn_pcall = RustClosureRef::new(crate::lua_std::impl_::pcall());

        value.set(
            KeyValue::String("pcall".into()),
            Value::Function(fn_pcall.into()),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn print<C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, Table<C>) -> Result<Table<C>, RuntimeError<C>>,
>
where
    C: ChunkCache,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, mut value: Table<C>| {
        let fn_print = RustClosureRef::new(crate::lua_std::impl_::print());

        value.set(
            KeyValue::String("print".into()),
            Value::Function(fn_print.into()),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn load<C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, Table<C>) -> Result<Table<C>, RuntimeError<C>>,
>
where
    C: ChunkCache,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, mut value: Table<C>| {
        let fn_load = RustClosureRef::new(crate::lua_std::impl_::load());

        value.set(
            KeyValue::String("load".into()),
            Value::Function(fn_load.into()),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn loadfile<C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, Table<C>) -> Result<Table<C>, RuntimeError<C>>,
>
where
    C: ChunkCache + KeyedChunkCache<Path>,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, mut value: Table<C>| {
        let fn_loadfile = RustClosureRef::new(crate::lua_std::impl_::loadfile());

        value.set(
            KeyValue::String("loadfile".into()),
            Value::Function(fn_loadfile.into()),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn setmetatable<C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, Table<C>) -> Result<Table<C>, RuntimeError<C>>,
> {
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, mut value: Table<C>| {
        let f = RustClosureRef::new(crate::lua_std::impl_::setmetatable());

        value.set(
            KeyValue::String("setmetatable".into()),
            Value::Function(f.into()),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}

pub fn getmetatable<C>() -> Part<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, Table<C>) -> Result<Table<C>, RuntimeError<C>>,
> {
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, mut value: Table<C>| {
        let f = RustClosureRef::new(crate::lua_std::impl_::getmetatable());

        value.set(
            KeyValue::String("getmetatable".into()),
            Value::Function(f.into()),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}
