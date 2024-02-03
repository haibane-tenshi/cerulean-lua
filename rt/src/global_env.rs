use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::path::Path;

use repr::chunk::{ChunkExtension, ClosureRecipe, Function};
use repr::literal::Literal;

use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::runtime::RuntimeView;
use crate::value::callable::{RustCallable, RustClosureRef};
use crate::value::table::{KeyValue, Table, TableRef};
use crate::value::{TypeProvider, Value};
use crate::value_builder::{ChunkRange, Part, ValueBuilder};

use crate::error::RuntimeError;

pub fn empty<Types, C>() -> ValueBuilder<
    impl for<'rt> FnOnce(
        RuntimeView<'rt, Types, C>,
        ChunkId,
        (),
    ) -> Result<Table<Types>, RuntimeError<Types>>,
>
where
    Types: TypeProvider,
    Value<Types>: Clone + Display,
{
    use crate::value_builder;

    let chunk_part = Part {
        chunk_ext: ChunkExtension::empty(),
        builder: |mut rt: RuntimeView<'_, Types, C>,
                  _,
                  _|
         -> Result<Table<Types>, RuntimeError<Types>> {
            rt.stack.clear();

            let value = Table::default();

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
        Table<Types>,
    ) -> Result<Table<Types>, RuntimeError<Types>>,
>
where
    C: ChunkCache,
    Types: TypeProvider<String = String, RustCallable = RustCallable<Types, C>>,
    Value<Types>: Clone,
    KeyValue<Types>: Hash + Eq,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Table<Types>| {
        let fn_assert = RustClosureRef::new(crate::lua_std::impl_::assert());

        value.set(
            KeyValue::String("assert".into()),
            Value::Function(fn_assert.into()),
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
        Table<Types>,
    ) -> Result<Table<Types>, RuntimeError<Types>>,
>
where
    C: ChunkCache,
    Types: TypeProvider<
        String = String,
        RustCallable = RustCallable<Types, C>,
        Table = TableRef<Types>,
    >,
    Value<Types>: Clone + PartialEq + Display,
    KeyValue<Types>: Hash + Eq,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Table<Types>| {
        let fn_pcall = RustClosureRef::new(crate::lua_std::impl_::pcall());

        value.set(
            KeyValue::String("pcall".into()),
            Value::Function(fn_pcall.into()),
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
        Table<Types>,
    ) -> Result<Table<Types>, RuntimeError<Types>>,
>
where
    C: ChunkCache,
    Types: TypeProvider<String = String, RustCallable = RustCallable<Types, C>>,
    Value<Types>: Clone + Display,
    KeyValue<Types>: Hash + Eq,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Table<Types>| {
        let fn_print = RustClosureRef::new(crate::lua_std::impl_::print());

        value.set(
            KeyValue::String("print".into()),
            Value::Function(fn_print.into()),
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
        Table<Types>,
    ) -> Result<Table<Types>, RuntimeError<Types>>,
>
where
    C: ChunkCache,
    Types: TypeProvider<
        String = String,
        RustCallable = RustCallable<Types, C>,
        Table = TableRef<Types>,
    >,
    Value<Types>: Clone + PartialEq + Debug + Display,
    KeyValue<Types>: Hash + Eq,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Table<Types>| {
        let fn_load = RustClosureRef::new(crate::lua_std::impl_::load());

        value.set(
            KeyValue::String("load".into()),
            Value::Function(fn_load.into()),
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
        Table<Types>,
    ) -> Result<Table<Types>, RuntimeError<Types>>,
>
where
    C: ChunkCache + KeyedChunkCache<Path>,
    Types: TypeProvider<
        String = String,
        RustCallable = RustCallable<Types, C>,
        Table = TableRef<Types>,
    >,
    Value<Types>: Clone + PartialEq + Debug + Display,
    KeyValue<Types>: Hash + Eq,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Table<Types>| {
        let fn_loadfile = RustClosureRef::new(crate::lua_std::impl_::loadfile());

        value.set(
            KeyValue::String("loadfile".into()),
            Value::Function(fn_loadfile.into()),
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
        Table<Types>,
    ) -> Result<Table<Types>, RuntimeError<Types>>,
>
where
    Types: TypeProvider<
        String = String,
        RustCallable = RustCallable<Types, C>,
        Table = TableRef<Types>,
    >,
    Value<Types>: Clone + Display,
    KeyValue<Types>: Hash + Eq,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Table<Types>| {
        let f = RustClosureRef::new(crate::lua_std::impl_::setmetatable());

        value.set(
            KeyValue::String("setmetatable".into()),
            Value::Function(f.into()),
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
        Table<Types>,
    ) -> Result<Table<Types>, RuntimeError<Types>>,
>
where
    Types: TypeProvider<
        String = String,
        RustCallable = RustCallable<Types, C>,
        Table = TableRef<Types>,
    >,
    Value<Types>: Clone + Debug + Display,
    KeyValue<Types>: Hash + Eq,
{
    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<Types, C>, _: ChunkRange, mut value: Table<Types>| {
        let f = RustClosureRef::new(crate::lua_std::impl_::getmetatable());

        value.set(
            KeyValue::String("getmetatable".into()),
            Value::Function(f.into()),
        );

        Ok(value)
    };

    Part { chunk_ext, builder }
}
