use std::path::Path;

use repr::chunk::{ChunkExtension, ClosureRecipe, Function};
use repr::index::StackSlot;
use repr::literal::Literal;

use crate::chunk_builder::{ChunkBuilder, ChunkPart, ChunkRange};
use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::runtime::RuntimeView;
use crate::value::callable::Callable;
use crate::value::table::KeyValue;
use crate::value::Value;

use crate::error::RuntimeError;

pub fn empty<C>() -> ChunkBuilder<
    impl for<'rt> FnOnce(RuntimeView<'rt, C>, ChunkId, &mut Value<C>) -> Result<(), RuntimeError<C>>,
> {
    use crate::chunk_builder;
    use crate::value::table::TableRef;

    let chunk_part = ChunkPart {
        chunk_ext: ChunkExtension::empty(),
        builder: |mut rt: RuntimeView<C>, _, value: &mut Value<C>| {
            *value = Value::Table(TableRef::new());
            rt.stack.clear();

            Ok(())
        },
    };

    chunk_builder::builder().add(chunk_part)
}

pub fn assert<C>() -> ChunkPart<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, &mut Value<C>) -> Result<(), RuntimeError<C>>,
>
where
    C: ChunkCache,
{
    use crate::value::callable::RustClosureMut;

    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, value: &mut Value<C>| {
        let Value::Table(table) = value else {
            return Err(
                Value::String("global env value is expected to be table".to_string()).into(),
            );
        };

        let fn_assert = RustClosureMut::new(crate::lua_std::impl_::assert());

        table
            .borrow_mut()
            .map_err(|_| Value::String("failed to borrow global env table".to_string()))?
            .set(
                KeyValue::String("assert".into()),
                Value::Function(fn_assert.into()),
            );

        Ok(())
    };

    ChunkPart { chunk_ext, builder }
}

pub fn pcall<C>() -> ChunkPart<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, &mut Value<C>) -> Result<(), RuntimeError<C>>,
>
where
    C: ChunkCache,
{
    use crate::value::callable::RustClosureRef;

    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, value: &mut Value<C>| {
        let Value::Table(table) = value else {
            return Err(
                Value::String("global env value is expected to be table".to_string()).into(),
            );
        };

        let fn_pcall = RustClosureRef::new(crate::lua_std::impl_::pcall());

        table
            .borrow_mut()
            .map_err(|_| Value::String("failed to borrow global env table".to_string()))?
            .set(
                KeyValue::String("pcall".into()),
                Value::Function(fn_pcall.into()),
            );

        Ok(())
    };

    ChunkPart { chunk_ext, builder }
}

pub fn print<C>() -> ChunkPart<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, &mut Value<C>) -> Result<(), RuntimeError<C>>,
>
where
    C: ChunkCache,
{
    use crate::value::callable::RustClosureRef;

    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, value: &mut Value<C>| {
        let Value::Table(table) = value else {
            return Err(
                Value::String("global env value is expected to be table".to_string()).into(),
            );
        };

        let fn_print = RustClosureRef::new(crate::lua_std::impl_::print());

        table
            .borrow_mut()
            .map_err(|_| Value::String("failed to borrow global env table".to_string()))?
            .set(
                KeyValue::String("print".into()),
                Value::Function(fn_print.into()),
            );

        Ok(())
    };

    ChunkPart { chunk_ext, builder }
}

pub fn load<C>() -> ChunkPart<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, &mut Value<C>) -> Result<(), RuntimeError<C>>,
>
where
    C: ChunkCache,
{
    use crate::value::callable::RustClosureRef;

    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, value: &mut Value<C>| {
        let Value::Table(table) = value else {
            return Err(
                Value::String("global env value is expected to be table".to_string()).into(),
            );
        };

        let fn_load = RustClosureRef::new(crate::lua_std::impl_::load());

        table
            .borrow_mut()
            .map_err(|_| Value::String("failed to borrow global env table".to_string()))?
            .set(
                KeyValue::String("load".into()),
                Value::Function(fn_load.into()),
            );

        Ok(())
    };

    ChunkPart { chunk_ext, builder }
}

pub fn loadfile<C>() -> ChunkPart<
    [Function; 0],
    [Literal; 0],
    [ClosureRecipe; 0],
    impl FnOnce(RuntimeView<C>, ChunkRange, &mut Value<C>) -> Result<(), RuntimeError<C>>,
>
where
    C: ChunkCache + KeyedChunkCache<Path>,
{
    use crate::value::callable::RustClosureRef;

    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, value: &mut Value<C>| {
        let Value::Table(table) = value else {
            return Err(
                Value::String("global env value is expected to be table".to_string()).into(),
            );
        };

        let fn_loadfile = RustClosureRef::new(crate::lua_std::impl_::loadfile());

        table
            .borrow_mut()
            .map_err(|_| Value::String("failed to borrow global env table".to_string()))?
            .set(
                KeyValue::String("loadfile".into()),
                Value::Function(fn_loadfile.into()),
            );

        Ok(())
    };

    ChunkPart { chunk_ext, builder }
}
