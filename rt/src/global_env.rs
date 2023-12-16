use repr::chunk::{ChunkExtension, ClosureRecipe, Function};
use repr::index::StackSlot;
use repr::literal::Literal;

use crate::chunk_builder::{ChunkBuilder, ChunkPart, ChunkRange};
use crate::chunk_cache::{ChunkCache, ChunkId};
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
    C: ChunkCache<ChunkId>,
{
    use crate::value::callable::RustClosureRef;

    let chunk_ext = ChunkExtension::empty();

    let builder = |mut _rt: RuntimeView<C>, _: ChunkRange, value: &mut Value<C>| {
        let Value::Table(table) = value else {
            return Err(
                Value::String("global env value is expected to be table".to_string()).into(),
            );
        };

        let fn_assert = RustClosureRef::with_name("lua_std::assert", |rt: RuntimeView<_>| {
            let Some(cond) = rt.stack.get(StackSlot(0)) else {
                return Err(
                    Value::String("assert expects at least one argument".to_string()).into(),
                );
            };

            if cond.to_bool() {
                Ok(())
            } else {
                let err = rt
                    .stack
                    .get(StackSlot(1))
                    .cloned()
                    .unwrap_or_else(|| Value::String("assertion failed!".to_string()));
                Err(err.into())
            }
        });

        table
            .borrow_mut()
            .map_err(|_| Value::String("failed to borrow global env table".to_string()))?
            .set(
                KeyValue::String("assert".into()),
                Value::Function(Callable::RustClosure(fn_assert)),
            );

        Ok(())
    };

    ChunkPart { chunk_ext, builder }
}
