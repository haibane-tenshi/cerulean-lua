use repr::chunk::{ChunkExtension, ClosureRecipe, Function, Signature};
use repr::index::{ConstId, FunctionId, InstrOffset, StackSlot};
use repr::literal::Literal;
use repr::opcode::OpCode;

use crate::chunk_builder::{ChunkBuilder, ChunkPart, ChunkRange};
use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::runtime::RuntimeView;
use crate::value::callable::Callable;
use crate::value::table::KeyValue;
use crate::value::Value;

use crate::RuntimeError;

pub fn empty<C>() -> ChunkBuilder<
    impl for<'rt> FnOnce(RuntimeView<'rt, C>, ChunkId, &mut Value<C>) -> Result<(), RuntimeError>,
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
