use crate::chunk_builder::ChunkBuilder;
use crate::chunk_cache::ChunkId;
use crate::runtime::RuntimeView;

use crate::RuntimeError;

pub fn empty<C>(
) -> ChunkBuilder<impl for<'rt> FnOnce(ChunkId, RuntimeView<'rt, C>) -> Result<(), RuntimeError>> {
    use crate::chunk_builder::{self, ChunkPart};
    use crate::ffi::IntoLuaFfi;
    use crate::value::table::TableRef;
    use crate::value::Value;
    use repr::chunk::ChunkExtension;

    let chunk_part = ChunkPart {
        chunk_ext: ChunkExtension::empty(),
        builder: |_| {
            (|mut rt: RuntimeView<C>| {
                rt.stack.clear();
                rt.stack.push(Value::Table(TableRef::new()));

                Ok(())
            })
            .into_lua_ffi()
        },
    };

    chunk_builder::builder().add(chunk_part)
}
