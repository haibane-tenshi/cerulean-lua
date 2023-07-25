use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::runtime::RuntimeView;
use crate::RuntimeError;

pub trait Ffi<C> {
    fn call(self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError>;
}

impl<C, F> Ffi<C> for F
where
    F: for<'rt> FnOnce(RuntimeView<'rt, C>) -> Result<(), RuntimeError>,
{
    fn call(self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError> {
        (self)(rt)
    }
}

pub fn call_chunk<C>(chunk_id: ChunkId) -> impl Ffi<C>
where
    C: ChunkCache,
{
    move |mut rt: RuntimeView<'_, C>| {
        use crate::chunk_cache::FunctionPtr;
        use repr::index::FunctionId;

        let ptr = FunctionPtr {
            chunk_id,
            function_id: FunctionId(0),
        };
        let offset = rt.stack.top();
        let frame = rt.make_frame(ptr, offset);

        rt.enter(frame)
    }
}
