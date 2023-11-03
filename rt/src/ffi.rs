use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::runtime::RuntimeView;
use crate::RuntimeError;

pub trait LuaFfiOnce<C> {
    fn call_once(self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError>;
}

impl<C, F> LuaFfiOnce<C> for F
where
    F: for<'rt> FnOnce(RuntimeView<'rt, C>) -> Result<(), RuntimeError>,
{
    fn call_once(self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError> {
        (self)(rt)
    }
}

pub trait LuaFfiMut<C>: LuaFfiOnce<C> {
    fn call_mut(&mut self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError>;
}

impl<C, F> LuaFfiMut<C> for F
where
    F: for<'rt> FnMut(RuntimeView<'rt, C>) -> Result<(), RuntimeError>,
{
    fn call_mut(&mut self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError> {
        (self)(rt)
    }
}

pub trait LuaFfi<C>: LuaFfiMut<C> {
    fn call(&self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError>;
}

impl<C, F> LuaFfi<C> for F
where
    F: for<'rt> Fn(RuntimeView<'rt, C>) -> Result<(), RuntimeError>,
{
    fn call(&self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError> {
        (self)(rt)
    }
}

pub fn call_chunk<C>(chunk_id: ChunkId) -> impl LuaFfi<C> + Copy + Send + Sync
where
    C: ChunkCache,
{
    move |mut rt: RuntimeView<'_, C>| {
        use crate::runtime::{ClosureRef, FunctionPtr};
        use repr::index::FunctionId;

        let ptr = FunctionPtr {
            chunk_id,
            function_id: FunctionId(0),
        };
        let offset = rt.stack.top();
        let closure = ptr.construct_closure(&mut rt)?;
        let closure = ClosureRef::new(closure);

        rt.enter(closure, offset)
    }
}

pub fn call_script<C, Q>(script: &Q) -> impl LuaFfi<C> + Copy + '_
where
    C: KeyedChunkCache<Q>,
    Q: ?Sized,
{
    move |mut rt: RuntimeView<'_, C>| {
        let chunk_id = rt.chunk_cache.lookup(script).ok_or(RuntimeError)?;
        rt.invoke(call_chunk(chunk_id))
    }
}
