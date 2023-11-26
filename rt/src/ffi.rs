use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::runtime::RuntimeView;
use crate::RuntimeError;

pub trait LuaFfiOnce<C> {
    fn call_once(self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError>;
}

pub trait LuaFfiMut<C>: LuaFfiOnce<C> {
    fn call_mut(&mut self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError>;
}

pub trait LuaFfi<C>: LuaFfiMut<C> {
    fn call(&self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError>;
}

impl<C, T> LuaFfiOnce<C> for &T
where
    T: LuaFfi<C> + ?Sized,
{
    fn call_once(self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError> {
        self.call(rt)
    }
}

impl<C, T> LuaFfiMut<C> for &T
where
    T: LuaFfi<C> + ?Sized,
{
    fn call_mut(&mut self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError> {
        self.call(rt)
    }
}

impl<C, T> LuaFfiOnce<C> for &mut T
where
    T: LuaFfiMut<C> + ?Sized,
{
    fn call_once(self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError> {
        self.call_mut(rt)
    }
}

#[derive(Clone, Copy)]
pub struct FnWrap<F>(F);

impl<C, F> LuaFfiOnce<C> for FnWrap<F>
where
    F: for<'rt> FnOnce(RuntimeView<'rt, C>) -> Result<(), RuntimeError>,
{
    fn call_once(self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError> {
        (self.0)(rt)
    }
}

impl<C, F> LuaFfiMut<C> for FnWrap<F>
where
    F: for<'rt> FnMut(RuntimeView<'rt, C>) -> Result<(), RuntimeError>,
{
    fn call_mut(&mut self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError> {
        (self.0)(rt)
    }
}

impl<C, F> LuaFfi<C> for FnWrap<F>
where
    F: for<'rt> Fn(RuntimeView<'rt, C>) -> Result<(), RuntimeError>,
{
    fn call(&self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError> {
        (self.0)(rt)
    }
}

impl<F> From<F> for FnWrap<F> {
    fn from(value: F) -> Self {
        FnWrap(value)
    }
}

pub fn call_chunk<C>(chunk_id: ChunkId) -> impl LuaFfi<C> + Copy + Send + Sync
where
    C: ChunkCache,
{
    let f = move |mut rt: RuntimeView<'_, C>| {
        use crate::runtime::{ClosureRef, FunctionPtr};
        use repr::index::FunctionId;

        let ptr = FunctionPtr {
            chunk_id,
            function_id: FunctionId(0),
        };
        let offset = rt.stack.top();
        let closure = rt.construct_closure(ptr, [rt.global_env.clone()])?;
        let closure = ClosureRef::new(closure);

        rt.enter(closure, offset)
    };

    let r: FnWrap<_> = f.into();
    r
}

pub fn call_script<C, Q>(script: &Q) -> impl LuaFfi<C> + Copy + '_
where
    C: KeyedChunkCache<Q>,
    Q: ?Sized,
{
    let f = move |mut rt: RuntimeView<'_, C>| {
        let chunk_id = rt.chunk_cache.lookup(script).ok_or(RuntimeError)?;
        rt.invoke(call_chunk(chunk_id))
    };

    let r: FnWrap<_> = f.into();
    r
}
