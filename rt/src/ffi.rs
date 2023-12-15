use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::error::RuntimeError;
use crate::runtime::RuntimeView;

pub trait LuaFfiOnce<C> {
    fn call_once(self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>>;
}

pub trait LuaFfiMut<C>: LuaFfiOnce<C> {
    fn call_mut(&mut self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>>;
}

pub trait LuaFfi<C>: LuaFfiMut<C> {
    fn call(&self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>>;
}

impl<C, T> LuaFfiOnce<C> for &T
where
    T: LuaFfi<C> + ?Sized,
{
    fn call_once(self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        self.call(rt)
    }
}

impl<C, T> LuaFfiMut<C> for &T
where
    T: LuaFfi<C> + ?Sized,
{
    fn call_mut(&mut self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        self.call(rt)
    }
}

impl<C, T> LuaFfiOnce<C> for &mut T
where
    T: LuaFfiMut<C> + ?Sized,
{
    fn call_once(self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        self.call_mut(rt)
    }
}

pub trait IntoLuaFfi<C> {
    type Output;

    fn into_lua_ffi(self) -> Self::Output;
}

impl<'rt, F, C> IntoLuaFfi<C> for F
where
    C: 'rt,
    F: FnOnce(RuntimeView<'rt, C>) -> Result<(), RuntimeError<C>>,
{
    type Output = FnWrap<F>;

    fn into_lua_ffi(self) -> Self::Output {
        FnWrap(self)
    }
}

#[derive(Clone, Copy)]
pub struct FnWrap<F>(F);

impl<C, F> LuaFfiOnce<C> for FnWrap<F>
where
    F: for<'rt> FnOnce(RuntimeView<'rt, C>) -> Result<(), RuntimeError<C>>,
{
    fn call_once(self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        (self.0)(rt)
    }
}

impl<C, F> LuaFfiMut<C> for FnWrap<F>
where
    F: for<'rt> FnMut(RuntimeView<'rt, C>) -> Result<(), RuntimeError<C>>,
{
    fn call_mut(&mut self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        (self.0)(rt)
    }
}

impl<C, F> LuaFfi<C> for FnWrap<F>
where
    F: for<'rt> Fn(RuntimeView<'rt, C>) -> Result<(), RuntimeError<C>>,
{
    fn call(&self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        (self.0)(rt)
    }
}

pub fn call_chunk<C>(chunk_id: ChunkId) -> impl LuaFfi<C> + Copy + Send + Sync
where
    C: ChunkCache<ChunkId>,
{
    let f = move |mut rt: RuntimeView<'_, C>| {
        use crate::runtime::{ClosureRef, FunctionPtr};
        use repr::index::{FunctionId, StackSlot};

        let ptr = FunctionPtr {
            chunk_id,
            function_id: FunctionId(0),
        };

        let closure = rt.construct_closure(ptr, [rt.global_env.clone()])?;
        let closure = ClosureRef::new(closure);

        rt.enter(closure, StackSlot(0))
    };

    f.into_lua_ffi()
}

pub fn call_script<C, Q>(script: &Q) -> impl LuaFfi<C> + Copy + '_
where
    C: KeyedChunkCache<ChunkId, Q>,
    Q: ?Sized,
{
    let f = move |mut rt: RuntimeView<'_, C>| {
        let chunk_id = rt
            .chunk_cache
            .lookup(script)
            .ok_or(RuntimeError::CatchAll)?;
        rt.invoke(call_chunk(chunk_id))
    };

    f.into_lua_ffi()
}
