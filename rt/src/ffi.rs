use std::fmt::Debug;
use std::path::Path;

use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::error::RuntimeError;
use crate::runtime::RuntimeView;

pub trait LuaFfiOnce<C> {
    fn call_once(self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>>;
    fn debug_info(&self) -> DebugInfo;
}

pub trait LuaFfiMut<C>: LuaFfiOnce<C> {
    fn call_mut(&mut self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>>;
}

pub trait LuaFfi<C>: LuaFfiMut<C> {
    fn call(&self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>>;
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct DebugInfo {
    pub name: String,
}

impl<C, F> LuaFfiOnce<C> for F
where
    F: for<'rt> FnOnce(RuntimeView<'rt, C>) -> Result<(), RuntimeError<C>>,
{
    fn call_once(self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        (self)(rt)
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: std::any::type_name::<F>().to_string(),
        }
    }
}

impl<C, F> LuaFfiMut<C> for F
where
    F: for<'rt> FnMut(RuntimeView<'rt, C>) -> Result<(), RuntimeError<C>>,
{
    fn call_mut(&mut self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        (self)(rt)
    }
}

impl<C, F> LuaFfi<C> for F
where
    F: for<'rt> Fn(RuntimeView<'rt, C>) -> Result<(), RuntimeError<C>>,
{
    fn call(&self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        (self)(rt)
    }
}

pub trait IntoLuaFfi<C> {
    type Output: LuaFfiOnce<C>;

    fn into_lua_ffi(self) -> Self::Output;
}

pub trait WithName<C>: Sized {
    fn with_name<N>(self, name: N) -> DebugInfoWrap<Self, N>;
}

impl<F, C> WithName<C> for F
where
    F: LuaFfiOnce<C>,
{
    fn with_name<N>(self, name: N) -> DebugInfoWrap<Self, N> {
        DebugInfoWrap { func: self, name }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DebugInfoWrap<F, N> {
    func: F,
    name: N,
}

impl<C, F, N> LuaFfiOnce<C> for DebugInfoWrap<F, N>
where
    F: LuaFfiOnce<C>,
    N: AsRef<str>,
{
    fn call_once(self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        self.func.call_once(rt)
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: self.name.as_ref().to_string(),
        }
    }
}

impl<C, F, N> LuaFfiMut<C> for DebugInfoWrap<F, N>
where
    F: LuaFfiMut<C>,
    N: AsRef<str>,
{
    fn call_mut(&mut self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        self.func.call_mut(rt)
    }
}

impl<C, F, N> LuaFfi<C> for DebugInfoWrap<F, N>
where
    F: LuaFfi<C>,
    N: AsRef<str>,
{
    fn call(&self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        self.func.call(rt)
    }
}

pub fn call_chunk<C>(chunk_id: ChunkId) -> impl LuaFfi<C> + Copy + Send + Sync
where
    C: ChunkCache,
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

    f.with_name("rt::ffi::call_chunk")
}

pub fn call_precompiled<C, Q>(script: &Q) -> impl LuaFfi<C> + Copy + '_
where
    C: ChunkCache + KeyedChunkCache<Q>,
    Q: ?Sized + Debug,
{
    let f = move |mut rt: RuntimeView<'_, C>| {
        use crate::value::Value;

        let chunk_id = rt.chunk_cache().get(script).ok_or(Value::String(format!(
            "chunk with key \"{script:?}\" does not exist"
        )))?;
        rt.invoke(call_chunk(chunk_id))
    };

    f.with_name("rt::ffi::call_precompiled")
}

pub fn call_file<C>(script: impl AsRef<Path>) -> impl LuaFfi<C>
where
    C: ChunkCache + KeyedChunkCache<Path>,
{
    let f = move |mut rt: RuntimeView<C>| {
        let script = script.as_ref();
        let chunk_id = rt.load_from_file(script)?;

        rt.invoke(call_chunk(chunk_id))
    };

    f.with_name("rt::ffi::call_file")
}
