use std::fmt::Debug;
use std::path::Path;

use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::error::RuntimeError;
use crate::runtime::RuntimeView;

pub trait LuaFfiOnce<C> {
    fn call_once(self, _: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>>;
    fn name(&self) -> String;
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

    fn name(&self) -> String {
        <T as LuaFfiOnce<C>>::name(self)
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

    fn name(&self) -> String {
        <T as LuaFfiOnce<C>>::name(self)
    }
}

pub trait IntoLuaFfi<C> {
    type Output;

    fn into_lua_ffi(self) -> Self::Output;
}

pub trait IntoLuaFfiWithName<C, N> {
    type Output;

    fn into_lua_ffi_with_name(self, name: N) -> Self::Output;
}

impl<'rt, F, C> IntoLuaFfi<C> for F
where
    C: 'rt,
    F: FnOnce(RuntimeView<'rt, C>) -> Result<(), RuntimeError<C>>,
{
    type Output = FnWrap<F, &'static str>;

    fn into_lua_ffi(self) -> Self::Output {
        FnWrap {
            func: self,
            name: std::any::type_name::<F>(),
        }
    }
}

impl<'rt, F, C, N> IntoLuaFfiWithName<C, N> for F
where
    C: 'rt,
    F: FnOnce(RuntimeView<'rt, C>) -> Result<(), RuntimeError<C>>,
{
    type Output = FnWrap<F, N>;

    fn into_lua_ffi_with_name(self, name: N) -> Self::Output {
        FnWrap { func: self, name }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FnWrap<F, N> {
    func: F,
    name: N,
}

impl<C, F, N> LuaFfiOnce<C> for FnWrap<F, N>
where
    F: for<'rt> FnOnce(RuntimeView<'rt, C>) -> Result<(), RuntimeError<C>>,
    N: AsRef<str>,
{
    fn call_once(self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        (self.func)(rt)
    }

    fn name(&self) -> String {
        self.name.as_ref().to_string()
    }
}

impl<C, F, N> LuaFfiMut<C> for FnWrap<F, N>
where
    F: for<'rt> FnMut(RuntimeView<'rt, C>) -> Result<(), RuntimeError<C>>,
    N: AsRef<str>,
{
    fn call_mut(&mut self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        (self.func)(rt)
    }
}

impl<C, F, N> LuaFfi<C> for FnWrap<F, N>
where
    F: for<'rt> Fn(RuntimeView<'rt, C>) -> Result<(), RuntimeError<C>>,
    N: AsRef<str>,
{
    fn call(&self, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        (self.func)(rt)
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

    f.into_lua_ffi_with_name("rt::ffi::call_chunk")
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

    f.into_lua_ffi_with_name("rt::ffi::call_precompiled")
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

    f.into_lua_ffi_with_name("rt::ffi::call_file")
}
