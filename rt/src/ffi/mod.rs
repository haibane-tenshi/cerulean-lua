mod arg_adapter;
mod signature;

use std::fmt::Debug;
use std::path::Path;

use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::error::RuntimeError;
use crate::runtime::RuntimeView;
use crate::value::Value;

use arg_adapter::{FormatReturns, ParseArgs};
use signature::Signature;

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

pub trait IntoLuaFfi<C, Marker> {
    type Output: LuaFfiOnce<C>;

    fn into_lua_ffi(self) -> Self::Output;
}

impl<C, F, Args> IntoLuaFfi<C, Args> for F
where
    FnThunk<F, ()>: LuaFfiOnce<C>,
{
    type Output = FnThunk<F, ()>;

    fn into_lua_ffi(self) -> Self::Output {
        FnThunk {
            func: self,
            _marker: std::marker::PhantomData,
        }
    }
}

pub struct FnThunk<F, In> {
    func: F,
    _marker: std::marker::PhantomData<In>,
}

impl<C, F, In> LuaFfiOnce<C> for FnThunk<F, In>
where
    F: Signature<In>,
    for<'rt> &'rt [crate::value::Value<C>]: ParseArgs<In>,
    <F as Signature<In>>::Output: FormatReturns<C>,
{
    fn call_once(self, mut rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        let (view, args) = rt
            .stack
            .as_slice()
            .raw
            .extract()
            .map_err(|err| Value::String(err.to_string()))?;

        if !view.is_empty() {
            let expected_args = rt.stack.len() - view.len();
            let recieved_args = rt.stack.len();

            return Err(Value::String(format!(
                "function expects {expected_args} arguments, but {recieved_args} was found"
            ))
            .into());
        }

        rt.stack.clear();

        let ret = self.func.call(args);

        rt.stack.extend(ret.format());

        Ok(())
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: std::any::type_name::<F>().to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Maybe<T> {
    #[default]
    None,
    Some(T),
}

impl<T> Maybe<T> {
    pub fn into_option(self) -> Option<T> {
        self.into()
    }
}

impl<T> Maybe<NilOr<T>> {
    pub fn flatten_into_option(self) -> Option<T> {
        self.into_option().map(NilOr::into_option).flatten()
    }
}

impl<T> From<Option<T>> for Maybe<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(t) => Maybe::Some(t),
            None => Maybe::None,
        }
    }
}

impl<T> From<Maybe<T>> for Option<T> {
    fn from(value: Maybe<T>) -> Self {
        match value {
            Maybe::None => None,
            Maybe::Some(t) => Some(t),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum NilOr<T> {
    #[default]
    Nil,
    Some(T),
}

impl<T> NilOr<T> {
    pub fn into_option(self) -> Option<T> {
        self.into()
    }
}

impl<T> From<Option<T>> for NilOr<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(t) => NilOr::Some(t),
            None => NilOr::Nil,
        }
    }
}

impl<T> From<NilOr<T>> for Option<T> {
    fn from(value: NilOr<T>) -> Self {
        match value {
            NilOr::Nil => None,
            NilOr::Some(t) => Some(t),
        }
    }
}

impl<T, C> TryFrom<Value<C>> for NilOr<T>
where
    T: TryFrom<Value<C>>,
{
    type Error = <T as TryFrom<Value<C>>>::Error;

    fn try_from(value: Value<C>) -> Result<Self, Self::Error> {
        match value {
            Value::Nil => Ok(Self::Nil),
            value => Ok(Self::Some(value.try_into()?)),
        }
    }
}

impl<T, C> From<NilOr<T>> for Value<C>
where
    T: Into<Value<C>>,
{
    fn from(value: NilOr<T>) -> Self {
        match value {
            NilOr::Nil => Value::Nil,
            NilOr::Some(value) => value.into(),
        }
    }
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
