use std::cell::RefCell;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use crate::chunk_cache::ChunkCache;
use crate::error::RuntimeError;
use crate::ffi::{DebugInfo, IntoLuaFfi, IntoLuaFfiWithName, LuaFfiMut, LuaFfiOnce};

pub use crate::runtime::{Closure as LuaClosure, ClosureRef as LuaClosureRef};

pub struct RustClosureRef<C>(Rc<Inner<RefCell<dyn LuaFfiMut<C> + 'static>>>);

struct Inner<T: ?Sized> {
    debug_info: DebugInfo,
    callable: T,
}

impl<C> RustClosureRef<C> {
    pub fn new<F>(value: F) -> Self
    where
        F: IntoLuaFfi<C>,
        <F as IntoLuaFfi<C>>::Output: LuaFfiMut<C> + 'static,
    {
        let value = value.into_lua_ffi();
        let debug_info = value.debug_info();
        let inner = Inner {
            debug_info,
            callable: RefCell::new(value),
        };
        let rc = Rc::new(inner);
        RustClosureRef(rc)
    }

    pub fn with_name<F, N>(name: N, value: F) -> Self
    where
        F: IntoLuaFfiWithName<C, N>,
        <F as IntoLuaFfiWithName<C, N>>::Output: LuaFfiMut<C> + 'static,
    {
        let value = value.into_lua_ffi_with_name(name);
        let debug_info = value.debug_info();
        let inner = Inner {
            debug_info,
            callable: RefCell::new(value),
        };
        let rc = Rc::new(inner);
        RustClosureRef(rc)
    }
}

impl<C> Debug for RustClosureRef<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RustClosureRef").field(&"<omitted>").finish()
    }
}

impl<C> Clone for RustClosureRef<C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<C> PartialEq for RustClosureRef<C> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<C> Eq for RustClosureRef<C> {}

impl<C> Hash for RustClosureRef<C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<C> LuaFfiOnce<C> for RustClosureRef<C> {
    fn call_once(mut self, rt: crate::runtime::RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        self.call_mut(rt)
    }

    fn debug_info(&self) -> crate::ffi::DebugInfo {
        self.0.debug_info.clone()
    }
}

impl<C> LuaFfiMut<C> for RustClosureRef<C> {
    fn call_mut(&mut self, rt: crate::runtime::RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        use crate::value::Value;

        let mut f = self
            .0
            .callable
            .try_borrow_mut()
            .map_err(|_| Value::String("failed to mutably borrow closure".to_string()))?;
        f.call_mut(rt)
    }
}

pub enum Callable<C> {
    LuaClosure(LuaClosureRef),
    RustClosure(RustClosureRef<C>),
}

impl<C> Debug for Callable<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LuaClosure(arg0) => f.debug_tuple("LuaClosure").field(arg0).finish(),
            Self::RustClosure(arg0) => f.debug_tuple("RustClosure").field(arg0).finish(),
        }
    }
}

impl<C> Clone for Callable<C> {
    fn clone(&self) -> Self {
        match self {
            Self::LuaClosure(arg0) => Self::LuaClosure(arg0.clone()),
            Self::RustClosure(arg0) => Self::RustClosure(arg0.clone()),
        }
    }
}

impl<C> PartialEq for Callable<C> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::LuaClosure(l0), Self::LuaClosure(r0)) => l0 == r0,
            (Self::RustClosure(l0), Self::RustClosure(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<C> Eq for Callable<C> {}

impl<C> Hash for Callable<C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            Self::LuaClosure(t) => t.hash(state),
            Self::RustClosure(t) => t.hash(state),
        }
    }
}

impl<C> LuaFfiOnce<C> for Callable<C>
where
    C: ChunkCache,
{
    fn call_once(self, mut rt: crate::runtime::RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        use repr::index::StackSlot;

        match self {
            Callable::LuaClosure(f) => rt.enter(f, StackSlot(0)),
            Callable::RustClosure(f) => rt.invoke(f),
        }
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: "<callable wrapper>".to_string(),
        }
    }
}
