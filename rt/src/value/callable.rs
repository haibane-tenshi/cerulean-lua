use std::cell::RefCell;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use crate::chunk_cache::ChunkCache;
use crate::error::RuntimeError;
use crate::ffi::{DebugInfo, LuaFfi, LuaFfiMut, LuaFfiOnce};

use super::{TypeMismatchError, Value};

pub use crate::runtime::{Closure as LuaClosure, ClosureRef as LuaClosureRef};

pub struct RustClosureMut<C>(Rc<Inner<RefCell<dyn LuaFfiMut<C> + 'static>>>);

struct Inner<T: ?Sized> {
    debug_info: DebugInfo,
    callable: T,
}

impl<C> RustClosureMut<C> {
    pub fn new(value: impl LuaFfiMut<C> + 'static) -> Self {
        let debug_info = value.debug_info();
        let inner = Inner {
            debug_info,
            callable: RefCell::new(value),
        };
        let rc = Rc::new(inner);
        RustClosureMut(rc)
    }

    pub fn with_name(name: impl AsRef<str> + 'static, value: impl LuaFfiMut<C> + 'static) -> Self {
        use crate::ffi::WithName;

        Self::new(value.with_name(name))
    }
}

impl<C> Debug for RustClosureMut<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RustClosureMut").field(&"<omitted>").finish()
    }
}

impl<C> Clone for RustClosureMut<C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<C> PartialEq for RustClosureMut<C> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<C> Eq for RustClosureMut<C> {}

impl<C> Hash for RustClosureMut<C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<C> LuaFfiOnce<C> for RustClosureMut<C> {
    fn call_once(mut self, rt: crate::runtime::RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        self.call_mut(rt)
    }

    fn debug_info(&self) -> crate::ffi::DebugInfo {
        self.0.debug_info.clone()
    }
}

impl<C> LuaFfiMut<C> for RustClosureMut<C> {
    fn call_mut(&mut self, rt: crate::runtime::RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        let mut f = self
            .0
            .callable
            .try_borrow_mut()
            .map_err(|_| Value::String("failed to mutably borrow closure".to_string()))?;
        f.call_mut(rt)
    }
}

pub struct RustClosureRef<C>(Rc<dyn LuaFfi<C> + 'static>);

impl<C> RustClosureRef<C> {
    pub fn new(value: impl LuaFfi<C> + 'static) -> Self {
        let rc = Rc::new(value);
        RustClosureRef(rc)
    }

    pub fn with_name(name: impl AsRef<str> + 'static, value: impl LuaFfi<C> + 'static) -> Self {
        use crate::ffi::WithName;

        Self::new(value.with_name(name))
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
    fn call_once(self, rt: crate::runtime::RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        self.call(rt)
    }

    fn debug_info(&self) -> crate::ffi::DebugInfo {
        self.0.debug_info()
    }
}

impl<C> LuaFfiMut<C> for RustClosureRef<C> {
    fn call_mut(&mut self, rt: crate::runtime::RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        self.call(rt)
    }
}

impl<C> LuaFfi<C> for RustClosureRef<C> {
    fn call(&self, rt: crate::runtime::RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        self.0.call(rt)
    }
}

pub enum RustCallable<C> {
    Mut(RustClosureMut<C>),
    Ref(RustClosureRef<C>),
}

impl<C> Debug for RustCallable<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mut(arg0) => f.debug_tuple("Mut").field(arg0).finish(),
            Self::Ref(arg0) => f.debug_tuple("Ref").field(arg0).finish(),
        }
    }
}

impl<C> Clone for RustCallable<C> {
    fn clone(&self) -> Self {
        match self {
            Self::Mut(arg0) => Self::Mut(arg0.clone()),
            Self::Ref(arg0) => Self::Ref(arg0.clone()),
        }
    }
}

impl<C> PartialEq for RustCallable<C> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Mut(l0), Self::Mut(r0)) => l0 == r0,
            (Self::Ref(l0), Self::Ref(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<C> Eq for RustCallable<C> {}

impl<C> Hash for RustCallable<C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            Self::Mut(t) => t.hash(state),
            Self::Ref(t) => t.hash(state),
        }
    }
}

impl<C> LuaFfiOnce<C> for RustCallable<C> {
    fn call_once(self, mut rt: crate::runtime::RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        match self {
            Self::Mut(f) => rt.invoke(f),
            Self::Ref(f) => rt.invoke(f),
        }
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: "{rust callable wrapper}".to_string(),
        }
    }
}

impl<C> From<RustClosureMut<C>> for RustCallable<C> {
    fn from(value: RustClosureMut<C>) -> Self {
        Self::Mut(value)
    }
}

impl<C> From<RustClosureRef<C>> for RustCallable<C> {
    fn from(value: RustClosureRef<C>) -> Self {
        Self::Ref(value)
    }
}

impl<C> From<LuaClosureRef> for Value<C> {
    fn from(value: LuaClosureRef) -> Self {
        Value::Function(value.into())
    }
}

impl<C> From<RustCallable<C>> for Value<C> {
    fn from(value: RustCallable<C>) -> Self {
        Value::Function(value.into())
    }
}

impl<C> From<RustClosureRef<C>> for Value<C> {
    fn from(value: RustClosureRef<C>) -> Self {
        Value::Function(value.into())
    }
}

impl<C> From<RustClosureMut<C>> for Value<C> {
    fn from(value: RustClosureMut<C>) -> Self {
        Value::Function(value.into())
    }
}

pub enum Callable<C> {
    Lua(LuaClosureRef),
    Rust(RustCallable<C>),
}

impl<C> Debug for Callable<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lua(arg0) => f.debug_tuple("LuaClosure").field(arg0).finish(),
            Self::Rust(arg0) => f.debug_tuple("RustCallable").field(arg0).finish(),
        }
    }
}

impl<C> Clone for Callable<C> {
    fn clone(&self) -> Self {
        match self {
            Self::Lua(arg0) => Self::Lua(arg0.clone()),
            Self::Rust(arg0) => Self::Rust(arg0.clone()),
        }
    }
}

impl<C> PartialEq for Callable<C> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Lua(l0), Self::Lua(r0)) => l0 == r0,
            (Self::Rust(l0), Self::Rust(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<C> Eq for Callable<C> {}

impl<C> Hash for Callable<C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            Self::Lua(t) => t.hash(state),
            Self::Rust(t) => t.hash(state),
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
            Callable::Lua(f) => rt.enter(f, StackSlot(0)),
            Callable::Rust(f) => rt.invoke(f),
        }
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: "{generic callable wrapper}".to_string(),
        }
    }
}

impl<C> From<LuaClosureRef> for Callable<C> {
    fn from(value: LuaClosureRef) -> Self {
        Self::Lua(value)
    }
}

impl<C> From<RustCallable<C>> for Callable<C> {
    fn from(value: RustCallable<C>) -> Self {
        Self::Rust(value)
    }
}

impl<C> From<RustClosureMut<C>> for Callable<C> {
    fn from(value: RustClosureMut<C>) -> Self {
        Self::Rust(value.into())
    }
}

impl<C> From<RustClosureRef<C>> for Callable<C> {
    fn from(value: RustClosureRef<C>) -> Self {
        Self::Rust(value.into())
    }
}

impl<C> TryFrom<Value<C>> for Callable<C> {
    type Error = TypeMismatchError;

    fn try_from(value: Value<C>) -> Result<Self, Self::Error> {
        use super::Type;

        match value {
            Value::Function(value) => Ok(value),
            value => {
                let err = TypeMismatchError {
                    expected: Type::Function,
                    found: value.type_(),
                };

                Err(err)
            }
        }
    }
}

impl<C> From<Callable<C>> for Value<C> {
    fn from(value: Callable<C>) -> Self {
        Value::Function(value)
    }
}
