use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

use crate::error::RuntimeError;
use crate::ffi::{DebugInfo, LuaFfi, LuaFfiMut, LuaFfiOnce};
use crate::gc::Gc as GarbageCollector;

use super::{TypeMismatchError, TypeProvider, Value};

pub use crate::runtime::{Closure as LuaClosure, ClosureRef as LuaClosureRef};

pub struct RustClosureMut<Gc, C>(Rc<Inner<RefCell<dyn LuaFfiMut<Gc, C> + 'static>>>);

struct Inner<T: ?Sized> {
    debug_info: DebugInfo,
    callable: T,
}

impl<Gc, C> RustClosureMut<Gc, C>
where
    Gc: TypeProvider,
{
    pub fn new(value: impl LuaFfiMut<Gc, C> + 'static) -> Self {
        let debug_info = value.debug_info();
        let inner = Inner {
            debug_info,
            callable: RefCell::new(value),
        };
        let rc = Rc::new(inner);
        RustClosureMut(rc)
    }

    pub fn with_name(
        name: impl AsRef<str> + 'static,
        value: impl LuaFfiMut<Gc, C> + 'static,
    ) -> Self {
        use crate::ffi::WithName;

        Self::new(value.with_name(name))
    }
}

impl<Gc, C> Debug for RustClosureMut<Gc, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RustClosureMut").field(&"<omitted>").finish()
    }
}

impl<Gc, C> Clone for RustClosureMut<Gc, C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Gc, C> PartialEq for RustClosureMut<Gc, C> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Gc, C> Eq for RustClosureMut<Gc, C> {}

impl<Gc, C> Hash for RustClosureMut<Gc, C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<Gc, C> LuaFfiOnce<Gc, C> for RustClosureMut<Gc, C>
where
    Gc: TypeProvider,
    Gc::String: From<&'static str>,
{
    fn call_once(
        mut self,
        rt: crate::runtime::RuntimeView<'_, Gc, C>,
    ) -> Result<(), RuntimeError<Gc>> {
        self.call_mut(rt)
    }

    fn debug_info(&self) -> crate::ffi::DebugInfo {
        self.0.debug_info.clone()
    }
}

impl<Gc, C> LuaFfiMut<Gc, C> for RustClosureMut<Gc, C>
where
    Gc: TypeProvider,
    Gc::String: From<&'static str>,
{
    fn call_mut(
        &mut self,
        rt: crate::runtime::RuntimeView<'_, Gc, C>,
    ) -> Result<(), RuntimeError<Gc>> {
        let mut f = self
            .0
            .callable
            .try_borrow_mut()
            .map_err(|_| Value::String("failed to mutably borrow closure".into()))?;
        f.call_mut(rt)
    }
}

pub struct RustClosureRef<Gc, C>(Rc<dyn LuaFfi<Gc, C> + 'static>);

impl<Gc, C> RustClosureRef<Gc, C>
where
    Gc: TypeProvider,
{
    pub fn new(value: impl LuaFfi<Gc, C> + 'static) -> Self {
        let rc = Rc::new(value);
        RustClosureRef(rc)
    }

    pub fn with_name(name: impl AsRef<str> + 'static, value: impl LuaFfi<Gc, C> + 'static) -> Self {
        use crate::ffi::WithName;

        Self::new(value.with_name(name))
    }
}

impl<Gc, C> Debug for RustClosureRef<Gc, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RustClosureRef").field(&"<omitted>").finish()
    }
}

impl<Gc, C> Clone for RustClosureRef<Gc, C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Gc, C> PartialEq for RustClosureRef<Gc, C> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Gc, C> Eq for RustClosureRef<Gc, C> {}

impl<Gc, C> Hash for RustClosureRef<Gc, C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<Gc, C> LuaFfiOnce<Gc, C> for RustClosureRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn call_once(self, rt: crate::runtime::RuntimeView<'_, Gc, C>) -> Result<(), RuntimeError<Gc>> {
        self.call(rt)
    }

    fn debug_info(&self) -> crate::ffi::DebugInfo {
        self.0.debug_info()
    }
}

impl<Gc, C> LuaFfiMut<Gc, C> for RustClosureRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn call_mut(
        &mut self,
        rt: crate::runtime::RuntimeView<'_, Gc, C>,
    ) -> Result<(), RuntimeError<Gc>> {
        self.call(rt)
    }
}

impl<Gc, C> LuaFfi<Gc, C> for RustClosureRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn call(&self, rt: crate::runtime::RuntimeView<'_, Gc, C>) -> Result<(), RuntimeError<Gc>> {
        self.0.call(rt)
    }
}

pub enum RustCallable<Gc, C> {
    Mut(RustClosureMut<Gc, C>),
    Ref(RustClosureRef<Gc, C>),
}

impl<Gc, C> Debug for RustCallable<Gc, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mut(arg0) => f.debug_tuple("Mut").field(arg0).finish(),
            Self::Ref(arg0) => f.debug_tuple("Ref").field(arg0).finish(),
        }
    }
}

impl<Gc, C> Display for RustCallable<Gc, C>
where
    Gc: TypeProvider,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // let name = self.debug_info().name;

        write!(f, "{{[rust] closure <name omitted>}}")
    }
}

impl<Gc, C> Clone for RustCallable<Gc, C> {
    fn clone(&self) -> Self {
        match self {
            Self::Mut(arg0) => Self::Mut(arg0.clone()),
            Self::Ref(arg0) => Self::Ref(arg0.clone()),
        }
    }
}

impl<Gc, C> PartialEq for RustCallable<Gc, C> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Mut(l0), Self::Mut(r0)) => l0 == r0,
            (Self::Ref(l0), Self::Ref(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<Gc, C> Eq for RustCallable<Gc, C> {}

impl<Gc, C> Hash for RustCallable<Gc, C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            Self::Mut(t) => t.hash(state),
            Self::Ref(t) => t.hash(state),
        }
    }
}

impl<Gc, C> LuaFfiOnce<Gc, C> for RustCallable<Gc, C>
where
    Gc: TypeProvider,
    Value<Gc>: Display,
{
    fn call_once(
        self,
        mut rt: crate::runtime::RuntimeView<'_, Gc, C>,
    ) -> Result<(), RuntimeError<Gc>> {
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

impl<Gc, C> From<RustClosureMut<Gc, C>> for RustCallable<Gc, C> {
    fn from(value: RustClosureMut<Gc, C>) -> Self {
        Self::Mut(value)
    }
}

impl<Gc, C> From<RustClosureRef<Gc, C>> for RustCallable<Gc, C> {
    fn from(value: RustClosureRef<Gc, C>) -> Self {
        Self::Ref(value)
    }
}

impl<Gc> From<LuaClosureRef> for Value<Gc>
where
    Gc: TypeProvider,
{
    fn from(value: LuaClosureRef) -> Self {
        Value::Function(value.into())
    }
}

impl<Gc, C> From<RustCallable<Gc, C>> for Value<Gc>
where
    Gc: TypeProvider<RustCallable = RustCallable<Gc, C>>,
{
    fn from(value: RustCallable<Gc, C>) -> Self {
        Value::Function(value.into())
    }
}

impl<Gc, C> From<RustClosureRef<Gc, C>> for Value<Gc>
where
    Gc: TypeProvider<RustCallable = RustCallable<Gc, C>>,
{
    fn from(value: RustClosureRef<Gc, C>) -> Self {
        Value::Function(value.into())
    }
}

impl<Gc, C> From<RustClosureMut<Gc, C>> for Value<Gc>
where
    Gc: TypeProvider<RustCallable = RustCallable<Gc, C>>,
{
    fn from(value: RustClosureMut<Gc, C>) -> Self {
        Value::Function(value.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Callable<RustCallable> {
    Lua(LuaClosureRef),
    Rust(RustCallable),
}

impl<RustCallable> Display for Callable<RustCallable>
where
    RustCallable: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lua(t) => write!(f, "{t}"),
            Self::Rust(t) => write!(f, "{t}"),
        }
    }
}

impl<Gc, C> LuaFfiOnce<Gc, C> for Callable<Gc::RustCallable>
where
    C: crate::chunk_cache::ChunkCache,
    Gc: GarbageCollector,
    Gc::RustCallable: LuaFfiOnce<Gc, C>,
    Value<Gc>: Debug + Display,
{
    fn call_once(
        self,
        mut rt: crate::runtime::RuntimeView<'_, Gc, C>,
    ) -> Result<(), RuntimeError<Gc>> {
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

impl<Gc, C> From<RustCallable<Gc, C>> for Callable<RustCallable<Gc, C>> {
    fn from(value: RustCallable<Gc, C>) -> Self {
        Self::Rust(value)
    }
}

impl<Gc, C> From<RustClosureMut<Gc, C>> for Callable<RustCallable<Gc, C>> {
    fn from(value: RustClosureMut<Gc, C>) -> Self {
        Self::Rust(value.into())
    }
}

impl<Gc, C> From<RustClosureRef<Gc, C>> for Callable<RustCallable<Gc, C>> {
    fn from(value: RustClosureRef<Gc, C>) -> Self {
        Self::Rust(value.into())
    }
}

impl<Gc: TypeProvider> TryFrom<Value<Gc>> for Callable<Gc::RustCallable> {
    type Error = TypeMismatchError;

    fn try_from(value: Value<Gc>) -> Result<Self, Self::Error> {
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

impl<Gc: TypeProvider> From<Callable<Gc::RustCallable>> for Value<Gc> {
    fn from(value: Callable<Gc::RustCallable>) -> Self {
        Value::Function(value)
    }
}
