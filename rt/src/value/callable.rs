use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

use crate::error::RuntimeError;
use crate::ffi::{DebugInfo, LuaFfi, LuaFfiMut, LuaFfiOnce};

use super::{TypeMismatchError, TypeProvider, Value};

pub use crate::runtime::{Closure as LuaClosure, ClosureRef as LuaClosureRef};

pub struct RustClosureMut<Types, C>(Rc<Inner<RefCell<dyn LuaFfiMut<Types, C> + 'static>>>);

struct Inner<T: ?Sized> {
    debug_info: DebugInfo,
    callable: T,
}

impl<Types, C> RustClosureMut<Types, C>
where
    Types: TypeProvider,
{
    pub fn new(value: impl LuaFfiMut<Types, C> + 'static) -> Self {
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
        value: impl LuaFfiMut<Types, C> + 'static,
    ) -> Self {
        use crate::ffi::WithName;

        Self::new(value.with_name(name))
    }
}

impl<Types, C> Debug for RustClosureMut<Types, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RustClosureMut").field(&"<omitted>").finish()
    }
}

impl<Types, C> Clone for RustClosureMut<Types, C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Types, C> PartialEq for RustClosureMut<Types, C> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Types, C> Eq for RustClosureMut<Types, C> {}

impl<Types, C> Hash for RustClosureMut<Types, C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<Types, C> LuaFfiOnce<Types, C> for RustClosureMut<Types, C>
where
    Types: TypeProvider,
    Types::String: From<&'static str>,
{
    fn call_once(
        mut self,
        rt: crate::runtime::RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
        self.call_mut(rt)
    }

    fn debug_info(&self) -> crate::ffi::DebugInfo {
        self.0.debug_info.clone()
    }
}

impl<Types, C> LuaFfiMut<Types, C> for RustClosureMut<Types, C>
where
    Types: TypeProvider,
    Types::String: From<&'static str>,
{
    fn call_mut(
        &mut self,
        rt: crate::runtime::RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
        let mut f = self
            .0
            .callable
            .try_borrow_mut()
            .map_err(|_| Value::String("failed to mutably borrow closure".into()))?;
        f.call_mut(rt)
    }
}

pub struct RustClosureRef<Types, C>(Rc<dyn LuaFfi<Types, C> + 'static>);

impl<Types, C> RustClosureRef<Types, C>
where
    Types: TypeProvider,
{
    pub fn new(value: impl LuaFfi<Types, C> + 'static) -> Self {
        let rc = Rc::new(value);
        RustClosureRef(rc)
    }

    pub fn with_name(
        name: impl AsRef<str> + 'static,
        value: impl LuaFfi<Types, C> + 'static,
    ) -> Self {
        use crate::ffi::WithName;

        Self::new(value.with_name(name))
    }
}

impl<Types, C> Debug for RustClosureRef<Types, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RustClosureRef").field(&"<omitted>").finish()
    }
}

impl<Types, C> Clone for RustClosureRef<Types, C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Types, C> PartialEq for RustClosureRef<Types, C> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Types, C> Eq for RustClosureRef<Types, C> {}

impl<Types, C> Hash for RustClosureRef<Types, C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<Types, C> LuaFfiOnce<Types, C> for RustClosureRef<Types, C>
where
    Types: TypeProvider,
{
    fn call_once(
        self,
        rt: crate::runtime::RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
        self.call(rt)
    }

    fn debug_info(&self) -> crate::ffi::DebugInfo {
        self.0.debug_info()
    }
}

impl<Types, C> LuaFfiMut<Types, C> for RustClosureRef<Types, C>
where
    Types: TypeProvider,
{
    fn call_mut(
        &mut self,
        rt: crate::runtime::RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
        self.call(rt)
    }
}

impl<Types, C> LuaFfi<Types, C> for RustClosureRef<Types, C>
where
    Types: TypeProvider,
{
    fn call(
        &self,
        rt: crate::runtime::RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
        self.0.call(rt)
    }
}

pub enum RustCallable<Types, C> {
    Mut(RustClosureMut<Types, C>),
    Ref(RustClosureRef<Types, C>),
}

impl<Types, C> Debug for RustCallable<Types, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mut(arg0) => f.debug_tuple("Mut").field(arg0).finish(),
            Self::Ref(arg0) => f.debug_tuple("Ref").field(arg0).finish(),
        }
    }
}

impl<Types, C> Display for RustCallable<Types, C>
where
    Types: TypeProvider,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // let name = self.debug_info().name;

        write!(f, "{{[rust] closure <name omitted>}}")
    }
}

impl<Types, C> Clone for RustCallable<Types, C> {
    fn clone(&self) -> Self {
        match self {
            Self::Mut(arg0) => Self::Mut(arg0.clone()),
            Self::Ref(arg0) => Self::Ref(arg0.clone()),
        }
    }
}

impl<Types, C> PartialEq for RustCallable<Types, C> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Mut(l0), Self::Mut(r0)) => l0 == r0,
            (Self::Ref(l0), Self::Ref(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<Types, C> Eq for RustCallable<Types, C> {}

impl<Types, C> Hash for RustCallable<Types, C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            Self::Mut(t) => t.hash(state),
            Self::Ref(t) => t.hash(state),
        }
    }
}

impl<Types, C> LuaFfiOnce<Types, C> for RustCallable<Types, C>
where
    Types: TypeProvider,
    Value<Types>: Display,
{
    fn call_once(
        self,
        mut rt: crate::runtime::RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
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

impl<Types, C> From<RustClosureMut<Types, C>> for RustCallable<Types, C> {
    fn from(value: RustClosureMut<Types, C>) -> Self {
        Self::Mut(value)
    }
}

impl<Types, C> From<RustClosureRef<Types, C>> for RustCallable<Types, C> {
    fn from(value: RustClosureRef<Types, C>) -> Self {
        Self::Ref(value)
    }
}

impl<Types> From<LuaClosureRef> for Value<Types>
where
    Types: TypeProvider,
{
    fn from(value: LuaClosureRef) -> Self {
        Value::Function(value.into())
    }
}

impl<Types, C> From<RustCallable<Types, C>> for Value<Types>
where
    Types: TypeProvider<RustCallable = RustCallable<Types, C>>,
{
    fn from(value: RustCallable<Types, C>) -> Self {
        Value::Function(value.into())
    }
}

impl<Types, C> From<RustClosureRef<Types, C>> for Value<Types>
where
    Types: TypeProvider<RustCallable = RustCallable<Types, C>>,
{
    fn from(value: RustClosureRef<Types, C>) -> Self {
        Value::Function(value.into())
    }
}

impl<Types, C> From<RustClosureMut<Types, C>> for Value<Types>
where
    Types: TypeProvider<RustCallable = RustCallable<Types, C>>,
{
    fn from(value: RustClosureMut<Types, C>) -> Self {
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

impl<Types, C> LuaFfiOnce<Types, C> for Callable<Types::RustCallable>
where
    C: crate::chunk_cache::ChunkCache,
    Types: TypeProvider,
    Types::RustCallable: LuaFfiOnce<Types, C>,
    Value<Types>: Debug + Display,
{
    fn call_once(
        self,
        mut rt: crate::runtime::RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
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

impl<Types, C> From<RustCallable<Types, C>> for Callable<RustCallable<Types, C>> {
    fn from(value: RustCallable<Types, C>) -> Self {
        Self::Rust(value)
    }
}

impl<Types, C> From<RustClosureMut<Types, C>> for Callable<RustCallable<Types, C>> {
    fn from(value: RustClosureMut<Types, C>) -> Self {
        Self::Rust(value.into())
    }
}

impl<Types, C> From<RustClosureRef<Types, C>> for Callable<RustCallable<Types, C>> {
    fn from(value: RustClosureRef<Types, C>) -> Self {
        Self::Rust(value.into())
    }
}

impl<Types: TypeProvider> TryFrom<Value<Types>> for Callable<Types::RustCallable> {
    type Error = TypeMismatchError;

    fn try_from(value: Value<Types>) -> Result<Self, Self::Error> {
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

impl<Types: TypeProvider> From<Callable<Types::RustCallable>> for Value<Types> {
    fn from(value: Callable<Types::RustCallable>) -> Self {
        Value::Function(value)
    }
}
