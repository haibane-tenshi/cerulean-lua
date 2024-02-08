use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

use crate::error::RuntimeError;
use crate::ffi::{DebugInfo, LuaFfi, LuaFfiFnPtr, LuaFfiMut, LuaFfiOnce};
use crate::gc::Gc as GarbageCollector;

use super::{TypeMismatchError, TypeProvider, Value};

pub use crate::runtime::{Closure as LuaClosure, ClosureRef as LuaClosureRef};

pub struct RustClosureRef<Gc>(Rc<dyn LuaFfi<Gc> + 'static>);

impl<Gc> RustClosureRef<Gc>
where
    Gc: TypeProvider,
{
    pub fn new(value: impl LuaFfi<Gc> + 'static) -> Self {
        let rc = Rc::new(value);
        RustClosureRef(rc)
    }

    pub fn new_mut(value: impl LuaFfiMut<Gc> + 'static) -> Self {
        use crate::error::BorrowError;
        use crate::ffi::WithName;
        use crate::runtime::RuntimeView;
        use std::cell::RefCell;

        let name = value.debug_info().name;
        let original = RefCell::new(value);
        let value = (move |rt: RuntimeView<'_, Gc>| {
            let mut f = original.try_borrow_mut().map_err(|_| BorrowError::Mut)?;
            f.call_mut(rt)
        })
        .with_name(name);

        Self::new(value)
    }

    pub fn new_once(value: impl LuaFfiOnce<Gc> + 'static) -> Self {
        use crate::error::AlreadyDroppedError;
        use crate::ffi::WithName;
        use crate::runtime::RuntimeView;
        use std::cell::Cell;

        let name = value.debug_info().name;
        let original = Cell::new(Some(value));
        let value = (move |rt: RuntimeView<'_, Gc>| {
            let f = original.take().ok_or(AlreadyDroppedError)?;
            f.call_once(rt)
        })
        .with_name(name);

        Self::new(value)
    }

    pub fn with_name(name: impl AsRef<str> + 'static, value: impl LuaFfi<Gc> + 'static) -> Self {
        use crate::ffi::WithName;

        Self::new(value.with_name(name))
    }
}

impl<Gc> Debug for RustClosureRef<Gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RustClosureRef").field(&"<omitted>").finish()
    }
}

impl<Gc> Clone for RustClosureRef<Gc> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Gc> PartialEq for RustClosureRef<Gc> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Gc> Eq for RustClosureRef<Gc> {}

impl<Gc> Hash for RustClosureRef<Gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<Gc> LuaFfiOnce<Gc> for RustClosureRef<Gc>
where
    Gc: TypeProvider,
{
    fn call_once(self, rt: crate::runtime::RuntimeView<'_, Gc>) -> Result<(), RuntimeError<Gc>> {
        self.call(rt)
    }

    fn debug_info(&self) -> crate::ffi::DebugInfo {
        self.0.debug_info()
    }
}

impl<Gc> LuaFfiMut<Gc> for RustClosureRef<Gc>
where
    Gc: TypeProvider,
{
    fn call_mut(
        &mut self,
        rt: crate::runtime::RuntimeView<'_, Gc>,
    ) -> Result<(), RuntimeError<Gc>> {
        self.call(rt)
    }
}

impl<Gc> LuaFfi<Gc> for RustClosureRef<Gc>
where
    Gc: TypeProvider,
{
    fn call(&self, rt: crate::runtime::RuntimeView<'_, Gc>) -> Result<(), RuntimeError<Gc>> {
        self.0.call(rt)
    }
}

pub enum RustCallable<Gc: TypeProvider> {
    Ref(RustClosureRef<Gc>),
    Ptr(LuaFfiFnPtr<Gc>),
}

impl<Gc> Debug for RustCallable<Gc>
where
    Gc: TypeProvider,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ref(arg0) => f.debug_tuple("Ref").field(arg0).finish(),
            Self::Ptr(arg0) => f.debug_tuple("Ptr").field(arg0).finish(),
        }
    }
}

impl<Gc> Display for RustCallable<Gc>
where
    Gc: TypeProvider,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // let name = self.debug_info().name;

        write!(f, "{{[rust] closure <name omitted>}}")
    }
}

impl<Gc> Clone for RustCallable<Gc>
where
    Gc: TypeProvider,
{
    fn clone(&self) -> Self {
        match self {
            Self::Ref(arg0) => Self::Ref(arg0.clone()),
            Self::Ptr(arg0) => Self::Ptr(arg0.clone()),
        }
    }
}

impl<Gc> PartialEq for RustCallable<Gc>
where
    Gc: TypeProvider,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ref(l0), Self::Ref(r0)) => l0 == r0,
            (Self::Ptr(l0), Self::Ptr(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<Gc> Eq for RustCallable<Gc> where Gc: TypeProvider {}

impl<Gc> Hash for RustCallable<Gc>
where
    Gc: TypeProvider,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            Self::Ref(t) => t.hash(state),
            Self::Ptr(t) => t.hash(state),
        }
    }
}

impl<Gc> LuaFfiOnce<Gc> for RustCallable<Gc>
where
    Gc: TypeProvider,
    Value<Gc>: Display,
{
    fn call_once(
        self,
        mut rt: crate::runtime::RuntimeView<'_, Gc>,
    ) -> Result<(), RuntimeError<Gc>> {
        match self {
            Self::Ref(f) => rt.invoke(f),
            Self::Ptr(f) => rt.invoke(f),
        }
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: "{rust callable wrapper}".to_string(),
        }
    }
}

impl<Gc> From<RustClosureRef<Gc>> for RustCallable<Gc>
where
    Gc: TypeProvider,
{
    fn from(value: RustClosureRef<Gc>) -> Self {
        Self::Ref(value)
    }
}

impl<Gc> From<LuaFfiFnPtr<Gc>> for RustCallable<Gc>
where
    Gc: TypeProvider,
{
    fn from(value: LuaFfiFnPtr<Gc>) -> Self {
        Self::Ptr(value)
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

impl<Gc> From<RustCallable<Gc>> for Value<Gc>
where
    Gc: TypeProvider<RustCallable = RustCallable<Gc>>,
{
    fn from(value: RustCallable<Gc>) -> Self {
        Value::Function(value.into())
    }
}

impl<Gc> From<RustClosureRef<Gc>> for Value<Gc>
where
    Gc: TypeProvider<RustCallable = RustCallable<Gc>>,
{
    fn from(value: RustClosureRef<Gc>) -> Self {
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

impl<Gc> LuaFfiOnce<Gc> for Callable<Gc::RustCallable>
where
    Gc: GarbageCollector,
    Gc::RustCallable: LuaFfiOnce<Gc>,
    Gc::Table: for<'a> crate::gc::Visit<Gc::Sweeper<'a>>,
    Value<Gc>: Debug + Display,
{
    fn call_once(
        self,
        mut rt: crate::runtime::RuntimeView<'_, Gc>,
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

impl<Gc> From<RustCallable<Gc>> for Callable<RustCallable<Gc>>
where
    Gc: TypeProvider,
{
    fn from(value: RustCallable<Gc>) -> Self {
        Self::Rust(value)
    }
}

impl<Gc> From<RustClosureRef<Gc>> for Callable<RustCallable<Gc>>
where
    Gc: TypeProvider,
{
    fn from(value: RustClosureRef<Gc>) -> Self {
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
