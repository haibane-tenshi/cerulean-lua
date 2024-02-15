use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

use gc::{Heap, Trace};

use crate::error::RuntimeError;
use crate::ffi::{DebugInfo, LuaFfi, LuaFfiFnPtr, LuaFfiMut, LuaFfiOnce};
use crate::gc::TryFromWithGc;

use super::{RootValue, Strong, TypeMismatchError, TypeProvider, Types, Value, Weak};

pub use crate::runtime::Closure as LuaClosure;

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

pub enum RustCallable<Ty>
where
    Ty: TypeProvider,
{
    Ref(RustClosureRef<Ty>),
    Ptr(LuaFfiFnPtr<Ty>),
}

impl<Ty> Trace for RustCallable<Ty>
where
    Ty: TypeProvider + 'static,
{
    fn trace(&self, collector: &mut gc::Collector) {
        todo!()
    }
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

impl<Ty> LuaFfiOnce<Ty> for RustCallable<Ty>
where
    Ty: TypeProvider,
    RootValue<Ty>: Display,
{
    fn call_once(
        self,
        mut rt: crate::runtime::RuntimeView<'_, Ty>,
    ) -> Result<(), RuntimeError<Ty>> {
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

pub enum Callable<Ty>
where
    Ty: Types,
{
    Lua(Ty::LuaCallable),
    Rust(Ty::RustCallable),
}

pub type RootCallable<Ty> = Callable<Strong<Ty>>;
pub type GcCallable<Ty> = Callable<Weak<Ty>>;

impl<Ty> TryFromWithGc<GcCallable<Ty>, Heap> for RootCallable<Ty>
where
    Ty: TypeProvider,
{
    type Error = crate::error::AlreadyDroppedError;

    fn try_from_with_gc(value: GcCallable<Ty>, gc: &mut Heap) -> Result<Self, Self::Error> {
        use crate::gc::TryIntoWithGc;

        let r = match value {
            Callable::Rust(t) => Callable::Rust(t),
            Callable::Lua(t) => Callable::Lua(t.try_into_with_gc(gc)?),
        };

        Ok(r)
    }
}

impl<Ty> Trace for Callable<Ty>
where
    Ty: Types + 'static,
    Ty::LuaCallable: Trace,
    Ty::RustCallable: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        match self {
            Callable::Lua(t) => t.trace(collector),
            Callable::Rust(t) => t.trace(collector),
        }
    }
}

impl<Ty> Debug for Callable<Ty>
where
    Ty: Types,
    Ty::LuaCallable: Debug,
    Ty::RustCallable: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lua(arg0) => f.debug_tuple("Lua").field(arg0).finish(),
            Self::Rust(arg0) => f.debug_tuple("Rust").field(arg0).finish(),
        }
    }
}

impl<Ty> Display for Callable<Ty>
where
    Ty: Types,
    Ty::LuaCallable: Display,
    Ty::RustCallable: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lua(t) => write!(f, "{t}"),
            Self::Rust(t) => write!(f, "{t}"),
        }
    }
}

impl<Ty> Clone for Callable<Ty>
where
    Ty: Types,
    Ty::LuaCallable: Clone,
    Ty::RustCallable: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Lua(arg0) => Self::Lua(arg0.clone()),
            Self::Rust(arg0) => Self::Rust(arg0.clone()),
        }
    }
}

impl<Ty> Copy for Callable<Ty>
where
    Ty: Types,
    Ty::LuaCallable: Copy,
    Ty::RustCallable: Copy,
{
}

impl<Ty> PartialEq for Callable<Ty>
where
    Ty: Types,
    Ty::LuaCallable: PartialEq,
    Ty::RustCallable: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Lua(l0), Self::Lua(r0)) => l0 == r0,
            (Self::Rust(l0), Self::Rust(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<Ty> Eq for Callable<Ty>
where
    Ty: Types,
    Ty::LuaCallable: Eq,
    Ty::RustCallable: Eq,
{
}

impl<Ty> Hash for Callable<Ty>
where
    Ty: Types,
    Ty::LuaCallable: Hash,
    Ty::RustCallable: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            Callable::Lua(t) => t.hash(state),
            Callable::Rust(t) => t.hash(state),
        }
    }
}

impl<Ty> LuaFfiOnce<Ty> for RootCallable<Ty>
where
    Ty: TypeProvider,
    Ty::RustCallable: LuaFfiOnce<Ty>,
    RootValue<Ty>: Display,
{
    fn call_once(
        self,
        mut rt: crate::runtime::RuntimeView<'_, Ty>,
    ) -> Result<(), RuntimeError<Ty>> {
        use repr::index::StackSlot;

        match self {
            Callable::Lua(f) => rt.enter(f.into(), StackSlot(0)),
            Callable::Rust(f) => rt.invoke(f),
        }
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: "{generic callable wrapper}".to_string(),
        }
    }
}

impl<Ty> TryFrom<Value<Ty>> for Callable<Ty>
where
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Ty>) -> Result<Self, Self::Error> {
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

impl<Ty: Types> From<Callable<Ty>> for Value<Ty> {
    fn from(value: Callable<Ty>) -> Self {
        Value::Function(value)
    }
}
