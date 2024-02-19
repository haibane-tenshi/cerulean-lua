use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

use gc::{Heap, Trace};

use crate::error::RuntimeError;
use crate::ffi::{DebugInfo, LuaFfi, LuaFfiMut, LuaFfiOnce, LuaFfiPtr};
use crate::gc::TryFromWithGc;

use super::{CoreTypes, RootValue, Strong, TypeMismatchError, Types, Value, Weak};

pub use crate::runtime::Closure as LuaClosure;

pub struct RustClosureRef<Ty>(Rc<dyn LuaFfiAndTrace<Ty> + 'static>);

trait LuaFfiAndTrace<Ty>: LuaFfi<Ty> + Trace
where
    Ty: CoreTypes,
{
}

impl<Ty, T> LuaFfiAndTrace<Ty> for T
where
    Ty: CoreTypes,
    T: LuaFfi<Ty> + Trace,
{
}

impl<Ty> RustClosureRef<Ty>
where
    Ty: CoreTypes,
{
    pub fn new(closure: impl LuaFfi<Ty> + 'static, trace: impl Trace) -> Self {
        struct Inner<T, F> {
            trace: T,
            func: F,
        }

        impl<T, F> Trace for Inner<T, F>
        where
            T: Trace,
            F: 'static,
        {
            fn trace(&self, collector: &mut gc::Collector) {
                self.trace.trace(collector)
            }
        }

        impl<Ty, T, F> LuaFfiOnce<Ty> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfiOnce<Ty>,
        {
            fn call_once(
                self,
                rt: crate::runtime::RuntimeView<'_, Ty>,
            ) -> Result<(), RuntimeError<Ty>> {
                self.func.call_once(rt)
            }

            fn debug_info(&self) -> DebugInfo {
                self.func.debug_info()
            }
        }

        impl<Ty, T, F> LuaFfiMut<Ty> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfiMut<Ty>,
        {
            fn call_mut(
                &mut self,
                rt: crate::runtime::RuntimeView<'_, Ty>,
            ) -> Result<(), RuntimeError<Ty>> {
                self.func.call_mut(rt)
            }
        }

        impl<Ty, T, F> LuaFfi<Ty> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfi<Ty>,
        {
            fn call(
                &self,
                rt: crate::runtime::RuntimeView<'_, Ty>,
            ) -> Result<(), RuntimeError<Ty>> {
                self.func.call(rt)
            }
        }

        let inner = Inner {
            trace,
            func: closure,
        };

        RustClosureRef(Rc::new(inner))
    }

    pub fn new_mut(closure: impl LuaFfiMut<Ty> + 'static, trace: impl Trace) -> Self {
        use crate::error::BorrowError;
        use crate::ffi::WithName;
        use crate::runtime::RuntimeView;
        use std::cell::RefCell;

        let name = closure.debug_info().name;
        let original = RefCell::new(closure);
        let value = (move |rt: RuntimeView<'_, Ty>| {
            let mut f = original.try_borrow_mut().map_err(|_| BorrowError::Mut)?;
            f.call_mut(rt)
        })
        .with_name(name);

        Self::new(value, trace)
    }

    pub fn new_once(closure: impl LuaFfiOnce<Ty> + 'static, trace: impl Trace) -> Self {
        use crate::error::AlreadyDroppedError;
        use std::cell::Cell;

        struct Inner<T, F> {
            trace: T,
            func: Cell<Option<F>>,
            debug_info: DebugInfo,
        }

        impl<T, F> Trace for Inner<T, F>
        where
            T: Trace,
            F: 'static,
        {
            fn trace(&self, collector: &mut gc::Collector) {
                // Don't trace when closure is already dropped.
                let func = self.func.take();
                if func.is_some() {
                    self.trace.trace(collector)
                }
                self.func.set(func);
            }
        }

        impl<Ty, T, F> LuaFfiOnce<Ty> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfiOnce<Ty>,
        {
            fn call_once(
                self,
                rt: crate::runtime::RuntimeView<'_, Ty>,
            ) -> Result<(), RuntimeError<Ty>> {
                self.call(rt)
            }

            fn debug_info(&self) -> DebugInfo {
                self.debug_info.clone()
            }
        }

        impl<Ty, T, F> LuaFfiMut<Ty> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfiOnce<Ty>,
        {
            fn call_mut(
                &mut self,
                rt: crate::runtime::RuntimeView<'_, Ty>,
            ) -> Result<(), RuntimeError<Ty>> {
                self.call(rt)
            }
        }

        impl<Ty, T, F> LuaFfi<Ty> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfiOnce<Ty>,
        {
            fn call(
                &self,
                rt: crate::runtime::RuntimeView<'_, Ty>,
            ) -> Result<(), RuntimeError<Ty>> {
                let f = self.func.take().ok_or(AlreadyDroppedError)?;
                f.call_once(rt)
            }
        }

        let debug_info = closure.debug_info();
        let inner = Inner {
            trace,
            func: Cell::new(Some(closure)),
            debug_info,
        };

        RustClosureRef(Rc::new(inner))
    }

    pub fn new_untrace(closure: impl LuaFfi<Ty> + 'static) -> Self {
        Self::new(closure, ())
    }

    pub fn new_untrace_mut(closure: impl LuaFfiMut<Ty> + 'static) -> Self {
        Self::new_mut(closure, ())
    }

    pub fn new_untrace_once(closure: impl LuaFfiOnce<Ty> + 'static) -> Self {
        Self::new_once(closure, ())
    }
}

impl<Ty> Debug for RustClosureRef<Ty> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RustClosureRef").field(&"<omitted>").finish()
    }
}

impl<Ty> Clone for RustClosureRef<Ty> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Ty> PartialEq for RustClosureRef<Ty> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Ty> Eq for RustClosureRef<Ty> {}

impl<Ty> Hash for RustClosureRef<Ty> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<Ty> LuaFfiOnce<Ty> for RustClosureRef<Ty>
where
    Ty: CoreTypes,
{
    fn call_once(self, rt: crate::runtime::RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>> {
        self.call(rt)
    }

    fn debug_info(&self) -> crate::ffi::DebugInfo {
        self.0.debug_info()
    }
}

impl<Ty> LuaFfiMut<Ty> for RustClosureRef<Ty>
where
    Ty: CoreTypes,
{
    fn call_mut(
        &mut self,
        rt: crate::runtime::RuntimeView<'_, Ty>,
    ) -> Result<(), RuntimeError<Ty>> {
        self.call(rt)
    }
}

impl<Ty> LuaFfi<Ty> for RustClosureRef<Ty>
where
    Ty: CoreTypes,
{
    fn call(&self, rt: crate::runtime::RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>> {
        self.0.call(rt)
    }
}

impl<Ty> Trace for RustClosureRef<Ty>
where
    Ty: CoreTypes + 'static,
{
    fn trace(&self, collector: &mut gc::Collector) {
        self.0.trace(collector)
    }
}

pub enum RustCallable<Ty, Closure>
where
    Ty: CoreTypes,
{
    Ref(Closure),
    Ptr(LuaFfiPtr<Ty>),
}

impl<Ty, Closure> Trace for RustCallable<Ty, Closure>
where
    Ty: CoreTypes + 'static,
    Closure: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        match self {
            RustCallable::Ptr(t) => t.trace(collector),
            RustCallable::Ref(t) => t.trace(collector),
        }
    }
}

impl<Ty, Closure> Debug for RustCallable<Ty, Closure>
where
    Ty: CoreTypes,
    Closure: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ref(arg0) => f.debug_tuple("Ref").field(arg0).finish(),
            Self::Ptr(arg0) => f.debug_tuple("Ptr").field(arg0).finish(),
        }
    }
}

impl<Ty, Closure> Display for RustCallable<Ty, Closure>
where
    Ty: CoreTypes,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // let name = self.debug_info().name;

        write!(f, "{{[rust] closure <name omitted>}}")
    }
}

impl<Ty, Closure> Clone for RustCallable<Ty, Closure>
where
    Ty: CoreTypes,
    Closure: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Ref(arg0) => Self::Ref(arg0.clone()),
            Self::Ptr(arg0) => Self::Ptr(*arg0),
        }
    }
}

impl<Ty, Closure> Copy for RustCallable<Ty, Closure>
where
    Ty: CoreTypes,
    Closure: Copy,
{
}

impl<Ty, Closure> PartialEq for RustCallable<Ty, Closure>
where
    Ty: CoreTypes,
    Closure: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ref(l0), Self::Ref(r0)) => l0 == r0,
            (Self::Ptr(l0), Self::Ptr(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<Ty, Closure> Eq for RustCallable<Ty, Closure>
where
    Ty: CoreTypes,
    Closure: Eq,
{
}

impl<Ty, Closure> Hash for RustCallable<Ty, Closure>
where
    Ty: CoreTypes,
    Closure: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            Self::Ref(t) => t.hash(state),
            Self::Ptr(t) => t.hash(state),
        }
    }
}

impl<Ty, Closure> LuaFfiOnce<Ty> for RustCallable<Ty, Closure>
where
    Ty: CoreTypes,
    Closure: LuaFfiOnce<Ty>,
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

impl<Ty, Closure> From<LuaFfiPtr<Ty>> for RustCallable<Ty, Closure>
where
    Ty: CoreTypes,
{
    fn from(value: LuaFfiPtr<Ty>) -> Self {
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

impl<Ty> TryFromWithGc<Callable<Weak<Ty>>, Heap> for Callable<Strong<Ty>>
where
    Ty: CoreTypes,
{
    type Error = crate::error::AlreadyDroppedError;

    fn try_from_with_gc(value: Callable<Weak<Ty>>, gc: &mut Heap) -> Result<Self, Self::Error> {
        use crate::gc::TryIntoWithGc;

        let r = match value {
            Callable::Rust(RustCallable::Ptr(t)) => Callable::Rust(RustCallable::Ptr(t)),
            Callable::Rust(RustCallable::Ref(t)) => {
                Callable::Rust(RustCallable::Ref(t.try_into_with_gc(gc)?))
            }
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

impl<Ty> LuaFfiOnce<Ty> for Callable<Strong<Ty>>
where
    Ty: CoreTypes,
    Ty::RustClosure: LuaFfi<Ty>,
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
