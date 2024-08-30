use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

use gc::Trace;

use crate::error::RuntimeError;
use crate::ffi::{DebugInfo, LuaFfi, LuaFfiMut, LuaFfiOnce, LuaFfiPtr};
use crate::gc::{DisplayWith, Heap, TryFromWithGc};

use super::{
    CoreTypes, Strong, StrongValue, TableIndex, TypeMismatchError, Types, Value, Weak, WeakValue,
};

pub use crate::runtime::{Closure as LuaClosure, RuntimeView};

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
                rt: RuntimeView<'_, Ty>,
            ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
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
                rt: RuntimeView<'_, Ty>,
            ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
                self.func.call_mut(rt)
            }
        }

        impl<Ty, T, F> LuaFfi<Ty> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfi<Ty>,
        {
            fn call(&self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<StrongValue<Ty>>> {
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
        use std::cell::RefCell;
        use RuntimeView;

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
                rt: RuntimeView<'_, Ty>,
            ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
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
                rt: RuntimeView<'_, Ty>,
            ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
                self.call(rt)
            }
        }

        impl<Ty, T, F> LuaFfi<Ty> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfiOnce<Ty>,
        {
            fn call(&self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<StrongValue<Ty>>> {
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
    fn call_once(self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<StrongValue<Ty>>> {
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
    fn call_mut(&mut self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<StrongValue<Ty>>> {
        self.call(rt)
    }
}

impl<Ty> LuaFfi<Ty> for RustClosureRef<Ty>
where
    Ty: CoreTypes,
{
    fn call(&self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<StrongValue<Ty>>> {
        self.0.call(rt)
    }
}

impl<Ty> Trace for RustClosureRef<Ty>
where
    Ty: CoreTypes,
{
    fn trace(&self, collector: &mut gc::Collector) {
        self.0.trace(collector)
    }
}

pub enum RustCallable<Ty>
where
    Ty: CoreTypes,
{
    Ref(RustClosureRef<Ty>),
    Ptr(LuaFfiPtr<Ty>),
}

impl<Ty> Trace for RustCallable<Ty>
where
    Ty: CoreTypes,
{
    fn trace(&self, collector: &mut gc::Collector) {
        match self {
            RustCallable::Ptr(t) => t.trace(collector),
            RustCallable::Ref(t) => t.trace(collector),
        }
    }
}

impl<Ty> Debug for RustCallable<Ty>
where
    Ty: CoreTypes,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ref(arg0) => f.debug_tuple("Ref").field(arg0).finish(),
            Self::Ptr(arg0) => f.debug_tuple("Ptr").field(arg0).finish(),
        }
    }
}

impl<Ty> Display for RustCallable<Ty>
where
    Ty: CoreTypes,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // let name = self.debug_info().name;

        write!(f, "{{[rust] closure <name omitted>}}")
    }
}

impl<Ty> Clone for RustCallable<Ty>
where
    Ty: CoreTypes,
{
    fn clone(&self) -> Self {
        match self {
            Self::Ref(arg0) => Self::Ref(arg0.clone()),
            Self::Ptr(arg0) => Self::Ptr(*arg0),
        }
    }
}

impl<Ty> PartialEq for RustCallable<Ty>
where
    Ty: CoreTypes,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ref(l0), Self::Ref(r0)) => l0 == r0,
            (Self::Ptr(l0), Self::Ptr(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<Ty> Eq for RustCallable<Ty> where Ty: CoreTypes {}

impl<Ty> Hash for RustCallable<Ty>
where
    Ty: CoreTypes,
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
    Ty: CoreTypes,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
{
    fn call_once(self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<StrongValue<Ty>>> {
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

impl<Ty> From<LuaFfiPtr<Ty>> for RustCallable<Ty>
where
    Ty: CoreTypes,
{
    fn from(value: LuaFfiPtr<Ty>) -> Self {
        Self::Ptr(value)
    }
}

pub type StrongCallable<Ty> = Callable<Strong, Ty>;
pub type WeakCallable<Ty> = Callable<Weak, Ty>;

pub enum Callable<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
{
    Lua(Rf::LuaCallable<Ty::LuaClosure>),
    Rust(Rf::RustCallable<Ty::RustClosure>),
}

impl<Ty> WeakCallable<Ty>
where
    Ty: CoreTypes,
{
    pub fn upgrade(self, heap: &Heap<Ty>) -> Option<StrongCallable<Ty>> {
        let r = match self {
            Callable::Rust(t) => Callable::Rust(t.upgrade(heap)?),
            Callable::Lua(t) => {
                let t = t.upgrade(heap)?;
                Callable::Lua(t)
            }
        };

        Some(r)
    }
}

impl<Ty> StrongCallable<Ty>
where
    Ty: CoreTypes,
{
    pub fn downgrade(&self) -> WeakCallable<Ty> {
        match self {
            Callable::Rust(t) => Callable::Rust(t.downgrade()),
            Callable::Lua(t) => Callable::Lua(t.downgrade()),
        }
    }
}

impl<Ty> TryFromWithGc<Callable<Weak, Ty>, Heap<Ty>> for Callable<Strong, Ty>
where
    Ty: CoreTypes,
{
    type Error = crate::error::AlreadyDroppedError;

    fn try_from_with_gc(
        value: Callable<Weak, Ty>,
        heap: &mut Heap<Ty>,
    ) -> Result<Self, Self::Error> {
        use crate::error::AlreadyDroppedError;

        value.upgrade(heap).ok_or(AlreadyDroppedError)
    }
}

impl<Rf, Ty> Trace for Callable<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::LuaCallable<Ty::LuaClosure>: Trace,
    Rf::RustCallable<Ty::RustClosure>: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        match self {
            Callable::Lua(t) => t.trace(collector),
            Callable::Rust(t) => t.trace(collector),
        }
    }
}

impl<Rf, Ty> Debug for Callable<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::LuaCallable<Ty::LuaClosure>: Debug,
    Rf::RustCallable<Ty::RustClosure>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lua(arg0) => f.debug_tuple("Lua").field(arg0).finish(),
            Self::Rust(arg0) => f.debug_tuple("Rust").field(arg0).finish(),
        }
    }
}

impl<Rf, Ty> Display for Callable<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::LuaCallable<Ty::LuaClosure>: Display,
    Rf::RustCallable<Ty::RustClosure>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lua(t) => write!(f, "{t}"),
            Self::Rust(t) => write!(f, "{t}"),
        }
    }
}

impl<Rf, Ty> Clone for Callable<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::LuaCallable<Ty::LuaClosure>: Clone,
    Rf::RustCallable<Ty::RustClosure>: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Lua(arg0) => Self::Lua(arg0.clone()),
            Self::Rust(arg0) => Self::Rust(arg0.clone()),
        }
    }
}

impl<Rf, Ty> Copy for Callable<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::LuaCallable<Ty::LuaClosure>: Copy,
    Rf::RustCallable<Ty::RustClosure>: Copy,
{
}

impl<Rf, Ty> PartialEq for Callable<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::LuaCallable<Ty::LuaClosure>: PartialEq,
    Rf::RustCallable<Ty::RustClosure>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Lua(l0), Self::Lua(r0)) => l0 == r0,
            (Self::Rust(l0), Self::Rust(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<Rf, Ty> Eq for Callable<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::LuaCallable<Ty::LuaClosure>: Eq,
    Rf::RustCallable<Ty::RustClosure>: Eq,
{
}

impl<Rf, Ty> Hash for Callable<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::LuaCallable<Ty::LuaClosure>: Hash,
    Rf::RustCallable<Ty::RustClosure>: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            Callable::Lua(t) => t.hash(state),
            Callable::Rust(t) => t.hash(state),
        }
    }
}

impl<Ty> LuaFfiOnce<Ty> for Callable<Strong, Ty>
where
    Ty: CoreTypes<LuaClosure = LuaClosure<Ty>>,
    Ty::Table: TableIndex<Weak, Ty>,
    Ty::RustClosure: LuaFfi<Ty>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
{
    fn call_once(self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<StrongValue<Ty>>> {
        use crate::gc::LuaPtr;

        match self {
            Callable::Lua(LuaPtr(f)) => rt.enter(f),
            Callable::Rust(f) => rt.invoke(f),
        }
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: "{generic callable wrapper}".to_string(),
        }
    }
}

impl<Ty> LuaFfiOnce<Ty> for Callable<Weak, Ty>
where
    Ty: CoreTypes<LuaClosure = LuaClosure<Ty>>,
    Ty::Table: TableIndex<Weak, Ty>,
    Ty::RustClosure: LuaFfi<Ty>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
{
    fn call_once(self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<StrongValue<Ty>>> {
        use crate::error::AlreadyDroppedError;
        use crate::gc::LuaPtr;

        match self {
            Callable::Lua(LuaPtr(f)) => {
                let f = rt.core.gc.upgrade(f).ok_or(AlreadyDroppedError)?;
                rt.enter(f)
            }
            Callable::Rust(f) => rt.invoke(f),
        }
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: "{generic callable wrapper}".to_string(),
        }
    }
}

impl<Rf, Ty> TryFrom<Value<Rf, Ty>> for Callable<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Rf, Ty>) -> Result<Self, Self::Error> {
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

impl<Rf, Ty> From<Callable<Rf, Ty>> for Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
{
    fn from(value: Callable<Rf, Ty>) -> Self {
        Value::Function(value)
    }
}
