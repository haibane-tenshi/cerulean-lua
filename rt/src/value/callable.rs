use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

use gc::{Heap, Trace};

use crate::error::RuntimeError;
use crate::ffi::{DebugInfo, LuaFfi, LuaFfiMut, LuaFfiOnce, LuaFfiPtr};
use crate::gc::{DisplayWith, TryFromWithGc};

use super::{
    CoreTypes, Strong, StrongValue, TableIndex, TypeMismatchError, Types, Value, Weak, WeakValue,
};

pub use crate::runtime::Closure as LuaClosure;

pub struct RustClosureRef<Ty, Conv>(Rc<dyn LuaFfiAndTrace<Ty, Conv> + 'static>);

trait LuaFfiAndTrace<Ty, Conv>: LuaFfi<Ty, Conv> + Trace
where
    Ty: CoreTypes,
{
}

impl<Ty, Conv, T> LuaFfiAndTrace<Ty, Conv> for T
where
    Ty: CoreTypes,
    T: LuaFfi<Ty, Conv> + Trace,
{
}

impl<Ty, Conv> RustClosureRef<Ty, Conv>
where
    Ty: CoreTypes,
{
    pub fn new(closure: impl LuaFfi<Ty, Conv> + 'static, trace: impl Trace) -> Self {
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

        impl<Ty, Conv, T, F> LuaFfiOnce<Ty, Conv> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfiOnce<Ty, Conv>,
        {
            fn call_once(
                self,
                rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
            ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
                self.func.call_once(rt)
            }

            fn debug_info(&self) -> DebugInfo {
                self.func.debug_info()
            }
        }

        impl<Ty, Conv, T, F> LuaFfiMut<Ty, Conv> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfiMut<Ty, Conv>,
        {
            fn call_mut(
                &mut self,
                rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
            ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
                self.func.call_mut(rt)
            }
        }

        impl<Ty, Conv, T, F> LuaFfi<Ty, Conv> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfi<Ty, Conv>,
        {
            fn call(
                &self,
                rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
            ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
                self.func.call(rt)
            }
        }

        let inner = Inner {
            trace,
            func: closure,
        };

        RustClosureRef(Rc::new(inner))
    }

    pub fn new_mut(closure: impl LuaFfiMut<Ty, Conv> + 'static, trace: impl Trace) -> Self {
        use crate::error::BorrowError;
        use crate::ffi::WithName;
        use crate::runtime::RuntimeView;
        use std::cell::RefCell;

        let name = closure.debug_info().name;
        let original = RefCell::new(closure);
        let value = (move |rt: RuntimeView<'_, Ty, Conv>| {
            let mut f = original.try_borrow_mut().map_err(|_| BorrowError::Mut)?;
            f.call_mut(rt)
        })
        .with_name(name);

        Self::new(value, trace)
    }

    pub fn new_once(closure: impl LuaFfiOnce<Ty, Conv> + 'static, trace: impl Trace) -> Self {
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

        impl<Ty, Conv, T, F> LuaFfiOnce<Ty, Conv> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfiOnce<Ty, Conv>,
        {
            fn call_once(
                self,
                rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
            ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
                self.call(rt)
            }

            fn debug_info(&self) -> DebugInfo {
                self.debug_info.clone()
            }
        }

        impl<Ty, Conv, T, F> LuaFfiMut<Ty, Conv> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfiOnce<Ty, Conv>,
        {
            fn call_mut(
                &mut self,
                rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
            ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
                self.call(rt)
            }
        }

        impl<Ty, Conv, T, F> LuaFfi<Ty, Conv> for Inner<T, F>
        where
            Ty: CoreTypes,
            F: LuaFfiOnce<Ty, Conv>,
        {
            fn call(
                &self,
                rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
            ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
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

    pub fn new_untrace(closure: impl LuaFfi<Ty, Conv> + 'static) -> Self {
        Self::new(closure, ())
    }

    pub fn new_untrace_mut(closure: impl LuaFfiMut<Ty, Conv> + 'static) -> Self {
        Self::new_mut(closure, ())
    }

    pub fn new_untrace_once(closure: impl LuaFfiOnce<Ty, Conv> + 'static) -> Self {
        Self::new_once(closure, ())
    }
}

impl<Ty, Conv> Debug for RustClosureRef<Ty, Conv> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RustClosureRef").field(&"<omitted>").finish()
    }
}

impl<Ty, Conv> Clone for RustClosureRef<Ty, Conv> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Ty, Conv> PartialEq for RustClosureRef<Ty, Conv> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Ty, Conv> Eq for RustClosureRef<Ty, Conv> {}

impl<Ty, Conv> Hash for RustClosureRef<Ty, Conv> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<Ty, Conv> LuaFfiOnce<Ty, Conv> for RustClosureRef<Ty, Conv>
where
    Ty: CoreTypes,
{
    fn call_once(
        self,
        rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
        self.call(rt)
    }

    fn debug_info(&self) -> crate::ffi::DebugInfo {
        self.0.debug_info()
    }
}

impl<Ty, Conv> LuaFfiMut<Ty, Conv> for RustClosureRef<Ty, Conv>
where
    Ty: CoreTypes,
{
    fn call_mut(
        &mut self,
        rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
        self.call(rt)
    }
}

impl<Ty, Conv> LuaFfi<Ty, Conv> for RustClosureRef<Ty, Conv>
where
    Ty: CoreTypes,
{
    fn call(
        &self,
        rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
        self.0.call(rt)
    }
}

impl<Ty, Conv> Trace for RustClosureRef<Ty, Conv>
where
    Ty: CoreTypes,
    Conv: 'static,
{
    fn trace(&self, collector: &mut gc::Collector) {
        self.0.trace(collector)
    }
}

pub enum RustCallable<Ty, Conv>
where
    Ty: CoreTypes,
{
    Ref(RustClosureRef<Ty, Conv>),
    Ptr(LuaFfiPtr<Ty, Conv>),
}

impl<Ty, Conv> Trace for RustCallable<Ty, Conv>
where
    Ty: CoreTypes,
    Conv: 'static,
{
    fn trace(&self, collector: &mut gc::Collector) {
        match self {
            RustCallable::Ptr(t) => t.trace(collector),
            RustCallable::Ref(t) => t.trace(collector),
        }
    }
}

impl<Ty, Conv> Debug for RustCallable<Ty, Conv>
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

impl<Ty, Conv> Display for RustCallable<Ty, Conv>
where
    Ty: CoreTypes,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // let name = self.debug_info().name;

        write!(f, "{{[rust] closure <name omitted>}}")
    }
}

impl<Ty, Conv> Clone for RustCallable<Ty, Conv>
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

impl<Ty, Conv> PartialEq for RustCallable<Ty, Conv>
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

impl<Ty, Conv> Eq for RustCallable<Ty, Conv> where Ty: CoreTypes {}

impl<Ty, Conv> Hash for RustCallable<Ty, Conv>
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

impl<Ty, Conv> LuaFfiOnce<Ty, Conv> for RustCallable<Ty, Conv>
where
    Ty: CoreTypes,
    WeakValue<Ty>: DisplayWith<Heap>,
{
    fn call_once(
        self,
        rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
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

impl<Ty, Conv> From<LuaFfiPtr<Ty, Conv>> for RustCallable<Ty, Conv>
where
    Ty: CoreTypes,
{
    fn from(value: LuaFfiPtr<Ty, Conv>) -> Self {
        Self::Ptr(value)
    }
}

pub type StrongCallable<Ty> = Callable<Strong<Ty>>;
pub type WeakCallable<Ty> = Callable<Weak<Ty>>;

pub enum Callable<Ty>
where
    Ty: Types,
{
    Lua(Ty::LuaCallable),
    Rust(Ty::RustCallable),
}

impl<Ty> WeakCallable<Ty>
where
    Ty: CoreTypes,
{
    pub fn upgrade(self, heap: &Heap) -> Option<StrongCallable<Ty>> {
        let r = match self {
            Callable::Rust(t) => Callable::Rust(t.upgrade_cell(heap)?),
            Callable::Lua(t) => {
                let t = t.upgrade_cell(heap)?;
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
            Callable::Rust(t) => Callable::Rust(t.downgrade_cell()),
            Callable::Lua(t) => Callable::Lua(t.downgrade_cell()),
        }
    }
}

impl<Ty> TryFromWithGc<Callable<Weak<Ty>>, Heap> for Callable<Strong<Ty>>
where
    Ty: CoreTypes,
{
    type Error = crate::error::AlreadyDroppedError;

    fn try_from_with_gc(value: Callable<Weak<Ty>>, heap: &mut Heap) -> Result<Self, Self::Error> {
        use crate::error::AlreadyDroppedError;

        value.upgrade(heap).ok_or(AlreadyDroppedError)
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

impl<Ty, Conv> LuaFfiOnce<Ty, Conv> for Callable<Strong<Ty>>
where
    Conv: 'static,
    Ty: CoreTypes<LuaClosure = LuaClosure<Ty>>,
    Ty::Table: TableIndex<Weak<Ty>>,
    Ty::RustClosure: LuaFfi<Ty, Conv>,
    WeakValue<Ty>: DisplayWith<Heap>,
{
    fn call_once(
        self,
        rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
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

impl<Ty, Conv> LuaFfiOnce<Ty, Conv> for Callable<Weak<Ty>>
where
    Conv: 'static,
    Ty: CoreTypes<LuaClosure = LuaClosure<Ty>>,
    Ty::Table: TableIndex<Weak<Ty>>,
    Ty::RustClosure: LuaFfi<Ty, Conv>,
    WeakValue<Ty>: DisplayWith<Heap>,
{
    fn call_once(
        self,
        rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
        use crate::error::AlreadyDroppedError;
        use crate::gc::LuaPtr;

        match self {
            Callable::Lua(LuaPtr(f)) => {
                let f = rt.core.gc.upgrade_cell(f).ok_or(AlreadyDroppedError)?;
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

impl<Ty> From<Callable<Ty>> for Value<Ty>
where
    Ty: Types,
{
    fn from(value: Callable<Ty>) -> Self {
        Value::Function(value)
    }
}
