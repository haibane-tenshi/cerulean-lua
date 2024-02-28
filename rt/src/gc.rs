//! Utilities to deal with our garbage collector idiosyncracies.

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use gc::{Collector, GcCell, Heap, RootCell, Trace};

use crate::error::AlreadyDroppedError;
use crate::ffi::{DebugInfo, LuaFfi, LuaFfiMut, LuaFfiOnce};
use crate::value::{CoreTypes, WeakValue};

macro_rules! ref_pair {
    ($gc_name:ident, $root_name:ident) => {
        pub struct $gc_name<T>(pub GcCell<T>);

        impl<T> From<GcCell<T>> for $gc_name<T> {
            fn from(value: GcCell<T>) -> Self {
                Self(value)
            }
        }

        impl<T> From<$gc_name<T>> for GcCell<T> {
            fn from(value: $gc_name<T>) -> Self {
                value.0
            }
        }

        impl<T> AsRef<GcCell<T>> for $gc_name<T> {
            fn as_ref(&self) -> &GcCell<T> {
                &self.0
            }
        }

        impl<T> AsMut<GcCell<T>> for $gc_name<T> {
            fn as_mut(&mut self) -> &mut GcCell<T> {
                &mut self.0
            }
        }

        impl<T> Deref for $gc_name<T> {
            type Target = GcCell<T>;

            fn deref(&self) -> &Self::Target {
                self.as_ref()
            }
        }

        impl<T> DerefMut for $gc_name<T> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                self.as_mut()
            }
        }

        impl<T> Trace for $gc_name<T>
        where
            T: Trace,
        {
            fn trace(&self, collector: &mut Collector) {
                self.0.trace(collector)
            }
        }

        impl<T> Clone for $gc_name<T> {
            fn clone(&self) -> Self {
                *self
            }
        }

        impl<T> Copy for $gc_name<T> {}

        impl<T> PartialEq for $gc_name<T> {
            fn eq(&self, other: &Self) -> bool {
                self.0.addr() == other.0.addr()
            }
        }

        impl<T> Eq for $gc_name<T> {}

        impl<T> PartialOrd for $gc_name<T> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl<T> Ord for $gc_name<T> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.0.addr().cmp(&other.0.addr())
            }
        }

        impl<T> Hash for $gc_name<T> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.addr().hash(state)
            }
        }

        pub struct $root_name<T>(pub RootCell<T>);

        impl<T> $root_name<T> {
            pub fn downgrade(&self) -> $gc_name<T> {
                $gc_name(self.0.downgrade())
            }
        }

        impl<T> From<RootCell<T>> for $root_name<T> {
            fn from(value: RootCell<T>) -> Self {
                Self(value)
            }
        }

        impl<T> From<$root_name<T>> for RootCell<T> {
            fn from(value: $root_name<T>) -> Self {
                value.0
            }
        }

        impl<T> AsRef<RootCell<T>> for $root_name<T> {
            fn as_ref(&self) -> &RootCell<T> {
                &self.0
            }
        }

        impl<T> AsMut<RootCell<T>> for $root_name<T> {
            fn as_mut(&mut self) -> &mut RootCell<T> {
                &mut self.0
            }
        }

        impl<T> Deref for $root_name<T> {
            type Target = RootCell<T>;

            fn deref(&self) -> &Self::Target {
                self.as_ref()
            }
        }

        impl<T> DerefMut for $root_name<T> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                self.as_mut()
            }
        }

        impl<T> Clone for $root_name<T> {
            fn clone(&self) -> Self {
                $root_name(self.0.clone())
            }
        }

        impl<T> PartialEq for $root_name<T> {
            fn eq(&self, other: &Self) -> bool {
                self.0.addr() == other.0.addr()
            }
        }

        impl<T> Eq for $root_name<T> {}

        impl<T> PartialOrd for $root_name<T> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl<T> Ord for $root_name<T> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.0.addr().cmp(&other.0.addr())
            }
        }

        impl<T> Hash for $root_name<T> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.addr().hash(state)
            }
        }

        impl<T> TryFromWithGc<$gc_name<T>, Heap> for $root_name<T>
        where
            T: Trace,
        {
            type Error = AlreadyDroppedError;

            fn try_from_with_gc(value: $gc_name<T>, gc: &mut Heap) -> Result<Self, Self::Error> {
                let value = gc.upgrade(value.0).ok_or(AlreadyDroppedError)?;
                Ok($root_name(value))
            }
        }
    };
}

ref_pair!(GcLuaClosure, RootLuaClosure);

impl<T> Debug for GcLuaClosure<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("GcLuaClosure").field(&self.0).finish()
    }
}

impl<T> Display for GcLuaClosure<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{[lua] closure <{:p}>}}", self.0.addr())
    }
}

impl<T> Debug for RootLuaClosure<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RootLuaClosure").field(&self.0).finish()
    }
}

impl<T> Display for RootLuaClosure<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.downgrade())
    }
}

ref_pair!(GcRustClosure, RootRustClosure);

impl<T> Debug for GcRustClosure<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("GcRustClosure").field(&self.0).finish()
    }
}

impl<T> Display for GcRustClosure<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{[rust] closure <{:p}>}}", self.0.addr())
    }
}

impl<T> Debug for RootRustClosure<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RootRustClosure").field(&self.0).finish()
    }
}

impl<T> Display for RootRustClosure<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.downgrade())
    }
}

impl<Ty, T> LuaFfiOnce<Ty> for GcRustClosure<T>
where
    Ty: CoreTypes,
    T: LuaFfi<Ty> + Clone + Trace,
    WeakValue<Ty>: Display,
{
    fn call_once(
        self,
        rt: crate::runtime::RuntimeView<'_, Ty>,
    ) -> Result<(), crate::error::RuntimeError<Ty>> {
        self.call(rt)
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: "<transient ref to rust closure>".to_string(),
        }
    }
}

impl<Ty, T> LuaFfiMut<Ty> for GcRustClosure<T>
where
    Ty: CoreTypes,
    T: LuaFfi<Ty> + Clone + Trace,
    WeakValue<Ty>: Display,
{
    fn call_mut(
        &mut self,
        rt: crate::runtime::RuntimeView<'_, Ty>,
    ) -> Result<(), crate::error::RuntimeError<Ty>> {
        self.call(rt)
    }
}

impl<Ty, T> LuaFfi<Ty> for GcRustClosure<T>
where
    Ty: CoreTypes,
    T: LuaFfi<Ty> + Clone + Trace,
    WeakValue<Ty>: Display,
{
    fn call(
        &self,
        rt: crate::runtime::RuntimeView<'_, Ty>,
    ) -> Result<(), crate::error::RuntimeError<Ty>> {
        let func = rt.core.gc.get(self.0).ok_or(AlreadyDroppedError)?.clone();
        rt.invoke(func)
    }
}

impl<Ty, T> LuaFfiOnce<Ty> for RootRustClosure<T>
where
    Ty: CoreTypes,
    T: LuaFfi<Ty> + Clone + Trace,
    WeakValue<Ty>: Display,
{
    fn call_once(
        self,
        rt: crate::runtime::RuntimeView<'_, Ty>,
    ) -> Result<(), crate::error::RuntimeError<Ty>> {
        self.call(rt)
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: "<rooted rust closure>".to_string(),
        }
    }
}

impl<Ty, T> LuaFfiMut<Ty> for RootRustClosure<T>
where
    Ty: CoreTypes,
    T: LuaFfi<Ty> + Clone + Trace,
    WeakValue<Ty>: Display,
{
    fn call_mut(
        &mut self,
        rt: crate::runtime::RuntimeView<'_, Ty>,
    ) -> Result<(), crate::error::RuntimeError<Ty>> {
        self.call(rt)
    }
}

impl<Ty, T> LuaFfi<Ty> for RootRustClosure<T>
where
    Ty: CoreTypes,
    T: LuaFfi<Ty> + Clone + Trace,
    WeakValue<Ty>: Display,
{
    fn call(
        &self,
        rt: crate::runtime::RuntimeView<'_, Ty>,
    ) -> Result<(), crate::error::RuntimeError<Ty>> {
        let func = rt.core.gc[&self].clone();
        rt.invoke(func)
    }
}

ref_pair!(GcTable, RootTable);

impl<T> Debug for GcTable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("GcTable").field(&self.0).finish()
    }
}

impl<T> Display for GcTable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{[lua] table <{:p}>}}", self.0.addr())
    }
}

impl<T> Debug for RootTable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RootTable").field(&self.0).finish()
    }
}

impl<T> Display for RootTable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.downgrade())
    }
}

ref_pair!(GcFullUserdata, RootFullUserdata);

impl<T> Debug for GcFullUserdata<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("GcFullUserdata").field(&self.0).finish()
    }
}

impl<T> Display for GcFullUserdata<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{[rust] userdata <{:p}>}}", self.addr())
    }
}

impl<T> Debug for RootFullUserdata<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RootFullUserdata").field(&self.0).finish()
    }
}

impl<T> Display for RootFullUserdata<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.downgrade())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct StringRef<T>(pub Rc<T>);

impl<T> StringRef<T> {
    pub fn new(value: T) -> Self {
        StringRef(Rc::new(value))
    }
}

impl<T, U> AsRef<U> for StringRef<T>
where
    U: ?Sized,
    T: AsRef<U>,
{
    fn as_ref(&self) -> &U {
        self.0.as_ref().as_ref()
    }
}

impl<T> Deref for StringRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl<T> Display for StringRef<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T> Trace for StringRef<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        self.deref().trace(collector)
    }
}

pub trait TryFromWithGc<T, Gc>: Sized {
    type Error;

    fn try_from_with_gc(value: T, gc: &mut Gc) -> Result<Self, Self::Error>;
}

impl<T, Gc, U> TryFromWithGc<T, Gc> for U
where
    T: TryInto<U>,
{
    type Error = <T as TryInto<U>>::Error;

    fn try_from_with_gc(value: T, _gc: &mut Gc) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

pub trait TryIntoWithGc<T, Gc> {
    type Error;

    fn try_into_with_gc(self, gc: &mut Gc) -> Result<T, Self::Error>;
}

impl<T, Gc, U> TryIntoWithGc<T, Gc> for U
where
    T: TryFromWithGc<U, Gc>,
{
    type Error = <T as TryFromWithGc<U, Gc>>::Error;

    fn try_into_with_gc(self, gc: &mut Gc) -> Result<T, Self::Error> {
        T::try_from_with_gc(self, gc)
    }
}

pub trait FromWithGc<T, Gc> {
    fn from_with_gc(value: T, gc: &mut Gc) -> Self;
}

impl<T, Gc, U> FromWithGc<T, Gc> for U
where
    T: TryIntoWithGc<U, Gc, Error = std::convert::Infallible>,
{
    fn from_with_gc(value: T, gc: &mut Gc) -> Self {
        match value.try_into_with_gc(gc) {
            Ok(t) => t,
            Err(err) => match err {},
        }
    }
}

pub trait IntoWithGc<T, Gc> {
    fn into_with_gc(self, gc: &mut Gc) -> T;
}

impl<T, Gc, U> IntoWithGc<T, Gc> for U
where
    T: FromWithGc<U, Gc>,
{
    fn into_with_gc(self, gc: &mut Gc) -> T {
        T::from_with_gc(self, gc)
    }
}
