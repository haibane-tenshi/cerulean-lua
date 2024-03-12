//! Utilities to deal with our garbage collector idiosyncracies.

use std::fmt::Display;
use std::hash::Hash;

use gc::{Gc, GcCell, Heap, Root, RootCell, Trace};

use crate::ffi::{LuaFfi, LuaFfiMut, LuaFfiOnce};
use crate::value::CoreTypes;

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

pub trait DisplayWith<Gc> {
    type Output<'a>: Display
    where
        Self: 'a,
        Gc: 'a;

    fn display<'a>(&'a self, extra: &'a Gc) -> Self::Output<'a>;
}

#[derive(Debug, Clone, Copy)]
pub struct LuaPtr<P>(pub P);

impl<P> LuaPtr<P> {
    pub fn into_inner(self) -> P {
        self.0
    }
}

impl<T> LuaPtr<Gc<T>>
where
    T: Trace,
{
    pub fn upgrade(self, heap: &Heap) -> Option<LuaPtr<Root<T>>> {
        let LuaPtr(ptr) = self;
        let ptr = heap.upgrade(ptr)?;
        Some(LuaPtr(ptr))
    }
}

impl<T> LuaPtr<GcCell<T>>
where
    T: Trace,
{
    pub fn upgrade_cell(self, heap: &Heap) -> Option<LuaPtr<RootCell<T>>> {
        let LuaPtr(ptr) = self;
        let ptr = heap.upgrade_cell(ptr)?;
        Some(LuaPtr(ptr))
    }
}

impl<T> LuaPtr<Root<T>> {
    pub fn downgrade(&self) -> LuaPtr<Gc<T>> {
        LuaPtr(self.0.downgrade())
    }
}

impl<T> LuaPtr<RootCell<T>> {
    pub fn downgrade_cell(&self) -> LuaPtr<GcCell<T>> {
        LuaPtr(self.0.downgrade())
    }
}

impl<P> Trace for LuaPtr<P>
where
    P: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        let LuaPtr(ptr) = self;
        ptr.trace(collector);
    }
}

impl<P> From<P> for LuaPtr<P> {
    fn from(value: P) -> Self {
        LuaPtr(value)
    }
}

impl<T> PartialEq for LuaPtr<Gc<T>> {
    fn eq(&self, other: &Self) -> bool {
        self.0.addr() == other.0.addr()
    }
}

impl<T> PartialEq for LuaPtr<GcCell<T>> {
    fn eq(&self, other: &Self) -> bool {
        self.0.addr() == other.0.addr()
    }
}

impl<T> PartialEq for LuaPtr<Root<T>> {
    fn eq(&self, other: &Self) -> bool {
        self.0.addr() == other.0.addr()
    }
}

impl<T> PartialEq for LuaPtr<RootCell<T>> {
    fn eq(&self, other: &Self) -> bool {
        self.0.addr() == other.0.addr()
    }
}

impl<T> Eq for LuaPtr<Gc<T>> {}

impl<T> Eq for LuaPtr<GcCell<T>> {}

impl<T> Eq for LuaPtr<Root<T>> {}

impl<T> Eq for LuaPtr<RootCell<T>> {}

impl<T> PartialOrd for LuaPtr<Gc<T>> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> PartialOrd for LuaPtr<GcCell<T>> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> PartialOrd for LuaPtr<Root<T>> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> PartialOrd for LuaPtr<RootCell<T>> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for LuaPtr<Gc<T>> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.addr().cmp(&other.0.addr())
    }
}

impl<T> Ord for LuaPtr<GcCell<T>> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.addr().cmp(&other.0.addr())
    }
}

impl<T> Ord for LuaPtr<Root<T>> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.addr().cmp(&other.0.addr())
    }
}

impl<T> Ord for LuaPtr<RootCell<T>> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.addr().cmp(&other.0.addr())
    }
}

impl<T> Hash for LuaPtr<Gc<T>> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.addr().hash(state);
    }
}

impl<T> Hash for LuaPtr<GcCell<T>> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.addr().hash(state);
    }
}

impl<T> Hash for LuaPtr<Root<T>> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.addr().hash(state);
    }
}

impl<T> Hash for LuaPtr<RootCell<T>> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.addr().hash(state);
    }
}

impl<Ty, Conv, P> LuaFfiOnce<Ty, Conv> for LuaPtr<P>
where
    Ty: CoreTypes,
    P: LuaFfiOnce<Ty, Conv>,
{
    fn call_once(
        self,
        rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), crate::error::RuntimeError<crate::value::StrongValue<Ty, Conv>>> {
        self.0.call_once(rt)
    }

    fn debug_info(&self) -> crate::ffi::DebugInfo {
        self.0.debug_info()
    }
}

impl<Ty, Conv, P> LuaFfiMut<Ty, Conv> for LuaPtr<P>
where
    Ty: CoreTypes,
    P: LuaFfiMut<Ty, Conv>,
{
    fn call_mut(
        &mut self,
        rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), crate::error::RuntimeError<crate::value::StrongValue<Ty, Conv>>> {
        self.0.call_mut(rt)
    }
}

impl<Ty, Conv, P> LuaFfi<Ty, Conv> for LuaPtr<P>
where
    Ty: CoreTypes,
    P: LuaFfi<Ty, Conv>,
{
    fn call(
        &self,
        rt: crate::runtime::RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), crate::error::RuntimeError<crate::value::StrongValue<Ty, Conv>>> {
        self.0.call(rt)
    }
}

impl<T> DisplayWith<Heap> for LuaPtr<Gc<T>>
where
    T: Trace + Display,
{
    type Output<'a> = LuaPtrDisplay<'a, Gc<T>>;

    fn display<'a>(&'a self, extra: &'a Heap) -> Self::Output<'a> {
        LuaPtrDisplay {
            ptr: &self.0,
            heap: extra,
        }
    }
}

impl<T> DisplayWith<Heap> for LuaPtr<GcCell<T>>
where
    T: Trace + Display,
{
    type Output<'a> = LuaPtrDisplay<'a, GcCell<T>>;

    fn display<'a>(&'a self, extra: &'a Heap) -> Self::Output<'a> {
        LuaPtrDisplay {
            ptr: &self.0,
            heap: extra,
        }
    }
}

impl<T> DisplayWith<Heap> for LuaPtr<Root<T>>
where
    T: Trace + Display,
{
    type Output<'a> = LuaPtrDisplay<'a, Root<T>>;

    fn display<'a>(&'a self, extra: &'a Heap) -> Self::Output<'a> {
        LuaPtrDisplay {
            ptr: &self.0,
            heap: extra,
        }
    }
}

impl<T> DisplayWith<Heap> for LuaPtr<RootCell<T>>
where
    T: Trace + Display,
{
    type Output<'a> = LuaPtrDisplay<'a, RootCell<T>>;

    fn display<'a>(&'a self, extra: &'a Heap) -> Self::Output<'a> {
        LuaPtrDisplay {
            ptr: &self.0,
            heap: extra,
        }
    }
}

pub struct LuaPtrDisplay<'a, Ptr> {
    ptr: &'a Ptr,
    heap: &'a Heap,
}

impl<'a, T> Display for LuaPtrDisplay<'a, Gc<T>>
where
    T: Trace + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LuaPtrDisplay { ptr, heap } = self;

        if let Some(value) = heap.get(**ptr) {
            write!(f, "{value}")?;
        }

        Ok(())
    }
}

impl<'a, T> Display for LuaPtrDisplay<'a, GcCell<T>>
where
    T: Trace + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LuaPtrDisplay { ptr, heap } = self;

        if let Some(value) = heap.get(**ptr) {
            write!(f, "{value}")?;
        }

        Ok(())
    }
}

impl<'a, T> Display for LuaPtrDisplay<'a, Root<T>>
where
    T: Trace + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LuaPtrDisplay { ptr, heap } = self;

        let value = heap.get_root(*ptr);
        write!(f, "{value}")
    }
}

impl<'a, T> Display for LuaPtrDisplay<'a, RootCell<T>>
where
    T: Trace + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LuaPtrDisplay { ptr, heap } = self;

        let value = heap.get_root(*ptr);
        write!(f, "{value}")
    }
}
