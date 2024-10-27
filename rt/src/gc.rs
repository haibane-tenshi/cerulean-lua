//! Utilities to deal with our garbage collector idiosyncracies.

use std::fmt::Display;
use std::hash::Hash;

use gc::index::{Access, Allocated, GcPtr, RootPtr};
use gc::userdata::Params;
use gc::{Gc, GcCell, Heap as TrueHeap, Root, RootCell, Trace};

use crate::value::userdata::DefaultParams;
use crate::value::Meta;

pub type Heap<Ty> = TrueHeap<Meta<Ty>, DefaultParams<Ty>>;

pub trait TryConvertFrom<T, Gc>: Sized {
    type Error;

    fn try_from_with_gc(value: T, gc: &mut Gc) -> Result<Self, Self::Error>;
}

impl<T, Gc, U> TryConvertFrom<T, Gc> for U
where
    T: TryInto<U>,
{
    type Error = <T as TryInto<U>>::Error;

    fn try_from_with_gc(value: T, _gc: &mut Gc) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

pub trait TryConvertInto<T, Gc> {
    type Error;

    fn try_into_with_gc(self, gc: &mut Gc) -> Result<T, Self::Error>;
}

impl<T, Gc, U> TryConvertInto<T, Gc> for U
where
    T: TryConvertFrom<U, Gc>,
{
    type Error = <T as TryConvertFrom<U, Gc>>::Error;

    fn try_into_with_gc(self, gc: &mut Gc) -> Result<T, Self::Error> {
        T::try_from_with_gc(self, gc)
    }
}

pub trait FromWithGc<T, Gc> {
    fn from_with_gc(value: T, gc: &mut Gc) -> Self;
}

impl<T, Gc, U> FromWithGc<T, Gc> for U
where
    T: TryConvertInto<U, Gc, Error = std::convert::Infallible>,
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

impl<T, A> LuaPtr<GcPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    pub fn upgrade<M, P>(self, heap: &TrueHeap<M, P>) -> Option<LuaPtr<RootPtr<T, A>>>
    where
        P: Params,
    {
        let LuaPtr(ptr) = self;
        let ptr = heap.upgrade(ptr)?;
        Some(LuaPtr(ptr))
    }
}

impl<T, A> LuaPtr<RootPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    pub fn downgrade(&self) -> LuaPtr<GcPtr<T, A>> {
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

impl<T, A> PartialEq for LuaPtr<GcPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.location() == other.0.location()
    }
}

impl<T, A> PartialEq for LuaPtr<RootPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.location() == other.0.location()
    }
}

impl<T, A> Eq for LuaPtr<GcPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
}

impl<T, A> Eq for LuaPtr<RootPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
}

impl<T, A> PartialOrd for LuaPtr<GcPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T, A> PartialOrd for LuaPtr<RootPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T, A> Ord for LuaPtr<GcPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.location().cmp(&other.0.location())
    }
}

impl<T, A> Ord for LuaPtr<RootPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.location().cmp(&other.0.location())
    }
}

impl<T, A> Hash for LuaPtr<GcPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.location().hash(state);
    }
}

impl<T, A> Hash for LuaPtr<RootPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.location().hash(state);
    }
}

impl<T, M, P> DisplayWith<TrueHeap<M, P>> for LuaPtr<Gc<T>>
where
    T: Allocated<M, P> + Display + ?Sized + 'static,
    M: 'static,
    P: Params,
{
    type Output<'a> = LuaPtrDisplay<'a, Gc<T>, M, P>;

    fn display<'a>(&'a self, extra: &'a TrueHeap<M, P>) -> Self::Output<'a> {
        LuaPtrDisplay {
            ptr: &self.0,
            heap: extra,
        }
    }
}

impl<T, M, P> DisplayWith<TrueHeap<M, P>> for LuaPtr<GcCell<T>>
where
    T: Allocated<M, P> + Display + ?Sized + 'static,
    M: 'static,
    P: Params,
{
    type Output<'a> = LuaPtrDisplay<'a, GcCell<T>, M, P>;

    fn display<'a>(&'a self, extra: &'a TrueHeap<M, P>) -> Self::Output<'a> {
        LuaPtrDisplay {
            ptr: &self.0,
            heap: extra,
        }
    }
}

impl<T, M, P> DisplayWith<TrueHeap<M, P>> for LuaPtr<Root<T>>
where
    T: Allocated<M, P> + Display + ?Sized + 'static,
    M: 'static,
    P: Params,
{
    type Output<'a> = LuaPtrDisplay<'a, Root<T>, M, P>;

    fn display<'a>(&'a self, extra: &'a TrueHeap<M, P>) -> Self::Output<'a> {
        LuaPtrDisplay {
            ptr: &self.0,
            heap: extra,
        }
    }
}

impl<T, M, P> DisplayWith<TrueHeap<M, P>> for LuaPtr<RootCell<T>>
where
    T: Allocated<M, P> + Display + ?Sized + 'static,
    M: 'static,
    P: Params,
{
    type Output<'a> = LuaPtrDisplay<'a, RootCell<T>, M, P>;

    fn display<'a>(&'a self, extra: &'a TrueHeap<M, P>) -> Self::Output<'a> {
        LuaPtrDisplay {
            ptr: &self.0,
            heap: extra,
        }
    }
}

pub struct LuaPtrDisplay<'a, Ptr, M, P> {
    ptr: &'a Ptr,
    heap: &'a TrueHeap<M, P>,
}

impl<'a, T, M, P> Display for LuaPtrDisplay<'a, Gc<T>, M, P>
where
    T: Allocated<M, P> + Display + ?Sized,
    P: Params,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LuaPtrDisplay { ptr, heap } = self;

        if let Some(value) = heap.get(*ptr) {
            write!(f, "{value}")?;
        }

        Ok(())
    }
}

impl<'a, T, M, P> Display for LuaPtrDisplay<'a, GcCell<T>, M, P>
where
    T: Allocated<M, P> + Display + ?Sized,
    P: Params,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LuaPtrDisplay { ptr, heap } = self;

        if let Some(value) = heap.get(**ptr) {
            write!(f, "{value}")?;
        }

        Ok(())
    }
}

impl<'a, T, M, P> Display for LuaPtrDisplay<'a, Root<T>, M, P>
where
    T: Allocated<M, P> + Display + ?Sized,
    P: Params,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LuaPtrDisplay { ptr, heap } = self;

        let value = heap.get_root(*ptr);
        write!(f, "{value}")
    }
}

impl<'a, T, M, P> Display for LuaPtrDisplay<'a, RootCell<T>, M, P>
where
    T: Allocated<M, P> + Display + ?Sized,
    P: Params,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LuaPtrDisplay { ptr, heap } = self;

        let value = heap.get_root(*ptr);
        write!(f, "{value}")
    }
}
