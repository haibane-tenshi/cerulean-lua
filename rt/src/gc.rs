//! Utilities to help dealing with our garbage collector idiosyncrasies.

use std::fmt::Display;
use std::hash::Hash;

use gc::index::{Access, Allocated, GcPtr, MutAccess, RefAccess, RootPtr};
use gc::userdata::Params;
use gc::{Gc, GcCell, Heap as TrueHeap, Root, RootCell, Trace};

use crate::value::userdata::DefaultParams;
use crate::value::Meta;

pub use crate::error::AlreadyDroppedError;

pub type Heap<Ty> = TrueHeap<Meta<Ty>, DefaultParams<Ty>>;

pub trait DisplayWith<Gc> {
    type Output<'a>: Display
    where
        Self: 'a,
        Gc: 'a;

    fn display<'a>(&'a self, extra: &'a Gc) -> Self::Output<'a>;
}

#[derive(Debug, Clone, Copy, Trace)]
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
    T: Allocated<TrueHeap<M, P>> + Display + ?Sized + 'static,
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
    T: Allocated<TrueHeap<M, P>> + Display + ?Sized + 'static,
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
    T: Allocated<TrueHeap<M, P>> + Display + ?Sized + 'static,
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
    T: Allocated<TrueHeap<M, P>> + Display + ?Sized + 'static,
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

impl<T, M, P> Display for LuaPtrDisplay<'_, Gc<T>, M, P>
where
    T: Allocated<TrueHeap<M, P>> + Display + ?Sized,
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

impl<T, M, P> Display for LuaPtrDisplay<'_, GcCell<T>, M, P>
where
    T: Allocated<TrueHeap<M, P>> + Display + ?Sized,
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

impl<T, M, P> Display for LuaPtrDisplay<'_, Root<T>, M, P>
where
    T: Allocated<TrueHeap<M, P>> + Display + ?Sized,
    P: Params,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LuaPtrDisplay { ptr, heap } = self;

        let value = heap.get_root(*ptr);
        write!(f, "{value}")
    }
}

impl<T, M, P> Display for LuaPtrDisplay<'_, RootCell<T>, M, P>
where
    T: Allocated<TrueHeap<M, P>> + Display + ?Sized,
    P: Params,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LuaPtrDisplay { ptr, heap } = self;

        let value = heap.get_root(*ptr);
        write!(f, "{value}")
    }
}

pub trait TryGet<T>
where
    T: ?Sized,
{
    fn try_get<A>(&self, ptr: GcPtr<T, A>) -> Result<&T, AlreadyDroppedError>
    where
        A: RefAccess;

    fn try_get_mut<A>(&mut self, ptr: GcPtr<T, A>) -> Result<&mut T, AlreadyDroppedError>
    where
        A: MutAccess;

    fn try_upgrade<A>(&self, ptr: GcPtr<T, A>) -> Result<RootPtr<T, A>, AlreadyDroppedError>
    where
        A: Access;
}

impl<T, P, M> TryGet<T> for TrueHeap<M, P>
where
    T: Allocated<Self> + ?Sized,
    P: Params,
{
    fn try_get<A>(&self, ptr: GcPtr<T, A>) -> Result<&T, AlreadyDroppedError>
    where
        A: RefAccess,
    {
        self.get(ptr).ok_or(AlreadyDroppedError)
    }

    fn try_get_mut<A>(&mut self, ptr: GcPtr<T, A>) -> Result<&mut T, AlreadyDroppedError>
    where
        A: MutAccess,
    {
        self.get_mut(ptr).ok_or(AlreadyDroppedError)
    }

    fn try_upgrade<A>(&self, ptr: GcPtr<T, A>) -> Result<RootPtr<T, A>, AlreadyDroppedError>
    where
        A: Access,
    {
        self.upgrade(ptr).ok_or(AlreadyDroppedError)
    }
}
