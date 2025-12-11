//! Utilities to help dealing with our garbage collector idiosyncrasies.

use std::hash::Hash;

use gc::index::{Access, Allocated, GcPtr, MutAccess, RefAccess, RootPtr};
use gc::userdata::Params;
use gc::{Heap as TrueHeap, Trace};

use crate::error::RuntimeError;
use crate::value::userdata::DefaultParams;
use crate::value::{Meta, StrongKey, StrongValue, Types};

pub use crate::error::AlreadyDroppedError;

pub type Heap<Ty> = TrueHeap<Meta<Ty>, DefaultParams<Ty>>;

#[derive(Debug, Clone, Copy, Trace)]
pub struct LuaPtr<P>(pub P);

impl<P> LuaPtr<P> {
    pub fn into_inner(self) -> P {
        self.0
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

pub trait TryGet: Sized {
    fn try_get<T, A>(&self, ptr: GcPtr<T, A>) -> Result<&T, AlreadyDroppedError>
    where
        T: Allocated<Self> + ?Sized,
        A: RefAccess;

    fn try_get_mut<T, A>(&mut self, ptr: GcPtr<T, A>) -> Result<&mut T, AlreadyDroppedError>
    where
        T: Allocated<Self> + ?Sized,
        A: MutAccess;

    fn try_upgrade<T, A>(&self, ptr: GcPtr<T, A>) -> Result<RootPtr<T, A>, AlreadyDroppedError>
    where
        T: ?Sized,
        A: Access;
}

impl<P, M> TryGet for TrueHeap<M, P>
where
    P: Params,
{
    fn try_get<T, A>(&self, ptr: GcPtr<T, A>) -> Result<&T, AlreadyDroppedError>
    where
        T: Allocated<Self> + ?Sized,
        A: RefAccess,
    {
        self.get(ptr).ok_or(AlreadyDroppedError)
    }

    fn try_get_mut<T, A>(&mut self, ptr: GcPtr<T, A>) -> Result<&mut T, AlreadyDroppedError>
    where
        T: Allocated<Self> + ?Sized,
        A: MutAccess,
    {
        self.get_mut(ptr).ok_or(AlreadyDroppedError)
    }

    fn try_upgrade<T, A>(&self, ptr: GcPtr<T, A>) -> Result<RootPtr<T, A>, AlreadyDroppedError>
    where
        T: ?Sized,
        A: Access,
    {
        self.upgrade(ptr).ok_or(AlreadyDroppedError)
    }
}

pub trait AllocExt<Ty>
where
    Ty: Types,
{
    fn alloc_str(&mut self, s: impl Into<Ty::String>) -> StrongValue<Ty>;

    fn alloc_str_key(&mut self, s: impl Into<Ty::String>) -> StrongKey<Ty>;

    fn alloc_error_msg(&mut self, s: impl Into<Ty::String>) -> RuntimeError<Ty>;
}

impl<Ty> AllocExt<Ty> for Heap<Ty>
where
    Ty: Types,
{
    fn alloc_str(&mut self, s: impl Into<<Ty as Types>::String>) -> StrongValue<Ty> {
        StrongValue::String(LuaPtr(self.intern(s.into())))
    }

    fn alloc_str_key(&mut self, s: impl Into<<Ty as Types>::String>) -> StrongKey<Ty> {
        StrongKey::String(LuaPtr(self.intern(s.into())))
    }

    fn alloc_error_msg(&mut self, msg: impl Into<Ty::String>) -> RuntimeError<Ty> {
        RuntimeError::from_value(self.alloc_str(msg))
    }
}

pub trait AsGc {
    type Access: Access;
    type Output: ?Sized;

    fn as_gc(&self) -> GcPtr<Self::Output, Self::Access>;
}

pub trait AsRoot: AsGc {
    fn as_root(&self) -> &RootPtr<Self::Output, Self::Access>;
}

pub trait Upgrade<Heap> {
    type Output;

    fn try_upgrade(&self, heap: &Heap) -> Result<Self::Output, AlreadyDroppedError>;

    fn upgrade(&self, heap: &Heap) -> Option<Self::Output> {
        self.try_upgrade(heap).ok()
    }
}

pub trait Downgrade {
    type Output;

    fn downgrade(&self) -> Self::Output;
}

impl<T, A> AsGc for GcPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    type Access = A;
    type Output = T;

    fn as_gc(&self) -> GcPtr<Self::Output, Self::Access> {
        *self
    }
}

impl<T, A, M, P> Upgrade<TrueHeap<M, P>> for GcPtr<T, A>
where
    T: ?Sized,
    A: Access,
    P: Params,
{
    type Output = RootPtr<T, A>;

    fn try_upgrade(&self, heap: &TrueHeap<M, P>) -> Result<Self::Output, AlreadyDroppedError> {
        heap.upgrade(*self).ok_or(AlreadyDroppedError)
    }
}

impl<T, A> AsGc for RootPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    type Access = A;
    type Output = T;

    fn as_gc(&self) -> GcPtr<Self::Output, Self::Access> {
        self.downgrade()
    }
}

impl<T, A> AsRoot for RootPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    fn as_root(&self) -> &RootPtr<Self::Output, Self::Access> {
        self
    }
}

impl<T, A> Downgrade for RootPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    type Output = GcPtr<T, A>;

    fn downgrade(&self) -> Self::Output {
        RootPtr::downgrade(self)
    }
}

impl<Ptr> AsGc for LuaPtr<Ptr>
where
    Ptr: AsGc,
{
    type Access = <Ptr as AsGc>::Access;
    type Output = <Ptr as AsGc>::Output;

    fn as_gc(&self) -> GcPtr<Self::Output, Self::Access> {
        self.0.as_gc()
    }
}

impl<Ptr> AsRoot for LuaPtr<Ptr>
where
    Ptr: AsRoot,
{
    fn as_root(&self) -> &RootPtr<Self::Output, Self::Access> {
        self.0.as_root()
    }
}

impl<Ptr, H> Upgrade<H> for LuaPtr<Ptr>
where
    Ptr: Upgrade<H>,
{
    type Output = LuaPtr<<Ptr as Upgrade<H>>::Output>;

    fn try_upgrade(&self, heap: &H) -> Result<Self::Output, AlreadyDroppedError> {
        Ok(LuaPtr(self.0.try_upgrade(heap)?))
    }
}

impl<Ptr> Downgrade for LuaPtr<Ptr>
where
    Ptr: Downgrade,
{
    type Output = LuaPtr<<Ptr as Downgrade>::Output>;

    fn downgrade(&self) -> Self::Output {
        LuaPtr(self.0.downgrade())
    }
}
