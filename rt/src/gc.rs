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

impl<T, A> LuaPtr<GcPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    pub fn upgrade<M, P>(self, heap: &TrueHeap<M, P>) -> Option<LuaPtr<RootPtr<T, A>>>
    where
        P: Params,
    {
        self.try_upgrade(heap).ok()
    }

    pub fn try_upgrade<M, P>(
        self,
        heap: &TrueHeap<M, P>,
    ) -> Result<LuaPtr<RootPtr<T, A>>, AlreadyDroppedError>
    where
        P: Params,
    {
        let LuaPtr(ptr) = self;
        let ptr = heap.try_upgrade(ptr)?;
        Ok(LuaPtr(ptr))
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
