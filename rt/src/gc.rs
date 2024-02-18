//! Utilities to deal with our garbage collector idiosyncracies.

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

use gc::{Collector, Gc, Heap, Root, Trace};

use crate::runtime::Closure;
use crate::value::userdata::FullUserdata;
use crate::value::{Table, TypeProvider, Types};

pub struct GcOrd<T>(pub Gc<T>);

impl<T> From<Gc<T>> for GcOrd<T> {
    fn from(value: Gc<T>) -> Self {
        Self(value)
    }
}

impl<T> From<GcOrd<T>> for Gc<T> {
    fn from(value: GcOrd<T>) -> Self {
        value.0
    }
}

impl<T> AsRef<Gc<T>> for GcOrd<T> {
    fn as_ref(&self) -> &Gc<T> {
        &self.0
    }
}

impl<T> AsMut<Gc<T>> for GcOrd<T> {
    fn as_mut(&mut self) -> &mut Gc<T> {
        &mut self.0
    }
}

impl<T> Deref for GcOrd<T> {
    type Target = Gc<T>;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T> DerefMut for GcOrd<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

impl<T> Trace for GcOrd<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        self.0.trace(collector)
    }
}

impl<T> Debug for GcOrd<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("GcOrd").field(&self.0).finish()
    }
}

impl Display for GcOrd<Closure> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{[lua] closure <{:p}>}}", self.0.addr())
    }
}

impl<T> Clone for GcOrd<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for GcOrd<T> {}

impl<T> PartialEq for GcOrd<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.addr() == other.0.addr()
    }
}

impl<T> Eq for GcOrd<T> {}

impl<T> PartialOrd for GcOrd<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for GcOrd<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.addr().cmp(&other.0.addr())
    }
}

impl<T> Hash for GcOrd<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.addr().hash(state)
    }
}

pub struct RootOrd<T>(pub Root<T>);

impl<T> RootOrd<T> {
    pub fn downgrade(self) -> GcOrd<T> {
        GcOrd(self.0.downgrade())
    }
}

impl<T> From<Root<T>> for RootOrd<T> {
    fn from(value: Root<T>) -> Self {
        Self(value)
    }
}

impl<T> From<RootOrd<T>> for Root<T> {
    fn from(value: RootOrd<T>) -> Self {
        value.0
    }
}

impl<T> AsRef<Root<T>> for RootOrd<T> {
    fn as_ref(&self) -> &Root<T> {
        &self.0
    }
}

impl<T> AsMut<Root<T>> for RootOrd<T> {
    fn as_mut(&mut self) -> &mut Root<T> {
        &mut self.0
    }
}

impl<T> Deref for RootOrd<T> {
    type Target = Root<T>;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T> DerefMut for RootOrd<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

impl<T> Debug for RootOrd<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RootOrd").field(&self.0).finish()
    }
}

impl Display for RootOrd<Closure> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{[lua] closure <{:p}>}}", self.addr())
    }
}

impl<Ty> Display for RootOrd<Table<Ty>>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{table <{:p}>}}", self.addr())
    }
}

impl<Ty> Display for RootOrd<Box<dyn FullUserdata<Ty>>>
where
    Ty: TypeProvider,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{userdata <{:p}>}}", self.addr())
    }
}

impl<T> Clone for RootOrd<T> {
    fn clone(&self) -> Self {
        RootOrd(self.0.clone())
    }
}

impl<T> PartialEq for RootOrd<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.addr() == other.0.addr()
    }
}

impl<T> Eq for RootOrd<T> {}

impl<T> PartialOrd for RootOrd<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for RootOrd<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.addr().cmp(&other.0.addr())
    }
}

impl<T> Hash for RootOrd<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.addr().hash(state)
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

impl<T> TryFromWithGc<GcOrd<T>, Heap> for RootOrd<T>
where
    T: Trace,
{
    type Error = crate::error::AlreadyDroppedError;

    fn try_from_with_gc(value: GcOrd<T>, gc: &mut Heap) -> Result<Self, Self::Error> {
        use crate::error::AlreadyDroppedError;

        let value = gc.upgrade(value.0).ok_or(AlreadyDroppedError)?;
        Ok(RootOrd(value))
    }
}
