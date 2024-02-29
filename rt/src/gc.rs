//! Utilities to deal with our garbage collector idiosyncracies.

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::Deref;
use std::rc::Rc;

use gc::{Collector, Trace};

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
