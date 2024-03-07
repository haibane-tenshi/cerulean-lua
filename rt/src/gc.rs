//! Utilities to deal with our garbage collector idiosyncracies.

use std::fmt::Display;

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
