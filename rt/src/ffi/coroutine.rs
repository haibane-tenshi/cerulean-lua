//! Coroutine trait surrogate and related types.

use std::pin::Pin;

/// State of coroutine execution.
pub enum State<Y, C> {
    Yielded(Y),
    Complete(C),
}

/// Coroutine trait.
///
/// This trait is a mirror of currently unstable `std`'s [`Coroutine`](std::ops::Coroutine) trait.
/// Obviously, there is no compiler support to be expected.
///
/// The purpose of this trait is to keep our [`Delegate`](super::delegate::Delegate) mechanism compatible with
/// how coroutines are supposed to work in Rust (and keep us sane).
/// It is expected to be replaced when real coroutine trait is stabilized as part of `std`.
pub trait Coroutine<R> {
    type Yielded;
    type Complete;

    fn resume(self: Pin<&mut Self>, args: R) -> State<Self::Yielded, Self::Complete>;
}

impl<R, T> Coroutine<R> for &mut T
where
    T: Coroutine<R> + Unpin + ?Sized,
{
    type Yielded = <T as Coroutine<R>>::Yielded;
    type Complete = <T as Coroutine<R>>::Complete;

    fn resume(mut self: Pin<&mut Self>, args: R) -> State<Self::Yielded, Self::Complete> {
        let this: Pin<&mut T> = Pin::new(*self);
        Coroutine::resume(this, args)
    }
}

impl<R, T> Coroutine<R> for Pin<&mut T>
where
    T: Coroutine<R> + ?Sized,
{
    type Yielded = <T as Coroutine<R>>::Yielded;
    type Complete = <T as Coroutine<R>>::Complete;

    fn resume(mut self: Pin<&mut Self>, args: R) -> State<Self::Yielded, Self::Complete> {
        let this: Pin<&mut T> = (*self).as_mut();
        Coroutine::resume(this, args)
    }
}

impl<R, T> Coroutine<R> for Box<T>
where
    T: Coroutine<R> + Unpin + ?Sized,
{
    type Yielded = <T as Coroutine<R>>::Yielded;
    type Complete = <T as Coroutine<R>>::Complete;

    fn resume(mut self: Pin<&mut Self>, args: R) -> State<Self::Yielded, Self::Complete> {
        let this: Pin<&mut T> = Pin::new((*self).as_mut());
        Coroutine::resume(this, args)
    }
}

impl<R, T> Coroutine<R> for Pin<Box<T>>
where
    T: Coroutine<R> + ?Sized,
{
    type Yielded = <T as Coroutine<R>>::Yielded;
    type Complete = <T as Coroutine<R>>::Complete;

    fn resume(self: Pin<&mut Self>, args: R) -> State<Self::Yielded, Self::Complete> {
        let this: Pin<&mut T> = Pin::into_inner(self).as_mut();
        Coroutine::resume(this, args)
    }
}
