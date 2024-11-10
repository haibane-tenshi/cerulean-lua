use std::cell::RefCell;
use std::marker::PhantomData;

use gc::userdata::Params;

use crate::error::{BorrowError, RtError};
use crate::ffi::tuple::{NonEmptyTuple, Tuple, TupleHead, TupleTail};
use crate::runtime::RuntimeView;
use crate::value::Types;

pub use gc::userdata::{FullUserdata, Userdata};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodId<'a> {
    pub scope: &'a str,
    pub name: &'a str,
}

pub struct DefaultParams<Ty>(PhantomData<Ty>);

impl<Ty> Params for DefaultParams<Ty>
where
    Ty: Types,
{
    type Id<'id> = MethodId<'id>;
    type Rt<'rt> = RuntimeView<'rt, Ty>;
    type Res = Result<(), RtError<Ty>>;
}

pub enum Method<Ref, Mut, Val> {
    Ref(Ref),
    Mut(Mut),
    Val(Val),
}

pub trait DispatchMethod<Marker, Ty>: Sized
where
    Ty: Types,
{
    const SCOPE_NAME: &'static str;

    type Ref;
    type Mut;
    type Val;

    fn dispatch_method(name: &str) -> Option<Method<Self::Ref, Self::Mut, Self::Val>>;

    fn call(&self, method: Self::Ref, rt: RuntimeView<'_, Ty>) -> Result<(), RtError<Ty>>;
    fn call_mut(&mut self, method: Self::Mut, rt: RuntimeView<'_, Ty>) -> Result<(), RtError<Ty>>;
    fn call_once(self, method: Self::Val, rt: RuntimeView<'_, Ty>) -> Result<(), RtError<Ty>>;
}

impl<Marker, Ty, T> DispatchMethod<Marker, Ty> for RefCell<T>
where
    Ty: Types,
    T: DispatchMethod<Marker, Ty>,
{
    const SCOPE_NAME: &'static str = <T as DispatchMethod<Marker, Ty>>::SCOPE_NAME;

    type Ref = Method<
        <T as DispatchMethod<Marker, Ty>>::Ref,
        <T as DispatchMethod<Marker, Ty>>::Mut,
        std::convert::Infallible,
    >;
    type Mut = std::convert::Infallible;
    type Val = <T as DispatchMethod<Marker, Ty>>::Val;

    fn dispatch_method(name: &str) -> Option<Method<Self::Ref, Self::Mut, Self::Val>> {
        let r = match T::dispatch_method(name)? {
            Method::Ref(t) => Method::Ref(Method::Ref(t)),
            Method::Mut(t) => Method::Ref(Method::Mut(t)),
            Method::Val(t) => Method::Val(t),
        };

        Some(r)
    }

    fn call(&self, method: Self::Ref, rt: RuntimeView<'_, Ty>) -> Result<(), RtError<Ty>> {
        match method {
            Method::Ref(m) => self.try_borrow().map_err(|_| BorrowError::Ref)?.call(m, rt),
            Method::Mut(m) => self
                .try_borrow_mut()
                .map_err(|_| BorrowError::Mut)?
                .call_mut(m, rt),
            Method::Val(m) => match m {},
        }
    }

    fn call_mut(&mut self, method: Self::Mut, _rt: RuntimeView<'_, Ty>) -> Result<(), RtError<Ty>> {
        match method {}
    }

    fn call_once(self, method: Self::Val, rt: RuntimeView<'_, Ty>) -> Result<(), RtError<Ty>> {
        self.into_inner().call_once(method, rt)
    }
}

impl<Marker, Ty, T> DispatchMethod<Marker, Ty> for Option<T>
where
    Ty: Types,
    Ty::String: From<&'static str>,
    T: DispatchMethod<Marker, Ty>,
{
    const SCOPE_NAME: &'static str = <T as DispatchMethod<Marker, Ty>>::SCOPE_NAME;

    type Ref = <T as DispatchMethod<Marker, Ty>>::Ref;
    type Mut = Method<
        std::convert::Infallible,
        <T as DispatchMethod<Marker, Ty>>::Mut,
        <T as DispatchMethod<Marker, Ty>>::Val,
    >;
    type Val = std::convert::Infallible;

    fn dispatch_method(name: &str) -> Option<Method<Self::Ref, Self::Mut, Self::Val>> {
        let r = match T::dispatch_method(name)? {
            Method::Ref(t) => Method::Ref(t),
            Method::Mut(t) => Method::Mut(Method::Mut(t)),
            Method::Val(t) => Method::Mut(Method::Val(t)),
        };

        Some(r)
    }

    fn call(&self, method: Self::Ref, rt: RuntimeView<'_, Ty>) -> Result<(), RtError<Ty>> {
        use crate::error::AlreadyDroppedError;

        let Some(value) = self else {
            return Err(AlreadyDroppedError.into());
        };

        value.call(method, rt)
    }

    fn call_mut(&mut self, method: Self::Mut, rt: RuntimeView<'_, Ty>) -> Result<(), RtError<Ty>> {
        use crate::error::AlreadyDroppedError;

        match method {
            Method::Ref(m) => match m {},
            Method::Mut(m) => {
                let Some(value) = self else {
                    return Err(AlreadyDroppedError.into());
                };

                value.call_mut(m, rt)
            }
            Method::Val(m) => {
                let Some(value) = self.take() else {
                    return Err(AlreadyDroppedError.into());
                };

                value.call_once(m, rt)
            }
        }
    }

    fn call_once(self, method: Self::Val, _rt: RuntimeView<'_, Ty>) -> Result<(), RtError<Ty>> {
        match method {}
    }
}

pub trait DispatchTrait<Traits, Ty>: Sized
where
    Traits: Tuple,
    Ty: Types,
{
    fn dispatch_trait(
        &self,
        id: MethodId,
        rt: RuntimeView<'_, Ty>,
    ) -> Option<Result<(), RtError<Ty>>>;
}

impl<Ty, T> DispatchTrait<(), Ty> for T
where
    Ty: Types,
{
    fn dispatch_trait(
        &self,
        _id: MethodId,
        _rt: RuntimeView<'_, Ty>,
    ) -> Option<Result<(), RtError<Ty>>> {
        None
    }
}

impl<T, Tup, Ty> DispatchTrait<Tup, Ty> for T
where
    Ty: Types,
    Tup: NonEmptyTuple,
    T: DispatchMethod<TupleHead<Tup>, Ty>,
    T: DispatchTrait<TupleTail<Tup>, Ty>,
{
    fn dispatch_trait(
        &self,
        id: MethodId,
        rt: RuntimeView<'_, Ty>,
    ) -> Option<Result<(), RtError<Ty>>> {
        if id.scope == T::SCOPE_NAME {
            if let Some(method) = T::dispatch_method(id.name) {
                let r = if let Method::Ref(f) = method {
                    T::call(self, f, rt)
                } else {
                    let err = rt.core.alloc_error_msg(
                        "userdata can only dispatch on methods with `&self` receiver",
                    );
                    Err(err)
                };

                return Some(r);
            }
        }

        <T as DispatchTrait<TupleTail<Tup>, Ty>>::dispatch_trait(self, id, rt)
    }
}
