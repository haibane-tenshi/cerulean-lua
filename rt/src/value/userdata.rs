use std::cell::RefCell;
use std::fmt::Debug;

use super::Metatable;
use crate::error::{BorrowError, RuntimeError};
use crate::ffi::tuple::{NonEmptyTuple, Tuple, TupleHead, TupleTail};
use crate::gc::Gc as GarbageCollector;
use crate::runtime::RuntimeView;
use crate::value::{TypeProvider, Value};

pub trait Userdata<Gc>
where
    Gc: TypeProvider,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc>,
    ) -> Option<Result<(), RuntimeError<Gc>>>;
}

pub enum Method<Ref, Mut, Val> {
    Ref(Ref),
    Mut(Mut),
    Val(Val),
}

pub trait DispatchMethod<Marker, Gc>: Sized
where
    Gc: TypeProvider,
{
    const SCOPE_NAME: &'static str;

    type Ref;
    type Mut;
    type Val;

    fn dispatch_method(name: &str) -> Option<Method<Self::Ref, Self::Mut, Self::Val>>;

    fn call(&self, method: Self::Ref, rt: RuntimeView<'_, Gc>) -> Result<(), RuntimeError<Gc>>;
    fn call_mut(
        &mut self,
        method: Self::Mut,
        rt: RuntimeView<'_, Gc>,
    ) -> Result<(), RuntimeError<Gc>>;
    fn call_once(self, method: Self::Val, rt: RuntimeView<'_, Gc>) -> Result<(), RuntimeError<Gc>>;
}

impl<Marker, Gc, T> DispatchMethod<Marker, Gc> for RefCell<T>
where
    Gc: TypeProvider,
    T: DispatchMethod<Marker, Gc>,
{
    const SCOPE_NAME: &'static str = <T as DispatchMethod<Marker, Gc>>::SCOPE_NAME;

    type Ref = Method<
        <T as DispatchMethod<Marker, Gc>>::Ref,
        <T as DispatchMethod<Marker, Gc>>::Mut,
        std::convert::Infallible,
    >;
    type Mut = std::convert::Infallible;
    type Val = <T as DispatchMethod<Marker, Gc>>::Val;

    fn dispatch_method(name: &str) -> Option<Method<Self::Ref, Self::Mut, Self::Val>> {
        let r = match T::dispatch_method(name)? {
            Method::Ref(t) => Method::Ref(Method::Ref(t)),
            Method::Mut(t) => Method::Ref(Method::Mut(t)),
            Method::Val(t) => Method::Val(t),
        };

        Some(r)
    }

    fn call(&self, method: Self::Ref, rt: RuntimeView<'_, Gc>) -> Result<(), RuntimeError<Gc>> {
        match method {
            Method::Ref(m) => self.try_borrow().map_err(|_| BorrowError::Ref)?.call(m, rt),
            Method::Mut(m) => self
                .try_borrow_mut()
                .map_err(|_| BorrowError::Mut)?
                .call_mut(m, rt),
            Method::Val(m) => match m {},
        }
    }

    fn call_mut(
        &mut self,
        method: Self::Mut,
        _rt: RuntimeView<'_, Gc>,
    ) -> Result<(), RuntimeError<Gc>> {
        match method {}
    }

    fn call_once(self, method: Self::Val, rt: RuntimeView<'_, Gc>) -> Result<(), RuntimeError<Gc>> {
        self.into_inner().call_once(method, rt)
    }
}

impl<Marker, Gc, T> DispatchMethod<Marker, Gc> for Option<T>
where
    Gc: TypeProvider,
    Gc::String: From<&'static str>,
    T: DispatchMethod<Marker, Gc>,
{
    const SCOPE_NAME: &'static str = <T as DispatchMethod<Marker, Gc>>::SCOPE_NAME;

    type Ref = <T as DispatchMethod<Marker, Gc>>::Ref;
    type Mut = Method<
        std::convert::Infallible,
        <T as DispatchMethod<Marker, Gc>>::Mut,
        <T as DispatchMethod<Marker, Gc>>::Val,
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

    fn call(&self, method: Self::Ref, rt: RuntimeView<'_, Gc>) -> Result<(), RuntimeError<Gc>> {
        use crate::error::AlreadyDroppedError;

        let Some(value) = self else {
            return Err(AlreadyDroppedError.into());
        };

        value.call(method, rt)
    }

    fn call_mut(
        &mut self,
        method: Self::Mut,
        rt: RuntimeView<'_, Gc>,
    ) -> Result<(), RuntimeError<Gc>> {
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

    fn call_once(
        self,
        method: Self::Val,
        _rt: RuntimeView<'_, Gc>,
    ) -> Result<(), RuntimeError<Gc>> {
        match method {}
    }
}

pub trait DispatchTrait<Traits: Tuple, Gc>: Sized
where
    Gc: TypeProvider,
{
    fn dispatch_trait(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc>,
    ) -> Option<Result<(), RuntimeError<Gc>>>;
}

impl<Gc, T> DispatchTrait<(), Gc> for T
where
    Gc: TypeProvider,
{
    fn dispatch_trait(
        &self,
        _scope: &str,
        _name: &str,
        _rt: RuntimeView<'_, Gc>,
    ) -> Option<Result<(), RuntimeError<Gc>>> {
        None
    }
}

impl<T, Tup, Gc> DispatchTrait<Tup, Gc> for T
where
    Gc: GarbageCollector,
    Tup: NonEmptyTuple,
    T: DispatchMethod<TupleHead<Tup>, Gc>,
    T: DispatchTrait<TupleTail<Tup>, Gc>,
{
    fn dispatch_trait(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc>,
    ) -> Option<Result<(), RuntimeError<Gc>>> {
        if scope == T::SCOPE_NAME {
            if let Some(method) = T::dispatch_method(name) {
                let r = if let Method::Ref(f) = method {
                    T::call(self, f, rt)
                } else {
                    let msg = rt.core.gc.alloc_string(
                        "userdata can only dispatch on methods with `&self` receiver".into(),
                    );
                    Err(Value::String(msg).into())
                };

                return Some(r);
            }
        }

        <T as DispatchTrait<TupleTail<Tup>, Gc>>::dispatch_trait(self, scope, name, rt)
    }
}

// Self: 'static requires C: 'static
// see https://github.com/rust-lang/rust/issues/57325
// https://rust-lang.github.io/rfcs/1214-projections-lifetimes-and-wf.html
pub struct Dispatchable<T, Gc: TypeProvider> {
    value: T,
    dispatcher: fn(&T, &str, &str, RuntimeView<'_, Gc>) -> Option<Result<(), RuntimeError<Gc>>>,
}

impl<T, Gc> Dispatchable<T, Gc>
where
    Gc: TypeProvider,
{
    pub fn new<Traits>(value: T) -> Self
    where
        Traits: Tuple,
        T: DispatchTrait<Traits, Gc>,
    {
        Dispatchable {
            value,
            dispatcher: <T as DispatchTrait<Traits, Gc>>::dispatch_trait,
        }
    }
}

impl<T, Gc> Userdata<Gc> for Dispatchable<T, Gc>
where
    Gc: TypeProvider,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc>,
    ) -> Option<Result<(), RuntimeError<Gc>>> {
        (self.dispatcher)(&self.value, scope, name, rt)
    }
}

pub struct DispatchableStatic<T, Traits> {
    value: T,
    traits: std::marker::PhantomData<Traits>,
}

impl<T, Traits> DispatchableStatic<T, Traits> {
    pub fn new(value: T) -> Self {
        DispatchableStatic {
            value,
            traits: std::marker::PhantomData,
        }
    }
}

impl<T, Traits, Gc> Userdata<Gc> for DispatchableStatic<T, Traits>
where
    Gc: TypeProvider,
    Traits: Tuple,
    T: DispatchTrait<Traits, Gc>,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc>,
    ) -> Option<Result<(), RuntimeError<Gc>>> {
        self.value.dispatch_trait(scope, name, rt)
    }
}

pub type FullUserdata<Gc> = UserdataValue<dyn Userdata<Gc>, <Gc as TypeProvider>::TableRef>;

#[doc(hidden)]
pub struct UserdataValue<T: ?Sized, TableRef> {
    pub(crate) metatable: RefCell<Option<TableRef>>,
    pub(crate) value: T,
}

impl<T, Gc> Userdata<Gc> for UserdataValue<T, Gc::TableRef>
where
    Gc: TypeProvider,
    T: Userdata<Gc> + ?Sized,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc>,
    ) -> Option<Result<(), RuntimeError<Gc>>> {
        self.as_ref().method(scope, name, rt)
    }
}

impl<T: ?Sized, TableRef> Debug for UserdataValue<T, TableRef> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UserdataValue")
            .field("metatable", &"<omitted>")
            .field("value", &"<omitted>")
            .finish()
    }
}

impl<Gc> Metatable<Gc::TableRef> for FullUserdata<Gc>
where
    Gc: TypeProvider,
{
    fn metatable(&self) -> Option<Gc::TableRef> {
        self.metatable.borrow().clone()
    }

    fn set_metatable(&mut self, mt: Option<Gc::TableRef>) -> Option<Gc::TableRef> {
        self.metatable.replace(mt)
    }
}

impl<T, TableRef> AsRef<T> for UserdataValue<T, TableRef>
where
    T: ?Sized,
{
    fn as_ref(&self) -> &T {
        &self.value
    }
}
