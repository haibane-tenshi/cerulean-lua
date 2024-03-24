use std::cell::RefCell;
use std::fmt::Debug;

use gc::{GcCell, Trace};

use super::Metatable;
use crate::error::{BorrowError, RuntimeError};
use crate::ffi::tuple::{NonEmptyTuple, Tuple, TupleHead, TupleTail};
use crate::runtime::RuntimeView;
use crate::value::{CoreTypes, StrongValue};

pub trait Userdata<Ty, Conv>: Trace
where
    Ty: CoreTypes,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Option<Result<(), RuntimeError<StrongValue<Ty>>>>;
}

pub enum Method<Ref, Mut, Val> {
    Ref(Ref),
    Mut(Mut),
    Val(Val),
}

pub trait DispatchMethod<Marker, Ty, Conv>: Sized
where
    Ty: CoreTypes,
{
    const SCOPE_NAME: &'static str;

    type Ref;
    type Mut;
    type Val;

    fn dispatch_method(name: &str) -> Option<Method<Self::Ref, Self::Mut, Self::Val>>;

    fn call(
        &self,
        method: Self::Ref,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>>;
    fn call_mut(
        &mut self,
        method: Self::Mut,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>>;
    fn call_once(
        self,
        method: Self::Val,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>>;
}

impl<Marker, Ty, Conv, T> DispatchMethod<Marker, Ty, Conv> for RefCell<T>
where
    Ty: CoreTypes,
    T: DispatchMethod<Marker, Ty, Conv>,
{
    const SCOPE_NAME: &'static str = <T as DispatchMethod<Marker, Ty, Conv>>::SCOPE_NAME;

    type Ref = Method<
        <T as DispatchMethod<Marker, Ty, Conv>>::Ref,
        <T as DispatchMethod<Marker, Ty, Conv>>::Mut,
        std::convert::Infallible,
    >;
    type Mut = std::convert::Infallible;
    type Val = <T as DispatchMethod<Marker, Ty, Conv>>::Val;

    fn dispatch_method(name: &str) -> Option<Method<Self::Ref, Self::Mut, Self::Val>> {
        let r = match T::dispatch_method(name)? {
            Method::Ref(t) => Method::Ref(Method::Ref(t)),
            Method::Mut(t) => Method::Ref(Method::Mut(t)),
            Method::Val(t) => Method::Val(t),
        };

        Some(r)
    }

    fn call(
        &self,
        method: Self::Ref,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
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
        _rt: RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
        match method {}
    }

    fn call_once(
        self,
        method: Self::Val,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
        self.into_inner().call_once(method, rt)
    }
}

impl<Marker, Ty, Conv, T> DispatchMethod<Marker, Ty, Conv> for Option<T>
where
    Ty: CoreTypes,
    Ty::String: From<&'static str>,
    T: DispatchMethod<Marker, Ty, Conv>,
{
    const SCOPE_NAME: &'static str = <T as DispatchMethod<Marker, Ty, Conv>>::SCOPE_NAME;

    type Ref = <T as DispatchMethod<Marker, Ty, Conv>>::Ref;
    type Mut = Method<
        std::convert::Infallible,
        <T as DispatchMethod<Marker, Ty, Conv>>::Mut,
        <T as DispatchMethod<Marker, Ty, Conv>>::Val,
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

    fn call(
        &self,
        method: Self::Ref,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
        use crate::error::AlreadyDroppedError;

        let Some(value) = self else {
            return Err(AlreadyDroppedError.into());
        };

        value.call(method, rt)
    }

    fn call_mut(
        &mut self,
        method: Self::Mut,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
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
        _rt: RuntimeView<'_, Ty, Conv>,
    ) -> Result<(), RuntimeError<StrongValue<Ty>>> {
        match method {}
    }
}

pub trait DispatchTrait<Traits, Ty, Conv>: Sized
where
    Traits: Tuple,
    Ty: CoreTypes,
{
    fn dispatch_trait(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Option<Result<(), RuntimeError<StrongValue<Ty>>>>;
}

impl<Ty, Conv, T> DispatchTrait<(), Ty, Conv> for T
where
    Ty: CoreTypes,
{
    fn dispatch_trait(
        &self,
        _scope: &str,
        _name: &str,
        _rt: RuntimeView<'_, Ty, Conv>,
    ) -> Option<Result<(), RuntimeError<StrongValue<Ty>>>> {
        None
    }
}

impl<T, Tup, Ty, Conv> DispatchTrait<Tup, Ty, Conv> for T
where
    Ty: CoreTypes,
    Tup: NonEmptyTuple,
    T: DispatchMethod<TupleHead<Tup>, Ty, Conv>,
    T: DispatchTrait<TupleTail<Tup>, Ty, Conv>,
{
    fn dispatch_trait(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Option<Result<(), RuntimeError<StrongValue<Ty>>>> {
        if scope == T::SCOPE_NAME {
            if let Some(method) = T::dispatch_method(name) {
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

        <T as DispatchTrait<TupleTail<Tup>, Ty, Conv>>::dispatch_trait(self, scope, name, rt)
    }
}

// Self: 'static requires Ty: 'static
// see https://github.com/rust-lang/rust/issues/57325
// https://rust-lang.github.io/rfcs/1214-projections-lifetimes-and-wf.html
pub struct Dispatchable<T, Ty, Conv>
where
    Ty: CoreTypes,
{
    value: T,
    #[allow(clippy::type_complexity)]
    dispatcher: fn(
        &T,
        &str,
        &str,
        RuntimeView<'_, Ty, Conv>,
    ) -> Option<Result<(), RuntimeError<StrongValue<Ty>>>>,
}

impl<T, Ty, Conv> Dispatchable<T, Ty, Conv>
where
    Ty: CoreTypes,
{
    pub fn new<Traits>(value: T) -> Self
    where
        Traits: Tuple,
        T: DispatchTrait<Traits, Ty, Conv>,
    {
        Dispatchable {
            value,
            dispatcher: <T as DispatchTrait<Traits, Ty, Conv>>::dispatch_trait,
        }
    }
}

impl<T, Ty, Conv> Trace for Dispatchable<T, Ty, Conv>
where
    T: Trace,
    Conv: 'static,
    Ty: CoreTypes,
{
    fn trace(&self, collector: &mut gc::Collector) {
        let Dispatchable {
            value,
            dispatcher: _,
        } = self;

        value.trace(collector);
    }
}

impl<T, Ty, Conv> Userdata<Ty, Conv> for Dispatchable<T, Ty, Conv>
where
    T: Trace,
    Conv: 'static,
    Ty: CoreTypes,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Option<Result<(), RuntimeError<StrongValue<Ty>>>> {
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

impl<T, Traits> Trace for DispatchableStatic<T, Traits>
where
    T: Trace,
    Traits: 'static,
{
    fn trace(&self, collector: &mut gc::Collector) {
        let DispatchableStatic { value, traits: _ } = self;

        value.trace(collector);
    }
}

impl<T, Traits, Ty, Conv> Userdata<Ty, Conv> for DispatchableStatic<T, Traits>
where
    Ty: CoreTypes,
    Traits: Tuple + 'static,
    T: DispatchTrait<Traits, Ty, Conv> + Trace,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Option<Result<(), RuntimeError<StrongValue<Ty>>>> {
        self.value.dispatch_trait(scope, name, rt)
    }
}

pub trait FullUserdata<Ty, Conv>: Userdata<Ty, Conv> + Metatable<GcCell<Ty::Table>>
where
    Ty: CoreTypes,
{
}

impl<Ty, Conv, T> FullUserdata<Ty, Conv> for T
where
    Ty: CoreTypes,
    T: Userdata<Ty, Conv> + Metatable<GcCell<Ty::Table>>,
{
}

impl<Ty, Conv, T> Userdata<Ty, Conv> for Box<T>
where
    Ty: CoreTypes,
    T: Userdata<Ty, Conv> + ?Sized,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Option<Result<(), RuntimeError<StrongValue<Ty>>>> {
        self.as_ref().method(scope, name, rt)
    }
}

impl<Table, T> Metatable<Table> for Box<T>
where
    T: Metatable<Table> + ?Sized,
{
    fn metatable(&self) -> Option<Table> {
        self.as_ref().metatable()
    }

    fn set_metatable(&mut self, mt: Option<Table>) -> Option<Table> {
        self.as_mut().set_metatable(mt)
    }
}

pub fn new_full_userdata<T, Ty, Conv>(
    value: T,
    metatable: Option<GcCell<Ty::Table>>,
) -> impl FullUserdata<Ty, Conv>
where
    Ty: CoreTypes,
    T: Userdata<Ty, Conv>,
{
    UserdataValue { value, metatable }
}

#[doc(hidden)]
struct UserdataValue<T: ?Sized, TableRef> {
    metatable: Option<TableRef>,
    value: T,
}

impl<T, TableRef> AsRef<T> for UserdataValue<T, TableRef>
where
    T: ?Sized,
{
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T, Table> Trace for UserdataValue<T, Table>
where
    T: Trace + ?Sized,
    Table: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        let UserdataValue { metatable, value } = self;

        value.trace(collector);
        metatable.trace(collector);
    }
}

impl<T, Ty, Conv, Table> Userdata<Ty, Conv> for UserdataValue<T, Table>
where
    Ty: CoreTypes,
    T: Userdata<Ty, Conv> + Trace + ?Sized,
    Table: Trace,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Ty, Conv>,
    ) -> Option<Result<(), RuntimeError<StrongValue<Ty>>>> {
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

impl<T, Table> Metatable<Table> for UserdataValue<T, Table>
where
    Table: Clone,
{
    fn metatable(&self) -> Option<Table> {
        self.metatable.clone()
    }

    fn set_metatable(&mut self, mt: Option<Table>) -> Option<Table> {
        self.metatable.replace(mt?)
    }
}
