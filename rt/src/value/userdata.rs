use std::cell::RefCell;
use std::fmt::Debug;

use gc::Trace;

use super::Metatable;
use crate::error::{BorrowError, RuntimeError};
use crate::ffi::tuple::{NonEmptyTuple, Tuple, TupleHead, TupleTail};
use crate::gc::GcTable;
use crate::runtime::RuntimeView;
use crate::value::{CoreTypes, Value};

pub trait Userdata<Gc>: Trace
where
    Gc: CoreTypes,
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
    Gc: CoreTypes,
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
    Gc: CoreTypes,
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
    Gc: CoreTypes,
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
    Gc: CoreTypes,
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
    Gc: CoreTypes,
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
    Gc: CoreTypes,
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
                    let msg = crate::gc::StringRef::new(
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

// Self: 'static requires Ty: 'static
// see https://github.com/rust-lang/rust/issues/57325
// https://rust-lang.github.io/rfcs/1214-projections-lifetimes-and-wf.html
pub struct Dispatchable<T, Ty: CoreTypes> {
    value: T,
    dispatcher: fn(&T, &str, &str, RuntimeView<'_, Ty>) -> Option<Result<(), RuntimeError<Ty>>>,
}

impl<T, Ty> Dispatchable<T, Ty>
where
    Ty: CoreTypes,
{
    pub fn new<Traits>(value: T) -> Self
    where
        Traits: Tuple,
        T: DispatchTrait<Traits, Ty>,
    {
        Dispatchable {
            value,
            dispatcher: <T as DispatchTrait<Traits, Ty>>::dispatch_trait,
        }
    }
}

impl<T, Ty> Trace for Dispatchable<T, Ty>
where
    T: Trace,
    Ty: CoreTypes + 'static,
{
    fn trace(&self, collector: &mut gc::Collector) {
        let Dispatchable {
            value,
            dispatcher: _,
        } = self;

        value.trace(collector);
    }
}

impl<T, Ty> Userdata<Ty> for Dispatchable<T, Ty>
where
    T: Trace,
    Ty: CoreTypes + 'static,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Ty>,
    ) -> Option<Result<(), RuntimeError<Ty>>> {
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

impl<T, Traits, Gc> Userdata<Gc> for DispatchableStatic<T, Traits>
where
    Gc: CoreTypes,
    Traits: Tuple + 'static,
    T: DispatchTrait<Traits, Gc> + Trace,
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

pub trait FullUserdata<Ty>: Userdata<Ty> + Metatable<GcTable<Ty::Table>>
where
    Ty: CoreTypes,
{
}

impl<Ty, T> FullUserdata<Ty> for T
where
    Ty: CoreTypes,
    T: Userdata<Ty> + Metatable<GcTable<Ty::Table>>,
{
}

impl<Ty, T> Userdata<Ty> for Box<T>
where
    Ty: CoreTypes,
    T: Userdata<Ty> + ?Sized,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Ty>,
    ) -> Option<Result<(), RuntimeError<Ty>>> {
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

pub fn new_full_userdata<T, Ty>(
    value: T,
    metatable: Option<GcTable<Ty::Table>>,
) -> impl FullUserdata<Ty>
where
    Ty: CoreTypes,
    T: Userdata<Ty>,
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

impl<T, Ty, Table> Userdata<Ty> for UserdataValue<T, Table>
where
    Ty: CoreTypes,
    T: Userdata<Ty> + Trace + ?Sized,
    Table: Trace,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Ty>,
    ) -> Option<Result<(), RuntimeError<Ty>>> {
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
