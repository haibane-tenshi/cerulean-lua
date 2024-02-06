use std::cell::RefCell;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use crate::error::{BorrowError, RuntimeError};
use crate::ffi::tuple::{NonEmptyTuple, Tuple, TupleHead, TupleTail};
use crate::runtime::RuntimeView;
use crate::value::{TypeMismatchError, TypeProvider, Value};

pub trait Userdata<Gc, C>
where
    Gc: TypeProvider,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc, C>,
    ) -> Option<Result<(), RuntimeError<Gc>>>;
}

pub enum Method<Ref, Mut, Val> {
    Ref(Ref),
    Mut(Mut),
    Val(Val),
}

pub trait DispatchMethod<Marker, Gc, C>: Sized
where
    Gc: TypeProvider,
{
    const SCOPE_NAME: &'static str;

    type Ref;
    type Mut;
    type Val;

    fn dispatch_method(name: &str) -> Option<Method<Self::Ref, Self::Mut, Self::Val>>;

    fn call(&self, method: Self::Ref, rt: RuntimeView<'_, Gc, C>) -> Result<(), RuntimeError<Gc>>;
    fn call_mut(
        &mut self,
        method: Self::Mut,
        rt: RuntimeView<'_, Gc, C>,
    ) -> Result<(), RuntimeError<Gc>>;
    fn call_once(
        self,
        method: Self::Val,
        rt: RuntimeView<'_, Gc, C>,
    ) -> Result<(), RuntimeError<Gc>>;
}

impl<Marker, Gc, C, T> DispatchMethod<Marker, Gc, C> for RefCell<T>
where
    Gc: TypeProvider,
    Gc::String: From<&'static str>,
    T: DispatchMethod<Marker, Gc, C>,
{
    const SCOPE_NAME: &'static str = <T as DispatchMethod<Marker, Gc, C>>::SCOPE_NAME;

    type Ref = Method<
        <T as DispatchMethod<Marker, Gc, C>>::Ref,
        <T as DispatchMethod<Marker, Gc, C>>::Mut,
        std::convert::Infallible,
    >;
    type Mut = std::convert::Infallible;
    type Val = <T as DispatchMethod<Marker, Gc, C>>::Val;

    fn dispatch_method(name: &str) -> Option<Method<Self::Ref, Self::Mut, Self::Val>> {
        let r = match T::dispatch_method(name)? {
            Method::Ref(t) => Method::Ref(Method::Ref(t)),
            Method::Mut(t) => Method::Ref(Method::Mut(t)),
            Method::Val(t) => Method::Val(t),
        };

        Some(r)
    }

    fn call(&self, method: Self::Ref, rt: RuntimeView<'_, Gc, C>) -> Result<(), RuntimeError<Gc>> {
        match method {
            Method::Ref(m) => self
                .try_borrow()
                .map_err(|_| Value::String("value is already mutably borrowed".into()))?
                .call(m, rt),
            Method::Mut(m) => self
                .try_borrow_mut()
                .map_err(|_| Value::String("value is already borrowed".into()))?
                .call_mut(m, rt),
            Method::Val(m) => match m {},
        }
    }

    fn call_mut(
        &mut self,
        method: Self::Mut,
        _rt: RuntimeView<'_, Gc, C>,
    ) -> Result<(), RuntimeError<Gc>> {
        match method {}
    }

    fn call_once(
        self,
        method: Self::Val,
        rt: RuntimeView<'_, Gc, C>,
    ) -> Result<(), RuntimeError<Gc>> {
        self.into_inner().call_once(method, rt)
    }
}

impl<Marker, Gc, C, T> DispatchMethod<Marker, Gc, C> for Option<T>
where
    Gc: TypeProvider,
    Gc::String: From<&'static str>,
    T: DispatchMethod<Marker, Gc, C>,
{
    const SCOPE_NAME: &'static str = <T as DispatchMethod<Marker, Gc, C>>::SCOPE_NAME;

    type Ref = <T as DispatchMethod<Marker, Gc, C>>::Ref;
    type Mut = Method<
        std::convert::Infallible,
        <T as DispatchMethod<Marker, Gc, C>>::Mut,
        <T as DispatchMethod<Marker, Gc, C>>::Val,
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

    fn call(&self, method: Self::Ref, rt: RuntimeView<'_, Gc, C>) -> Result<(), RuntimeError<Gc>> {
        let Some(value) = self else {
            return Err(Value::String("value is already moved out".into()).into());
        };

        value.call(method, rt)
    }

    fn call_mut(
        &mut self,
        method: Self::Mut,
        rt: RuntimeView<'_, Gc, C>,
    ) -> Result<(), RuntimeError<Gc>> {
        match method {
            Method::Ref(m) => match m {},
            Method::Mut(m) => {
                let Some(value) = self else {
                    return Err(Value::String("value is already moved out".into()).into());
                };

                value.call_mut(m, rt)
            }
            Method::Val(m) => {
                let Some(value) = self.take() else {
                    return Err(Value::String("value is already moved out".into()).into());
                };

                value.call_once(m, rt)
            }
        }
    }

    fn call_once(
        self,
        method: Self::Val,
        _rt: RuntimeView<'_, Gc, C>,
    ) -> Result<(), RuntimeError<Gc>> {
        match method {}
    }
}

pub trait DispatchTrait<Traits: Tuple, Gc, C>: Sized
where
    Gc: TypeProvider,
{
    fn dispatch_trait(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc, C>,
    ) -> Option<Result<(), RuntimeError<Gc>>>;
}

impl<Gc, Cache, T> DispatchTrait<(), Gc, Cache> for T
where
    Gc: TypeProvider,
{
    fn dispatch_trait(
        &self,
        _scope: &str,
        _name: &str,
        _rt: RuntimeView<'_, Gc, Cache>,
    ) -> Option<Result<(), RuntimeError<Gc>>> {
        None
    }
}

impl<T, Tup, Gc, Cache> DispatchTrait<Tup, Gc, Cache> for T
where
    Gc: TypeProvider,
    Gc::String: From<&'static str>,
    Tup: NonEmptyTuple,
    T: DispatchMethod<TupleHead<Tup>, Gc, Cache>,
    T: DispatchTrait<TupleTail<Tup>, Gc, Cache>,
{
    fn dispatch_trait(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc, Cache>,
    ) -> Option<Result<(), RuntimeError<Gc>>> {
        if scope == T::SCOPE_NAME {
            if let Some(method) = T::dispatch_method(name) {
                let r = if let Method::Ref(f) = method {
                    T::call(self, f, rt)
                } else {
                    Err(Value::String(
                        "userdata can only dispatch on methods with `&self` receiver".into(),
                    )
                    .into())
                };

                return Some(r);
            }
        }

        <T as DispatchTrait<TupleTail<Tup>, Gc, Cache>>::dispatch_trait(self, scope, name, rt)
    }
}

// Self: 'static requires C: 'static
// see https://github.com/rust-lang/rust/issues/57325
// https://rust-lang.github.io/rfcs/1214-projections-lifetimes-and-wf.html
pub struct Dispatchable<T, Gc: TypeProvider, C> {
    value: T,
    dispatcher: fn(&T, &str, &str, RuntimeView<'_, Gc, C>) -> Option<Result<(), RuntimeError<Gc>>>,
}

impl<T, Gc, C> Dispatchable<T, Gc, C>
where
    Gc: TypeProvider,
{
    pub fn new<Traits>(value: T) -> Self
    where
        Traits: Tuple,
        T: DispatchTrait<Traits, Gc, C>,
    {
        Dispatchable {
            value,
            dispatcher: <T as DispatchTrait<Traits, Gc, C>>::dispatch_trait,
        }
    }
}

impl<T, Gc, C> Userdata<Gc, C> for Dispatchable<T, Gc, C>
where
    Gc: TypeProvider,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc, C>,
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

impl<T, Traits, Gc, C> Userdata<Gc, C> for DispatchableStatic<T, Traits>
where
    Gc: TypeProvider,
    Traits: Tuple,
    T: DispatchTrait<Traits, Gc, C>,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc, C>,
    ) -> Option<Result<(), RuntimeError<Gc>>> {
        self.value.dispatch_trait(scope, name, rt)
    }
}

#[doc(hidden)]
pub struct UserdataValue<T: ?Sized, TableRef> {
    metatable: RefCell<Option<TableRef>>,
    value: T,
}

impl<T, TableRef> UserdataValue<T, TableRef> {
    pub(crate) fn new(value: T) -> Self {
        UserdataValue {
            metatable: Default::default(),
            value,
        }
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

pub type FullUserdata<Gc, C> = UserdataValue<dyn Userdata<Gc, C>, <Gc as TypeProvider>::TableRef>;

impl<Gc, C> super::Metatable<Gc::TableRef> for FullUserdata<Gc, C>
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

pub struct UserdataRef<Gc: TypeProvider, C>(Rc<UserdataValue<dyn Userdata<Gc, C>, Gc::TableRef>>);

impl<Gc, C> UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    pub fn new<U>(userdata: U) -> Self
    where
        U: Userdata<Gc, C> + 'static,
    {
        let value = UserdataValue {
            metatable: Default::default(),
            value: userdata,
        };

        UserdataRef(Rc::new(value))
    }

    pub fn with_dispatcher<Traits, T>(value: T) -> Self
    where
        Traits: Tuple + 'static,
        T: DispatchTrait<Traits, Gc, C> + 'static,
    {
        let value = UserdataValue {
            metatable: Default::default(),
            value: DispatchableStatic::new(value),
        };

        UserdataRef(Rc::new(value))
    }

    pub fn set_metatable(&self, metatable: Option<Gc::TableRef>) -> Option<Gc::TableRef> {
        self.0.metatable.replace(metatable)
    }
}

impl<Gc, C> UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    pub fn metatable(&self) -> Option<Gc::TableRef> {
        self.0.metatable.borrow().clone()
    }
}

impl<Gc, C> Userdata<Gc, C> for UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc, C>,
    ) -> Option<Result<(), RuntimeError<Gc>>> {
        self.0.value.method(scope, name, rt)
    }
}

impl<Gc, C> super::Borrow<FullUserdata<Gc, C>> for UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    type Error = BorrowError;

    fn with_ref<R>(&self, f: impl FnOnce(&FullUserdata<Gc, C>) -> R) -> Result<R, Self::Error> {
        Ok(f(&self.0))
    }

    fn with_mut<R>(
        &self,
        _f: impl FnOnce(&mut FullUserdata<Gc, C>) -> R,
    ) -> Result<R, Self::Error> {
        Err(BorrowError::Mut)
    }
}

impl<Gc, C> Debug for UserdataRef<Gc, C>
where
    Gc: TypeProvider,
    // Gc::TableRef: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UserdataRef")
            .field("addr", &Rc::as_ptr(&self.0))
            .field("value", &"<omitted>")
            .field("metatable", &"<omitted>") //&self.0.metatable.borrow())
            .finish()
    }
}

impl<Gc, C> Clone for UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Gc, C> PartialEq for UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Gc, C> Eq for UserdataRef<Gc, C> where Gc: TypeProvider {}

impl<Gc, C> Hash for UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<Gc, C> From<UserdataRef<Gc, C>> for Value<Gc>
where
    Gc: TypeProvider<FullUserdataRef = UserdataRef<Gc, C>>,
{
    fn from(value: UserdataRef<Gc, C>) -> Self {
        Value::Userdata(value)
    }
}

impl<Gc, C> TryFrom<Value<Gc>> for UserdataRef<Gc, C>
where
    Gc: TypeProvider<FullUserdataRef = Self>,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Gc>) -> Result<Self, Self::Error> {
        use super::Type;

        match value {
            Value::Userdata(t) => Ok(t),
            value => {
                let err = TypeMismatchError {
                    expected: Type::Userdata,
                    found: value.type_(),
                };

                Err(err)
            }
        }
    }
}
