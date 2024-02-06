use std::cell::RefCell;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use crate::error::{BorrowError, RuntimeError};
use crate::ffi::tuple::{NonEmptyTuple, Tuple, TupleHead, TupleTail};
use crate::runtime::RuntimeView;
use crate::value::{TypeMismatchError, TypeProvider, Value};

pub trait Userdata<Types, C>
where
    Types: TypeProvider,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Types, C>,
    ) -> Option<Result<(), RuntimeError<Types>>>;
}

pub enum Method<Ref, Mut, Val> {
    Ref(Ref),
    Mut(Mut),
    Val(Val),
}

pub trait DispatchMethod<Marker, Types, C>: Sized
where
    Types: TypeProvider,
{
    const SCOPE_NAME: &'static str;

    type Ref;
    type Mut;
    type Val;

    fn dispatch_method(name: &str) -> Option<Method<Self::Ref, Self::Mut, Self::Val>>;

    fn call(
        &self,
        method: Self::Ref,
        rt: RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>>;
    fn call_mut(
        &mut self,
        method: Self::Mut,
        rt: RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>>;
    fn call_once(
        self,
        method: Self::Val,
        rt: RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>>;
}

impl<Marker, Types, C, T> DispatchMethod<Marker, Types, C> for RefCell<T>
where
    Types: TypeProvider,
    Types::String: From<&'static str>,
    T: DispatchMethod<Marker, Types, C>,
{
    const SCOPE_NAME: &'static str = <T as DispatchMethod<Marker, Types, C>>::SCOPE_NAME;

    type Ref = Method<
        <T as DispatchMethod<Marker, Types, C>>::Ref,
        <T as DispatchMethod<Marker, Types, C>>::Mut,
        std::convert::Infallible,
    >;
    type Mut = std::convert::Infallible;
    type Val = <T as DispatchMethod<Marker, Types, C>>::Val;

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
        rt: RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
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
        _rt: RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
        match method {}
    }

    fn call_once(
        self,
        method: Self::Val,
        rt: RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
        self.into_inner().call_once(method, rt)
    }
}

impl<Marker, Types, C, T> DispatchMethod<Marker, Types, C> for Option<T>
where
    Types: TypeProvider,
    Types::String: From<&'static str>,
    T: DispatchMethod<Marker, Types, C>,
{
    const SCOPE_NAME: &'static str = <T as DispatchMethod<Marker, Types, C>>::SCOPE_NAME;

    type Ref = <T as DispatchMethod<Marker, Types, C>>::Ref;
    type Mut = Method<
        std::convert::Infallible,
        <T as DispatchMethod<Marker, Types, C>>::Mut,
        <T as DispatchMethod<Marker, Types, C>>::Val,
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
        rt: RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
        let Some(value) = self else {
            return Err(Value::String("value is already moved out".into()).into());
        };

        value.call(method, rt)
    }

    fn call_mut(
        &mut self,
        method: Self::Mut,
        rt: RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
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
        _rt: RuntimeView<'_, Types, C>,
    ) -> Result<(), RuntimeError<Types>> {
        match method {}
    }
}

pub trait DispatchTrait<Traits: Tuple, Types, C>: Sized
where
    Types: TypeProvider,
{
    fn dispatch_trait(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Types, C>,
    ) -> Option<Result<(), RuntimeError<Types>>>;
}

impl<Types, Cache, T> DispatchTrait<(), Types, Cache> for T
where
    Types: TypeProvider,
{
    fn dispatch_trait(
        &self,
        _scope: &str,
        _name: &str,
        _rt: RuntimeView<'_, Types, Cache>,
    ) -> Option<Result<(), RuntimeError<Types>>> {
        None
    }
}

impl<T, Tup, Types, Cache> DispatchTrait<Tup, Types, Cache> for T
where
    Types: TypeProvider,
    Types::String: From<&'static str>,
    Tup: NonEmptyTuple,
    T: DispatchMethod<TupleHead<Tup>, Types, Cache>,
    T: DispatchTrait<TupleTail<Tup>, Types, Cache>,
{
    fn dispatch_trait(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Types, Cache>,
    ) -> Option<Result<(), RuntimeError<Types>>> {
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

        <T as DispatchTrait<TupleTail<Tup>, Types, Cache>>::dispatch_trait(self, scope, name, rt)
    }
}

// Self: 'static requires C: 'static
// see https://github.com/rust-lang/rust/issues/57325
// https://rust-lang.github.io/rfcs/1214-projections-lifetimes-and-wf.html
pub struct Dispatchable<T, Types: TypeProvider, C> {
    value: T,
    dispatcher:
        fn(&T, &str, &str, RuntimeView<'_, Types, C>) -> Option<Result<(), RuntimeError<Types>>>,
}

impl<T, Types, C> Dispatchable<T, Types, C>
where
    Types: TypeProvider,
{
    pub fn new<Traits>(value: T) -> Self
    where
        Traits: Tuple,
        T: DispatchTrait<Traits, Types, C>,
    {
        Dispatchable {
            value,
            dispatcher: <T as DispatchTrait<Traits, Types, C>>::dispatch_trait,
        }
    }
}

impl<T, Types, C> Userdata<Types, C> for Dispatchable<T, Types, C>
where
    Types: TypeProvider,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Types, C>,
    ) -> Option<Result<(), RuntimeError<Types>>> {
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

impl<T, Traits, Types, C> Userdata<Types, C> for DispatchableStatic<T, Traits>
where
    Types: TypeProvider,
    Traits: Tuple,
    T: DispatchTrait<Traits, Types, C>,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Types, C>,
    ) -> Option<Result<(), RuntimeError<Types>>> {
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

pub type FullUserdata<Types, C> =
    UserdataValue<dyn Userdata<Types, C>, <Types as TypeProvider>::TableRef>;

impl<Types, C> super::Metatable<Types::TableRef> for FullUserdata<Types, C>
where
    Types: TypeProvider,
{
    fn metatable(&self) -> Option<Types::TableRef> {
        self.metatable.borrow().clone()
    }

    fn set_metatable(&mut self, mt: Option<Types::TableRef>) -> Option<Types::TableRef> {
        self.metatable.replace(mt)
    }
}

pub struct UserdataRef<Types: TypeProvider, C>(
    Rc<UserdataValue<dyn Userdata<Types, C>, Types::TableRef>>,
);

impl<Types, C> UserdataRef<Types, C>
where
    Types: TypeProvider,
{
    pub fn new<U>(userdata: U) -> Self
    where
        U: Userdata<Types, C> + 'static,
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
        T: DispatchTrait<Traits, Types, C> + 'static,
    {
        let value = UserdataValue {
            metatable: Default::default(),
            value: DispatchableStatic::new(value),
        };

        UserdataRef(Rc::new(value))
    }

    pub fn set_metatable(&self, metatable: Option<Types::TableRef>) -> Option<Types::TableRef> {
        self.0.metatable.replace(metatable)
    }
}

impl<Types, C> UserdataRef<Types, C>
where
    Types: TypeProvider,
{
    pub fn metatable(&self) -> Option<Types::TableRef> {
        self.0.metatable.borrow().clone()
    }
}

impl<Types, C> Userdata<Types, C> for UserdataRef<Types, C>
where
    Types: TypeProvider,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Types, C>,
    ) -> Option<Result<(), RuntimeError<Types>>> {
        self.0.value.method(scope, name, rt)
    }
}

impl<Types, C> super::Borrow<FullUserdata<Types, C>> for UserdataRef<Types, C>
where
    Types: TypeProvider,
{
    type Error = BorrowError;

    fn with_ref<R>(&self, f: impl FnOnce(&FullUserdata<Types, C>) -> R) -> Result<R, Self::Error> {
        Ok(f(&self.0))
    }

    fn with_mut<R>(
        &self,
        _f: impl FnOnce(&mut FullUserdata<Types, C>) -> R,
    ) -> Result<R, Self::Error> {
        Err(BorrowError::Mut)
    }
}

impl<Types, C> Debug for UserdataRef<Types, C>
where
    Types: TypeProvider,
    // Types::TableRef: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UserdataRef")
            .field("addr", &Rc::as_ptr(&self.0))
            .field("value", &"<omitted>")
            .field("metatable", &"<omitted>") //&self.0.metatable.borrow())
            .finish()
    }
}

impl<Types, C> Clone for UserdataRef<Types, C>
where
    Types: TypeProvider,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Types, C> PartialEq for UserdataRef<Types, C>
where
    Types: TypeProvider,
{
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Types, C> Eq for UserdataRef<Types, C> where Types: TypeProvider {}

impl<Types, C> Hash for UserdataRef<Types, C>
where
    Types: TypeProvider,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<Types, C> From<UserdataRef<Types, C>> for Value<Types>
where
    Types: TypeProvider<FullUserdataRef = UserdataRef<Types, C>>,
{
    fn from(value: UserdataRef<Types, C>) -> Self {
        Value::Userdata(value)
    }
}

impl<Types, C> TryFrom<Value<Types>> for UserdataRef<Types, C>
where
    Types: TypeProvider<FullUserdataRef = Self>,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Types>) -> Result<Self, Self::Error> {
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
