use std::cell::{Cell, RefCell};
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use crate::error::RuntimeError;
use crate::ffi::tuple::{NonEmptyTuple, Tuple, TupleHead, TupleTail};
use crate::runtime::RuntimeView;
use crate::value::{TableRef, TypeMismatchError, Value};

pub trait Userdata<C> {
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, C>,
    ) -> Option<Result<(), RuntimeError<C>>>;
}

pub enum Method<Ref, Mut, Val> {
    Ref(Ref),
    Mut(Mut),
    Val(Val),
}

pub trait DispatchMethod<Marker, C>: Sized {
    const SCOPE_NAME: &'static str;

    type Ref;
    type Mut;
    type Val;

    fn dispatch_method(name: &str) -> Option<Method<Self::Ref, Self::Mut, Self::Val>>;

    fn call(&self, method: Self::Ref, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>>;
    fn call_mut(
        &mut self,
        method: Self::Mut,
        rt: RuntimeView<'_, C>,
    ) -> Result<(), RuntimeError<C>>;
    fn call_once(self, method: Self::Val, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>>;
}

impl<Marker, C, T> DispatchMethod<Marker, C> for RefCell<T>
where
    T: DispatchMethod<Marker, C>,
{
    const SCOPE_NAME: &'static str = <T as DispatchMethod<Marker, C>>::SCOPE_NAME;

    type Ref = Method<
        <T as DispatchMethod<Marker, C>>::Ref,
        <T as DispatchMethod<Marker, C>>::Mut,
        std::convert::Infallible,
    >;
    type Mut = std::convert::Infallible;
    type Val = <T as DispatchMethod<Marker, C>>::Val;

    fn dispatch_method(name: &str) -> Option<Method<Self::Ref, Self::Mut, Self::Val>> {
        let r = match T::dispatch_method(name)? {
            Method::Ref(t) => Method::Ref(Method::Ref(t)),
            Method::Mut(t) => Method::Ref(Method::Mut(t)),
            Method::Val(t) => Method::Val(t),
        };

        Some(r)
    }

    fn call(&self, method: Self::Ref, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        use crate::value::Value;

        match method {
            Method::Ref(m) => self
                .try_borrow()
                .map_err(|_| Value::String("value is already mutably borrowed".to_string()))?
                .call(m, rt),
            Method::Mut(m) => self
                .try_borrow_mut()
                .map_err(|_| Value::String("value is already borrowed".to_string()))?
                .call_mut(m, rt),
            Method::Val(m) => match m {},
        }
    }

    fn call_mut(
        &mut self,
        method: Self::Mut,
        _rt: RuntimeView<'_, C>,
    ) -> Result<(), RuntimeError<C>> {
        match method {}
    }

    fn call_once(self, method: Self::Val, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        self.into_inner().call_once(method, rt)
    }
}

impl<Marker, C, T> DispatchMethod<Marker, C> for Option<T>
where
    T: DispatchMethod<Marker, C>,
{
    const SCOPE_NAME: &'static str = <T as DispatchMethod<Marker, C>>::SCOPE_NAME;

    type Ref = <T as DispatchMethod<Marker, C>>::Ref;
    type Mut = Method<
        std::convert::Infallible,
        <T as DispatchMethod<Marker, C>>::Mut,
        <T as DispatchMethod<Marker, C>>::Val,
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

    fn call(&self, method: Self::Ref, rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        use crate::value::Value;

        let Some(value) = self else {
            return Err(Value::String("value is already moved out".to_string()).into());
        };

        value.call(method, rt)
    }

    fn call_mut(
        &mut self,
        method: Self::Mut,
        rt: RuntimeView<'_, C>,
    ) -> Result<(), RuntimeError<C>> {
        use crate::value::Value;

        match method {
            Method::Ref(m) => match m {},
            Method::Mut(m) => {
                let Some(value) = self else {
                    return Err(Value::String("value is already moved out".to_string()).into());
                };

                value.call_mut(m, rt)
            }
            Method::Val(m) => {
                let Some(value) = self.take() else {
                    return Err(Value::String("value is already moved out".to_string()).into());
                };

                value.call_once(m, rt)
            }
        }
    }

    fn call_once(self, method: Self::Val, _rt: RuntimeView<'_, C>) -> Result<(), RuntimeError<C>> {
        match method {}
    }
}

pub trait DispatchTrait<Traits: Tuple, C>: Sized {
    fn dispatch_trait(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, C>,
    ) -> Option<Result<(), RuntimeError<C>>>;
}

impl<Cache, T> DispatchTrait<(), Cache> for T {
    fn dispatch_trait(
        &self,
        _scope: &str,
        _name: &str,
        _rt: RuntimeView<'_, Cache>,
    ) -> Option<Result<(), RuntimeError<Cache>>> {
        None
    }
}

impl<T, Tup, Cache> DispatchTrait<Tup, Cache> for T
where
    Tup: NonEmptyTuple,
    T: DispatchMethod<TupleHead<Tup>, Cache>,
    T: DispatchTrait<TupleTail<Tup>, Cache>,
{
    fn dispatch_trait(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Cache>,
    ) -> Option<Result<(), RuntimeError<Cache>>> {
        use crate::value::Value;

        if scope == T::SCOPE_NAME {
            if let Some(method) = T::dispatch_method(name) {
                let r = if let Method::Ref(f) = method {
                    T::call(self, f, rt)
                } else {
                    Err(Value::String(
                        "userdata can only dispatch on methods with `&self` receiver".to_string(),
                    )
                    .into())
                };

                return Some(r);
            }
        }

        <T as DispatchTrait<TupleTail<Tup>, Cache>>::dispatch_trait(self, scope, name, rt)
    }
}

// Self: 'static requires C: 'static
// see https://github.com/rust-lang/rust/issues/57325
// https://rust-lang.github.io/rfcs/1214-projections-lifetimes-and-wf.html
pub struct Dispatchable<T, C> {
    value: T,
    dispatcher: fn(&T, &str, &str, RuntimeView<'_, C>) -> Option<Result<(), RuntimeError<C>>>,
}

impl<T, C> Dispatchable<T, C> {
    pub fn new<Traits>(value: T) -> Self
    where
        Traits: Tuple,
        T: DispatchTrait<Traits, C>,
    {
        Dispatchable {
            value,
            dispatcher: <T as DispatchTrait<Traits, C>>::dispatch_trait,
        }
    }
}

impl<T, C> Userdata<C> for Dispatchable<T, C> {
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, C>,
    ) -> Option<Result<(), RuntimeError<C>>> {
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

impl<T, Traits, C> Userdata<C> for DispatchableStatic<T, Traits>
where
    Traits: Tuple,
    T: DispatchTrait<Traits, C>,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, C>,
    ) -> Option<Result<(), RuntimeError<C>>> {
        self.value.dispatch_trait(scope, name, rt)
    }
}

struct UserdataValue<T: ?Sized, TableRef> {
    metatable: RefCell<Option<TableRef>>,
    value: T,
}

pub struct UserdataRef<C>(Rc<UserdataValue<dyn Userdata<C>, TableRef<C>>>);

impl<C> UserdataRef<C> {
    pub fn new<U>(userdata: U) -> Self
    where
        U: Userdata<C> + 'static,
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
        T: DispatchTrait<Traits, C> + 'static,
    {
        let value = UserdataValue {
            metatable: Default::default(),
            value: DispatchableStatic::new(value),
        };

        UserdataRef(Rc::new(value))
    }

    pub fn metatable(&self) -> Option<TableRef<C>> {
        self.0.metatable.borrow().clone()
    }

    pub fn set_metatable(&self, metatable: Option<TableRef<C>>) -> Option<TableRef<C>> {
        self.0.metatable.replace(metatable)
    }
}

impl<C> Userdata<C> for UserdataRef<C> {
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, C>,
    ) -> Option<Result<(), RuntimeError<C>>> {
        self.0.value.method(scope, name, rt)
    }
}

impl<C> Debug for UserdataRef<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UserdataRef")
            .field("value", &"<omitted>")
            .field("metatable", &self.0.metatable)
            .finish()
    }
}

impl<C> Clone for UserdataRef<C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<C> PartialEq for UserdataRef<C> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<C> Eq for UserdataRef<C> {}

impl<C> Hash for UserdataRef<C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<C> From<UserdataRef<C>> for Value<C> {
    fn from(value: UserdataRef<C>) -> Self {
        Value::Userdata(value)
    }
}

impl<C> TryFrom<Value<C>> for UserdataRef<C> {
    type Error = TypeMismatchError;

    fn try_from(value: Value<C>) -> Result<Self, Self::Error> {
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
