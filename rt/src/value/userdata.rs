use std::marker::PhantomData;

use gc::userdata::Params;
use gc::Root;

use crate::ffi::tuple::{NonEmptyTuple, Tuple, TupleHead, TupleTail};
use crate::ffi::DynDelegate;
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
    type Rt<'rt> = ();
    type Res = DynDelegate<Ty>;
}

pub trait DispatchMethod<Marker, Ty>: Sized
where
    Ty: Types,
{
    const SCOPE_NAME: &'static str;

    fn dispatch(this: Root<Self>, name: &str) -> Option<DynDelegate<Ty>>;
}

pub trait DispatchTrait<Traits, Ty>: Sized
where
    Traits: Tuple,
    Ty: Types,
{
    fn dispatch(this: Root<Self>, id: MethodId) -> Option<DynDelegate<Ty>>;
}

impl<Ty, T> DispatchTrait<(), Ty> for T
where
    Ty: Types,
{
    fn dispatch(_this: Root<Self>, _id: MethodId) -> Option<DynDelegate<Ty>> {
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
    fn dispatch(this: Root<Self>, id: MethodId) -> Option<DynDelegate<Ty>> {
        if id.scope == T::SCOPE_NAME {
            if let Some(delegate) = <T as DispatchMethod<_, _>>::dispatch(this.clone(), id.name) {
                return Some(delegate);
            }
        }

        <T as DispatchTrait<TupleTail<Tup>, Ty>>::dispatch(this, id)
    }
}
