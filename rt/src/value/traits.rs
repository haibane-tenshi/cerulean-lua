use std::marker::PhantomData;
use std::rc::Rc;

use super::string::PossiblyUtf8Vec;
use super::userdata::FullUserdata;
use super::{KeyValue, Value};
use crate::error::{AlreadyDroppedError, BorrowError, RefAccessError};
use crate::gc::{GcOrd, RootOrd};
use crate::runtime::Closure;

use gc::Trace;

pub trait Types: Sized {
    type String;
    type LuaCallable;
    type RustCallable;
    type Table;
    type FullUserdata;
}

pub struct Strong<Ty>(PhantomData<(Ty,)>);

impl<Ty> Types for Strong<Ty>
where
    Ty: TypeProvider,
{
    type String = Rc<<Ty as TypeProvider>::String>;
    type LuaCallable = RootOrd<Closure>;
    type RustCallable = <Ty as TypeProvider>::RustCallable;
    type Table = RootOrd<<Ty as TypeProvider>::Table>;
    type FullUserdata = RootOrd<<Ty as TypeProvider>::FullUserdata>;
}

pub struct Weak<Ty>(PhantomData<(Ty,)>);

impl<Ty> Types for Weak<Ty>
where
    Ty: TypeProvider,
{
    type String = Rc<<Ty as TypeProvider>::String>;
    type LuaCallable = GcOrd<Closure>;
    type RustCallable = <Ty as TypeProvider>::RustCallable;
    type Table = GcOrd<<Ty as TypeProvider>::Table>;
    type FullUserdata = GcOrd<<Ty as TypeProvider>::FullUserdata>;
}

pub trait TypeProvider: Sized {
    type String: Concat + Len + Clone + PartialOrd + From<String> + From<&'static str>;
    type RustCallable: Clone + PartialEq + Trace;
    type Table: Default + Trace + TableIndex<Weak<Self>> + Metatable<GcOrd<Self::Table>>;
    type FullUserdata: Trace + Metatable<GcOrd<Self::Table>>;
}

pub struct DefaultTypes;

impl TypeProvider for DefaultTypes {
    type String = PossiblyUtf8Vec;
    type RustCallable = super::callable::RustCallable<Self>;
    type Table = super::Table<Weak<Self>>;
    type FullUserdata = Box<dyn FullUserdata<Self>>;
}

pub trait Borrow<T: ?Sized> {
    fn with_ref<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, RefAccessError>;
    fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> Result<R, RefAccessError>;
}

impl<T> Borrow<T> for std::cell::RefCell<T> {
    fn with_ref<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, RefAccessError> {
        let b = self.try_borrow().map_err(|_| BorrowError::Ref)?;
        Ok(f(&b))
    }

    fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> Result<R, RefAccessError> {
        let mut b = self.try_borrow_mut().map_err(|_| BorrowError::Mut)?;
        Ok(f(&mut b))
    }
}

impl<T, U> Borrow<T> for std::rc::Rc<U>
where
    U: Borrow<T>,
{
    fn with_ref<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, RefAccessError> {
        self.as_ref().with_ref(f)
    }

    fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> Result<R, RefAccessError> {
        self.as_ref().with_mut(f)
    }
}

impl<T, U> Borrow<T> for std::rc::Weak<U>
where
    U: Borrow<T>,
{
    fn with_ref<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, RefAccessError> {
        self.upgrade()
            .ok_or(AlreadyDroppedError)?
            .with_ref(f)
            .map_err(Into::into)
    }

    fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> Result<R, RefAccessError> {
        self.upgrade()
            .ok_or(AlreadyDroppedError)?
            .with_mut(f)
            .map_err(Into::into)
    }
}

impl<'a, U, T> Borrow<T> for &'a U
where
    U: Borrow<T> + ?Sized,
    T: ?Sized,
{
    fn with_ref<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, RefAccessError> {
        <U as Borrow<T>>::with_ref(self, f)
    }

    fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> Result<R, RefAccessError> {
        <U as Borrow<T>>::with_mut(self, f)
    }
}

impl Borrow<PossiblyUtf8Vec> for PossiblyUtf8Vec {
    fn with_ref<R>(&self, f: impl FnOnce(&PossiblyUtf8Vec) -> R) -> Result<R, RefAccessError> {
        Ok(f(self))
    }

    fn with_mut<R>(&self, _: impl FnOnce(&mut PossiblyUtf8Vec) -> R) -> Result<R, RefAccessError> {
        Err(BorrowError::Mut.into())
    }
}

pub trait Metatable<TableRef> {
    fn metatable(&self) -> Option<TableRef>;
    fn set_metatable(&mut self, mt: Option<TableRef>) -> Option<TableRef>;
}

pub trait TableIndex<Ty: Types> {
    fn get(&self, key: &KeyValue<Ty>) -> Value<Ty>;
    fn set(&mut self, key: KeyValue<Ty>, value: Value<Ty>);
    fn border(&self) -> i64;
    fn contains_key(&self, key: &KeyValue<Ty>) -> bool {
        !matches!(self.get(key), Value::Nil)
    }
}

pub trait Len {
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Len for String {
    fn len(&self) -> usize {
        String::len(self)
    }
}

impl Len for Vec<u8> {
    fn len(&self) -> usize {
        Vec::len(self)
    }
}

pub trait Concat {
    fn concat(&mut self, other: &Self);
}

impl Concat for String {
    fn concat(&mut self, other: &Self) {
        self.push_str(other);
    }
}

impl Concat for Vec<u8> {
    fn concat(&mut self, other: &Self) {
        self.extend(other)
    }
}
