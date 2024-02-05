use std::error::Error;

use super::{KeyValue, Value};
use crate::error::{AlreadyDroppedError, AlreadyDroppedOrError, BorrowError};

pub trait TypeProvider: Sized {
    type String: Clone + PartialOrd + From<String> + From<&'static str> + Concat + Len;
    type RustCallable: Clone + PartialEq;
    type Table: Default + TableIndex<Self> + Metatable<Self::TableRef>;
    type TableRef: Clone + PartialEq + Default + Borrow<Self::Table>;
    type FullUserdata: ?Sized + Metatable<Self::TableRef>;
    type FullUserdataRef: Clone + PartialEq + Borrow<Self::FullUserdata>;
}

pub trait Borrow<T: ?Sized> {
    type Error: Error;

    fn with_ref<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, Self::Error>;
    fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> Result<R, Self::Error>;
}

impl<T> Borrow<T> for std::cell::RefCell<T> {
    type Error = BorrowError;

    fn with_ref<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, Self::Error> {
        let b = self.try_borrow().map_err(|_| BorrowError::Ref)?;
        Ok(f(&b))
    }

    fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> Result<R, Self::Error> {
        let mut b = self.try_borrow_mut().map_err(|_| BorrowError::Mut)?;
        Ok(f(&mut b))
    }
}

impl<T, U> Borrow<T> for std::rc::Rc<U>
where
    U: Borrow<T>,
{
    type Error = <U as Borrow<T>>::Error;

    fn with_ref<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, Self::Error> {
        self.as_ref().with_ref(f)
    }

    fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> Result<R, Self::Error> {
        self.as_ref().with_mut(f)
    }
}

impl<T, U> Borrow<T> for std::rc::Weak<U>
where
    U: Borrow<T>,
{
    type Error = AlreadyDroppedOrError<<U as Borrow<T>>::Error>;

    fn with_ref<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, Self::Error> {
        self.upgrade()
            .ok_or(AlreadyDroppedError)?
            .with_ref(f)
            .map_err(AlreadyDroppedOrError::Other)
    }

    fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> Result<R, Self::Error> {
        self.upgrade()
            .ok_or(AlreadyDroppedError)?
            .with_mut(f)
            .map_err(AlreadyDroppedOrError::Other)
    }
}

impl<'a, T> Borrow<T> for &'a T {
    type Error = BorrowError;

    fn with_ref<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, Self::Error> {
        Ok(f(self))
    }

    fn with_mut<R>(&self, _: impl FnOnce(&mut T) -> R) -> Result<R, Self::Error> {
        Err(BorrowError::Mut)
    }
}

pub trait Metatable<TableRef> {
    fn metatable(&self) -> Option<TableRef>;
    fn set_metatable(&mut self, mt: Option<TableRef>) -> Option<TableRef>;
}

pub trait TableIndex<Types: TypeProvider> {
    fn get(&self, key: &KeyValue<Types>) -> Value<Types>;
    fn set(&mut self, key: KeyValue<Types>, value: Value<Types>);
    fn border(&self) -> i64;
    fn contains_key(&self, key: &KeyValue<Types>) -> bool {
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
