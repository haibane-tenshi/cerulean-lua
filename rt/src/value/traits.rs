use super::string::PossiblyUtf8Vec;
use super::{KeyValue, Value};
use crate::error::{AlreadyDroppedError, BorrowError, RefAccessError};

pub trait TypeProvider: Sized {
    type String: Clone + PartialOrd + From<String> + From<&'static str> + Concat + Len;
    type StringRef: Clone + PartialEq + Borrow<Self::String>;
    type RustCallable: Clone + PartialEq;
    type Table: Default + TableIndex<Self> + Metatable<Self::TableRef>;
    type TableRef: Clone + PartialEq + Borrow<Self::Table>;
    type FullUserdata: ?Sized + Metatable<Self::TableRef>;
    type FullUserdataRef: Clone + PartialEq + Borrow<Self::FullUserdata>;
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

pub trait TableIndex<Gc: TypeProvider> {
    fn get(&self, key: &KeyValue<Gc>) -> Value<Gc>;
    fn set(&mut self, key: KeyValue<Gc>, value: Value<Gc>);
    fn border(&self) -> i64;
    fn contains_key(&self, key: &KeyValue<Gc>) -> bool {
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
