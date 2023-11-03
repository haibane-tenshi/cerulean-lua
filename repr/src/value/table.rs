use std::cell::{BorrowError, BorrowMutError, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use ordered_float::NotNan;
use thiserror::Error;

use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Table<Closure>
where
    Closure: Eq + Hash,
{
    data: HashMap<KeyValue<Closure>, Value<Closure>>,
}

impl<Closure> Table<Closure>
where
    Closure: Eq + Hash + Clone,
{
    pub fn get(&self, key: KeyValue<Closure>) -> Value<Closure> {
        self.data.get(&key).cloned().unwrap_or_default()
    }
}

impl<Closure> Table<Closure>
where
    Closure: Eq + Hash,
{
    pub fn set(&mut self, key: KeyValue<Closure>, value: Value<Closure>) {
        if value == Value::Nil {
            self.data.remove(&key);
        } else {
            self.data.insert(key, value);
        }
    }

    pub fn border(&self) -> i64 {
        // Inefficient, but will get fixed when table layout is improved.
        (0..)
            .find(|&i| !self.data.contains_key(&KeyValue::Int(i + 1)))
            .unwrap_or(i64::MAX)
    }
}

impl<Closure> Default for Table<Closure>
where
    Closure: Eq + Hash,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum KeyValue<Closure>
where
    Closure: Eq + Hash,
{
    Bool(bool),
    Int(i64),
    Float(NotNan<f64>),
    String(String),
    Table(TableRef<Closure>),
}

#[derive(Debug, Error)]
#[error("value cannot be used to index tables")]
pub struct InvalidTableKeyError;

impl<Closure> TryFrom<Value<Closure>> for KeyValue<Closure>
where
    Closure: Eq + Hash,
{
    type Error = InvalidTableKeyError;

    fn try_from(value: Value<Closure>) -> Result<Self, Self::Error> {
        let r = match value {
            Value::Bool(t) => KeyValue::Bool(t),
            Value::Int(t) => KeyValue::Int(t),
            Value::Float(t) => {
                let t = NotNan::new(t).map_err(|_| InvalidTableKeyError)?;
                KeyValue::Float(t)
            }
            Value::String(t) => KeyValue::String(t),
            Value::Table(t) => KeyValue::Table(t),
            Value::Nil | Value::Function(_) => return Err(InvalidTableKeyError),
        };

        Ok(r)
    }
}

impl<Closure> From<KeyValue<Closure>> for Value<Closure>
where
    Closure: Eq + Hash,
{
    fn from(value: KeyValue<Closure>) -> Self {
        match value {
            KeyValue::Bool(t) => Value::Bool(t),
            KeyValue::Int(t) => Value::Int(t),
            KeyValue::Float(t) => Value::Float(t.into_inner()),
            KeyValue::String(t) => Value::String(t),
            KeyValue::Table(t) => Value::Table(t),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TableRef<Closure>(Rc<RefCell<Table<Closure>>>)
where
    Closure: Eq + Hash;

impl<Closure> TableRef<Closure>
where
    Closure: Eq + Hash,
{
    pub fn new() -> Self {
        Default::default()
    }

    pub fn borrow(&self) -> Result<Ref<Table<Closure>>, BorrowError> {
        self.0.try_borrow()
    }

    pub fn borrow_mut(&self) -> Result<RefMut<Table<Closure>>, BorrowMutError> {
        self.0.try_borrow_mut()
    }
}

impl<Closure> Default for TableRef<Closure>
where
    Closure: Eq + Hash,
{
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Closure> PartialEq for TableRef<Closure>
where
    Closure: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Closure> Eq for TableRef<Closure> where Closure: Eq + Hash {}

impl<Closure> Hash for TableRef<Closure>
where
    Closure: Eq + Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
    }
}
