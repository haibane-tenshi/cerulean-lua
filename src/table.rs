use std::cell::{BorrowError, BorrowMutError, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use ordered_float::NotNan;
use thiserror::Error;

use crate::value::Value;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Table {
    data: HashMap<KeyValue, Value>,
}

impl Table {
    pub fn get(&self, key: KeyValue) -> Value {
        self.data.get(&key).cloned().unwrap_or_default()
    }

    pub fn set(&mut self, key: KeyValue, value: Value) {
        if value == Value::Nil {
            self.data.remove(&key);
        } else {
            self.data.insert(key, value);
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum KeyValue {
    Bool(bool),
    Int(i64),
    Float(NotNan<f64>),
    String(String),
    Table(TableRef),
}

#[derive(Debug, Error)]
#[error("value cannot be used to index tables")]
pub struct InvalidTableKeyError;

impl TryFrom<Value> for KeyValue {
    type Error = InvalidTableKeyError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
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

impl From<KeyValue> for Value {
    fn from(value: KeyValue) -> Self {
        match value {
            KeyValue::Bool(t) => Value::Bool(t),
            KeyValue::Int(t) => Value::Int(t),
            KeyValue::Float(t) => Value::Float(t.into_inner()),
            KeyValue::String(t) => Value::String(t),
            KeyValue::Table(t) => Value::Table(t),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct TableRef(Rc<RefCell<Table>>);

impl TableRef {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn borrow(&self) -> Result<Ref<Table>, BorrowError> {
        self.0.try_borrow()
    }

    pub fn borrow_mut(&self) -> Result<RefMut<Table>, BorrowMutError> {
        self.0.try_borrow_mut()
    }
}

impl PartialEq for TableRef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for TableRef {}

impl Hash for TableRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
    }
}
