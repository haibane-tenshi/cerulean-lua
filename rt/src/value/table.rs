use std::cell::{BorrowError, BorrowMutError, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use ordered_float::NotNan;

use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Table<Callable>
where
    Callable: Eq + Hash,
{
    data: HashMap<KeyValue<Callable>, Value<Callable>>,
}

impl<Callable> Table<Callable>
where
    Callable: Eq + Hash + Clone,
{
    pub fn get(&self, key: KeyValue<Callable>) -> Value<Callable> {
        self.data.get(&key).cloned().unwrap_or_default()
    }
}

impl<Callable> Table<Callable>
where
    Callable: Eq + Hash,
{
    pub fn set(&mut self, key: KeyValue<Callable>, value: Value<Callable>) {
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

impl<Callable> Default for Table<Callable>
where
    Callable: Eq + Hash,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum KeyValue<Callable>
where
    Callable: Eq + Hash,
{
    Bool(bool),
    Int(i64),
    Float(NotNan<f64>),
    String(String),
    Table(TableRef<Callable>),
}

#[derive(Debug)]
//#[error("value cannot be used to index tables")]
pub struct InvalidTableKeyError;

impl<Callable> TryFrom<Value<Callable>> for KeyValue<Callable>
where
    Callable: Eq + Hash,
{
    type Error = InvalidTableKeyError;

    fn try_from(value: Value<Callable>) -> Result<Self, Self::Error> {
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

impl<Callable> From<KeyValue<Callable>> for Value<Callable>
where
    Callable: Eq + Hash,
{
    fn from(value: KeyValue<Callable>) -> Self {
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
pub struct TableRef<Callable>(Rc<RefCell<Table<Callable>>>)
where
    Callable: Eq + Hash;

impl<Callable> TableRef<Callable>
where
    Callable: Eq + Hash,
{
    pub fn new() -> Self {
        Default::default()
    }

    pub fn borrow(&self) -> Result<Ref<Table<Callable>>, BorrowError> {
        self.0.try_borrow()
    }

    pub fn borrow_mut(&self) -> Result<RefMut<Table<Callable>>, BorrowMutError> {
        self.0.try_borrow_mut()
    }
}

impl<Callable> Default for TableRef<Callable>
where
    Callable: Eq + Hash,
{
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Callable> PartialEq for TableRef<Callable>
where
    Callable: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Callable> Eq for TableRef<Callable> where Callable: Eq + Hash {}

impl<Callable> Hash for TableRef<Callable>
where
    Callable: Eq + Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
    }
}
