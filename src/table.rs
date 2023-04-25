use std::collections::HashMap;

use ordered_float::NotNan;
use thiserror::Error;

use crate::value::Value;

#[derive(Debug, Clone)]
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
        }
    }
}
