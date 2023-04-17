use std::fmt::Display;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Uint(u64),
    Float(f64),
}

impl Value {
    pub fn is_falsey(self) -> bool {
        matches!(self, Value::Nil | Value::Bool(false))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;

        match *self {
            Nil => write!(f, "nil"),
            Bool(v) => write!(f, "{v}"),
            Uint(v) => write!(f, "{v}_u64"),
            Float(v) => write!(f, "{v}_f64"),
        }
    }
}
