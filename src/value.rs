use std::fmt::Display;

use decorum::Finite;

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

impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Nil => Value::Nil,
            Literal::Bool(value) => Value::Bool(value),
            Literal::Uint(value) => Value::Uint(value),
            Literal::Float(value) => Value::Float(value.into_inner()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Nil,
    Bool(bool),
    Uint(u64),
    Float(Finite<f64>),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let t: Value = Into::into(*self);
        write!(f, "{t}")
    }
}
