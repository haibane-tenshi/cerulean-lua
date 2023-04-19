use std::cmp::Ordering;
use std::fmt::Display;

use decorum::Finite;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Type {
    Nil,
    Bool,
    Int,
    Float,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
}

impl Value {
    pub fn is_falsey(self) -> bool {
        matches!(self, Value::Nil | Value::Bool(false))
    }

    pub fn type_(self) -> Type {
        match self {
            Value::Nil => Type::Nil,
            Value::Bool(_) => Type::Bool,
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Value::*;

        match (self, other) {
            (Nil, Nil) => Some(Ordering::Equal),
            (Bool(lhs), Bool(rhs)) => Some(lhs.cmp(rhs)),
            (Int(lhs), Int(rhs)) => Some(lhs.cmp(rhs)),
            (Float(lhs), Float(rhs)) => lhs.partial_cmp(rhs),
            _ => None,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;

        match *self {
            Nil => write!(f, "nil"),
            Bool(v) => write!(f, "{v}"),
            Int(v) => write!(f, "{v}_i64"),
            Float(v) => write!(f, "{v}_f64"),
        }
    }
}

impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Nil => Value::Nil,
            Literal::Bool(value) => Value::Bool(value),
            Literal::Int(value) => Value::Int(value),
            Literal::Float(value) => Value::Float(value.into_inner()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Nil,
    Bool(bool),
    Int(i64),
    Float(Finite<f64>),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let t: Value = Into::into(*self);
        write!(f, "{t}")
    }
}
