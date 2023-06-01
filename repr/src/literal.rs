use std::fmt::Display;

use decorum::Finite;

use crate::index::FunctionId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Nil,
    Bool(bool),
    Int(i64),
    Float(Finite<f64>),
    String(String),
    Function(FunctionId),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::value::Value;

        let t: Value = Into::into(self.clone());
        write!(f, "{t}")
    }
}
