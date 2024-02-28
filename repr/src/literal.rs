use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Literal::*;

        match self {
            Nil => write!(f, "nil"),
            Bool(v) => write!(f, "{v}"),
            Int(v) => write!(f, "{v}_i64"),
            Float(v) => write!(f, "{v}_f64"),
            String(v) => write!(f, "{v:?}"),
        }
    }
}
