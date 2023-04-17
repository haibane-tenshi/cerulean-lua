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
