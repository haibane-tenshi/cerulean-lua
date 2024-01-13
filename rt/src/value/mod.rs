pub mod boolean;
pub mod callable;
pub mod float;
pub mod int;
pub mod nil;
pub mod table;

use std::fmt::{Debug, Display};

use enumoid::{EnumMap, Enumoid};
use repr::literal::Literal;
use repr::opcode::{AriBinOp, BinOp, BitBinOp, EqBinOp, RelBinOp, StrBinOp};

pub use boolean::Boolean;
use callable::Callable;
pub use float::Float;
pub use int::Int;
pub use nil::Nil;
pub use table::{Table, TableRef};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Nil,
    Bool,
    Int,
    Float,
    String,
    Function,
    Table,
}

impl Type {
    /// Produce a string with Lua name of the type.
    ///
    /// Note that this is a lossy conversion!
    /// Technically Lua has only one numeric type - `number`,
    /// but on the implementation side we distinguish between ints and floats.
    pub fn to_lua_name(self) -> &'static str {
        let ty: LuaType = self.into();
        ty.to_str()
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::Nil => "nil",
            Type::Bool => "bool",
            Type::Int => "int",
            Type::Float => "float",
            Type::String => "string",
            Type::Function => "function",
            Type::Table => "table",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum LuaType {
    Nil,
    Boolean,
    Number,
    String,
    Function,
    Table,
}

impl LuaType {
    pub fn to_str(self) -> &'static str {
        use LuaType::*;

        match self {
            Nil => "nil",
            Boolean => "boolean",
            Number => "number",
            String => "string",
            Function => "function",
            Table => "table",
        }
    }
}

impl Display for LuaType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl From<Type> for LuaType {
    fn from(value: Type) -> Self {
        match value {
            Type::Nil => LuaType::Nil,
            Type::Bool => LuaType::Boolean,
            Type::Int => LuaType::Number,
            Type::Float => LuaType::Number,
            Type::String => LuaType::String,
            Type::Function => LuaType::Function,
            Type::Table => LuaType::Table,
        }
    }
}

impl From<LuaTypeWithoutMetatable> for LuaType {
    fn from(value: LuaTypeWithoutMetatable) -> Self {
        match value {
            LuaTypeWithoutMetatable::Nil => LuaType::Nil,
            LuaTypeWithoutMetatable::Boolean => LuaType::Boolean,
            LuaTypeWithoutMetatable::Number => LuaType::Number,
            LuaTypeWithoutMetatable::String => LuaType::String,
            LuaTypeWithoutMetatable::Function => LuaType::Function,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Enumoid)]
pub enum LuaTypeWithoutMetatable {
    Nil,
    Boolean,
    Number,
    String,
    Function,
}

impl LuaTypeWithoutMetatable {
    pub fn to_str(self) -> &'static str {
        let ty: LuaType = self.into();
        ty.to_str()
    }
}

impl Display for LuaTypeWithoutMetatable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

/// Enum representing all possible Lua values.
///
/// Value supports normal and alternate rendering for `Display` impl:
///
/// ```rust
///     # use rt::value::Value;
///     # local value = Value::Nil;
///     println!("normal:    {value}");
///     println!("alternate: {value:#}");
/// ```
///
/// Default rendering will only include the contents.
/// Alternate rendering will include type information as well,
/// but looks a little bit nicer compared to `Debug` output.
pub enum Value<C> {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Function(Callable<C>),
    Table(TableRef<C>),
}

impl<C> Value<C> {
    pub fn to_bool(&self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false))
    }

    pub fn type_(&self) -> Type {
        match self {
            Value::Nil => Type::Nil,
            Value::Bool(_) => Type::Bool,
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::String(_) => Type::String,
            Value::Function(_) => Type::Function,
            Value::Table(_) => Type::Table,
        }
    }

    pub fn take(&mut self) -> Value<C> {
        std::mem::take(self)
    }

    pub(crate) fn metatable<'a>(
        &'a self,
        primary_metatables: &'a EnumMap<LuaTypeWithoutMetatable, Option<TableRef<C>>>,
    ) -> Option<TableRef<C>> {
        match self {
            Value::Nil => primary_metatables[LuaTypeWithoutMetatable::Nil].clone(),
            Value::Bool(_) => primary_metatables[LuaTypeWithoutMetatable::Boolean].clone(),
            Value::Int(_) | Value::Float(_) => {
                primary_metatables[LuaTypeWithoutMetatable::Number].clone()
            }
            Value::String(_) => primary_metatables[LuaTypeWithoutMetatable::String].clone(),
            Value::Function(_) => primary_metatables[LuaTypeWithoutMetatable::Function].clone(),
            Value::Table(t) => t.borrow().unwrap().metatable(),
        }
    }
}

impl<C> Debug for Value<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Function(arg0) => f.debug_tuple("Function").field(arg0).finish(),
            Self::Table(arg0) => f.debug_tuple("Table").field(arg0).finish(),
        }
    }
}

// Autogenerated, keep it around until garbage collector.
#[allow(clippy::clone_on_copy)]
impl<C> Clone for Value<C> {
    fn clone(&self) -> Self {
        match self {
            Self::Nil => Self::Nil,
            Self::Bool(arg0) => Self::Bool(arg0.clone()),
            Self::Int(arg0) => Self::Int(arg0.clone()),
            Self::Float(arg0) => Self::Float(arg0.clone()),
            Self::String(arg0) => Self::String(arg0.clone()),
            Self::Function(arg0) => Self::Function(arg0.clone()),
            Self::Table(arg0) => Self::Table(arg0.clone()),
        }
    }
}

impl<C> PartialEq for Value<C> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Function(l0), Self::Function(r0)) => l0 == r0,
            (Self::Table(l0), Self::Table(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

// No, clippy, you cannot.
#[allow(clippy::derivable_impls)]
impl<C> Default for Value<C> {
    fn default() -> Self {
        Value::Nil
    }
}

impl<C> Display for Value<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;

        match self {
            Nil => write!(f, "{}", nil::Nil),
            Bool(v) => write!(f, "{}", boolean::Boolean(*v)),
            Int(v) => {
                write!(f, "{}", int::Int(*v))
            }
            Float(v) => {
                write!(f, "{}", float::Float(*v))
            }
            String(v) => {
                if f.alternate() {
                    write!(f, "{v:?}")
                } else {
                    write!(f, "{v}")
                }
            }
            Function(v) => write!(f, "{v:?}"),
            Table(v) => write!(f, "{{table <{v:?}>}}"),
        }
    }
}

impl<C> From<Literal> for Value<C> {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Nil => Value::Nil,
            Literal::Bool(value) => Value::Bool(value),
            Literal::Int(value) => Value::Int(value),
            Literal::Float(value) => Value::Float(value.into_inner()),
            Literal::String(value) => Value::String(value),
        }
    }
}

impl<C> From<Nil> for Value<C> {
    fn from(_value: Nil) -> Self {
        Value::Nil
    }
}

impl<C> From<Boolean> for Value<C> {
    fn from(value: Boolean) -> Self {
        let Boolean(value) = value;
        Value::Bool(value)
    }
}

impl<C> From<Int> for Value<C> {
    fn from(value: Int) -> Self {
        let Int(value) = value;
        Value::Int(value)
    }
}

impl<C> From<Float> for Value<C> {
    fn from(value: Float) -> Self {
        let Float(value) = value;
        Value::Float(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MetaValue {
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Rem,
    Pow,
    BitNot,
    BitAnd,
    BitOr,
    BitXor,
    ShL,
    ShR,
    Concat,
    Len,
    Eq,
    Lt,
    LtEq,
    Index,
    NewIndex,
    Call,
}

impl MetaValue {
    pub fn to_str(&self) -> &'static str {
        use MetaValue::*;

        match self {
            Neg => "__unm",
            Add => "__add",
            Sub => "__sub",
            Mul => "__mul",
            Div => "__div",
            FloorDiv => "__idiv",
            Rem => "__mod",
            Pow => "__pow",
            BitNot => "__bnot",
            BitAnd => "__band",
            BitOr => "__bor",
            BitXor => "__bxor",
            ShL => "__shl",
            ShR => "__shr",
            Concat => "__concat",
            Len => "__len",
            Eq => "__eq",
            Lt => "__lt",
            LtEq => "__le",
            Index => "__index",
            NewIndex => "__newindex",
            Call => "__call",
        }
    }
}

impl Display for MetaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl From<BinOp> for MetaValue {
    fn from(value: BinOp) -> Self {
        match value {
            BinOp::Ari(t) => t.into(),
            BinOp::Bit(t) => t.into(),
            BinOp::Str(t) => t.into(),
            BinOp::Eq(t) => t.into(),
            BinOp::Rel(t) => t.into(),
        }
    }
}

impl From<AriBinOp> for MetaValue {
    fn from(value: AriBinOp) -> Self {
        match value {
            AriBinOp::Add => MetaValue::Add,
            AriBinOp::Sub => MetaValue::Sub,
            AriBinOp::Mul => MetaValue::Mul,
            AriBinOp::Div => MetaValue::Div,
            AriBinOp::FloorDiv => MetaValue::FloorDiv,
            AriBinOp::Rem => MetaValue::Rem,
            AriBinOp::Pow => MetaValue::Pow,
        }
    }
}

impl From<BitBinOp> for MetaValue {
    fn from(value: BitBinOp) -> Self {
        match value {
            BitBinOp::And => MetaValue::BitAnd,
            BitBinOp::Or => MetaValue::BitOr,
            BitBinOp::Xor => MetaValue::BitXor,
            BitBinOp::ShL => MetaValue::ShL,
            BitBinOp::ShR => MetaValue::ShR,
        }
    }
}

impl From<StrBinOp> for MetaValue {
    fn from(value: StrBinOp) -> Self {
        match value {
            StrBinOp::Concat => MetaValue::Concat,
        }
    }
}

impl From<EqBinOp> for MetaValue {
    fn from(value: EqBinOp) -> Self {
        match value {
            EqBinOp::Eq => MetaValue::Eq,
        }
    }
}

impl From<RelBinOp> for MetaValue {
    fn from(value: RelBinOp) -> Self {
        match value {
            RelBinOp::Gt | RelBinOp::Lt => MetaValue::Lt,
            RelBinOp::GtEq | RelBinOp::LtEq => MetaValue::LtEq,
        }
    }
}
