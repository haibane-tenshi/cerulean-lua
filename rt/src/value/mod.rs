pub mod boolean;
pub mod callable;
pub mod float;
pub mod int;
pub mod nil;
pub mod table;
pub mod userdata;

use std::error::Error;
use std::fmt::{Debug, Display};

use enumoid::{EnumMap, Enumoid};
use repr::literal::Literal;

pub use boolean::Boolean;
pub use callable::Callable;
pub use float::Float;
pub use int::Int;
pub use nil::{Nil, NilOr};
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

impl From<TypeWithoutMetatable> for Type {
    fn from(value: TypeWithoutMetatable) -> Self {
        match value {
            TypeWithoutMetatable::Nil => Type::Nil,
            TypeWithoutMetatable::Bool => Type::Bool,
            TypeWithoutMetatable::Int => Type::Int,
            TypeWithoutMetatable::Float => Type::Float,
            TypeWithoutMetatable::String => Type::String,
            TypeWithoutMetatable::Function => Type::Function,
        }
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

impl From<TypeWithoutMetatable> for LuaType {
    fn from(value: TypeWithoutMetatable) -> Self {
        match value {
            TypeWithoutMetatable::Nil => LuaType::Nil,
            TypeWithoutMetatable::Bool => LuaType::Boolean,
            TypeWithoutMetatable::Int => LuaType::Number,
            TypeWithoutMetatable::Float => LuaType::Number,
            TypeWithoutMetatable::String => LuaType::String,
            TypeWithoutMetatable::Function => LuaType::Function,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Enumoid)]
pub enum TypeWithoutMetatable {
    Nil,
    Bool,
    Int,
    Float,
    String,
    Function,
}

impl TypeWithoutMetatable {
    pub fn to_str(self) -> &'static str {
        let ty: LuaType = self.into();
        ty.to_str()
    }
}

impl Display for TypeWithoutMetatable {
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
        primitive_metatables: &'a EnumMap<TypeWithoutMetatable, Option<TableRef<C>>>,
    ) -> Option<TableRef<C>> {
        match self {
            Value::Nil => primitive_metatables[TypeWithoutMetatable::Nil].clone(),
            Value::Bool(_) => primitive_metatables[TypeWithoutMetatable::Bool].clone(),
            Value::Int(_) => primitive_metatables[TypeWithoutMetatable::Int].clone(),
            Value::Float(_) => primitive_metatables[TypeWithoutMetatable::Float].clone(),
            Value::String(_) => primitive_metatables[TypeWithoutMetatable::String].clone(),
            Value::Function(_) => primitive_metatables[TypeWithoutMetatable::Function].clone(),
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

impl<C> From<String> for Value<C> {
    fn from(value: String) -> Self {
        Value::String(value)
    }
}

impl<C> TryFrom<Value<C>> for String {
    type Error = TypeMismatchError;

    fn try_from(value: Value<C>) -> Result<Self, Self::Error> {
        match value {
            Value::String(value) => Ok(value),
            value => {
                let err = TypeMismatchError {
                    expected: Type::String,
                    found: value.type_(),
                };

                Err(err)
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeMismatchError {
    pub found: Type,
    pub expected: Type,
}

impl Display for TypeMismatchError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let TypeMismatchError { found, expected } = self;

        write!(f, "expected value of type `{expected}`, found `{found}`")
    }
}

impl Error for TypeMismatchError {}

#[derive(Debug, Clone, Copy)]
pub enum TypeMismatchOrError<E> {
    TypeMismatch(TypeMismatchError),
    Other(E),
}

impl<E> Display for TypeMismatchOrError<E>
where
    E: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeMismatchOrError::TypeMismatch(err) => write!(f, "{err}"),
            TypeMismatchOrError::Other(err) => write!(f, "{err}"),
        }
    }
}

impl<E> Error for TypeMismatchOrError<E> where Self: Debug + Display {}
