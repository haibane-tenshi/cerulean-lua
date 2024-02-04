pub mod boolean;
pub mod callable;
pub mod float;
pub mod int;
pub mod nil;
pub mod string;
pub mod table;
pub mod traits;
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
pub use string::LuaString;
pub use table::{KeyValue, LuaTable, Table, TableRef};
pub use traits::{Borrow, Concat, Len, Metatable, TableIndex, TypeProvider};
pub use userdata::UserdataRef;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Nil,
    Bool,
    Int,
    Float,
    String,
    Function,
    Table,
    Userdata,
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
            Type::Userdata => "userdata",
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
    Userdata,
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
            Userdata => "userdata",
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
            Type::Userdata => LuaType::Userdata,
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

pub struct DefaultTypes<C>(std::marker::PhantomData<C>);

impl<C> TypeProvider for DefaultTypes<C> {
    type String = string::PossiblyUtf8Vec;
    type RustCallable = callable::RustCallable<Self, C>;
    type Table = Table<Self>;
    type TableRef = TableRef<Self>;
    type FullUserdata = userdata::FullUserdata<Self, C>;
    type FullUserdataRef = UserdataRef<Self, C>;
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
pub enum Value<Types: TypeProvider> {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(Types::String),
    Function(Callable<Types::RustCallable>),
    Table(Types::TableRef),
    Userdata(Types::FullUserdataRef),
}

impl<Types: TypeProvider> Value<Types> {
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
            Value::Userdata(_) => Type::Userdata,
        }
    }

    pub fn take(&mut self) -> Self {
        std::mem::take(self)
    }

    pub(crate) fn metatable<'a>(
        &'a self,
        primitive_metatables: &'a EnumMap<TypeWithoutMetatable, Option<Types::TableRef>>,
    ) -> Option<Types::TableRef> {
        match self {
            Value::Nil => primitive_metatables[TypeWithoutMetatable::Nil].clone(),
            Value::Bool(_) => primitive_metatables[TypeWithoutMetatable::Bool].clone(),
            Value::Int(_) => primitive_metatables[TypeWithoutMetatable::Int].clone(),
            Value::Float(_) => primitive_metatables[TypeWithoutMetatable::Float].clone(),
            Value::String(_) => primitive_metatables[TypeWithoutMetatable::String].clone(),
            Value::Function(_) => primitive_metatables[TypeWithoutMetatable::Function].clone(),
            Value::Table(t) => {
                let Ok(r) = t.with_ref(|t| t.metatable()) else {
                    todo!()
                };

                r
            }
            Value::Userdata(t) => {
                let Ok(r) = t.with_ref(|t| t.metatable()) else {
                    todo!()
                };

                r
            }
        }
    }
}

impl<Types> Debug for Value<Types>
where
    Types: TypeProvider,
    Types::String: Debug,
    Types::RustCallable: Debug,
    Types::TableRef: Debug,
    Types::FullUserdataRef: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Function(arg0) => f.debug_tuple("Function").field(arg0).finish(),
            Self::Table(arg0) => f.debug_tuple("Table").field(arg0).finish(),
            Self::Userdata(arg0) => f.debug_tuple("Userdata").field(arg0).finish(),
        }
    }
}

impl<Types> Clone for Value<Types>
where
    Types: TypeProvider,
    Types::String: Clone,
    Types::RustCallable: Clone,
    Types::TableRef: Clone,
    Types::FullUserdataRef: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Nil => Self::Nil,
            Self::Bool(arg0) => Self::Bool(arg0.clone()),
            Self::Int(arg0) => Self::Int(arg0.clone()),
            Self::Float(arg0) => Self::Float(arg0.clone()),
            Self::String(arg0) => Self::String(arg0.clone()),
            Self::Function(arg0) => Self::Function(arg0.clone()),
            Self::Table(arg0) => Self::Table(arg0.clone()),
            Self::Userdata(arg0) => Self::Userdata(arg0.clone()),
        }
    }
}

impl<Types> PartialEq for Value<Types>
where
    Types: TypeProvider,
    Types::String: PartialEq,
    Types::RustCallable: PartialEq,
    Types::TableRef: PartialEq,
    Types::FullUserdataRef: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Function(l0), Self::Function(r0)) => l0 == r0,
            (Self::Table(l0), Self::Table(r0)) => l0 == r0,
            (Self::Userdata(l0), Self::Userdata(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

// No, clippy, you cannot.
#[allow(clippy::derivable_impls)]
impl<Types> Default for Value<Types>
where
    Types: TypeProvider,
{
    fn default() -> Self {
        Value::Nil
    }
}

impl<Types> Display for Value<Types>
where
    Types: TypeProvider,
    Types::String: Debug + Display,
    Types::RustCallable: Debug,
    Types::TableRef: Debug,
    Types::FullUserdataRef: Debug,
{
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
            Userdata(v) => write!(f, "{{userdata <{v:?}>}}"),
        }
    }
}

impl<Types> From<Literal> for Value<Types>
where
    Types: TypeProvider,
{
    fn from(value: Literal) -> Self {
        match value {
            Literal::Nil => Value::Nil,
            Literal::Bool(value) => Value::Bool(value),
            Literal::Int(value) => Value::Int(value),
            Literal::Float(value) => Value::Float(value.into_inner()),
            Literal::String(value) => Value::String(value.into()),
        }
    }
}

impl<Types> From<String> for Value<Types>
where
    Types: TypeProvider,
{
    fn from(value: String) -> Self {
        Value::String(value.into())
    }
}

impl<Types> TryFrom<Value<Types>> for String
where
    Types: TypeProvider,
    String: From<Types::String>,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Types>) -> Result<Self, Self::Error> {
        match value {
            Value::String(value) => Ok(value.into()),
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
