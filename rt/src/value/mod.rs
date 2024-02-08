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

use crate::error::BorrowError;
use crate::gc::{Gc as GarbageCollector, Visit};
use enumoid::{EnumMap, Enumoid};
use repr::literal::Literal;

pub use boolean::Boolean;
pub use callable::Callable;
pub use float::Float;
pub use int::Int;
pub use nil::{Nil, NilOr};
pub use string::LuaString;
pub use table::{KeyValue, LuaTable, Table};
pub use traits::{Borrow, Concat, Len, Metatable, TableIndex, TypeProvider};

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
pub enum Value<Gc: TypeProvider> {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(Gc::StringRef),
    Function(Callable<Gc::RustCallable>),
    Table(Gc::TableRef),
    Userdata(Gc::FullUserdataRef),
}

impl<Gc: TypeProvider> Value<Gc> {
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
        primitive_metatables: &'a EnumMap<TypeWithoutMetatable, Option<Gc::TableRef>>,
    ) -> Option<Gc::TableRef> {
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

impl<Gc> Value<Gc>
where
    Gc: GarbageCollector,
{
    pub fn from_literal(literal: Literal, gc: &mut Gc) -> Self {
        match literal {
            Literal::Nil => Value::Nil,
            Literal::Bool(value) => Value::Bool(value),
            Literal::Int(value) => Value::Int(value),
            Literal::Float(value) => Value::Float(value.into_inner()),
            Literal::String(value) => {
                let value = gc.alloc_string(value.into());
                Value::String(value)
            }
        }
    }
}

impl<Gc> Debug for Value<Gc>
where
    Gc: TypeProvider,
    Gc::StringRef: Debug,
    Gc::RustCallable: Debug,
    Gc::TableRef: Debug,
    Gc::FullUserdataRef: Debug,
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

impl<Gc> Clone for Value<Gc>
where
    Gc: TypeProvider,
    Gc::String: Clone,
    Gc::RustCallable: Clone,
    Gc::TableRef: Clone,
    Gc::FullUserdataRef: Clone,
{
    #[allow(clippy::clone_on_copy)]
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

impl<Gc> PartialEq for Value<Gc>
where
    Gc: TypeProvider,
    Gc::String: PartialEq,
    Gc::RustCallable: PartialEq,
    Gc::TableRef: PartialEq,
    Gc::FullUserdataRef: PartialEq,
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
impl<Gc> Default for Value<Gc>
where
    Gc: TypeProvider,
{
    fn default() -> Self {
        Value::Nil
    }
}

impl<Gc> Display for Value<Gc>
where
    Gc: TypeProvider,
    Gc::StringRef: Display,
    Gc::RustCallable: Display,
    Gc::TableRef: Display,
    Gc::FullUserdataRef: Display,
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
                write!(f, "{v}")
            }
            Function(v) => write!(f, "{v}"),
            Table(v) => write!(f, "{v}"),
            Userdata(v) => write!(f, "{v}"),
        }
    }
}

impl<Gc> Visit<Gc::Sweeper<'_>> for Value<Gc>
where
    Gc: GarbageCollector,
    Gc::Table: for<'a> Visit<Gc::Sweeper<'a>>,
{
    fn visit(&self, sweeper: &mut Gc::Sweeper<'_>) -> Result<(), BorrowError> {
        use crate::gc::Sweeper;
        use std::ops::ControlFlow;
        use Value::*;

        match self {
            Nil | Bool(_) | Int(_) | Float(_) | Function(_) => (),
            String(t) => sweeper.mark_string(t),
            Table(t) => {
                if let ControlFlow::Continue(_) = sweeper.mark_table(t) {
                    crate::gc::visit_borrow(t, sweeper)?;
                }
            }
            Userdata(t) => sweeper.mark_userdata(t),
        }

        Ok(())
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
