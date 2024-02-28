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

use enumoid::Enumoid;
use gc::{Heap, Trace};
use repr::literal::Literal;

use crate::gc::TryFromWithGc;

pub use boolean::Boolean;
pub use callable::Callable;
pub use float::Float;
pub use int::Int;
pub use nil::{Nil, NilOr};
pub use string::LuaString;
pub use table::{KeyValue, LuaTable, Table};
pub use traits::{Concat, CoreTypes, Len, Metatable, Strong, TableIndex, Types, Weak};

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

pub type StrongValue<Ty> = Value<Strong<Ty>>;
pub type WeakValue<Ty> = Value<Weak<Ty>>;

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
pub enum Value<Ty: Types> {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(Ty::String),
    Function(Callable<Ty>),
    Table(Ty::Table),
    Userdata(Ty::FullUserdata),
}

impl<Ty: Types> Value<Ty> {
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
}

impl<Ty> StrongValue<Ty>
where
    Ty: CoreTypes,
    Ty::RustClosure: Clone,
{
    pub fn downgrade(&self) -> WeakValue<Ty> {
        self.clone().into()
    }
}

impl<Ty> WeakValue<Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn is_transient(&self) -> bool {
        use callable::RustCallable;
        use Value::*;

        match self {
            Nil | Bool(_) | Int(_) | Float(_) => false,
            // In current impl strings are not gc allocated.
            // Will need to be adjusted when that changes.
            String(_) => false,
            // Pointers are curently passed by-value.
            // Will need to be adjusted if that changes.
            Function(Callable::Rust(RustCallable::Ptr(_))) => false,
            Function(Callable::Rust(RustCallable::Ref(_)))
            | Function(Callable::Lua(_))
            | Table(_)
            | Userdata(_) => true,
        }
    }
}

impl<Ty> From<StrongValue<Ty>> for WeakValue<Ty>
where
    Ty: CoreTypes,
{
    fn from(value: StrongValue<Ty>) -> Self {
        use callable::RustCallable;

        match value {
            Value::Nil => Value::Nil,
            Value::Bool(t) => Value::Bool(t),
            Value::Int(t) => Value::Int(t),
            Value::Float(t) => Value::Float(t),
            Value::String(t) => Value::String(t),
            Value::Function(Callable::Lua(t)) => Value::Function(Callable::Lua(t.downgrade())),
            Value::Function(Callable::Rust(RustCallable::Ptr(t))) => {
                Value::Function(Callable::Rust(RustCallable::Ptr(t)))
            }
            Value::Function(Callable::Rust(RustCallable::Ref(t))) => {
                Value::Function(Callable::Rust(RustCallable::Ref(t.downgrade())))
            }
            Value::Table(t) => Value::Table(t.downgrade()),
            Value::Userdata(t) => Value::Userdata(t.downgrade()),
        }
    }
}

impl<Ty> TryFromWithGc<Literal, Heap> for StrongValue<Ty>
where
    Ty: CoreTypes,
    Ty::String: From<String>,
{
    type Error = std::convert::Infallible;

    fn try_from_with_gc(value: Literal, _gc: &mut Heap) -> Result<Self, Self::Error> {
        match value {
            Literal::Nil => Ok(Value::Nil),
            Literal::Bool(t) => Ok(Value::Bool(t)),
            Literal::Int(t) => Ok(Value::Int(t)),
            Literal::Float(t) => Ok(Value::Float(t.into_inner())),
            Literal::String(t) => {
                let t = crate::gc::StringRef::new(t.into());
                Ok(Value::String(t))
            }
        }
    }
}

impl<Ty> TryFromWithGc<WeakValue<Ty>, Heap> for StrongValue<Ty>
where
    Ty: CoreTypes,
    Ty::Table: Trace,
    Ty::FullUserdata: Trace,
{
    type Error = crate::error::AlreadyDroppedError;

    fn try_from_with_gc(value: WeakValue<Ty>, heap: &mut Heap) -> Result<Self, Self::Error> {
        use crate::gc::TryIntoWithGc;
        use callable::RustCallable;

        let r = match value {
            Value::Nil => Value::Nil,
            Value::Bool(t) => Value::Bool(t),
            Value::Int(t) => Value::Int(t),
            Value::Float(t) => Value::Float(t),
            Value::String(t) => Value::String(t),
            Value::Function(Callable::Lua(t)) => {
                Value::Function(Callable::Lua(t.try_into_with_gc(heap)?))
            }
            Value::Function(Callable::Rust(RustCallable::Ptr(t))) => {
                Value::Function(Callable::Rust(RustCallable::Ptr(t)))
            }
            Value::Function(Callable::Rust(RustCallable::Ref(t))) => {
                Value::Function(Callable::Rust(RustCallable::Ref(t.try_into_with_gc(heap)?)))
            }
            Value::Table(t) => Value::Table(t.try_into_with_gc(heap)?),
            Value::Userdata(t) => Value::Userdata(t.try_into_with_gc(heap)?),
        };

        Ok(r)
    }
}

impl<Ty> Trace for Value<Ty>
where
    Ty: Types + 'static,
    Ty::String: Trace,
    Ty::LuaCallable: Trace,
    Ty::RustCallable: Trace,
    Ty::Table: Trace,
    Ty::FullUserdata: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        use Value::*;

        match self {
            Nil | Bool(_) | Int(_) | Float(_) => (),
            String(t) => t.trace(collector),
            Function(t) => t.trace(collector),
            Table(t) => t.trace(collector),
            Userdata(t) => t.trace(collector),
        }
    }
}

impl<Ty> Debug for Value<Ty>
where
    Ty: Types,
    Ty::String: Debug,
    Ty::LuaCallable: Debug,
    Ty::RustCallable: Debug,
    Ty::Table: Debug,
    Ty::FullUserdata: Debug,
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

impl<Ty> Clone for Value<Ty>
where
    Ty: Types,
    Ty::String: Clone,
    Ty::LuaCallable: Clone,
    Ty::RustCallable: Clone,
    Ty::Table: Clone,
    Ty::FullUserdata: Clone,
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

impl<Ty> PartialEq for Value<Ty>
where
    Ty: Types,
    Ty::String: PartialEq,
    Ty::LuaCallable: PartialEq,
    Ty::RustCallable: PartialEq,
    Ty::Table: PartialEq,
    Ty::FullUserdata: PartialEq,
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
impl<Ty> Default for Value<Ty>
where
    Ty: Types,
{
    fn default() -> Self {
        Value::Nil
    }
}

impl<Ty> Display for Value<Ty>
where
    Ty: Types,
    Ty::String: Display,
    Ty::LuaCallable: Display,
    Ty::RustCallable: Display,
    Ty::Table: Display,
    Ty::FullUserdata: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;

        match self {
            Nil => write!(f, "{}", nil::Nil),
            Bool(v) => write!(f, "{}", boolean::Boolean(*v)),
            Int(v) => write!(f, "{}", int::Int(*v)),

            Float(v) => write!(f, "{}", float::Float(*v)),

            String(v) => write!(f, "{v}"),

            Function(v) => write!(f, "{v}"),
            Table(v) => write!(f, "{v}"),
            Userdata(v) => write!(f, "{v}"),
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
