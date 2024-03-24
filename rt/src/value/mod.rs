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

use crate::gc::{DisplayWith, TryFromWithGc};

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
        match self {
            Value::Nil => Value::Nil,
            Value::Bool(t) => Value::Bool(*t),
            Value::Int(t) => Value::Int(*t),
            Value::Float(t) => Value::Float(*t),
            Value::String(t) => Value::String(t.downgrade()),
            Value::Function(Callable::Lua(t)) => Value::Function(Callable::Lua(t.downgrade_cell())),
            Value::Function(Callable::Rust(t)) => {
                Value::Function(Callable::Rust(t.downgrade_cell()))
            }
            Value::Table(t) => Value::Table(t.downgrade_cell()),
            Value::Userdata(t) => Value::Userdata(t.downgrade_cell()),
        }
    }
}

impl<Ty> WeakValue<Ty>
where
    Ty: CoreTypes,
{
    pub fn upgrade(self, heap: &Heap) -> Option<StrongValue<Ty>> {
        let r = match self {
            Value::Nil => Value::Nil,
            Value::Bool(t) => Value::Bool(t),
            Value::Int(t) => Value::Int(t),
            Value::Float(t) => Value::Float(t),
            Value::String(t) => Value::String(t.upgrade(heap)?),
            Value::Function(Callable::Lua(t)) => {
                Value::Function(Callable::Lua(t.upgrade_cell(heap)?))
            }
            Value::Function(Callable::Rust(t)) => {
                Value::Function(Callable::Rust(t.upgrade_cell(heap)?))
            }
            Value::Table(t) => Value::Table(t.upgrade_cell(heap)?),
            Value::Userdata(t) => Value::Userdata(t.upgrade_cell(heap)?),
        };

        Some(r)
    }
}

impl<Ty> WeakValue<Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn is_transient(&self) -> bool {
        use Value::*;

        match self {
            Nil | Bool(_) | Int(_) | Float(_) => false,
            String(_)
            | Function(Callable::Rust(_))
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
        value.downgrade()
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
        use crate::error::AlreadyDroppedError;

        value.upgrade(heap).ok_or(AlreadyDroppedError)
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

impl<Ty> Copy for Value<Ty>
where
    Ty: Types,
    Ty::String: Copy,
    Ty::LuaCallable: Copy,
    Ty::RustCallable: Copy,
    Ty::Table: Copy,
    Ty::FullUserdata: Copy,
{
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

impl<Ty> Eq for Value<Ty>
where
    Ty: Types,
    Ty::String: Eq,
    Ty::LuaCallable: Eq,
    Ty::RustCallable: Eq,
    Ty::Table: Eq,
    Ty::FullUserdata: Eq,
{
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

pub struct ValueWith<'a, Value, Heap>(&'a Value, &'a Heap);

impl<'a, Ty> Display for ValueWith<'a, WeakValue<Ty>, Heap>
where
    Ty: CoreTypes,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;

        let ValueWith(value, heap) = self;

        match value {
            Nil => write!(f, "nil"),
            Bool(t) => write!(f, "{t}"),
            Int(t) => write!(f, "{t}"),
            Float(t) => write!(f, "{t}"),
            String(t) => {
                if let Some(s) = heap.get(t.0).map(AsRef::as_ref) {
                    write!(f, "{s}")
                } else {
                    Ok(())
                }
            }
            Function(Callable::Lua(t)) => write!(f, "{{[lua] closure <{t:p}>}}"),
            Function(Callable::Rust(t)) => write!(f, "{{[rust] closure <{t:p}>}}"),
            Table(t) => write!(f, "{{table <{t:p}>}}"),
            Userdata(t) => write!(f, "{{userdata <{t:p}>}}"),
        }
    }
}

impl<'a, Ty> Display for ValueWith<'a, StrongValue<Ty>, Heap>
where
    Ty: CoreTypes,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;

        let ValueWith(value, heap) = self;

        match value {
            Nil => write!(f, "nil"),
            Bool(t) => write!(f, "{t}"),
            Int(t) => write!(f, "{t}"),
            Float(t) => write!(f, "{t}"),
            String(t) => {
                let s = heap.get_root(&t.0).as_ref();
                write!(f, "{s}")
            }
            Function(Callable::Lua(t)) => write!(f, "{{[lua] closure <{t:p}>}}"),
            Function(Callable::Rust(t)) => write!(f, "{{[rust] closure <{t:p}>}}"),
            Table(t) => write!(f, "{{table <{t:p}>}}"),
            Userdata(t) => write!(f, "{{userdata <{t:p}>}}"),
        }
    }
}

impl<Ty> DisplayWith<Heap> for WeakValue<Ty>
where
    Ty: CoreTypes,
{
    type Output<'a> = ValueWith<'a, Self, Heap>
    where
        Self: 'a;

    fn display<'a>(&'a self, extra: &'a Heap) -> Self::Output<'a> {
        ValueWith(self, extra)
    }
}

impl<Ty> DisplayWith<Heap> for StrongValue<Ty>
where
    Ty: CoreTypes,
{
    type Output<'a> = ValueWith<'a, Self, Heap>
    where
        Self: 'a;

    fn display<'a>(&'a self, extra: &'a Heap) -> Self::Output<'a> {
        ValueWith(self, extra)
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
