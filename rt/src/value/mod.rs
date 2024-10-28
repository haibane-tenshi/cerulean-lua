pub mod boolean;
pub mod callable;
pub mod float;
pub mod int;
pub mod nil;
pub mod string;
pub mod table;
pub mod traits;
pub mod userdata;

use std::fmt::{Debug, Display};

use enumoid::Enumoid;
use gc::Trace;

use crate::gc::{DisplayWith, Heap};

pub use boolean::Boolean;
pub use callable::Callable;
pub use float::Float;
pub use int::Int;
pub use nil::Nil;
pub use table::{KeyValue, Table};
pub use traits::{Concat, CoreTypes, Len, Meta, Metatable, Strong, TableIndex, Types, Weak};
pub use userdata::DefaultParams;

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
    /// # Note
    ///
    /// This is a lossy conversion!
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

pub type StrongValue<Ty> = Value<Strong, Ty>;
pub type WeakValue<Ty> = Value<Weak, Ty>;

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
pub enum Value<Rf: Types, Ty: CoreTypes> {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(Rf::String<Ty::String>),
    Function(Callable<Rf, Ty>),
    Table(Rf::Table<Ty::Table>),
    Userdata(Rf::FullUserdata<Ty::FullUserdata>),
}

impl<Rf, Ty> Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
{
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
{
    pub fn downgrade(&self) -> WeakValue<Ty> {
        match self {
            Value::Nil => Value::Nil,
            Value::Bool(t) => Value::Bool(*t),
            Value::Int(t) => Value::Int(*t),
            Value::Float(t) => Value::Float(*t),
            Value::String(t) => Value::String(t.downgrade()),
            Value::Function(Callable::Lua(t)) => Value::Function(Callable::Lua(t.downgrade())),
            Value::Function(Callable::Rust(t)) => Value::Function(Callable::Rust(t.downgrade())),
            Value::Table(t) => Value::Table(t.downgrade()),
            Value::Userdata(t) => Value::Userdata(t.downgrade()),
        }
    }
}

impl<Ty> WeakValue<Ty>
where
    Ty: CoreTypes,
{
    pub fn upgrade(self, heap: &Heap<Ty>) -> Option<StrongValue<Ty>> {
        let r = match self {
            Value::Nil => Value::Nil,
            Value::Bool(t) => Value::Bool(t),
            Value::Int(t) => Value::Int(t),
            Value::Float(t) => Value::Float(t),
            Value::String(t) => Value::String(t.upgrade(heap)?),
            Value::Function(Callable::Lua(t)) => Value::Function(Callable::Lua(t.upgrade(heap)?)),
            Value::Function(Callable::Rust(t)) => Value::Function(Callable::Rust(t.upgrade(heap)?)),
            Value::Table(t) => Value::Table(t.upgrade(heap)?),
            Value::Userdata(t) => Value::Userdata(t.upgrade(heap)?),
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

// impl<Ty> TryConvertFrom<WeakValue<Ty>, Heap<Ty>> for StrongValue<Ty>
// where
//     Ty: CoreTypes,
// {
//     type Error = crate::error::AlreadyDroppedError;

//     fn try_from_with_gc(value: WeakValue<Ty>, heap: &mut Heap<Ty>) -> Result<Self, Self::Error> {
//         use crate::error::AlreadyDroppedError;

//         value.upgrade(heap).ok_or(AlreadyDroppedError)
//     }
// }

impl<Rf, Ty> Trace for Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::String<Ty::String>: Trace,
    Callable<Rf, Ty>: Trace,
    Rf::Table<Ty::Table>: Trace,
    Rf::FullUserdata<Ty::FullUserdata>: Trace,
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

impl<Rf, Ty> Debug for Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::String<Ty::String>: Debug,
    Callable<Rf, Ty>: Debug,
    Rf::Table<Ty::Table>: Debug,
    Rf::FullUserdata<Ty::FullUserdata>: Debug,
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

impl<Rf, Ty> Clone for Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::String<Ty::String>: Clone,
    Callable<Rf, Ty>: Clone,
    Rf::Table<Ty::Table>: Clone,
    Rf::FullUserdata<Ty::FullUserdata>: Clone,
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

impl<Rf, Ty> Copy for Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::String<Ty::String>: Copy,
    Callable<Rf, Ty>: Copy,
    Rf::Table<Ty::Table>: Copy,
    Rf::FullUserdata<Ty::FullUserdata>: Copy,
{
}

impl<Rf, Ty> PartialEq for Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::String<Ty::String>: PartialEq,
    Callable<Rf, Ty>: PartialEq,
    Rf::Table<Ty::Table>: PartialEq,
    Rf::FullUserdata<Ty::FullUserdata>: PartialEq,
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

impl<Rf, Ty> Eq for Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::String<Ty::String>: Eq,
    Callable<Rf, Ty>: Eq,
    Rf::Table<Ty::Table>: Eq,
    Rf::FullUserdata<Ty::FullUserdata>: Eq,
{
}

// No, clippy, you cannot.
#[allow(clippy::derivable_impls)]
impl<Rf, Ty> Default for Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
{
    fn default() -> Self {
        Value::Nil
    }
}

pub struct ValueWith<'a, Value, Heap>(&'a Value, &'a Heap);

impl<'a, Ty> Display for ValueWith<'a, WeakValue<Ty>, Heap<Ty>>
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

impl<'a, Ty> Display for ValueWith<'a, StrongValue<Ty>, Heap<Ty>>
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

impl<Ty> DisplayWith<Heap<Ty>> for WeakValue<Ty>
where
    Ty: CoreTypes,
{
    type Output<'a> = ValueWith<'a, Self, Heap<Ty>>
    where
        Self: 'a;

    fn display<'a>(&'a self, extra: &'a Heap<Ty>) -> Self::Output<'a> {
        ValueWith(self, extra)
    }
}

impl<Ty> DisplayWith<Heap<Ty>> for StrongValue<Ty>
where
    Ty: CoreTypes,
{
    type Output<'a> = ValueWith<'a, Self, Heap<Ty>>
    where
        Self: 'a;

    fn display<'a>(&'a self, extra: &'a Heap<Ty>) -> Self::Output<'a> {
        ValueWith(self, extra)
    }
}
