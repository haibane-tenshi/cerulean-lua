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

use crate::error::{AlreadyDroppedError, InvalidKeyError};
use crate::gc::{DisplayWith, Heap};
use crate::runtime::MetatableRegistry;

pub use boolean::Boolean;
pub use callable::Callable;
pub use float::Float;
pub use int::Int;
pub use nil::Nil;
pub use table::{KeyValue, Table};
pub use traits::{Concat, Len, Meta, Metatable, Refs, Strong, TableIndex, Types, Weak};
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

    pub(crate) fn cmp(self, other: Self) -> std::cmp::Ordering {
        fn to_discr(value: Type) -> u8 {
            match value {
                Type::Nil => 0,
                Type::Bool => 1,
                Type::Int => 2,
                Type::Float => 3,
                Type::String => 4,
                Type::Function => 5,
                Type::Table => 6,
                Type::Userdata => 7,
            }
        }

        to_discr(self).cmp(&to_discr(other))
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

impl From<SolitaryType> for Type {
    fn from(value: SolitaryType) -> Self {
        match value {
            SolitaryType::Nil => Type::Nil,
            SolitaryType::Bool => Type::Bool,
            SolitaryType::Int => Type::Int,
            SolitaryType::Float => Type::Float,
            SolitaryType::String => Type::String,
            SolitaryType::Function => Type::Function,
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

impl From<SolitaryType> for LuaType {
    fn from(value: SolitaryType) -> Self {
        match value {
            SolitaryType::Nil => LuaType::Nil,
            SolitaryType::Bool => LuaType::Boolean,
            SolitaryType::Int => LuaType::Number,
            SolitaryType::Float => LuaType::Number,
            SolitaryType::String => LuaType::String,
            SolitaryType::Function => LuaType::Function,
        }
    }
}

/// Lua type which doesn't carry a metatable.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Enumoid)]
pub enum SolitaryType {
    Nil,
    Bool,
    Int,
    Float,
    String,
    Function,
}

impl SolitaryType {
    pub fn to_str(self) -> &'static str {
        let ty: LuaType = self.into();
        ty.to_str()
    }
}

impl Display for SolitaryType {
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
pub enum Value<Rf: Refs, Ty: Types> {
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
    Rf: Refs,
    Ty: Types,
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

    /// Convert value into table key.
    ///
    /// Lua does not permit table indexing using `nil` or `NaN`.
    /// This function ensures that value does not contain invalid entry.
    ///
    /// If you are in charge of constructing a key, it might be better to do it directly
    /// in case the value is not float.
    pub fn into_key(self) -> Result<KeyValue<Rf, Ty>, InvalidKeyError> {
        self.try_into()
    }
}

impl<Ty> StrongValue<Ty>
where
    Ty: Types,
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

    /// Produce metatable reference.
    ///
    /// If the value carries a metatable (it is table or userdata), that metatable will be returned.
    /// Heap is required to follow through reference.
    ///
    /// If the value doesn't carry a metatable (e.g. one of [`SolitaryType`]), metatable will be taken from the registry.
    pub fn metatable(
        &self,
        heap: &Heap<Ty>,
        registry: &MetatableRegistry<Ty::Table>,
    ) -> Option<Meta<Ty>> {
        use crate::gc::LuaPtr;
        use SolitaryType::*;

        match self {
            Value::Nil => registry.get_gc(Nil),
            Value::Bool(_) => registry.get_gc(Bool),
            Value::Int(_) => registry.get_gc(Int),
            Value::Float(_) => registry.get_gc(Float),
            Value::String(_) => registry.get_gc(String),
            Value::Function(_) => registry.get_gc(Function),
            Value::Table(LuaPtr(t)) => heap.get_root(t).metatable().copied(),
            Value::Userdata(LuaPtr(t)) => heap.get_root(t).metatable().copied(),
        }
    }
}

impl<Ty> WeakValue<Ty>
where
    Ty: Types,
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

    /// Produce metatable reference.
    ///
    /// If the value carries a metatable (it is table or userdata), that metatable will be returned.
    /// Heap is required to follow through reference.
    /// Error will be returned if the value behind reference was garbage collected.
    ///
    /// If the value doesn't carry a metatable (e.g. one of [`SolitaryType`]), metatable will be taken from the registry.
    pub fn metatable(
        &self,
        heap: &Heap<Ty>,
        registry: &MetatableRegistry<Ty::Table>,
    ) -> Result<Option<Meta<Ty>>, AlreadyDroppedError> {
        use crate::gc::LuaPtr;
        use SolitaryType::*;

        let r = match self {
            Value::Nil => registry.get_gc(Nil),
            Value::Bool(_) => registry.get_gc(Bool),
            Value::Int(_) => registry.get_gc(Int),
            Value::Float(_) => registry.get_gc(Float),
            Value::String(_) => registry.get_gc(String),
            Value::Function(_) => registry.get_gc(Function),
            Value::Table(LuaPtr(t)) => heap
                .get(*t)
                .ok_or(AlreadyDroppedError)?
                .metatable()
                .copied(),
            Value::Userdata(LuaPtr(t)) => heap
                .get(*t)
                .ok_or(AlreadyDroppedError)?
                .metatable()
                .copied(),
        };

        Ok(r)
    }
}

impl<Ty> WeakValue<Ty>
where
    Ty: Types,
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
    Ty: Types,
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
    Rf: Refs,
    Ty: Types,
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
    Rf: Refs,
    Ty: Types,
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
    Rf: Refs,
    Ty: Types,
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
    Rf: Refs,
    Ty: Types,
    Rf::String<Ty::String>: Copy,
    Callable<Rf, Ty>: Copy,
    Rf::Table<Ty::Table>: Copy,
    Rf::FullUserdata<Ty::FullUserdata>: Copy,
{
}

impl<Rf, Ty> PartialEq for Value<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
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
    Rf: Refs,
    Ty: Types,
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
    Rf: Refs,
    Ty: Types,
{
    fn default() -> Self {
        Value::Nil
    }
}

pub struct ValueWith<'a, Value, Heap>(&'a Value, &'a Heap);

impl<Ty> Display for ValueWith<'_, WeakValue<Ty>, Heap<Ty>>
where
    Ty: Types,
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

impl<Ty> Display for ValueWith<'_, StrongValue<Ty>, Heap<Ty>>
where
    Ty: Types,
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
    Ty: Types,
{
    type Output<'a>
        = ValueWith<'a, Self, Heap<Ty>>
    where
        Self: 'a;

    fn display<'a>(&'a self, extra: &'a Heap<Ty>) -> Self::Output<'a> {
        ValueWith(self, extra)
    }
}

impl<Ty> DisplayWith<Heap<Ty>> for StrongValue<Ty>
where
    Ty: Types,
{
    type Output<'a>
        = ValueWith<'a, Self, Heap<Ty>>
    where
        Self: 'a;

    fn display<'a>(&'a self, extra: &'a Heap<Ty>) -> Self::Output<'a> {
        ValueWith(self, extra)
    }
}
