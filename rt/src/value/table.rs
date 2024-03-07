use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use gc::Trace;
use ordered_float::NotNan;

use super::callable::Callable;
use super::{
    CoreTypes, Metatable, Strong, TableIndex, TypeMismatchError, TypeMismatchOrError, Types, Value,
    Weak,
};

pub struct Table<Ty: Types> {
    data: HashMap<KeyValue<Ty>, Value<Ty>>,
    metatable: Option<Ty::Table>,
}

impl<Ty> TableIndex<Ty> for Table<Ty>
where
    Ty: Types,
    KeyValue<Ty>: Hash + Eq,
    Value<Ty>: Clone,
{
    fn get(&self, key: &KeyValue<Ty>) -> Value<Ty> {
        self.data.get(key).cloned().unwrap_or_default()
    }

    fn set(&mut self, key: KeyValue<Ty>, value: Value<Ty>) {
        match value {
            Value::Nil => {
                self.data.remove(&key);
            }
            value => {
                self.data.insert(key, value);
            }
        }
    }

    fn border(&self) -> i64 {
        Table::border(self)
    }

    fn contains_key(&self, key: &KeyValue<Ty>) -> bool {
        self.data.contains_key(key)
    }
}

impl<Ty> Table<Ty>
where
    Ty: Types,
    KeyValue<Ty>: Hash + Eq,
    Value<Ty>: Clone,
{
    pub fn get(&self, key: &KeyValue<Ty>) -> Value<Ty> {
        self.data.get(key).cloned().unwrap_or_default()
    }
}

impl<Ty> Table<Ty>
where
    Ty: Types,
    KeyValue<Ty>: Hash + Eq,
{
    pub fn get_ref<'s>(&'s self, key: &KeyValue<Ty>) -> Option<&'s Value<Ty>> {
        self.data.get(key)
    }

    pub fn set(&mut self, key: KeyValue<Ty>, value: Value<Ty>) {
        match value {
            Value::Nil => {
                self.data.remove(&key);
            }
            value => {
                self.data.insert(key, value);
            }
        }
    }

    pub fn contains_key(&self, key: &KeyValue<Ty>) -> bool {
        self.data.contains_key(key)
    }

    pub fn border(&self) -> i64 {
        // Inefficient, but will get fixed when table layout is improved.
        (0..)
            .find(|&i| !self.data.contains_key(&KeyValue::Int(i + 1)))
            .unwrap_or(i64::MAX)
    }
}
impl<Ty> Table<Ty>
where
    Ty: Types,
    Ty::Table: Clone,
{
    pub fn metatable(&self) -> Option<Ty::Table> {
        self.metatable.clone()
    }
}

impl<Ty> Table<Ty>
where
    Ty: Types,
{
    pub fn set_metatable(&mut self, metatable: Option<Ty::Table>) -> Option<Ty::Table> {
        std::mem::replace(&mut self.metatable, metatable)
    }
}

impl<Ty> Metatable<Ty::Table> for Table<Ty>
where
    Ty: Types,
    Ty::Table: Clone,
{
    fn metatable(&self) -> Option<Ty::Table> {
        Table::metatable(self)
    }

    fn set_metatable(&mut self, mt: Option<Ty::Table>) -> Option<Ty::Table> {
        Table::set_metatable(self, mt)
    }
}

impl<Ty> Trace for Table<Weak<Ty>>
where
    Ty: CoreTypes,
    Ty::String: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        for (key, value) in self.data.iter() {
            key.trace(collector);
            value.trace(collector);
        }

        self.metatable.trace(collector);
    }
}

impl<Ty> Debug for Table<Ty>
where
    Ty: Types,
    KeyValue<Ty>: Debug,
    Value<Ty>: Debug,
    Ty::Table: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Table")
            .field("data", &self.data)
            .field("metatable", &self.metatable)
            .finish()
    }
}

impl<Ty> Clone for Table<Ty>
where
    Ty: Types,
    KeyValue<Ty>: Clone,
    Value<Ty>: Clone,
    Ty::Table: Clone,
{
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            metatable: self.metatable.clone(),
        }
    }
}

impl<Ty> PartialEq for Table<Weak<Ty>>
where
    Ty: CoreTypes,
    Ty::String: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
            && self.metatable.map(|t| t.addr()) == other.metatable.map(|t| t.addr())
    }
}

impl<Ty> PartialEq for Table<Strong<Ty>>
where
    Ty: CoreTypes,
    Ty::String: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
            && self.metatable.as_ref().map(|t| t.addr())
                == other.metatable.as_ref().map(|t| t.addr())
    }
}

impl<Gc> Default for Table<Gc>
where
    Gc: Types,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
            metatable: Default::default(),
        }
    }
}

pub enum KeyValue<Ty: Types> {
    Bool(bool),
    Int(i64),
    Float(NotNan<f64>),
    String(Ty::String),
    Function(Callable<Ty>),
    Table(Ty::Table),
    Userdata(Ty::FullUserdata),
}

impl<Ty> Trace for KeyValue<Ty>
where
    Ty: Types + 'static,
    Ty::String: Trace,
    Ty::LuaCallable: Trace,
    Ty::RustCallable: Trace,
    Ty::Table: Trace,
    Ty::FullUserdata: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        use KeyValue::*;

        match self {
            Bool(_) | Int(_) | Float(_) => (),
            String(t) => t.trace(collector),
            Function(t) => t.trace(collector),
            Table(t) => t.trace(collector),
            Userdata(t) => t.trace(collector),
        }
    }
}

impl<Ty> Debug for KeyValue<Ty>
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

impl<Ty> Clone for KeyValue<Ty>
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

impl<Ty> PartialEq for KeyValue<Weak<Ty>>
where
    Ty: CoreTypes,
    Ty::String: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        use super::callable::RustCallable;

        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0.addr() == r0.addr(),
            (Self::Function(Callable::Lua(l0)), Self::Function(Callable::Lua(r0))) => {
                l0.addr() == r0.addr()
            }
            (
                Self::Function(Callable::Rust(RustCallable::Ref(l0))),
                Self::Function(Callable::Rust(RustCallable::Ref(r0))),
            ) => l0.addr() == r0.addr(),
            (
                Self::Function(Callable::Rust(RustCallable::Ptr(l0))),
                Self::Function(Callable::Rust(RustCallable::Ptr(r0))),
            ) => l0 == r0,
            (Self::Table(l0), Self::Table(r0)) => l0.addr() == r0.addr(),
            (Self::Userdata(l0), Self::Userdata(r0)) => l0.addr() == r0.addr(),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl<Ty> PartialEq for KeyValue<Strong<Ty>>
where
    Ty: CoreTypes,
    Ty::String: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0.addr() == r0.addr(),
            (Self::Function(l0), Self::Function(r0)) => l0 == r0,
            (Self::Table(l0), Self::Table(r0)) => l0.addr() == r0.addr(),
            (Self::Userdata(l0), Self::Userdata(r0)) => l0.addr() == r0.addr(),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl<Ty> Eq for KeyValue<Weak<Ty>>
where
    Ty: CoreTypes,
    Ty::String: Eq,
{
}

impl<Ty> Eq for KeyValue<Strong<Ty>>
where
    Ty: CoreTypes,
    Ty::String: Eq,
{
}

impl<Ty> Hash for KeyValue<Weak<Ty>>
where
    Ty: CoreTypes,
    Ty::String: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use KeyValue::*;

        core::mem::discriminant(self).hash(state);

        match self {
            Bool(val) => val.hash(state),
            Int(val) => val.hash(state),
            Float(val) => val.hash(state),
            String(val) => val.addr().hash(state),
            Function(val) => val.hash(state),
            Table(val) => val.addr().hash(state),
            Userdata(val) => val.addr().hash(state),
        }
    }
}

impl<Ty> Hash for KeyValue<Strong<Ty>>
where
    Ty: CoreTypes,
    Ty::String: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use KeyValue::*;

        core::mem::discriminant(self).hash(state);

        match self {
            Bool(val) => val.hash(state),
            Int(val) => val.hash(state),
            Float(val) => val.hash(state),
            String(val) => val.addr().hash(state),
            Function(val) => val.hash(state),
            Table(val) => val.addr().hash(state),
            Userdata(val) => val.addr().hash(state),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum InvalidTableKeyError {
    Nil,
    Nan,
}

impl InvalidTableKeyError {
    pub(crate) fn value_str(self) -> &'static str {
        match self {
            InvalidTableKeyError::Nan => "NaN",
            InvalidTableKeyError::Nil => "nil",
        }
    }
}

impl<Gc> TryFrom<Value<Gc>> for KeyValue<Gc>
where
    Gc: Types,
{
    type Error = InvalidTableKeyError;

    fn try_from(value: Value<Gc>) -> Result<Self, Self::Error> {
        let r = match value {
            Value::Bool(t) => KeyValue::Bool(t),
            Value::Int(t) => KeyValue::Int(t),
            Value::Float(t) => {
                let t = NotNan::new(t).map_err(|_| InvalidTableKeyError::Nan)?;
                KeyValue::Float(t)
            }
            Value::String(t) => KeyValue::String(t),
            Value::Function(t) => KeyValue::Function(t),
            Value::Table(t) => KeyValue::Table(t),
            Value::Userdata(t) => KeyValue::Userdata(t),
            Value::Nil => return Err(InvalidTableKeyError::Nil),
        };

        Ok(r)
    }
}

impl<Gc> From<KeyValue<Gc>> for Value<Gc>
where
    Gc: Types,
{
    fn from(value: KeyValue<Gc>) -> Self {
        match value {
            KeyValue::Bool(t) => Value::Bool(t),
            KeyValue::Int(t) => Value::Int(t),
            KeyValue::Float(t) => Value::Float(t.into_inner()),
            KeyValue::Function(t) => Value::Function(t),
            KeyValue::String(t) => Value::String(t),
            KeyValue::Table(t) => Value::Table(t),
            KeyValue::Userdata(t) => Value::Userdata(t),
        }
    }
}

pub struct LuaTable<T>(pub T);

impl<Gc, T> TryInto<LuaTable<T>> for Value<Gc>
where
    Gc: Types,
    Gc::Table: TryInto<T>,
{
    type Error = TypeMismatchOrError<<Gc::Table as TryInto<T>>::Error>;

    fn try_into(self) -> Result<LuaTable<T>, Self::Error> {
        match self {
            Value::Table(t) => t
                .try_into()
                .map(LuaTable)
                .map_err(TypeMismatchOrError::Other),
            value => {
                use super::Type;

                let err = TypeMismatchError {
                    found: value.type_(),
                    expected: Type::Table,
                };

                Err(TypeMismatchOrError::TypeMismatch(err))
            }
        }
    }
}

impl<Gc, T> From<LuaTable<T>> for Value<Gc>
where
    Gc: Types,
    Gc::Table: From<T>,
{
    fn from(value: LuaTable<T>) -> Self {
        let LuaTable(value) = value;
        Value::Table(value.into())
    }
}
