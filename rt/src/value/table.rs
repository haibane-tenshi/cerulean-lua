use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use gc::Trace;
use ordered_float::NotNan;

use super::callable::Callable;
use super::{
    CoreTypes, Meta, Metatable, TableIndex, TypeMismatchError, TypeMismatchOrError, Types, Value,
};

pub struct Table<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
{
    data: HashMap<KeyValue<Rf, Ty>, Value<Rf, Ty>>,
    metatable: Option<Meta<Ty>>,
}

impl<Rf, Ty> TableIndex<Rf, Ty> for Table<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    KeyValue<Rf, Ty>: Hash + Eq,
    Value<Rf, Ty>: Clone,
{
    fn get(&self, key: &KeyValue<Rf, Ty>) -> Value<Rf, Ty> {
        self.data.get(key).cloned().unwrap_or_default()
    }

    fn set(&mut self, key: KeyValue<Rf, Ty>, value: Value<Rf, Ty>) {
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

    fn contains_key(&self, key: &KeyValue<Rf, Ty>) -> bool {
        self.data.contains_key(key)
    }
}

impl<Rf, Ty> Table<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    KeyValue<Rf, Ty>: Hash + Eq,
{
    pub fn get_ref<'s>(&'s self, key: &KeyValue<Rf, Ty>) -> Option<&'s Value<Rf, Ty>> {
        self.data.get(key)
    }

    pub fn border(&self) -> i64 {
        // Inefficient, but will get fixed when table layout is improved.
        (0..)
            .find(|&i| !self.data.contains_key(&KeyValue::Int(i + 1)))
            .unwrap_or(i64::MAX)
    }
}

impl<Rf, Ty> Metatable<Meta<Ty>> for Table<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
{
    fn metatable(&self) -> Option<&Meta<Ty>> {
        self.metatable.as_ref()
    }

    fn set_metatable(&mut self, mt: Option<Meta<Ty>>) -> Option<Meta<Ty>> {
        std::mem::replace(&mut self.metatable, mt)
    }
}

impl<Rf, Ty> Trace for Table<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::String<Ty::String>: Trace,
    Callable<Rf, Ty>: Trace,
    Rf::Table<Ty::Table>: Trace,
    Rf::FullUserdata<Ty::FullUserdata>: Trace,
    Meta<Ty>: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        for (key, value) in self.data.iter() {
            key.trace(collector);
            value.trace(collector);
        }

        self.metatable.trace(collector);
    }
}

impl<Rf, Ty> Debug for Table<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    KeyValue<Rf, Ty>: Debug,
    Value<Rf, Ty>: Debug,
    Meta<Ty>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Table")
            .field("data", &self.data)
            .field("metatable", &self.metatable)
            .finish()
    }
}

impl<Rf, Ty> Clone for Table<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    KeyValue<Rf, Ty>: Clone,
    Value<Rf, Ty>: Clone,
    Meta<Ty>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            metatable: self.metatable,
        }
    }
}

impl<Rf, Ty> Default for Table<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
            metatable: Default::default(),
        }
    }
}

pub enum KeyValue<Rf: Types, Ty: CoreTypes> {
    Bool(bool),
    Int(i64),
    Float(NotNan<f64>),
    String(Rf::String<Ty::String>),
    Function(Callable<Rf, Ty>),
    Table(Rf::Table<Ty::Table>),
    Userdata(Rf::FullUserdata<Ty::FullUserdata>),
}

impl<Rf, Ty> Trace for KeyValue<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::String<Ty::String>: Trace,
    Callable<Rf, Ty>: Trace,
    Rf::Table<Ty::Table>: Trace,
    Rf::FullUserdata<Ty::FullUserdata>: Trace,
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

impl<Rf, Ty> Debug for KeyValue<Rf, Ty>
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

impl<Rf, Ty> Clone for KeyValue<Rf, Ty>
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

impl<Rf, Ty> Copy for KeyValue<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::String<Ty::String>: Copy,
    Callable<Rf, Ty>: Copy,
    Rf::Table<Ty::Table>: Copy,
    Rf::FullUserdata<Ty::FullUserdata>: Copy,
{
}

impl<Rf, Ty> PartialEq for KeyValue<Rf, Ty>
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
            _ => false,
        }
    }
}

impl<Rf, Ty> Eq for KeyValue<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::String<Ty::String>: Eq,
    Callable<Rf, Ty>: Eq,
    Rf::Table<Ty::Table>: Eq,
    Rf::FullUserdata<Ty::FullUserdata>: Eq,
{
}

impl<Rf, Ty> Hash for KeyValue<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::String<Ty::String>: Hash,
    Callable<Rf, Ty>: Hash,
    Rf::Table<Ty::Table>: Hash,
    Rf::FullUserdata<Ty::FullUserdata>: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use KeyValue::*;

        core::mem::discriminant(self).hash(state);

        match self {
            Bool(val) => val.hash(state),
            Int(val) => val.hash(state),
            Float(val) => val.hash(state),
            String(val) => val.hash(state),
            Function(val) => val.hash(state),
            Table(val) => val.hash(state),
            Userdata(val) => val.hash(state),
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

impl<Rf, Ty> TryFrom<Value<Rf, Ty>> for KeyValue<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
{
    type Error = InvalidTableKeyError;

    fn try_from(value: Value<Rf, Ty>) -> Result<Self, Self::Error> {
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

impl<Rf, Ty> From<KeyValue<Rf, Ty>> for Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
{
    fn from(value: KeyValue<Rf, Ty>) -> Self {
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

impl<Rf, Ty, T> TryInto<LuaTable<T>> for Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::Table<Ty::Table>: TryInto<T>,
{
    type Error = TypeMismatchOrError<<Rf::Table<Ty::Table> as TryInto<T>>::Error>;

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

impl<Rf, Ty, T> From<LuaTable<T>> for Value<Rf, Ty>
where
    Rf: Types,
    Ty: CoreTypes,
    Rf::Table<Ty::Table>: From<T>,
{
    fn from(value: LuaTable<T>) -> Self {
        let LuaTable(value) = value;
        Value::Table(value.into())
    }
}
