use std::collections::BTreeMap;
use std::fmt::Debug;
use std::hash::Hash;

use gc::Trace;
use ordered_float::NotNan;

use super::callable::Callable;
use super::traits::Len;
use super::{Int, Meta, Metatable, Refs, Strong, TableIndex, Type, Types, Value, Weak};
use crate::error::{AlreadyDroppedError, InvalidKeyError};
use crate::gc::Heap;

pub struct Table<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    data: BTreeMap<KeyValue<Rf, Ty>, Value<Rf, Ty>>,
    metatable: Option<Meta<Ty>>,
}

impl<Rf, Ty> TableIndex<Rf, Ty> for Table<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    KeyValue<Rf, Ty>: Ord,
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

    fn first_key(&self) -> Option<&KeyValue<Rf, Ty>> {
        self.data.first_key_value().map(|(key, _)| key)
    }

    fn next_key(&self, key: &KeyValue<Rf, Ty>) -> Option<&KeyValue<Rf, Ty>> {
        use std::ops::Bound;

        let (key, _) = self
            .data
            .range((Bound::Excluded(key), Bound::Unbounded))
            .next()?;
        Some(key)
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
    Rf: Refs,
    Ty: Types,
    KeyValue<Rf, Ty>: Ord,
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

impl<Rf, Ty> Len for Table<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    KeyValue<Rf, Ty>: Ord,
{
    fn len(&self) -> Int {
        Int(self.border())
    }
}

impl<Rf, Ty> Metatable<Meta<Ty>> for Table<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
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
    Rf: Refs,
    Ty: Types,
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
    Rf: Refs,
    Ty: Types,
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
    Rf: Refs,
    Ty: Types,
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
    Rf: Refs,
    Ty: Types,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
            metatable: Default::default(),
        }
    }
}

pub enum KeyValue<Rf: Refs, Ty: Types> {
    Bool(bool),
    Int(i64),
    Float(NotNan<f64>),
    String(Rf::String<Ty::String>),
    Function(Callable<Rf, Ty>),
    Table(Rf::Table<Ty::Table>),
    Userdata(Rf::FullUserdata<Ty::FullUserdata>),
}

impl<Rf, Ty> KeyValue<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    pub fn type_(&self) -> Type {
        match self {
            KeyValue::Bool(_) => Type::Bool,
            KeyValue::Int(_) => Type::Int,
            KeyValue::Float(_) => Type::Float,
            KeyValue::String(_) => Type::String,
            KeyValue::Function(_) => Type::Function,
            KeyValue::Table(_) => Type::Table,
            KeyValue::Userdata(_) => Type::Userdata,
        }
    }
}

impl<Ty> KeyValue<Strong, Ty>
where
    Ty: Types,
{
    pub fn downgrade(&self) -> KeyValue<Weak, Ty> {
        match self {
            KeyValue::Bool(t) => KeyValue::Bool(*t),
            KeyValue::Int(t) => KeyValue::Int(*t),
            KeyValue::Float(not_nan) => KeyValue::Float(*not_nan),
            KeyValue::String(t) => KeyValue::String(t.downgrade()),
            KeyValue::Function(callable) => KeyValue::Function(callable.downgrade()),
            KeyValue::Table(t) => KeyValue::Table(t.downgrade()),
            KeyValue::Userdata(t) => KeyValue::Userdata(t.downgrade()),
        }
    }
}

impl<Ty> KeyValue<Weak, Ty>
where
    Ty: Types,
{
    pub fn upgrade(self, heap: &Heap<Ty>) -> Option<KeyValue<Strong, Ty>> {
        self.try_upgrade(heap).ok()
    }

    pub fn try_upgrade(self, heap: &Heap<Ty>) -> Result<KeyValue<Strong, Ty>, AlreadyDroppedError> {
        let r = match self {
            KeyValue::Bool(t) => KeyValue::Bool(t),
            KeyValue::Int(t) => KeyValue::Int(t),
            KeyValue::Float(not_nan) => KeyValue::Float(not_nan),
            KeyValue::String(t) => KeyValue::String(t.try_upgrade(heap)?),
            KeyValue::Function(callable) => KeyValue::Function(callable.try_upgrade(heap)?),
            KeyValue::Table(t) => KeyValue::Table(t.try_upgrade(heap)?),
            KeyValue::Userdata(t) => KeyValue::Userdata(t.try_upgrade(heap)?),
        };

        Ok(r)
    }
}

impl<Rf, Ty> Trace for KeyValue<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
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
    Rf: Refs,
    Ty: Types,
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
    Rf: Refs,
    Ty: Types,
    Rf::String<Ty::String>: Copy,
    Callable<Rf, Ty>: Copy,
    Rf::Table<Ty::Table>: Copy,
    Rf::FullUserdata<Ty::FullUserdata>: Copy,
{
}

impl<Rf, Ty> PartialEq for KeyValue<Rf, Ty>
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
            _ => false,
        }
    }
}

impl<Rf, Ty> Eq for KeyValue<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::String<Ty::String>: Eq,
    Callable<Rf, Ty>: Eq,
    Rf::Table<Ty::Table>: Eq,
    Rf::FullUserdata<Ty::FullUserdata>: Eq,
{
}

impl<Rf, Ty> PartialOrd for KeyValue<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::String<Ty::String>: PartialOrd,
    Callable<Rf, Ty>: PartialOrd,
    Rf::Table<Ty::Table>: PartialOrd,
    Rf::FullUserdata<Ty::FullUserdata>: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use KeyValue::*;

        match (self, other) {
            (Bool(lhs), Bool(rhs)) => lhs.partial_cmp(rhs),
            (Int(lhs), Int(rhs)) => lhs.partial_cmp(rhs),
            (Float(lhs), Float(rhs)) => lhs.partial_cmp(rhs),
            (String(lhs), String(rhs)) => lhs.partial_cmp(rhs),
            (Function(lhs), Function(rhs)) => lhs.partial_cmp(rhs),
            (Table(lhs), Table(rhs)) => lhs.partial_cmp(rhs),
            (Userdata(lhs), Userdata(rhs)) => lhs.partial_cmp(rhs),
            (lhs, rhs) => {
                let lhs = lhs.type_();
                let rhs = rhs.type_();

                let res = lhs.cmp(rhs);
                debug_assert_ne!(res, std::cmp::Ordering::Equal);
                Some(res)
            }
        }
    }
}

impl<Rf, Ty> Ord for KeyValue<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::String<Ty::String>: Ord,
    Callable<Rf, Ty>: Ord,
    Rf::Table<Ty::Table>: Ord,
    Rf::FullUserdata<Ty::FullUserdata>: Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use KeyValue::*;

        match (self, other) {
            (Bool(lhs), Bool(rhs)) => lhs.cmp(rhs),
            (Int(lhs), Int(rhs)) => lhs.cmp(rhs),
            (Float(lhs), Float(rhs)) => lhs.cmp(rhs),
            (String(lhs), String(rhs)) => lhs.cmp(rhs),
            (Function(lhs), Function(rhs)) => lhs.cmp(rhs),
            (Table(lhs), Table(rhs)) => lhs.cmp(rhs),
            (Userdata(lhs), Userdata(rhs)) => lhs.cmp(rhs),
            (lhs, rhs) => {
                let lhs = lhs.type_();
                let rhs = rhs.type_();

                let res = lhs.cmp(rhs);
                debug_assert_ne!(res, std::cmp::Ordering::Equal);
                res
            }
        }
    }
}

impl<Rf, Ty> Hash for KeyValue<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
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

impl<Rf, Ty> TryFrom<Value<Rf, Ty>> for KeyValue<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    type Error = InvalidKeyError;

    fn try_from(value: Value<Rf, Ty>) -> Result<Self, Self::Error> {
        let r = match value {
            Value::Bool(t) => KeyValue::Bool(t),
            Value::Int(t) => KeyValue::Int(t),
            Value::Float(t) => {
                let t = NotNan::new(t).map_err(|_| InvalidKeyError::Nan)?;
                KeyValue::Float(t)
            }
            Value::String(t) => KeyValue::String(t),
            Value::Function(t) => KeyValue::Function(t),
            Value::Table(t) => KeyValue::Table(t),
            Value::Userdata(t) => KeyValue::Userdata(t),
            Value::Nil => return Err(InvalidKeyError::Nil),
        };

        Ok(r)
    }
}

impl<Rf, Ty> From<KeyValue<Rf, Ty>> for Value<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
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
