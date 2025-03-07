use std::collections::BTreeMap;
use std::fmt::Debug;
use std::hash::Hash;

use gc::Trace;
use ordered_float::NotNan;

use super::callable::Callable;
use super::ops::Len;
use super::{Int, Meta, Metatable, Refs, Strong, Type, Types, Value, Weak};
use crate::error::{AlreadyDroppedError, InvalidKeyError};
use crate::gc::Heap;

pub trait TableIndex<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    fn get(&self, key: &Key<Rf, Ty>) -> Value<Rf, Ty>;
    fn set(&mut self, key: Key<Rf, Ty>, value: Value<Rf, Ty>);
    fn first_key(&self) -> Option<&Key<Rf, Ty>>;
    fn next_key(&self, key: &Key<Rf, Ty>) -> Option<&Key<Rf, Ty>>;
    fn border(&self) -> i64;
    fn contains_key(&self, key: &Key<Rf, Ty>) -> bool {
        !matches!(self.get(key), Value::Nil)
    }
}

pub struct Table<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    data: BTreeMap<Key<Rf, Ty>, Value<Rf, Ty>>,
    metatable: Option<Meta<Ty>>,
}

impl<Rf, Ty> TableIndex<Rf, Ty> for Table<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Key<Rf, Ty>: Ord,
    Value<Rf, Ty>: Clone,
{
    fn get(&self, key: &Key<Rf, Ty>) -> Value<Rf, Ty> {
        self.data.get(key).cloned().unwrap_or_default()
    }

    fn set(&mut self, key: Key<Rf, Ty>, value: Value<Rf, Ty>) {
        match value {
            Value::Nil => {
                self.data.remove(&key);
            }
            value => {
                self.data.insert(key, value);
            }
        }
    }

    fn first_key(&self) -> Option<&Key<Rf, Ty>> {
        self.data.first_key_value().map(|(key, _)| key)
    }

    fn next_key(&self, key: &Key<Rf, Ty>) -> Option<&Key<Rf, Ty>> {
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

    fn contains_key(&self, key: &Key<Rf, Ty>) -> bool {
        self.data.contains_key(key)
    }
}

impl<Rf, Ty> Table<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Key<Rf, Ty>: Ord,
{
    pub fn get_ref<'s>(&'s self, key: &Key<Rf, Ty>) -> Option<&'s Value<Rf, Ty>> {
        self.data.get(key)
    }

    pub fn border(&self) -> i64 {
        // Inefficient, but will get fixed when table layout is improved.
        (0..)
            .find(|&i| !self.data.contains_key(&Key::Int(i + 1)))
            .unwrap_or(i64::MAX)
    }
}

impl<Rf, Ty> Len for Table<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Key<Rf, Ty>: Ord,
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
    Key<Rf, Ty>: Debug,
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
    Key<Rf, Ty>: Clone,
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

pub enum Key<Rf: Refs, Ty: Types> {
    Bool(bool),
    Int(i64),
    Float(NotNan<f64>),
    String(Rf::String<Ty::String>),
    Function(Callable<Rf, Ty>),
    Table(Rf::Table<Ty::Table>),
    Userdata(Rf::FullUserdata<Ty::FullUserdata>),
}

impl<Rf, Ty> Key<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    pub fn type_(&self) -> Type {
        match self {
            Key::Bool(_) => Type::Bool,
            Key::Int(_) => Type::Int,
            Key::Float(_) => Type::Float,
            Key::String(_) => Type::String,
            Key::Function(_) => Type::Function,
            Key::Table(_) => Type::Table,
            Key::Userdata(_) => Type::Userdata,
        }
    }
}

impl<Ty> Key<Strong, Ty>
where
    Ty: Types,
{
    pub fn downgrade(&self) -> Key<Weak, Ty> {
        use crate::gc::Downgrade;

        match self {
            Key::Bool(t) => Key::Bool(*t),
            Key::Int(t) => Key::Int(*t),
            Key::Float(not_nan) => Key::Float(*not_nan),
            Key::String(t) => Key::String(t.downgrade()),
            Key::Function(callable) => Key::Function(callable.downgrade()),
            Key::Table(t) => Key::Table(t.downgrade()),
            Key::Userdata(t) => Key::Userdata(t.downgrade()),
        }
    }
}

impl<Ty> Key<Weak, Ty>
where
    Ty: Types,
{
    pub fn upgrade(self, heap: &Heap<Ty>) -> Option<Key<Strong, Ty>> {
        self.try_upgrade(heap).ok()
    }

    pub fn try_upgrade(self, heap: &Heap<Ty>) -> Result<Key<Strong, Ty>, AlreadyDroppedError> {
        use crate::gc::Upgrade;

        let r = match self {
            Key::Bool(t) => Key::Bool(t),
            Key::Int(t) => Key::Int(t),
            Key::Float(not_nan) => Key::Float(not_nan),
            Key::String(t) => Key::String(t.try_upgrade(heap)?),
            Key::Function(callable) => Key::Function(callable.try_upgrade(heap)?),
            Key::Table(t) => Key::Table(t.try_upgrade(heap)?),
            Key::Userdata(t) => Key::Userdata(t.try_upgrade(heap)?),
        };

        Ok(r)
    }
}

impl<Rf, Ty> Trace for Key<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::String<Ty::String>: Trace,
    Callable<Rf, Ty>: Trace,
    Rf::Table<Ty::Table>: Trace,
    Rf::FullUserdata<Ty::FullUserdata>: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        use Key::*;

        match self {
            Bool(_) | Int(_) | Float(_) => (),
            String(t) => t.trace(collector),
            Function(t) => t.trace(collector),
            Table(t) => t.trace(collector),
            Userdata(t) => t.trace(collector),
        }
    }
}

impl<Rf, Ty> Debug for Key<Rf, Ty>
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

impl<Rf, Ty> Clone for Key<Rf, Ty>
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

impl<Rf, Ty> Copy for Key<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::String<Ty::String>: Copy,
    Callable<Rf, Ty>: Copy,
    Rf::Table<Ty::Table>: Copy,
    Rf::FullUserdata<Ty::FullUserdata>: Copy,
{
}

impl<Rf, Ty> PartialEq for Key<Rf, Ty>
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

impl<Rf, Ty> Eq for Key<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::String<Ty::String>: Eq,
    Callable<Rf, Ty>: Eq,
    Rf::Table<Ty::Table>: Eq,
    Rf::FullUserdata<Ty::FullUserdata>: Eq,
{
}

impl<Rf, Ty> PartialOrd for Key<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::String<Ty::String>: PartialOrd,
    Callable<Rf, Ty>: PartialOrd,
    Rf::Table<Ty::Table>: PartialOrd,
    Rf::FullUserdata<Ty::FullUserdata>: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Key::*;

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

impl<Rf, Ty> Ord for Key<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::String<Ty::String>: Ord,
    Callable<Rf, Ty>: Ord,
    Rf::Table<Ty::Table>: Ord,
    Rf::FullUserdata<Ty::FullUserdata>: Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use Key::*;

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

impl<Rf, Ty> Hash for Key<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::String<Ty::String>: Hash,
    Callable<Rf, Ty>: Hash,
    Rf::Table<Ty::Table>: Hash,
    Rf::FullUserdata<Ty::FullUserdata>: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use Key::*;

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

impl<Rf, Ty> TryFrom<Value<Rf, Ty>> for Key<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    type Error = InvalidKeyError;

    fn try_from(value: Value<Rf, Ty>) -> Result<Self, Self::Error> {
        let r = match value {
            Value::Bool(t) => Key::Bool(t),
            Value::Int(t) => Key::Int(t),
            Value::Float(t) => {
                let t = NotNan::new(t).map_err(|_| InvalidKeyError::Nan)?;
                Key::Float(t)
            }
            Value::String(t) => Key::String(t),
            Value::Function(t) => Key::Function(t),
            Value::Table(t) => Key::Table(t),
            Value::Userdata(t) => Key::Userdata(t),
            Value::Nil => return Err(InvalidKeyError::Nil),
        };

        Ok(r)
    }
}

impl<Rf, Ty> From<Key<Rf, Ty>> for Value<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    fn from(value: Key<Rf, Ty>) -> Self {
        match value {
            Key::Bool(t) => Value::Bool(t),
            Key::Int(t) => Value::Int(t),
            Key::Float(t) => Value::Float(t.into_inner()),
            Key::Function(t) => Value::Function(t),
            Key::String(t) => Value::String(t),
            Key::Table(t) => Value::Table(t),
            Key::Userdata(t) => Value::Userdata(t),
        }
    }
}
