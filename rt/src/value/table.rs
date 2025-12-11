use std::collections::BTreeMap;
use std::fmt::Debug;
use std::hash::Hash;

use gc::Trace;
use ordered_float::NotNan;

use super::callable::Callable;
use super::ops::Len;
use super::{Int, Metatable, Refs, Strong, Type, Types, Value, Weak};
use crate::error::{AlreadyDroppedError, InvalidKeyError};
use crate::gc::{Downgrade, Heap, Upgrade};

pub trait TableIndex<Rf>
where
    Rf: Refs,
{
    fn get(&self, key: &Key<Rf>) -> Value<Rf>;
    fn set(&mut self, key: Key<Rf>, value: Value<Rf>);
    fn first_key(&self) -> Option<&Key<Rf>>;
    fn next_key(&self, key: &Key<Rf>) -> Option<&Key<Rf>>;
    fn border(&self) -> i64;
    fn contains_key(&self, key: &Key<Rf>) -> bool {
        !matches!(self.get(key), Value::Nil)
    }
}

pub struct Table<Rf>
where
    Rf: Refs,
{
    data: BTreeMap<Key<Rf>, Value<Rf>>,
    metatable: Option<Rf::Meta>,
}

impl<Rf> TableIndex<Rf> for Table<Rf>
where
    Rf: Refs,
    Key<Rf>: Ord,
    Value<Rf>: Clone,
{
    fn get(&self, key: &Key<Rf>) -> Value<Rf> {
        self.data.get(key).cloned().unwrap_or_default()
    }

    fn set(&mut self, key: Key<Rf>, value: Value<Rf>) {
        match value {
            Value::Nil => {
                self.data.remove(&key);
            }
            value => {
                self.data.insert(key, value);
            }
        }
    }

    fn first_key(&self) -> Option<&Key<Rf>> {
        self.data.first_key_value().map(|(key, _)| key)
    }

    fn next_key(&self, key: &Key<Rf>) -> Option<&Key<Rf>> {
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

    fn contains_key(&self, key: &Key<Rf>) -> bool {
        self.data.contains_key(key)
    }
}

impl<Rf> Table<Rf>
where
    Rf: Refs,
    Key<Rf>: Ord,
{
    pub fn get_ref<'s>(&'s self, key: &Key<Rf>) -> Option<&'s Value<Rf>> {
        self.data.get(key)
    }

    pub fn border(&self) -> i64 {
        // Inefficient, but will get fixed when table layout is improved.
        (0..)
            .find(|&i| !self.data.contains_key(&Key::Int(i + 1)))
            .unwrap_or(i64::MAX)
    }
}

impl<Rf> Len for Table<Rf>
where
    Rf: Refs,
    Key<Rf>: Ord,
{
    fn len(&self) -> Int {
        Int(self.border())
    }
}

impl<Rf> Metatable<Rf::Meta> for Table<Rf>
where
    Rf: Refs,
{
    fn metatable(&self) -> Option<&Rf::Meta> {
        self.metatable.as_ref()
    }

    fn set_metatable(&mut self, mt: Option<Rf::Meta>) -> Option<Rf::Meta> {
        std::mem::replace(&mut self.metatable, mt)
    }
}

impl<Rf> Trace for Table<Rf>
where
    Rf: Refs,
    Rf::String: Trace,
    Callable<Rf>: Trace,
    Rf::Table: Trace,
    Rf::FullUserdata: Trace,
    Rf::Meta: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        for (key, value) in self.data.iter() {
            key.trace(collector);
            value.trace(collector);
        }

        self.metatable.trace(collector);
    }
}

impl<Rf> Debug for Table<Rf>
where
    Rf: Refs,
    Key<Rf>: Debug,
    Value<Rf>: Debug,
    Rf::Meta: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Table")
            .field("data", &self.data)
            .field("metatable", &self.metatable)
            .finish()
    }
}

impl<Rf> Clone for Table<Rf>
where
    Rf: Refs,
    Key<Rf>: Clone,
    Value<Rf>: Clone,
    Rf::Meta: Clone,
{
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            metatable: self.metatable.clone(),
        }
    }
}

impl<Rf> Default for Table<Rf>
where
    Rf: Refs,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
            metatable: Default::default(),
        }
    }
}

pub type WeakKey<Ty> = Key<Weak<Ty>>;
pub type StrongKey<Ty> = Key<Strong<Ty>>;

pub enum Key<Rf: Refs> {
    Bool(bool),
    Int(i64),
    Float(NotNan<f64>),
    String(Rf::String),
    Function(Callable<Rf>),
    Table(Rf::Table),
    Userdata(Rf::FullUserdata),
}

impl<Rf> Key<Rf>
where
    Rf: Refs,
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

impl<Ty> Downgrade for StrongKey<Ty>
where
    Ty: Types,
{
    type Output = WeakKey<Ty>;

    fn downgrade(&self) -> Self::Output {
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

impl<Ty> Upgrade<Heap<Ty>> for WeakKey<Ty>
where
    Ty: Types,
{
    type Output = StrongKey<Ty>;

    fn try_upgrade(&self, heap: &Heap<Ty>) -> Result<Self::Output, AlreadyDroppedError> {
        let r = match self {
            Key::Bool(t) => Key::Bool(*t),
            Key::Int(t) => Key::Int(*t),
            Key::Float(not_nan) => Key::Float(*not_nan),
            Key::String(t) => Key::String(t.try_upgrade(heap)?),
            Key::Function(callable) => Key::Function(callable.try_upgrade(heap)?),
            Key::Table(t) => Key::Table(t.try_upgrade(heap)?),
            Key::Userdata(t) => Key::Userdata(t.try_upgrade(heap)?),
        };

        Ok(r)
    }
}

impl<Rf> Trace for Key<Rf>
where
    Rf: Refs,
    Rf::String: Trace,
    Callable<Rf>: Trace,
    Rf::Table: Trace,
    Rf::FullUserdata: Trace,
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

impl<Rf> Debug for Key<Rf>
where
    Rf: Refs,
    Rf::String: Debug,
    Callable<Rf>: Debug,
    Rf::Table: Debug,
    Rf::FullUserdata: Debug,
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

impl<Rf> Clone for Key<Rf>
where
    Rf: Refs,
    Rf::String: Clone,
    Callable<Rf>: Clone,
    Rf::Table: Clone,
    Rf::FullUserdata: Clone,
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

impl<Rf> Copy for Key<Rf>
where
    Rf: Refs,
    Rf::String: Copy,
    Callable<Rf>: Copy,
    Rf::Table: Copy,
    Rf::FullUserdata: Copy,
{
}

impl<Rf> PartialEq for Key<Rf>
where
    Rf: Refs,
    Rf::String: PartialEq,
    Callable<Rf>: PartialEq,
    Rf::Table: PartialEq,
    Rf::FullUserdata: PartialEq,
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

impl<Rf> Eq for Key<Rf>
where
    Rf: Refs,
    Rf::String: Eq,
    Callable<Rf>: Eq,
    Rf::Table: Eq,
    Rf::FullUserdata: Eq,
{
}

impl<Rf> PartialOrd for Key<Rf>
where
    Rf: Refs,
    Rf::String: PartialOrd,
    Callable<Rf>: PartialOrd,
    Rf::Table: PartialOrd,
    Rf::FullUserdata: PartialOrd,
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

impl<Rf> Ord for Key<Rf>
where
    Rf: Refs,
    Rf::String: Ord,
    Callable<Rf>: Ord,
    Rf::Table: Ord,
    Rf::FullUserdata: Ord,
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

impl<Rf> Hash for Key<Rf>
where
    Rf: Refs,
    Rf::String: Hash,
    Callable<Rf>: Hash,
    Rf::Table: Hash,
    Rf::FullUserdata: Hash,
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

impl<Rf> TryFrom<Value<Rf>> for Key<Rf>
where
    Rf: Refs,
{
    type Error = InvalidKeyError;

    fn try_from(value: Value<Rf>) -> Result<Self, Self::Error> {
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

impl<Rf> From<Key<Rf>> for Value<Rf>
where
    Rf: Refs,
{
    fn from(value: Key<Rf>) -> Self {
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
