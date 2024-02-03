use std::cell::{BorrowError, BorrowMutError, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use ordered_float::NotNan;

use super::callable::Callable;
use super::{TypeMismatchError, TypeProvider, Value};

pub struct Table<Types: TypeProvider> {
    data: HashMap<KeyValue<Types>, Value<Types>>,
    metatable: Option<Types::Table>,
}

impl<Types> Table<Types>
where
    Types: TypeProvider,
    KeyValue<Types>: Hash + Eq,
    Value<Types>: Clone,
{
    pub fn get(&self, key: &KeyValue<Types>) -> Value<Types> {
        self.data.get(key).cloned().unwrap_or_default()
    }
}

impl<Types> Table<Types>
where
    Types: TypeProvider,
    KeyValue<Types>: Hash + Eq,
{
    pub fn get_ref<'s>(&'s self, key: &KeyValue<Types>) -> Option<&'s Value<Types>> {
        self.data.get(key)
    }

    pub fn set(&mut self, key: KeyValue<Types>, value: Value<Types>) {
        match value {
            Value::Nil => {
                self.data.remove(&key);
            }
            value => {
                self.data.insert(key, value);
            }
        }
    }

    pub fn contains_key(&self, key: &KeyValue<Types>) -> bool {
        self.data.contains_key(key)
    }

    pub fn border(&self) -> i64 {
        // Inefficient, but will get fixed when table layout is improved.
        (0..)
            .find(|&i| !self.data.contains_key(&KeyValue::Int(i + 1)))
            .unwrap_or(i64::MAX)
    }
}
impl<Types> Table<Types>
where
    Types: TypeProvider,
    Types::Table: Clone,
{
    pub fn metatable(&self) -> Option<Types::Table> {
        self.metatable.clone()
    }
}

impl<Types> Table<Types>
where
    Types: TypeProvider,
{
    pub fn set_metatable(&mut self, metatable: Option<Types::Table>) -> Option<Types::Table> {
        std::mem::replace(&mut self.metatable, metatable)
    }
}

impl<Types> Debug for Table<Types>
where
    Types: TypeProvider,
    KeyValue<Types>: Debug,
    Value<Types>: Debug,
    Types::Table: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Table")
            .field("data", &self.data)
            .field("metatable", &self.metatable)
            .finish()
    }
}

impl<Types> Clone for Table<Types>
where
    Types: TypeProvider,
    KeyValue<Types>: Clone,
    Value<Types>: Clone,
    Types::Table: Clone,
{
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            metatable: self.metatable.clone(),
        }
    }
}

impl<Types> PartialEq for Table<Types>
where
    Types: TypeProvider,
    KeyValue<Types>: Hash + Eq,
    Value<Types>: PartialEq,
    Types::Table: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data && self.metatable == other.metatable
    }
}

impl<Types> Default for Table<Types>
where
    Types: TypeProvider,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
            metatable: Default::default(),
        }
    }
}

pub enum KeyValue<Types: TypeProvider> {
    Bool(bool),
    Int(i64),
    Float(NotNan<f64>),
    String(Types::String),
    Function(Callable<Types::RustCallable>),
    Table(Types::Table),
    Userdata(Types::FullUserdata),
}

impl<Types> Debug for KeyValue<Types>
where
    Types: TypeProvider,
    Types::String: Debug,
    Types::RustCallable: Debug,
    Types::Table: Debug,
    Types::FullUserdata: Debug,
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

impl<Types> Clone for KeyValue<Types>
where
    Types: TypeProvider,
    Types::String: Clone,
    Types::RustCallable: Clone,
    Types::Table: Clone,
    Types::FullUserdata: Clone,
{
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

impl<Types> PartialEq for KeyValue<Types>
where
    Types: TypeProvider,
    Types::String: PartialEq,
    Types::RustCallable: PartialEq,
    Types::Table: PartialEq,
    Types::FullUserdata: PartialEq,
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

impl<Types> Eq for KeyValue<Types>
where
    Types: TypeProvider,
    Types::String: Eq,
    Types::RustCallable: Eq,
    Types::Table: Eq,
    Types::FullUserdata: Eq,
{
}

impl<Types> Hash for KeyValue<Types>
where
    Types: TypeProvider,
    Types::String: Hash,
    Types::RustCallable: Hash,
    Types::Table: Hash,
    Types::FullUserdata: Hash,
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

impl<Types> TryFrom<Value<Types>> for KeyValue<Types>
where
    Types: TypeProvider,
{
    type Error = InvalidTableKeyError;

    fn try_from(value: Value<Types>) -> Result<Self, Self::Error> {
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

impl<Types> From<KeyValue<Types>> for Value<Types>
where
    Types: TypeProvider,
{
    fn from(value: KeyValue<Types>) -> Self {
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

pub struct TableRef<Types: TypeProvider<Table = Self>>(Rc<RefCell<Table<Types>>>);

impl<Types: TypeProvider<Table = Self>> TableRef<Types> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn borrow(&self) -> Result<Ref<Table<Types>>, BorrowError> {
        self.0.try_borrow()
    }

    pub fn borrow_mut(&self) -> Result<RefMut<Table<Types>>, BorrowMutError> {
        self.0.try_borrow_mut()
    }
}

impl<Types> Debug for TableRef<Types>
where
    Types: TypeProvider<Table = Self>,
    Table<Types>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut printer = f.debug_struct("TableRef");
        printer.field("addr", &Rc::as_ptr(&self.0));

        let inner = self.0.try_borrow();
        let value: &dyn Debug = match &inner {
            Ok(table) => table,
            Err(_) => &"<borrowed>",
        };

        printer.field("table", value).finish()
    }
}

impl<Types> Clone for TableRef<Types>
where
    Types: TypeProvider<Table = Self>,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Types> Default for TableRef<Types>
where
    Types: TypeProvider<Table = Self>,
{
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Types> PartialEq for TableRef<Types>
where
    Types: TypeProvider<Table = Self>,
{
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Types> Eq for TableRef<Types> where Types: TypeProvider<Table = Self> {}

impl<Types> Hash for TableRef<Types>
where
    Types: TypeProvider<Table = Self>,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
    }
}

impl<Types> From<Table<Types>> for TableRef<Types>
where
    Types: TypeProvider<Table = Self>,
{
    fn from(value: Table<Types>) -> Self {
        TableRef(Rc::new(RefCell::new(value)))
    }
}

impl<Types> TryFrom<Value<Types>> for TableRef<Types>
where
    Types: TypeProvider<Table = Self>,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Types>) -> Result<Self, Self::Error> {
        use super::Type;

        match value {
            Value::Table(value) => Ok(value),
            value => {
                let err = TypeMismatchError {
                    expected: Type::Table,
                    found: value.type_(),
                };

                Err(err)
            }
        }
    }
}

impl<Types> From<TableRef<Types>> for Value<Types>
where
    Types: TypeProvider<Table = TableRef<Types>>,
{
    fn from(value: TableRef<Types>) -> Self {
        Value::Table(value)
    }
}
