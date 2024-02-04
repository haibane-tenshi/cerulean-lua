use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use ordered_float::NotNan;

use super::callable::Callable;
use super::{Metatable, TableIndex, TypeMismatchError, TypeMismatchOrError, TypeProvider, Value};
use crate::error::BorrowError;

pub struct Table<Types: TypeProvider> {
    data: HashMap<KeyValue<Types>, Value<Types>>,
    metatable: Option<Types::TableRef>,
}

impl<Types> TableIndex<Types> for Table<Types>
where
    Types: TypeProvider,
    KeyValue<Types>: Hash + Eq,
{
    fn get(&self, key: &KeyValue<Types>) -> Value<Types> {
        self.data.get(key).cloned().unwrap_or_default()
    }

    fn set(&mut self, key: KeyValue<Types>, value: Value<Types>) {
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

    fn contains_key(&self, key: &KeyValue<Types>) -> bool {
        self.data.contains_key(key)
    }
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
    Types::TableRef: Clone,
{
    pub fn metatable(&self) -> Option<Types::TableRef> {
        self.metatable.clone()
    }
}

impl<Types> Table<Types>
where
    Types: TypeProvider,
{
    pub fn set_metatable(&mut self, metatable: Option<Types::TableRef>) -> Option<Types::TableRef> {
        std::mem::replace(&mut self.metatable, metatable)
    }
}

impl<Types> Metatable<Types::TableRef> for Table<Types>
where
    Types: TypeProvider,
    Types::TableRef: Clone,
{
    fn metatable(&self) -> Option<Types::TableRef> {
        Table::metatable(self)
    }

    fn set_metatable(&mut self, mt: Option<Types::TableRef>) -> Option<Types::TableRef> {
        Table::set_metatable(self, mt)
    }
}

impl<Types> Debug for Table<Types>
where
    Types: TypeProvider,
    KeyValue<Types>: Debug,
    Value<Types>: Debug,
    Types::TableRef: Debug,
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
    Types::TableRef: Clone,
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
    Types::TableRef: PartialEq,
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
    Table(Types::TableRef),
    Userdata(Types::FullUserdataRef),
}

impl<Types> Debug for KeyValue<Types>
where
    Types: TypeProvider,
    Types::String: Debug,
    Types::RustCallable: Debug,
    Types::TableRef: Debug,
    Types::FullUserdataRef: Debug,
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
    Types::TableRef: Clone,
    Types::FullUserdataRef: Clone,
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
    Types::TableRef: PartialEq,
    Types::FullUserdataRef: PartialEq,
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
    Types::TableRef: Eq,
    Types::FullUserdataRef: Eq,
{
}

impl<Types> Hash for KeyValue<Types>
where
    Types: TypeProvider,
    Types::String: Hash,
    Types::RustCallable: Hash,
    Types::TableRef: Hash,
    Types::FullUserdataRef: Hash,
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

pub struct TableRef<Types: TypeProvider<TableRef = Self>>(Rc<RefCell<Table<Types>>>);

impl<Types: TypeProvider<TableRef = Self>> TableRef<Types> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn borrow(&self) -> Result<Ref<Table<Types>>, std::cell::BorrowError> {
        self.0.try_borrow()
    }

    pub fn borrow_mut(&self) -> Result<RefMut<Table<Types>>, std::cell::BorrowMutError> {
        self.0.try_borrow_mut()
    }
}

impl<Types> super::Borrow<Table<Types>> for TableRef<Types>
where
    Types: TypeProvider<TableRef = Self>,
{
    type Error = BorrowError;

    fn with_ref<R>(&self, f: impl FnOnce(&Table<Types>) -> R) -> Result<R, Self::Error> {
        self.0.with_ref(f)
    }

    fn with_mut<R>(&self, f: impl FnOnce(&mut Table<Types>) -> R) -> Result<R, Self::Error> {
        self.0.with_mut(f)
    }
}

impl<Types> Debug for TableRef<Types>
where
    Types: TypeProvider<TableRef = Self>,
    Types::String: Debug,
    Types::RustCallable: Debug,
    Types::FullUserdataRef: Debug,
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
    Types: TypeProvider<TableRef = Self>,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Types> Default for TableRef<Types>
where
    Types: TypeProvider<TableRef = Self>,
{
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Types> PartialEq for TableRef<Types>
where
    Types: TypeProvider<TableRef = Self>,
{
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Types> Eq for TableRef<Types> where Types: TypeProvider<TableRef = Self> {}

impl<Types> Hash for TableRef<Types>
where
    Types: TypeProvider<TableRef = Self>,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
    }
}

impl<Types> From<Table<Types>> for TableRef<Types>
where
    Types: TypeProvider<TableRef = Self>,
{
    fn from(value: Table<Types>) -> Self {
        TableRef(Rc::new(RefCell::new(value)))
    }
}

impl<Types> TryFrom<Value<Types>> for TableRef<Types>
where
    Types: TypeProvider<TableRef = Self>,
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
    Types: TypeProvider<TableRef = TableRef<Types>>,
{
    fn from(value: TableRef<Types>) -> Self {
        Value::Table(value)
    }
}

pub struct LuaTable<T>(pub T);

impl<Types, T> TryInto<LuaTable<T>> for Value<Types>
where
    Types: TypeProvider,
    Types::TableRef: TryInto<T>,
{
    type Error = TypeMismatchOrError<<Types::TableRef as TryInto<T>>::Error>;

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

impl<Types, T> From<LuaTable<T>> for Value<Types>
where
    Types: TypeProvider,
    Types::TableRef: From<T>,
{
    fn from(value: LuaTable<T>) -> Self {
        let LuaTable(value) = value;
        Value::Table(value.into())
    }
}
