use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use ordered_float::NotNan;

use super::callable::Callable;
use super::{Metatable, TableIndex, TypeMismatchError, TypeMismatchOrError, TypeProvider, Value};
use crate::error::DroppedOrBorrowedError;

pub struct Table<Gc: TypeProvider> {
    data: HashMap<KeyValue<Gc>, Value<Gc>>,
    metatable: Option<Gc::TableRef>,
}

impl<Gc> TableIndex<Gc> for Table<Gc>
where
    Gc: TypeProvider,
    KeyValue<Gc>: Hash + Eq,
{
    fn get(&self, key: &KeyValue<Gc>) -> Value<Gc> {
        self.data.get(key).cloned().unwrap_or_default()
    }

    fn set(&mut self, key: KeyValue<Gc>, value: Value<Gc>) {
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

    fn contains_key(&self, key: &KeyValue<Gc>) -> bool {
        self.data.contains_key(key)
    }
}

impl<Gc> Table<Gc>
where
    Gc: TypeProvider,
    KeyValue<Gc>: Hash + Eq,
    Value<Gc>: Clone,
{
    pub fn get(&self, key: &KeyValue<Gc>) -> Value<Gc> {
        self.data.get(key).cloned().unwrap_or_default()
    }
}

impl<Gc> Table<Gc>
where
    Gc: TypeProvider,
    KeyValue<Gc>: Hash + Eq,
{
    pub fn get_ref<'s>(&'s self, key: &KeyValue<Gc>) -> Option<&'s Value<Gc>> {
        self.data.get(key)
    }

    pub fn set(&mut self, key: KeyValue<Gc>, value: Value<Gc>) {
        match value {
            Value::Nil => {
                self.data.remove(&key);
            }
            value => {
                self.data.insert(key, value);
            }
        }
    }

    pub fn contains_key(&self, key: &KeyValue<Gc>) -> bool {
        self.data.contains_key(key)
    }

    pub fn border(&self) -> i64 {
        // Inefficient, but will get fixed when table layout is improved.
        (0..)
            .find(|&i| !self.data.contains_key(&KeyValue::Int(i + 1)))
            .unwrap_or(i64::MAX)
    }
}
impl<Gc> Table<Gc>
where
    Gc: TypeProvider,
    Gc::TableRef: Clone,
{
    pub fn metatable(&self) -> Option<Gc::TableRef> {
        self.metatable.clone()
    }
}

impl<Gc> Table<Gc>
where
    Gc: TypeProvider,
{
    pub fn set_metatable(&mut self, metatable: Option<Gc::TableRef>) -> Option<Gc::TableRef> {
        std::mem::replace(&mut self.metatable, metatable)
    }
}

impl<Gc> Metatable<Gc::TableRef> for Table<Gc>
where
    Gc: TypeProvider,
    Gc::TableRef: Clone,
{
    fn metatable(&self) -> Option<Gc::TableRef> {
        Table::metatable(self)
    }

    fn set_metatable(&mut self, mt: Option<Gc::TableRef>) -> Option<Gc::TableRef> {
        Table::set_metatable(self, mt)
    }
}

impl<Gc> Debug for Table<Gc>
where
    Gc: TypeProvider,
    KeyValue<Gc>: Debug,
    Value<Gc>: Debug,
    Gc::TableRef: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Table")
            .field("data", &self.data)
            .field("metatable", &self.metatable)
            .finish()
    }
}

impl<Gc> Clone for Table<Gc>
where
    Gc: TypeProvider,
    KeyValue<Gc>: Clone,
    Value<Gc>: Clone,
    Gc::TableRef: Clone,
{
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            metatable: self.metatable.clone(),
        }
    }
}

impl<Gc> PartialEq for Table<Gc>
where
    Gc: TypeProvider,
    KeyValue<Gc>: Hash + Eq,
    Value<Gc>: PartialEq,
    Gc::TableRef: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data && self.metatable == other.metatable
    }
}

impl<Gc> Default for Table<Gc>
where
    Gc: TypeProvider,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
            metatable: Default::default(),
        }
    }
}

pub enum KeyValue<Gc: TypeProvider> {
    Bool(bool),
    Int(i64),
    Float(NotNan<f64>),
    String(Gc::StringRef),
    Function(Callable<Gc::RustCallable>),
    Table(Gc::TableRef),
    Userdata(Gc::FullUserdataRef),
}

impl<Gc> Debug for KeyValue<Gc>
where
    Gc: TypeProvider,
    Gc::StringRef: Debug,
    Gc::RustCallable: Debug,
    Gc::TableRef: Debug,
    Gc::FullUserdataRef: Debug,
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

impl<Gc> Clone for KeyValue<Gc>
where
    Gc: TypeProvider,
    Gc::String: Clone,
    Gc::RustCallable: Clone,
    Gc::TableRef: Clone,
    Gc::FullUserdataRef: Clone,
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

impl<Gc> PartialEq for KeyValue<Gc>
where
    Gc: TypeProvider,
    Gc::String: PartialEq,
    Gc::RustCallable: PartialEq,
    Gc::TableRef: PartialEq,
    Gc::FullUserdataRef: PartialEq,
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

impl<Gc> Eq for KeyValue<Gc>
where
    Gc: TypeProvider,
    Gc::String: Eq,
    Gc::RustCallable: Eq,
    Gc::TableRef: Eq,
    Gc::FullUserdataRef: Eq,
{
}

impl<Gc> Hash for KeyValue<Gc>
where
    Gc: TypeProvider,
    Gc::StringRef: Hash,
    Gc::RustCallable: Hash,
    Gc::TableRef: Hash,
    Gc::FullUserdataRef: Hash,
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

impl<Gc> TryFrom<Value<Gc>> for KeyValue<Gc>
where
    Gc: TypeProvider,
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
    Gc: TypeProvider,
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

pub struct TableRef<Gc: TypeProvider<TableRef = Self>>(Rc<RefCell<Table<Gc>>>);

impl<Gc: TypeProvider<TableRef = Self>> TableRef<Gc> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn borrow(&self) -> Result<Ref<Table<Gc>>, std::cell::BorrowError> {
        self.0.try_borrow()
    }

    pub fn borrow_mut(&self) -> Result<RefMut<Table<Gc>>, std::cell::BorrowMutError> {
        self.0.try_borrow_mut()
    }
}

impl<Gc> super::Borrow<Table<Gc>> for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
{
    fn with_ref<R>(&self, f: impl FnOnce(&Table<Gc>) -> R) -> Result<R, DroppedOrBorrowedError> {
        self.0.with_ref(f)
    }

    fn with_mut<R>(
        &self,
        f: impl FnOnce(&mut Table<Gc>) -> R,
    ) -> Result<R, DroppedOrBorrowedError> {
        self.0.with_mut(f)
    }
}

impl<Gc> Debug for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
    Gc::StringRef: Debug,
    Gc::RustCallable: Debug,
    Gc::FullUserdataRef: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug = f.debug_struct("TableRef");
        debug.field("addr", &Rc::as_ptr(&self.0));

        match self.0.try_borrow() {
            Ok(table) => debug.field("table", &table),
            Err(_) => debug.field("table", &"<borrowed>"),
        };

        debug.finish()
    }
}

impl<Gc> Clone for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Gc> Default for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
{
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Gc> PartialEq for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
{
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<Gc> Eq for TableRef<Gc> where Gc: TypeProvider<TableRef = Self> {}

impl<Gc> Hash for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
    }
}

impl<Gc> From<Table<Gc>> for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
{
    fn from(value: Table<Gc>) -> Self {
        TableRef(Rc::new(RefCell::new(value)))
    }
}

impl<Gc> TryFrom<Value<Gc>> for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Gc>) -> Result<Self, Self::Error> {
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

impl<Gc> From<TableRef<Gc>> for Value<Gc>
where
    Gc: TypeProvider<TableRef = TableRef<Gc>>,
{
    fn from(value: TableRef<Gc>) -> Self {
        Value::Table(value)
    }
}

pub struct LuaTable<T>(pub T);

impl<Gc, T> TryInto<LuaTable<T>> for Value<Gc>
where
    Gc: TypeProvider,
    Gc::TableRef: TryInto<T>,
{
    type Error = TypeMismatchOrError<<Gc::TableRef as TryInto<T>>::Error>;

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
    Gc: TypeProvider,
    Gc::TableRef: From<T>,
{
    fn from(value: LuaTable<T>) -> Self {
        let LuaTable(value) = value;
        Value::Table(value.into())
    }
}
