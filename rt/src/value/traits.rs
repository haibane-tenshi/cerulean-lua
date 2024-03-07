use std::fmt::Display;
use std::hash::Hash;
use std::marker::PhantomData;

use gc::{Gc, GcCell, Root, RootCell};

use super::callable::{RustCallable, RustClosureRef};
use super::string::PossiblyUtf8Vec;
use super::userdata::FullUserdata;
use super::{KeyValue, Value};
use crate::runtime::{Closure, Interned};

use gc::Trace;

pub trait Types: Sized {
    type String;
    type LuaCallable;
    type RustCallable;
    type Table;
    type FullUserdata;
}

pub struct Strong<Ty>(PhantomData<(Ty,)>);

impl<Ty> Types for Strong<Ty>
where
    Ty: CoreTypes,
{
    type String = Root<Interned<<Ty as CoreTypes>::String>>;
    type LuaCallable = RootCell<Closure<Ty>>;
    type RustCallable = RustCallable<Ty, RootCell<<Ty as CoreTypes>::RustClosure>>;
    type Table = RootCell<<Ty as CoreTypes>::Table>;
    type FullUserdata = RootCell<<Ty as CoreTypes>::FullUserdata>;
}

pub struct Weak<Ty>(PhantomData<(Ty,)>);

impl<Ty> Types for Weak<Ty>
where
    Ty: CoreTypes,
{
    type String = Gc<Interned<<Ty as CoreTypes>::String>>;
    type LuaCallable = GcCell<Closure<Ty>>;
    type RustCallable = RustCallable<Ty, GcCell<<Ty as CoreTypes>::RustClosure>>;
    type Table = GcCell<<Ty as CoreTypes>::Table>;
    type FullUserdata = GcCell<<Ty as CoreTypes>::FullUserdata>;
}

pub trait CoreTypes: Sized + 'static {
    type String: Trace
        + Concat
        + Len
        + Clone
        + Ord
        + Hash
        + Display
        + From<String>
        + From<&'static str>
        + AsRef<[u8]>;
    type RustClosure: Clone + PartialEq + Trace;
    type Table: Default + Trace + TableIndex<Weak<Self>> + Metatable<GcCell<Self::Table>>;
    type FullUserdata: Trace + Metatable<GcCell<Self::Table>>;
}

pub struct DefaultTypes;

impl CoreTypes for DefaultTypes {
    type String = PossiblyUtf8Vec;
    type RustClosure = RustClosureRef<Self>;
    type Table = super::Table<Weak<Self>>;
    type FullUserdata = Box<dyn FullUserdata<Self>>;
}

pub trait Metatable<TableRef> {
    fn metatable(&self) -> Option<TableRef>;
    fn set_metatable(&mut self, mt: Option<TableRef>) -> Option<TableRef>;
}

pub trait TableIndex<Ty: Types> {
    fn get(&self, key: &KeyValue<Ty>) -> Value<Ty>;
    fn set(&mut self, key: KeyValue<Ty>, value: Value<Ty>);
    fn border(&self) -> i64;
    fn contains_key(&self, key: &KeyValue<Ty>) -> bool {
        !matches!(self.get(key), Value::Nil)
    }
}

pub trait Len {
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Len for String {
    fn len(&self) -> usize {
        String::len(self)
    }
}

impl Len for Vec<u8> {
    fn len(&self) -> usize {
        Vec::len(self)
    }
}

pub trait Concat {
    fn concat(&mut self, other: &Self);
}

impl Concat for String {
    fn concat(&mut self, other: &Self) {
        self.push_str(other);
    }
}

impl Concat for Vec<u8> {
    fn concat(&mut self, other: &Self) {
        self.extend(other)
    }
}
