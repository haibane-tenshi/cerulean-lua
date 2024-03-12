use std::fmt::Display;
use std::hash::Hash;
use std::marker::PhantomData;

use gc::{Gc, GcCell, Root, RootCell, Trace};

use super::callable::{RustCallable, RustClosureRef};
use super::string::PossiblyUtf8Vec;
use super::userdata::FullUserdata;
use super::{KeyValue, Value};
use crate::gc::LuaPtr;
use crate::runtime::{Closure, Interned};

pub trait Types: Sized {
    type String;
    type LuaCallable;
    type RustCallable;
    type Table;
    type FullUserdata;
}

pub struct Strong<Ty, Conv>(PhantomData<(Ty, Conv)>);

impl<Ty, Conv> Types for Strong<Ty, Conv>
where
    Ty: CoreTypes,
{
    type String = LuaPtr<Root<Interned<<Ty as CoreTypes>::String>>>;
    type LuaCallable = LuaPtr<RootCell<Closure<Ty, Conv>>>;
    type RustCallable = RustCallable<Ty, Conv, LuaPtr<RootCell<<Ty as CoreTypes>::RustClosure>>>;
    type Table = LuaPtr<RootCell<<Ty as CoreTypes>::Table>>;
    type FullUserdata = LuaPtr<RootCell<<Ty as CoreTypes>::FullUserdata>>;
}

pub struct Weak<Ty, Conv>(PhantomData<(Ty, Conv)>);

impl<Ty, Conv> Types for Weak<Ty, Conv>
where
    Ty: CoreTypes,
{
    type String = LuaPtr<Gc<Interned<<Ty as CoreTypes>::String>>>;
    type LuaCallable = LuaPtr<GcCell<Closure<Ty, Conv>>>;
    type RustCallable = RustCallable<Ty, Conv, LuaPtr<GcCell<<Ty as CoreTypes>::RustClosure>>>;
    type Table = LuaPtr<GcCell<<Ty as CoreTypes>::Table>>;
    type FullUserdata = LuaPtr<GcCell<<Ty as CoreTypes>::FullUserdata>>;
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
    type Table: Default + Trace + Metatable<LuaPtr<GcCell<Self::Table>>>;
    type FullUserdata: Trace + Metatable<GcCell<Self::Table>>;
}

pub struct DefaultTypes<Conv>(PhantomData<Conv>);

impl<Conv> CoreTypes for DefaultTypes<Conv>
where
    Conv: 'static,
{
    type String = PossiblyUtf8Vec;
    type RustClosure = RustClosureRef<Self, Conv>;
    type Table = super::Table<Weak<Self, Conv>>;
    type FullUserdata = Box<dyn FullUserdata<Self, Conv>>;
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
