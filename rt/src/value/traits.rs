use std::marker::PhantomData;

use super::callable::{RustCallable, RustClosureRef};
use super::string::PossiblyUtf8Vec;
use super::userdata::FullUserdata;
use super::{KeyValue, Value};
use crate::gc::{
    GcFullUserdata, GcLuaClosure, GcRustClosure, GcTable, RootFullUserdata, RootLuaClosure,
    RootRustClosure, RootTable, StringRef,
};
use crate::runtime::Closure;

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
    type String = StringRef<<Ty as CoreTypes>::String>;
    type LuaCallable = RootLuaClosure<Closure<Ty>>;
    type RustCallable = RustCallable<Ty, RootRustClosure<<Ty as CoreTypes>::RustClosure>>;
    type Table = RootTable<<Ty as CoreTypes>::Table>;
    type FullUserdata = RootFullUserdata<<Ty as CoreTypes>::FullUserdata>;
}

pub struct Weak<Ty>(PhantomData<(Ty,)>);

impl<Ty> Types for Weak<Ty>
where
    Ty: CoreTypes,
{
    type String = StringRef<<Ty as CoreTypes>::String>;
    type LuaCallable = GcLuaClosure<Closure<Ty>>;
    type RustCallable = RustCallable<Ty, GcRustClosure<<Ty as CoreTypes>::RustClosure>>;
    type Table = GcTable<<Ty as CoreTypes>::Table>;
    type FullUserdata = GcFullUserdata<<Ty as CoreTypes>::FullUserdata>;
}

pub trait CoreTypes: Sized + 'static {
    type String: Trace + Concat + Len + Clone + PartialOrd + From<String> + From<&'static str>;
    type RustClosure: Clone + PartialEq + Trace;
    type Table: Default + Trace + TableIndex<Weak<Self>> + Metatable<GcTable<Self::Table>>;
    type FullUserdata: Trace + Metatable<GcTable<Self::Table>>;
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
