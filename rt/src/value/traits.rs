use std::fmt::Display;
use std::hash::Hash;

use gc::index::Allocated;
use gc::{Gc, GcCell, Interned, Root, RootCell, Trace};

use super::string::PossiblyUtf8Vec;
use super::userdata::{DefaultParams, FullUserdata};
use super::{KeyValue, Value};
use crate::ffi::DLuaFfi;
use crate::gc::LuaPtr;
use crate::runtime::Closure;

pub use gc::userdata::Metatable;

pub trait Refs: Sized + 'static {
    type String<T>;
    type LuaCallable<T>;
    type RustCallable<T>;
    type Table<T>;
    type FullUserdata<T: ?Sized>;
}

pub struct Strong;

impl Refs for Strong {
    type String<T> = LuaPtr<Root<Interned<T>>>;
    type LuaCallable<T> = LuaPtr<Root<T>>;
    type RustCallable<T> = LuaPtr<RootCell<T>>;
    type Table<T> = LuaPtr<RootCell<T>>;
    type FullUserdata<T: ?Sized> = LuaPtr<RootCell<T>>;
}

pub struct Weak;

impl Refs for Weak {
    type String<T> = LuaPtr<Gc<Interned<T>>>;
    type LuaCallable<T> = LuaPtr<Gc<T>>;
    type RustCallable<T> = LuaPtr<GcCell<T>>;
    type Table<T> = LuaPtr<GcCell<T>>;
    type FullUserdata<T: ?Sized> = LuaPtr<GcCell<T>>;
}

pub trait Types: Sized + 'static {
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
    type LuaClosure: Trace;
    type RustClosure: Trace;
    type Table: Metatable<Meta<Self>> + TableIndex<Weak, Self> + Default + Trace;
    type FullUserdata: FullUserdata<Meta<Self>, DefaultParams<Self>>
        + Allocated<Meta<Self>, DefaultParams<Self>>
        + ?Sized;
}

pub type Meta<Ty> = GcCell<<Ty as Types>::Table>;

pub struct DefaultTypes;

impl Types for DefaultTypes {
    type String = PossiblyUtf8Vec;
    type LuaClosure = Closure<Self>;
    type RustClosure = Box<dyn DLuaFfi<Self>>;
    type Table = super::Table<Weak, Self>;
    type FullUserdata = dyn FullUserdata<Meta<Self>, DefaultParams<Self>>;
}

pub trait TableIndex<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    fn get(&self, key: &KeyValue<Rf, Ty>) -> Value<Rf, Ty>;
    fn set(&mut self, key: KeyValue<Rf, Ty>, value: Value<Rf, Ty>);
    fn border(&self) -> i64;
    fn contains_key(&self, key: &KeyValue<Rf, Ty>) -> bool {
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
