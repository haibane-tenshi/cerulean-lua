//! Raw operations on Lua primitives.
//!
//! This module contains traits describing all ops permitted on Lua primitive values.
//! It mostly consist of reimports of `std` ops (such as [`Add`], [`Mul`] and others),
//! but there are also Lua-specific methods mixed in (such as [`Len`])
//!
//! Note that traits inside this module adhere to Rust's conventions.
//! From perspective of Lua they describe **raw** operations on underlying values.
//! It is used as a building block to build more complex and featureful operations.
//! For this reason traits are only implemented on selection of strongly-typed Lua values
//! ([`Int`](super::Int), [`Float`](super::Float) and others) but not on [`Value`]s.
//!
//! If you are looking for a way to operate on `Value`s or handle metamethods/coercions,
//! refer to [`builtins`](crate::builtins) module instead.
//!
//! # Provided ops
//!
//! Unmarked traits are simply reimports from Rust's `std::ops`.
//!
//! Arithmetic ops:
//!
//! * [`Neg`]
//! * [`Add`]
//! * [`AddAssign`]
//! * [`Sub`]
//! * [`SubAssign`]
//! * [`Mul`]
//! * [`MulAssign`]
//! * [`Div`]
//! * [`DivAssign`]
//! * `CheckedDiv` - to add
//! * [`FloorDiv`] - [Lua specific] division that rounds to nearest smaller integer
//! * `CheckedFloorDic` - to add?
//! * [`Rem`]
//! * [`RemAssign`]
//! * `CheckedRem` - to add
//! * [`Pow`] - [Lua specific] raise number to the power
//! * `CheckedPow` - to add?
//!
//! Bitwise ops:
//!
//! * [`BitNot`]
//! * [`BitAnd`]
//! * [`BitAndAssign`]
//! * [`BitOr`]
//! * [`BitOrAssign`]
//! * [`BitXor`]
//! * [`BitXorAssign`]
//! * [`Shl`]
//! * [`ShlAssign`]
//! * [`Shr`]
//! * [`ShrAssign`]
//!
//! String ops:
//!
//! * [`Concat`] - [Lua specific] concatenate byte content of two strings
//! * [`Len`] - [Lua specific] length of a string in bytes or *border* of a table
//!
//! # Checked vs unchecked ops
//!
//! Some arithmetic operations exist in both checked ([`CheckedDiv`], [`CheckedFloorDiv`], [`CheckedRem`] and [`CheckedPow`]) and
//! unchecked form ([`Div`], [`FloorDiv`], [`Rem`] and [`Pow`]).
//!
//! According to Lua spec, before performing any of those operations integer arguments must get coerced to floats.
//! For float values there is no reason to have checked versions: it can always emit a `NaN` in case of trouble,
//! and most programs expect this behavior.
//!
//! However, coercions inside our runtime are configurable.
//! This means that it is possible that those ops will be performed on integer arguments,
//! which can lead to Rust panics when provided with bad inputs.
//!
//! To handle the matter gracefully, when those ops of when invoked
//! runtime and builtins will always use **checked versions for integers** (and **unchecked for floats**).
//! Failing to produce a value is then elevated into Lua runtime error.

use std::hash::Hash;

use gc::index::Allocated;
use gc::{Gc, GcCell, Interned, Root, RootCell, Trace};

use super::string::{FromEncoding, IntoEncoding, PossiblyUtf8Vec};
use super::userdata::{DefaultParams, FullUserdata};
use super::{Int, KeyValue, Value};
use crate::ffi::DLuaFfi;
use crate::gc::{Heap, LuaPtr};
use crate::runtime::Closure;

pub use gc::userdata::Metatable;
pub use std::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div, DivAssign,
    Mul, MulAssign, Neg, Not as BitNot, Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub,
    SubAssign,
};

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
    type String: Trace + Concat + Len + Clone + Ord + Hash + IntoEncoding + FromEncoding;
    type LuaClosure: Trace;
    type RustClosure: Trace;
    type Table: Len + Metatable<Meta<Self>> + TableIndex<Weak, Self> + Default + Trace;
    type FullUserdata: FullUserdata<Meta<Self>, DefaultParams<Self>>
        + Allocated<Heap<Self>>
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
    fn first_key(&self) -> Option<&KeyValue<Rf, Ty>>;
    fn next_key(&self, key: &KeyValue<Rf, Ty>) -> Option<&KeyValue<Rf, Ty>>;
    fn border(&self) -> i64;
    fn contains_key(&self, key: &KeyValue<Rf, Ty>) -> bool {
        !matches!(self.get(key), Value::Nil)
    }
}

pub trait FloorDiv<Rhs = Self> {
    type Output;

    fn floor_div(self, rhs: Rhs) -> Self::Output;
}

pub trait Pow<Rhs = Self>: Sized {
    type Output;

    fn checked_pow(self, rhs: Rhs) -> Option<Self::Output>;

    fn pow(self, rhs: Rhs) -> Self::Output {
        self.checked_pow(rhs).unwrap()
    }
}

pub trait Len {
    fn len(&self) -> Int;
    fn is_empty(&self) -> bool {
        self.len() == Int(0)
    }
}

impl Len for String {
    fn len(&self) -> Int {
        let len = String::len(self);
        Int(len.try_into().unwrap())
    }
}

impl Len for Vec<u8> {
    fn len(&self) -> Int {
        let len = Vec::len(self);
        Int(len.try_into().unwrap())
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
