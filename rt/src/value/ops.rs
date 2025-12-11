//! Raw operations on Lua primitives.
//!
//! This module contains traits describing all ops permitted on Lua primitives.
//! It mostly consist of `std::ops` reimports (such as [`Add`], [`Mul`] and others),
//! but there are also Lua-specific methods mixed in (such as [`Len`])
//!
//! Note that traits inside this module adhere to Rust's conventions.
//! From perspective of Lua they describe **raw** operations on underlying values.
//! It is used as a building block for more complex and featureful operations.
//! For this reason traits are only implemented on selection of strongly-typed Lua values
//! ([`Int`], [`Float`](super::Float) and others) but not on [`Value`](super::Value)s.
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
//! * [`CheckedDiv`] - \[custom\] variant that shouldn't panic
//! * [`FloorDiv`] - \[Lua specific\] division that rounds to nearest smaller integer
//! * [`CheckedFloorDiv`] - \[Lua specific\]\[custom\] variant that shouldn't panic
//! * [`Rem`]
//! * [`RemAssign`]
//! * [`CheckedRem`] - \[custom\] variant that shouldn't panic
//! * [`Pow`] - \[Lua specific\] raise number to the power
//! * [`CheckedPow`] - \[custom\] variant that shouldn't panic
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
//! According to Lua spec, before performing any of those operations integer arguments must get coerced to floats likely to prevent integer exceptions.
//! (Other ops don't panic because Lua defines integer arithmetic as wrapping.)
//! For floats there is no reason to have checked versions: it can always emit a `NaN` in case of trouble,
//! and most programs expect this behavior.
//!
//! However, you should remember that coercions are performed in a separate step,
//! moreover our runtime allows to configure them.
//! This means that it is possible for any op to be applied to integer arguments,
//! which can lead to Rust panics on bad inputs.
//!
//! To handle the matter gracefully, builtins will always use **checked versions for integers** and **unchecked for floats**.
//! Failing to produce a value in checked ops is then elevated into Lua runtime error.

use super::Int;

pub use std::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div, DivAssign,
    Mul, MulAssign, Neg, Not as BitNot, Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub,
    SubAssign,
};

pub trait CheckedDiv<Rhs = Self>: Div<Rhs> {
    fn checked_div(self, rhs: Rhs) -> Option<Self::Output>;
}

pub trait CheckedRem<Rhs = Self>: Rem<Rhs> {
    fn checked_rem(self, rhs: Rhs) -> Option<Self::Output>;
}

pub trait FloorDiv<Rhs = Self> {
    type Output;

    fn floor_div(self, rhs: Rhs) -> Self::Output;
}

pub trait CheckedFloorDiv<Rhs = Self>: FloorDiv<Rhs> {
    fn checked_floor_div(self, rhs: Rhs) -> Option<Self::Output>;
}

pub trait Pow<Rhs = Self>: Sized {
    type Output;

    fn pow(self, rhs: Rhs) -> Self::Output;
}

pub trait CheckedPow<Rhs = Self>: Pow<Rhs> {
    fn checked_pow(self, rhs: Rhs) -> Option<Self::Output>;
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
