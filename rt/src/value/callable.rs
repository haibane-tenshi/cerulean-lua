use std::fmt::{Debug, Display};
use std::hash::Hash;

use gc::Trace;

use super::{Refs, Strong, Types, Value, Weak};
use crate::ffi::arg_parser::TypeMismatchError;
use crate::gc::Heap;

pub use crate::runtime::{Closure as LuaClosure, RuntimeView};

pub type StrongCallable<Ty> = Callable<Strong, Ty>;
pub type WeakCallable<Ty> = Callable<Weak, Ty>;

pub enum Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    Lua(Rf::LuaCallable<Ty::LuaClosure>),
    Rust(Rf::RustCallable<Ty::RustClosure>),
}

impl<Ty> WeakCallable<Ty>
where
    Ty: Types,
{
    pub fn upgrade(self, heap: &Heap<Ty>) -> Option<StrongCallable<Ty>> {
        let r = match self {
            Callable::Rust(t) => Callable::Rust(t.upgrade(heap)?),
            Callable::Lua(t) => {
                let t = t.upgrade(heap)?;
                Callable::Lua(t)
            }
        };

        Some(r)
    }
}

impl<Ty> StrongCallable<Ty>
where
    Ty: Types,
{
    pub fn downgrade(&self) -> WeakCallable<Ty> {
        match self {
            Callable::Rust(t) => Callable::Rust(t.downgrade()),
            Callable::Lua(t) => Callable::Lua(t.downgrade()),
        }
    }
}

impl<Rf, Ty> Trace for Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::LuaCallable<Ty::LuaClosure>: Trace,
    Rf::RustCallable<Ty::RustClosure>: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        match self {
            Callable::Lua(t) => t.trace(collector),
            Callable::Rust(t) => t.trace(collector),
        }
    }
}

impl<Rf, Ty> Debug for Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::LuaCallable<Ty::LuaClosure>: Debug,
    Rf::RustCallable<Ty::RustClosure>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lua(arg0) => f.debug_tuple("Lua").field(arg0).finish(),
            Self::Rust(arg0) => f.debug_tuple("Rust").field(arg0).finish(),
        }
    }
}

impl<Rf, Ty> Display for Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::LuaCallable<Ty::LuaClosure>: Display,
    Rf::RustCallable<Ty::RustClosure>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lua(t) => write!(f, "{t}"),
            Self::Rust(t) => write!(f, "{t}"),
        }
    }
}

impl<Rf, Ty> Clone for Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::LuaCallable<Ty::LuaClosure>: Clone,
    Rf::RustCallable<Ty::RustClosure>: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Lua(arg0) => Self::Lua(arg0.clone()),
            Self::Rust(arg0) => Self::Rust(arg0.clone()),
        }
    }
}

impl<Rf, Ty> Copy for Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::LuaCallable<Ty::LuaClosure>: Copy,
    Rf::RustCallable<Ty::RustClosure>: Copy,
{
}

impl<Rf, Ty> PartialEq for Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::LuaCallable<Ty::LuaClosure>: PartialEq,
    Rf::RustCallable<Ty::RustClosure>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Lua(l0), Self::Lua(r0)) => l0 == r0,
            (Self::Rust(l0), Self::Rust(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<Rf, Ty> Eq for Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::LuaCallable<Ty::LuaClosure>: Eq,
    Rf::RustCallable<Ty::RustClosure>: Eq,
{
}

impl<Rf, Ty> PartialOrd for Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::LuaCallable<Ty::LuaClosure>: PartialOrd,
    Rf::RustCallable<Ty::RustClosure>: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering;

        match (self, other) {
            (Callable::Lua(lhs), Callable::Lua(rhs)) => lhs.partial_cmp(rhs),
            (Callable::Rust(lhs), Callable::Rust(rhs)) => lhs.partial_cmp(rhs),
            (Callable::Lua(_), Callable::Rust(_)) => Some(Ordering::Less),
            (Callable::Rust(_), Callable::Lua(_)) => Some(Ordering::Greater),
        }
    }
}

impl<Rf, Ty> Ord for Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::LuaCallable<Ty::LuaClosure>: Ord,
    Rf::RustCallable<Ty::RustClosure>: Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering;

        match (self, other) {
            (Callable::Lua(lhs), Callable::Lua(rhs)) => lhs.cmp(rhs),
            (Callable::Rust(lhs), Callable::Rust(rhs)) => lhs.cmp(rhs),
            (Callable::Lua(_), Callable::Rust(_)) => Ordering::Less,
            (Callable::Rust(_), Callable::Lua(_)) => Ordering::Greater,
        }
    }
}

impl<Rf, Ty> Hash for Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::LuaCallable<Ty::LuaClosure>: Hash,
    Rf::RustCallable<Ty::RustClosure>: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            Callable::Lua(t) => t.hash(state),
            Callable::Rust(t) => t.hash(state),
        }
    }
}

impl<Rf, Ty> TryFrom<Value<Rf, Ty>> for Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Rf, Ty>) -> Result<Self, Self::Error> {
        use super::Type;

        match value {
            Value::Function(value) => Ok(value),
            value => {
                let err = TypeMismatchError {
                    expected: Type::Function,
                    found: value.type_(),
                };

                Err(err)
            }
        }
    }
}

impl<Rf, Ty> From<Callable<Rf, Ty>> for Value<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    fn from(value: Callable<Rf, Ty>) -> Self {
        Value::Function(value)
    }
}
