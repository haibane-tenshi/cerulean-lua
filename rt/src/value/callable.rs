use std::fmt::{Debug, Display};
use std::hash::Hash;

use gc::Trace;

use super::{Refs, Strong, Types, Value, Weak};
use crate::error::AlreadyDroppedError;
use crate::ffi::arg_parser::TypeMismatchError;
use crate::gc::{Downgrade, Heap, Upgrade};

pub use crate::runtime::Closure as LuaClosure;

pub type StrongCallable<Ty> = Callable<Strong<Ty>>;
pub type WeakCallable<Ty> = Callable<Weak<Ty>>;

pub enum Callable<Rf>
where
    Rf: Refs,
{
    Lua(Rf::LuaClosure),
    Rust(Rf::RustClosure),
}

impl<Ty> Upgrade<Heap<Ty>> for WeakCallable<Ty>
where
    Ty: Types,
{
    type Output = StrongCallable<Ty>;

    fn try_upgrade(&self, heap: &Heap<Ty>) -> Result<Self::Output, AlreadyDroppedError> {
        let r = match self {
            Callable::Rust(t) => Callable::Rust(t.try_upgrade(heap)?),
            Callable::Lua(t) => Callable::Lua(t.try_upgrade(heap)?),
        };

        Ok(r)
    }
}

impl<Ty> Downgrade for StrongCallable<Ty>
where
    Ty: Types,
{
    type Output = WeakCallable<Ty>;

    fn downgrade(&self) -> Self::Output {
        match self {
            Callable::Rust(t) => Callable::Rust(t.downgrade()),
            Callable::Lua(t) => Callable::Lua(t.downgrade()),
        }
    }
}

impl<Rf> Trace for Callable<Rf>
where
    Rf: Refs,
    Rf::LuaClosure: Trace,
    Rf::RustClosure: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        match self {
            Callable::Lua(t) => t.trace(collector),
            Callable::Rust(t) => t.trace(collector),
        }
    }
}

impl<Rf> Debug for Callable<Rf>
where
    Rf: Refs,
    Rf::LuaClosure: Debug,
    Rf::RustClosure: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lua(arg0) => f.debug_tuple("Lua").field(arg0).finish(),
            Self::Rust(arg0) => f.debug_tuple("Rust").field(arg0).finish(),
        }
    }
}

impl<Ty> Display for WeakCallable<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Callable::Lua(t) => write!(f, "{{[lua] closure <{}>}}", t.0.location()),
            Callable::Rust(t) => write!(f, "{{[rust] closure <{}>}}", t.0.location()),
        }
    }
}

impl<Ty> Display for StrongCallable<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Callable::Lua(t) => write!(f, "{{[lua] closure <{}>}}", t.0.location()),
            Callable::Rust(t) => write!(f, "{{[rust] closure <{}>}}", t.0.location()),
        }
    }
}

impl<Rf> Clone for Callable<Rf>
where
    Rf: Refs,
    Rf::LuaClosure: Clone,
    Rf::RustClosure: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Lua(arg0) => Self::Lua(arg0.clone()),
            Self::Rust(arg0) => Self::Rust(arg0.clone()),
        }
    }
}

impl<Rf> Copy for Callable<Rf>
where
    Rf: Refs,
    Rf::LuaClosure: Copy,
    Rf::RustClosure: Copy,
{
}

impl<Rf> PartialEq for Callable<Rf>
where
    Rf: Refs,
    Rf::LuaClosure: PartialEq,
    Rf::RustClosure: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Lua(l0), Self::Lua(r0)) => l0 == r0,
            (Self::Rust(l0), Self::Rust(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<Rf> Eq for Callable<Rf>
where
    Rf: Refs,
    Rf::LuaClosure: Eq,
    Rf::RustClosure: Eq,
{
}

impl<Rf> PartialOrd for Callable<Rf>
where
    Rf: Refs,
    Rf::LuaClosure: PartialOrd,
    Rf::RustClosure: PartialOrd,
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

impl<Rf> Ord for Callable<Rf>
where
    Rf: Refs,
    Rf::LuaClosure: Ord,
    Rf::RustClosure: Ord,
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

impl<Rf> Hash for Callable<Rf>
where
    Rf: Refs,
    Rf::LuaClosure: Hash,
    Rf::RustClosure: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);

        match self {
            Callable::Lua(t) => t.hash(state),
            Callable::Rust(t) => t.hash(state),
        }
    }
}

impl<Rf> TryFrom<Value<Rf>> for Callable<Rf>
where
    Rf: Refs,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Rf>) -> Result<Self, Self::Error> {
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

impl<Rf> From<Callable<Rf>> for Value<Rf>
where
    Rf: Refs,
{
    fn from(value: Callable<Rf>) -> Self {
        Value::Function(value)
    }
}
