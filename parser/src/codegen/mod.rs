pub(crate) mod const_table;
pub(crate) mod fragment;
pub(crate) mod func_table;
pub(crate) mod function;
pub(crate) mod jumps;
pub(crate) mod labels;
pub(crate) mod loop_stack;
pub(crate) mod pending_adjust;
pub(crate) mod reachability;
pub(crate) mod recipe_table;
pub(crate) mod stack;
pub(crate) mod upvalues;

use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident<'s>(pub &'s str);

impl<'s> Ident<'s> {
    pub fn env() -> Self {
        Ident("_ENV")
    }

    pub fn self_() -> Self {
        Ident("self")
    }
}

impl<'s> Display for Ident<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
