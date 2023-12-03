use repr::debug_info::opcode;

use crate::value::Type;

#[derive(Debug)]
pub enum Error {
    TabGet {
        debug_info: Option<opcode::TabSet>,
        cause: TabGetCause,
    },
}

#[derive(Debug)]
pub enum TabGetCause {
    NoTable,
    NoTableAndIndex,
    TableTypeMismatch(Type),
    InvalidIndexValue(TabInvalidIndexValue),
}

#[derive(Debug)]
pub enum TabInvalidIndexValue {
    Nil,
    Nan,
}
