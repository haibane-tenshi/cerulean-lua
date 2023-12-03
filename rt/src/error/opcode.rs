use repr::debug_info::opcode;

use crate::value::table::InvalidTableKeyError;
use crate::value::Type;

#[derive(Debug)]
pub enum Error {
    TabGet {
        debug_info: Option<opcode::TabGet>,
        cause: TabGetCause,
    },
}

#[derive(Debug)]
pub enum TabGetCause {
    NoTable,
    NoTableAndIndex,
    TableTypeMismatch(Type),
    InvalidKey(InvalidTableKeyError),
}
