pub mod opcode;

use std::ops::Range;

use crate::index::{FunctionId, InstrId};
use crate::tivec::TiVec;

pub use opcode::DebugInfo as OpCodeDebugInfo;

#[derive(Debug, Clone)]
pub struct DebugInfo {
    pub functions: TiVec<FunctionId, FunctionDebugInfo>,
    pub line_breaks: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct FunctionDebugInfo {
    pub name: String,
    pub span: Range<usize>,
    pub opcodes: TiVec<InstrId, OpCodeDebugInfo>,
}
