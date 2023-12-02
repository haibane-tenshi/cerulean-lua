use std::ops::Range;

use crate::index::{FunctionId, InstrId};
use crate::tivec::TiVec;

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

#[derive(Debug, Clone)]
pub struct OpCodeDebugInfo {
    pub span: Range<usize>,
}
