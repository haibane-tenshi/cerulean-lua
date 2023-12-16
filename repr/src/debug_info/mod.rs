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

impl DebugInfo {
    pub fn line_column(&self, offset: usize) -> (usize, usize) {
        let line = match self.line_breaks.binary_search(&offset) {
            Ok(index) => index,
            Err(index) => index,
        };

        let line_start = line
            .checked_sub(1)
            .and_then(|index| self.line_breaks.get(index))
            .copied()
            .unwrap_or_default();

        // Make it 1-based.
        (line + 1, offset - line_start)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDebugInfo {
    pub name: String,
    pub span: Range<usize>,
    pub opcodes: TiVec<InstrId, OpCodeDebugInfo>,
}
