use thiserror::Error;

use crate::index_vec::IndexVec;
use crate::opcode::{Function, InstrId, OpCode};

#[derive(Debug, Error)]
#[error("exceeded indexing capacity of instruction ids")]
pub struct ExceededInstrIdError;

#[derive(Debug, Default)]
pub struct OpCodeTracker {
    codes: IndexVec<InstrId, OpCode>,
}

impl OpCodeTracker {
    pub fn next(&self) -> Result<InstrId, ExceededInstrIdError> {
        self.codes.len().map_err(|_| ExceededInstrIdError)
    }

    pub fn emit(&mut self, opcode: OpCode) -> Result<InstrId, ExceededInstrIdError> {
        self.codes.push(opcode).map_err(|_| ExceededInstrIdError)
    }

    // pub fn get(&self, index: InstrId) -> Option<&OpCode> {
    //     let index: usize = index.0.try_into().ok()?;
    //     self.codes.get(index)
    // }

    pub fn get_mut(&mut self, index: InstrId) -> Option<&mut OpCode> {
        self.codes.get_mut(index)
    }

    pub fn resolve(self, height: u32) -> Function {
        let OpCodeTracker { codes } = self;

        Function {
            codes,
            lines: Default::default(),
            height,
        }
    }
}
