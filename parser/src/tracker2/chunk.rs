use repr::index_vec::IndexVec;
use repr::opcode::{Function, FunctionId};
use crate::tracker2::const_::Constants;

#[derive(Debug, Default)]
pub struct Chunk {
    pub functions: IndexVec<FunctionId, Function>,
    pub constants: Constants,
}

impl Chunk {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with_script() -> Self {
        Chunk {
            functions: vec![Default::default()].try_into().unwrap(),
            constants: Default::default(),
        }
    }

    pub fn resolve(self) -> repr::opcode::Chunk {
        let Chunk {
            functions,
            constants,
        } = self;

        let constants = constants.resolve();

        repr::opcode::Chunk {
            functions,
            constants,
        }
    }
}
