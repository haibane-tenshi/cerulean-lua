use crate::tracker2::const_::Constants;
use repr::chunk::Function;
use repr::index::FunctionId;
use repr::index_vec::IndexVec;

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

    pub fn resolve(self) -> repr::chunk::Chunk {
        let Chunk {
            functions,
            constants,
        } = self;

        let constants = constants.resolve();

        repr::chunk::Chunk {
            functions,
            constants,
        }
    }
}
