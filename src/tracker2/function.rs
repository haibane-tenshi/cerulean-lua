use crate::index_vec::IndexVec;
use crate::opcode::{InstrCountError, InstrId, OpCode};
use crate::tracker2::fragment::Fragment;
use crate::tracker2::stack::StackView;

#[derive(Debug)]
pub struct Function {
    opcodes: IndexVec<InstrId, OpCode>,
}

impl Function {
    pub fn new() -> Self {
        Function {
            opcodes: Default::default(),
        }
    }

    pub fn resolve(self, height: u32) -> crate::opcode::Function {
        let Function { opcodes: codes } = self;

        crate::opcode::Function {
            codes,
            lines: Default::default(),
            height,
        }
    }
}

pub struct FunctionView<'fun> {
    fun: &'fun mut Function,
    start: InstrId,
}

impl<'fun> FunctionView<'fun> {
    pub fn new(fun: &'fun mut Function) -> Self {
        let start = fun.opcodes.len();

        FunctionView { fun, start }
    }

    pub fn emit(&mut self, opcode: OpCode) -> Result<InstrId, InstrCountError> {
        self.fun.opcodes.push(opcode)
    }

    pub fn new_block(&mut self) -> FunctionView {
        FunctionView::new(self.fun)
    }

    pub fn finalize(self) {
        // Prevent drop impls from rolling back changes.
        std::mem::forget(self)
    }
}

impl<'fun> Drop for FunctionView<'fun> {
    fn drop(&mut self) {
        self.fun.opcodes.truncate(self.start)
    }
}
