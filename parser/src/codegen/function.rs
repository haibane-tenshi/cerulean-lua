use repr::chunk::UpvalueSource;
use repr::index::{InstrCountError, InstrId, UpvalueSlot};
use repr::index_vec::IndexVec;
use repr::opcode::OpCode;

#[derive(Debug, Clone, Default)]
pub struct Signature {
    pub height: u32,
    pub is_variadic: bool,
}

#[derive(Debug)]
pub struct Function {
    opcodes: IndexVec<InstrId, OpCode>,
    signature: Signature,
}

impl Function {
    pub fn new(signature: Signature) -> Self {
        Function {
            opcodes: Default::default(),
            signature,
        }
    }

    pub fn view(&mut self) -> FunctionView {
        FunctionView::new(self)
    }

    pub fn resolve(self, upvalues: IndexVec<UpvalueSlot, UpvalueSource>) -> repr::chunk::Function {
        let Function {
            opcodes: codes,
            signature:
                Signature {
                    height,
                    is_variadic,
                },
        } = self;

        let signature = repr::chunk::Signature {
            height,
            is_variadic,
            upvalues,
        };

        repr::chunk::Function {
            codes,
            lines: Default::default(),
            signature,
        }
    }

    fn state(&self) -> InnerState {
        InnerState {
            start: self.opcodes.len(),
        }
    }

    fn apply(&mut self, state: InnerState) {
        let InnerState { start } = state;

        self.opcodes.truncate(start);
    }
}

#[derive(Debug, Copy, Clone)]
struct InnerState {
    start: InstrId,
}

#[derive(Debug)]
pub struct FunctionView<'fun> {
    fun: &'fun mut Function,
    prev_state: InnerState,
}

impl<'fun> FunctionView<'fun> {
    pub fn new(fun: &'fun mut Function) -> Self {
        let prev_state = fun.state();

        FunctionView { fun, prev_state }
    }

    pub fn borrow(&mut self) -> &mut Function {
        self.fun
    }

    pub fn start(&self) -> InstrId {
        self.prev_state.start
    }

    pub fn signature(&self) -> &Signature {
        &self.fun.signature
    }

    pub fn len(&self) -> InstrId {
        self.fun.opcodes.len()
    }

    pub fn emit(&mut self, opcode: OpCode) -> Result<InstrId, InstrCountError> {
        self.fun.opcodes.push(opcode)
    }

    pub fn get_mut(&mut self, instr_id: InstrId) -> Option<&mut OpCode> {
        if instr_id < self.start() {
            return None;
        }

        self.fun.opcodes.get_mut(instr_id)
    }

    pub fn commit(self) {
        // Prevent drop impls from rolling back changes.
        std::mem::forget(self);
    }
}

impl<'fun> Drop for FunctionView<'fun> {
    fn drop(&mut self) {
        self.fun.apply(self.prev_state);
    }
}
