use repr::debug_info::OpCodeDebugInfo;
use repr::index::InstrId;
use repr::opcode::OpCode;
use repr::tivec::TiVec;

#[derive(Debug, Clone, Default)]
pub struct Signature {
    pub arg_count: usize,
    pub is_variadic: bool,
}

#[derive(Debug)]
pub struct Function {
    opcodes: TiVec<InstrId, OpCode>,
    debug_info: TiVec<InstrId, OpCodeDebugInfo>,
    signature: Signature,
}

impl Function {
    pub fn new(signature: Signature) -> Self {
        Function {
            opcodes: Default::default(),
            debug_info: Default::default(),
            signature,
        }
    }

    pub fn view(&mut self) -> FunctionView {
        FunctionView::new(self)
    }

    // pub fn len(&self) -> usize {
    //     self.opcodes.len()
    // }

    pub fn next_id(&self) -> InstrId {
        self.opcodes.next_key()
    }

    pub fn push(&mut self, opcode: OpCode, debug_info: OpCodeDebugInfo) -> InstrId {
        let debug_id = self.debug_info.push_and_get_key(debug_info);
        let instr_id = self.opcodes.push_and_get_key(opcode);

        debug_assert_eq!(instr_id, debug_id);

        instr_id
    }

    pub fn resolve(
        self,
        upvalue_count: usize,
    ) -> (repr::chunk::Function, TiVec<InstrId, OpCodeDebugInfo>) {
        let Function {
            opcodes,
            debug_info,
            signature:
                Signature {
                    arg_count,
                    is_variadic,
                },
        } = self;

        let signature = repr::chunk::Signature {
            arg_count,
            is_variadic,
            upvalue_count,
        };

        let func = repr::chunk::Function { opcodes, signature };

        (func, debug_info)
    }

    fn state(&self) -> InnerState {
        InnerState {
            start: self.opcodes.next_key(),
        }
    }

    fn apply(&mut self, state: InnerState) {
        let InnerState { start } = state;

        self.opcodes.truncate(start.into());
        self.debug_info.truncate(start.into());
    }
}

#[derive(Debug, Copy, Clone)]
struct InnerState {
    start: InstrId,
}

#[derive(Debug)]
pub struct FunctionView<'fun> {
    func: &'fun mut Function,
    prev_state: InnerState,
}

impl<'fun> FunctionView<'fun> {
    pub fn new(func: &'fun mut Function) -> Self {
        let prev_state = func.state();

        FunctionView { func, prev_state }
    }

    pub fn borrow(&mut self) -> &mut Function {
        self.func
    }

    pub fn start(&self) -> InstrId {
        self.prev_state.start
    }

    pub fn end(&self) -> InstrId {
        self.next_id()
    }

    pub fn next_id(&self) -> InstrId {
        self.func.next_id()
    }

    pub fn signature(&self) -> &Signature {
        &self.func.signature
    }

    pub fn emit(&mut self, opcode: OpCode, debug_info: OpCodeDebugInfo) -> InstrId {
        self.func.push(opcode, debug_info)
    }

    pub fn get_mut(&mut self, instr_id: InstrId) -> Option<&mut OpCode> {
        if instr_id < self.start() {
            return None;
        }

        self.func.opcodes.get_mut(instr_id)
    }

    pub fn get_debug_info_mut(&mut self, instr_id: InstrId) -> Option<&mut OpCodeDebugInfo> {
        if instr_id < self.start() {
            return None;
        }

        self.func.debug_info.get_mut(instr_id)
    }

    pub fn commit(self) {
        // Prevent drop impl from rolling back changes.
        std::mem::forget(self);
    }
}

impl<'fun> Drop for FunctionView<'fun> {
    fn drop(&mut self) {
        self.func.apply(self.prev_state);
    }
}
