use crate::index_vec::IndexVec;
use crate::opcode::{InstrCountError, InstrId, OpCode};
use crate::tracker2::fragment::FragmentId;
use crate::tracker2::stack::StackState;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Function {
    opcodes: IndexVec<InstrId, OpCode>,
    jumps: HashMap<FragmentId, Vec<(InstrId, StackState)>>,
}

impl Function {
    pub fn new() -> Self {
        Function {
            opcodes: Default::default(),
            jumps: Default::default(),
        }
    }

    pub fn resolve(self, height: u32) -> crate::opcode::Function {
        let Function {
            opcodes: codes,
            jumps,
        } = self;

        debug_assert!(jumps.is_empty());

        crate::opcode::Function {
            codes,
            lines: Default::default(),
            height,
        }
    }
}

pub(super) struct FunctionView<'fun> {
    fragment_id: FragmentId,
    fun: &'fun mut Function,
    start: InstrId,
}

impl<'fun> FunctionView<'fun> {
    pub fn new(fun: &'fun mut Function) -> Self {
        let start = fun.opcodes.len();

        FunctionView {
            fragment_id: Default::default(),
            fun,
            start,
        }
    }

    pub fn id(&self) -> FragmentId {
        self.fragment_id
    }

    pub fn start(&self) -> InstrId {
        self.start
    }

    pub fn len(&self) -> InstrId {
        self.fun.opcodes.len()
    }

    pub fn emit(&mut self, opcode: OpCode) -> Result<InstrId, InstrCountError> {
        self.fun.opcodes.push(opcode)
    }

    pub fn register_jump(&mut self, target: FragmentId, instr: InstrId, state: StackState) {
        self.fun
            .jumps
            .entry(target)
            .or_default()
            .push((instr, state));
    }

    pub fn get_mut(&mut self, instr_id: InstrId) -> Option<&mut OpCode> {
        if instr_id < self.start {
            return None;
        }

        self.fun.opcodes.get_mut(instr_id)
    }

    pub fn new_block(&mut self) -> FunctionView {
        let start = self.fun.opcodes.len();
        let fragment_id = self.fragment_id + 1;
        let fun = &mut self.fun;

        FunctionView {
            fragment_id,
            fun,
            start,
        }
    }

    pub fn commit(self) -> Option<StackState> {
        let mut stack_state = None;

        if let Some(to_backpatch) = self.fun.jumps.remove(&self.fragment_id) {
            let start = self.start;
            let end = self.fun.opcodes.len();

            // Note: instruction pointer is moved before instruction is executed,
            // so we need to take that into account.
            for (instr, state) in to_backpatch {
                match self.fun.opcodes.get_mut(instr) {
                    Some(OpCode::Jump { offset, .. } | OpCode::JumpIf { offset, .. }) => {
                        *offset = end - instr - 1;
                    }
                    Some(OpCode::Loop { offset, .. } | OpCode::LoopIf { offset, .. }) => {
                        *offset = instr - start + 1;
                    }
                    _ => (),
                }

                stack_state = match stack_state {
                    Some(acc) => Some(acc | state),
                    None => Some(state),
                };
            }
        }

        // Prevent drop impls from rolling back changes.
        std::mem::forget(self);

        stack_state
    }
}

impl<'fun> Drop for FunctionView<'fun> {
    fn drop(&mut self) {
        self.fun.opcodes.truncate(self.start);
        self.fun.jumps.remove(&self.fragment_id);
    }
}
