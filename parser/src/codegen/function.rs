use crate::codegen::fragment::FragmentId;
use crate::codegen::stack::StackState;
use repr::index::{InstrCountError, InstrId};
use repr::index_vec::IndexVec;
use repr::opcode::OpCode;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Function {
    opcodes: IndexVec<InstrId, OpCode>,
    jumps: HashMap<FragmentId, Vec<(InstrId, StackState)>>,
    reachable: bool,
}

impl Function {
    pub fn new() -> Self {
        Function {
            opcodes: Default::default(),
            jumps: Default::default(),
            reachable: true,
        }
    }

    pub fn view(&mut self) -> FunctionView {
        FunctionView::new(self)
    }

    pub fn resolve(self, height: u32) -> repr::chunk::Function {
        let Function {
            opcodes: codes,
            jumps,
            reachable: _,
        } = self;

        // Fixme: jumps not adjusted on rollback leading to panic here
        debug_assert!(jumps.is_empty());

        repr::chunk::Function {
            codes,
            lines: Default::default(),
            height,
        }
    }

    fn state(&self) -> InnerState {
        InnerState {
            start: self.opcodes.len(),
            reachable: self.reachable,
        }
    }

    fn apply(&mut self, state: InnerState) {
        let InnerState { start, reachable } = state;

        self.opcodes.truncate(start);
        self.reachable = reachable;
    }
}

#[derive(Debug, Copy, Clone)]
struct InnerState {
    start: InstrId,
    reachable: bool,
}

#[derive(Debug)]
pub struct FunctionView<'fun> {
    fragment_id: FragmentId,
    fun: &'fun mut Function,
    prev_state: InnerState,
}

impl<'fun> FunctionView<'fun> {
    pub fn new(fun: &'fun mut Function) -> Self {
        let prev_state = fun.state();

        FunctionView {
            fragment_id: Default::default(),
            fun,
            prev_state,
        }
    }

    pub fn id(&self) -> FragmentId {
        self.fragment_id
    }

    pub fn start(&self) -> InstrId {
        self.prev_state.start
    }

    pub fn reachable(&self) -> bool {
        self.fun.reachable
    }

    pub fn len(&self) -> InstrId {
        self.fun.opcodes.len()
    }

    pub fn emit(&mut self, opcode: OpCode) -> Result<InstrId, InstrCountError> {
        use OpCode::*;

        self.fun.reachable = match opcode {
            Return(_) | Jump { .. } | Loop { .. } | Panic => false,
            _ => true,
        };

        self.fun.opcodes.push(opcode)
    }

    pub fn register_jump(&mut self, target: FragmentId, instr: InstrId, state: StackState) {
        self.fun
            .jumps
            .entry(target)
            .or_default()
            .push((instr, state));
    }

    // pub fn get_mut(&mut self, instr_id: InstrId) -> Option<&mut OpCode> {
    //     if instr_id < self.start() {
    //         return None;
    //     }
    //
    //     self.fun.opcodes.get_mut(instr_id)
    // }

    pub fn new_block(&mut self) -> FunctionView {
        let fragment_id = self.fragment_id + 1;
        let prev_state = self.fun.state();
        let fun = &mut self.fun;

        FunctionView {
            fragment_id,
            fun,
            prev_state,
        }
    }

    pub fn commit(self) -> Option<StackState> {
        let mut stack_state = None;

        if let Some(to_backpatch) = self.fun.jumps.remove(&self.fragment_id) {
            let start = self.start();
            let end = self.len();

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
        self.fun.apply(self.prev_state)
    }
}
