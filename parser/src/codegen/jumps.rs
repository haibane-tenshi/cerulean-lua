use super::fragment::FragmentId;
use super::function::FunctionView;
use super::stack::StackState;
use repr::index::InstrId;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Jumps {
    jumps: HashMap<FragmentId, Vec<(InstrId, StackState)>>,
}

impl Jumps {
    pub(super) fn new() -> Self {
        Jumps {
            jumps: Default::default(),
        }
    }

    pub fn view(&mut self, fragment_id: FragmentId, start: InstrId) -> JumpsView<'_> {
        JumpsView::new(self, fragment_id, start)
    }
}

#[derive(Debug)]
pub struct JumpsView<'a> {
    fragment_id: FragmentId,
    instr_id: InstrId,
    store: &'a mut Jumps,
}

impl<'a> JumpsView<'a> {
    pub fn new(jumps: &'a mut Jumps, fragment_id: FragmentId, start: InstrId) -> Self {
        JumpsView {
            fragment_id,
            instr_id: start,
            store: jumps,
        }
    }

    pub fn borrow(&mut self) -> &mut Jumps {
        self.store
    }

    pub fn register_jump(&mut self, target: FragmentId, instr_id: InstrId, state: StackState) {
        self.store
            .jumps
            .entry(target)
            .or_default()
            .push((instr_id, state));
    }

    pub fn commit(self, fun: &mut FunctionView) -> Option<StackState> {
        use repr::opcode::OpCode;

        let mut stack_state = None;

        if let Some(to_backpatch) = self.store.jumps.remove(&self.fragment_id) {
            let start = fun.start();
            let end = fun.len();

            // Note: instruction pointer is moved before instruction is executed,
            // so we need to take that into account.
            for (instr, state) in to_backpatch {
                match fun.get_mut(instr) {
                    Some(OpCode::Jump { offset, .. } | OpCode::JumpIf { offset, .. }) => {
                        *offset = end - instr;
                    }
                    Some(OpCode::Loop { offset, .. })=> {
                        *offset = instr - start;
                    }
                    _ => (),
                }

                stack_state = match stack_state {
                    Some(acc) => Some(acc | state),
                    None => Some(state),
                };
            }
        }

        stack_state
    }
}

impl<'a> Drop for JumpsView<'a> {
    fn drop(&mut self) {
        self.store.jumps.remove(&self.fragment_id);
        for vec in self.store.jumps.values_mut() {
            let index = vec
                .iter()
                .enumerate()
                .find_map(|(i, (instr_id, _))| (*instr_id >= self.instr_id).then_some(i));

            if let Some(index) = index {
                vec.truncate(index);
            }
        }
    }
}
