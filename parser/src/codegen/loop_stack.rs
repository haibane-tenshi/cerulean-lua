use super::fragment::FragmentId;

#[derive(Debug)]
pub struct LoopStack {
    loops: Vec<FragmentId>,
    frame_base: usize,
}

#[derive(Debug, Clone, Copy)]
struct InnerState {
    frame_base: usize,
    loops_count: usize,
}

impl LoopStack {
    pub fn new() -> Self {
        LoopStack {
            loops: Default::default(),
            frame_base: 0,
        }
    }

    pub fn view(&mut self) -> LoopStackView {
        let prev_state = self.inner_state();

        LoopStackView {
            stack: self,
            prev_state,
        }
    }

    pub fn frame(&mut self) -> LoopStackView {
        let prev_state = self.inner_state();
        self.frame_base = self.loops.len();

        LoopStackView {
            stack: self,
            prev_state,
        }
    }

    fn push(&mut self, id: FragmentId) {
        if self.loops.last().copied() != Some(id) {
            self.loops.push(id)
        }
    }

    fn innermost_loop(&self) -> Option<FragmentId> {
        if self.loops.len() > self.frame_base {
            self.loops.last().copied()
        } else {
            None
        }
    }

    fn inner_state(&self) -> InnerState {
        InnerState {
            frame_base: self.frame_base,
            loops_count: self.loops.len(),
        }
    }

    fn apply(&mut self, state: InnerState) {
        let InnerState {
            frame_base,
            loops_count,
        } = state;

        self.loops.truncate(loops_count);
        self.frame_base = frame_base;
    }
}

#[derive(Debug)]
pub struct LoopStackView<'a> {
    stack: &'a mut LoopStack,
    prev_state: InnerState,
}

impl<'a> LoopStackView<'a> {
    pub fn borrow(&mut self) -> &mut LoopStack {
        self.stack
    }

    pub fn push(&mut self, id: FragmentId) {
        self.stack.push(id)
    }

    pub fn innermost_loop(&self) -> Option<FragmentId> {
        self.stack.innermost_loop()
    }

    pub fn commit(self) {
        self.stack.apply(self.prev_state)
    }
}

impl<'a> Drop for LoopStackView<'a> {
    fn drop(&mut self) {
        self.stack.apply(self.prev_state)
    }
}
