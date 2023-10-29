use super::fragment::FragmentId;

#[derive(Debug, Clone, Copy)]
pub struct LoopStack {
    innermost_loop: Option<FragmentId>,
}

impl LoopStack {
    pub fn new() -> Self {
        LoopStack {
            innermost_loop: None,
        }
    }

    pub fn push(&mut self, id: FragmentId) {
        self.innermost_loop = Some(id);
    }

    pub fn innermost_loop(&self) -> Option<FragmentId> {
        self.innermost_loop
    }
}
