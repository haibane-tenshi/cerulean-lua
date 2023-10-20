use repr::opcode::OpCode;

#[derive(Debug, Clone, Copy)]
pub struct Reachability {
    reachable: bool,
}

impl Reachability {
    pub fn new() -> Self {
        Reachability { reachable: true }
    }

    pub fn emit(&mut self, opcode: &OpCode) {
        use OpCode::*;

        self.reachable &= matches!(opcode, Return(_) | Jump { .. } | Loop { .. } | Panic);
    }

    pub fn make_reachable(&mut self) {
        self.reachable = true;
    }

    pub fn commit(self) -> bool {
        self.reachable
    }
}
