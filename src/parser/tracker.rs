use std::collections::HashMap;

use crate::opcode::{Chunk, ConstId, OpCode, StackSlot};
use crate::value::Literal;

#[derive(Debug, Copy, Clone)]
pub(super) struct InstrId(pub u32);

#[derive(Debug, Default)]
struct ConstTracker {
    constants: Vec<Literal>,
    backlinks: HashMap<Literal, ConstId>,
}

impl ConstTracker {
    pub fn insert(&mut self, value: Literal) -> ConstId {
        *self.backlinks.entry(value.clone()).or_insert_with(|| {
            let index = self.constants.len().try_into().unwrap();
            self.constants.push(value);

            ConstId(index)
        })
    }

    pub fn resolve(self) -> Vec<Literal> {
        self.constants
    }
}

#[derive(Debug, Default)]
struct StackTracker<'s> {
    stack: Vec<Option<&'s str>>,
    backlinks: HashMap<&'s str, Vec<StackSlot>>,
    frames: Vec<usize>,
}

impl<'s> StackTracker<'s> {
    pub fn push(&mut self) -> StackSlot {
        let index = self.stack.len().try_into().unwrap();
        self.stack.push(None);

        StackSlot(index)
    }

    pub fn push_named(&mut self, name: &'s str) -> StackSlot {
        let index = self.stack.len().try_into().unwrap();
        self.stack.push(Some(name));

        let r = StackSlot(index);

        self.backlinks.entry(name).or_default().push(r);

        r
    }

    pub fn pop(&mut self) {
        let Some(Some(name)) = self.stack.pop() else {
            return
        };

        let Some(backlink) = self.backlinks.get_mut(&name) else {
            return
        };

        backlink.pop();

        if backlink.is_empty() {
            self.backlinks.remove(&name);
        }
    }

    pub fn lookup_local(&self, name: &str) -> Option<StackSlot> {
        self.backlinks.get(name).and_then(|bl| bl.last()).copied()
    }

    pub fn push_frame(&mut self) {
        self.frames.push(self.stack.len());
    }

    pub fn pop_frame(&mut self) -> Option<u32> {
        let index = self.frames.pop()?;
        let count = self.stack.len().checked_sub(index)?.try_into().ok()?;

        for _ in 0..count {
            self.pop();
        }

        Some(count)
    }

    pub fn pop_ghost_frame(&mut self) -> Option<u32> {
        let &index = self.frames.last()?;
        self.stack.len().checked_sub(index)?.try_into().ok()
    }
}

#[derive(Debug, Default)]
struct OpCodeTracker {
    codes: Vec<OpCode>,
}

impl OpCodeTracker {
    pub fn next(&self) -> InstrId {
        let n = self.codes.len().try_into().unwrap();
        InstrId(n)
    }

    pub fn push(&mut self, opcode: OpCode) -> InstrId {
        let index = self.next();
        self.codes.push(opcode);

        index
    }

    pub fn get(&self, index: InstrId) -> Option<&OpCode> {
        let index: usize = index.0.try_into().ok()?;
        self.codes.get(index)
    }

    pub fn get_mut(&mut self, index: InstrId) -> Option<&mut OpCode> {
        let index: usize = index.0.try_into().ok()?;
        self.codes.get_mut(index)
    }

    pub fn resolve(self) -> Vec<OpCode> {
        self.codes
    }
}

#[derive(Debug, Default)]
pub(super) struct ChunkTracker<'s> {
    constants: ConstTracker,
    stack: StackTracker<'s>,
    codes: OpCodeTracker,
}

impl<'s> ChunkTracker<'s> {
    pub fn empty() -> Self {
        Default::default()
    }

    pub fn resolve(self) -> Chunk {
        use crate::opcode::Function;

        let ChunkTracker {
            codes,
            constants,
            stack: _,
        } = self;

        let codes = codes.resolve();
        let constants = constants.resolve();

        let fun = Function {
            codes,
            lines: Default::default(),
        };

        Chunk {
            functions: vec![fun],
            constants,
        }
    }

    pub fn next_instr(&self) -> InstrId {
        self.codes.next()
    }

    pub fn get(&self, index: InstrId) -> Option<&OpCode> {
        self.codes.get(index)
    }

    pub fn get_mut(&mut self, index: InstrId) -> Option<&mut OpCode> {
        self.codes.get_mut(index)
    }

    pub fn push(&mut self, opcode: OpCode) -> InstrId {
        use OpCode::*;

        // Keep stack state consistent
        match opcode {
            Return => (),
            LoadConstant(_) | LoadStack(_) => {
                self.stack.push();
            }
            StoreStack(_) => self.stack.pop(),
            PopStack(count) => {
                todo!()
            }
            AriUnaOp(_) | BitUnaOp(_) => (),
            AriBinOp(_) | BitBinOp(_) | RelBinOp(_) | StrBinOp(_) => {
                self.stack.pop();
            }
            Jump { .. } | Loop { .. } => (),
            JumpIf { .. } | LoopIf { .. } => self.stack.pop(),
        }

        self.codes.push(opcode)
    }

    pub fn push_loop_to(&mut self, target: InstrId) -> InstrId {
        let offset = self.next_instr().0 - target.0 + 1;
        self.push(OpCode::Loop { offset })
    }

    pub fn push_frame(&mut self) {
        self.stack.push_frame()
    }

    pub fn pop_frame(&mut self) -> Result<(), ()> {
        let count = self.stack.pop_frame().ok_or(())?;

        // Remove excessive temporaries upon exiting frame.
        if let Ok(extra_stack) = count.try_into() {
            // Use raw push: we already popped temporaries off the stack.
            self.codes.push(OpCode::PopStack(extra_stack));
        }

        Ok(())
    }

    pub fn pop_ghost_frame(&mut self) -> Result<(), ()> {
        let count = self.stack.pop_ghost_frame().ok_or(())?;

        // Remove excessive temporaries upon exiting frame.
        if let Ok(extra_stack) = count.try_into() {
            // Use raw push: we already popped temporaries off the stack.
            self.codes.push(OpCode::PopStack(extra_stack));
        }

        Ok(())
    }

    pub fn insert_literal(&mut self, value: Literal) -> ConstId {
        self.constants.insert(value)
    }

    pub fn lookup_local(&self, ident: &str) -> Option<StackSlot> {
        self.stack.lookup_local(ident)
    }

    pub fn name_local(&mut self, ident: &'s str) {
        self.stack.pop();
        self.stack.push_named(ident);
    }
}
