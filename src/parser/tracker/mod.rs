mod stack;

use std::collections::HashMap;

use crate::opcode::{Chunk, ConstId, Function, FunctionId, OpCode, StackSlot};
use crate::value::Literal;

use stack::{StackStateError, StackTracker};

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
struct OpCodeTracker {
    codes: Vec<OpCode>,
}

impl OpCodeTracker {
    pub fn next(&self) -> InstrId {
        let n = self.codes.len().try_into().unwrap();
        InstrId(n)
    }

    pub fn emit(&mut self, opcode: OpCode) -> InstrId {
        let index = self.next();
        self.codes.push(opcode);

        index
    }

    // pub fn get(&self, index: InstrId) -> Option<&OpCode> {
    //     let index: usize = index.0.try_into().ok()?;
    //     self.codes.get(index)
    // }

    pub fn get_mut(&mut self, index: InstrId) -> Option<&mut OpCode> {
        let index: usize = index.0.try_into().ok()?;
        self.codes.get_mut(index)
    }

    pub fn resolve(self, height: u32) -> Function {
        let OpCodeTracker { codes } = self;

        Function {
            codes,
            lines: Default::default(),
            height,
        }
    }
}

#[derive(Debug)]
pub(super) struct ChunkTracker<'s> {
    finalized: Vec<Function>,
    suspended: Vec<OpCodeTracker>,
    constants: ConstTracker,
    stack: StackTracker<'s>,
}

impl<'s> ChunkTracker<'s> {
    pub fn empty() -> Self {
        Default::default()
    }

    pub fn resolve(self) -> Result<Chunk, ()> {
        let ChunkTracker {
            mut finalized,
            mut suspended,
            constants,
            stack: _,
        } = self;

        let script = {
            let script = suspended.pop().ok_or(())?;

            if !suspended.is_empty() {
                return Err(());
            }

            script.resolve(0)
        };

        *finalized.get_mut(0).unwrap() = script;

        let constants = constants.resolve();

        let r = Chunk {
            functions: finalized,
            constants,
        };

        Ok(r)
    }

    pub fn next_instr(&self) -> InstrId {
        self.suspended
            .last()
            .map(|opcodes| opcodes.next())
            .unwrap_or(InstrId(0))
    }

    // pub fn get(&self, index: InstrId) -> Option<&OpCode> {
    //     self.suspended.last()?.get(index)
    // }

    pub fn get_mut(&mut self, index: InstrId) -> Option<&mut OpCode> {
        self.suspended.last_mut()?.get_mut(index)
    }

    pub fn emit(&mut self, opcode: OpCode) -> InstrId {
        use OpCode::*;

        // Keep stack state consistent
        match opcode {
            Invoke(slot) => {
                self.stack.adjust_to(slot).unwrap();
                self.stack.make_variadic();
            }
            Return(_) => (),
            LoadConstant(_) | LoadStack(_) => {
                self.stack.push(None).unwrap();
            }
            StoreStack(_) => {
                self.stack.pop().unwrap();
            }
            AdjustStack(slot) => {
                self.stack.adjust_to(slot).unwrap();
            }
            AriUnaOp(_) | BitUnaOp(_) => (),
            AriBinOp(_) | BitBinOp(_) | RelBinOp(_) | StrBinOp(_) => {
                self.stack.pop().unwrap();
            }
            Jump { .. } | Loop { .. } => (),
            JumpIf { .. } | LoopIf { .. } => self.stack.pop().unwrap(),
        }

        self.suspended.last_mut().unwrap().emit(opcode)
    }

    pub fn stack_top(&self) -> Result<StackSlot, StackStateError> {
        self.stack.top()
    }

    pub fn emit_loop_to(&mut self, target: InstrId) -> InstrId {
        let offset = self.next_instr().0 - target.0 + 1;
        self.emit(OpCode::Loop { offset })
    }

    pub fn emit_adjust_to(&mut self, slot: StackSlot) -> Result<Option<InstrId>, StackStateError> {
        let needs_adjustment = self.stack.adjust_to(slot)?;

        let instr_id = if needs_adjustment {
            let instr_id = self
                .suspended
                .last_mut()
                .unwrap()
                .emit(OpCode::AdjustStack(slot));

            Some(instr_id)
        } else {
            None
        };

        Ok(instr_id)
    }

    pub fn push_block(&mut self) -> Result<(), StackStateError> {
        self.stack.push_block()
    }

    pub fn pop_block(&mut self) -> Result<(), StackStateError> {
        let current = self.stack.top()?;
        let slot = self.stack.pop_block()?;

        // Remove excessive temporaries upon exiting frame.
        if slot <= current {
            // Use raw emit: we already popped temporaries off the stack.
            self.suspended
                .last_mut()
                .unwrap()
                .emit(OpCode::AdjustStack(slot));
        }

        Ok(())
    }

    pub fn pop_ghost_block(&mut self) -> Result<(), StackStateError> {
        let current = self.stack.top()?;
        let slot = self.stack.pop_ghost_block()?;

        // Remove excessive temporaries upon exiting frame.
        if slot <= current {
            // Use raw emit: we already popped temporaries off the stack.
            self.suspended
                .last_mut()
                .unwrap()
                .emit(OpCode::AdjustStack(slot));
        }

        Ok(())
    }

    pub fn insert_literal(&mut self, value: Literal) -> ConstId {
        self.constants.insert(value)
    }

    pub fn lookup_local(&self, ident: &str) -> Option<StackSlot> {
        self.stack.lookup_local(ident)
    }

    pub fn name_local(&mut self, ident: &'s str) -> Result<(), StackStateError> {
        self.stack.pop()?;
        self.stack.push(Some(ident)).unwrap();

        Ok(())
    }

    pub fn push_frame(&mut self) -> Result<(), StackStateError> {
        self.stack.push_frame()?;
        self.suspended.push(Default::default());

        Ok(())
    }

    pub fn pop_frame(&mut self, height: u32) -> Result<FunctionId, StackStateError> {
        let mut opcodes = self.suspended.pop().ok_or(StackStateError::MissingFrame)?;

        let current = self.stack.top()?;
        let slot = self.stack.pop_frame()?;
        if slot <= current {
            opcodes.emit(OpCode::AdjustStack(slot));
        }

        let fun = opcodes.resolve(height);

        let id = FunctionId(self.finalized.len().try_into().unwrap());
        self.finalized.push(fun);

        Ok(id)
    }

    pub fn push_stack(&mut self, name: Option<&'s str>) -> Result<StackSlot, StackStateError> {
        self.stack.push(name)
    }
}

impl<'s> Default for ChunkTracker<'s> {
    fn default() -> Self {
        // 0th slot is the entry point.
        // Reserve it for the script itself.
        ChunkTracker {
            finalized: vec![Default::default()],
            suspended: vec![Default::default()],
            constants: Default::default(),
            stack: Default::default(),
        }
    }
}
