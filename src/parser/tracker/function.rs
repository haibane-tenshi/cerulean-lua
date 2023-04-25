use thiserror::Error;

use crate::opcode::{Function, InstrId, OpCode, StackSlot};

use super::{ExceededInstrIdError, OpCodeTracker, StackStateError, StackTracker};

#[derive(Debug, Error)]
pub enum BackpatchError {
    #[error(transparent)]
    InstrId(#[from] ExceededInstrIdError),

    #[error("there is no instruction under this index")]
    IndexOutOfBounds,
}

#[derive(Debug, Error)]
pub enum EmitError {
    #[error("reached erroneous stack state")]
    Stack(#[from] StackStateError),

    #[error("failed to emit instruction")]
    InstrId(#[from] ExceededInstrIdError),
}

#[derive(Debug, Default)]
pub struct FunctionTracker<'s> {
    opcodes: OpCodeTracker,
    stack: StackTracker<'s>,
}

impl<'s> FunctionTracker<'s> {
    pub fn next_instr(&self) -> Result<InstrId, ExceededInstrIdError> {
        self.opcodes.next()
    }

    // pub fn get(&self, index: InstrId) -> Option<&OpCode> {
    //     self.suspended.last()?.get(index)
    // }

    pub fn get_mut(&mut self, index: InstrId) -> Option<&mut OpCode> {
        self.opcodes.get_mut(index)
    }

    pub fn emit(&mut self, opcode: OpCode) -> Result<InstrId, EmitError> {
        use OpCode::*;

        // Keep stack state consistent
        match opcode {
            Invoke(slot) => {
                self.stack.adjust_to(slot)?;
                self.stack.make_variadic();
            }
            Return(_) => (),
            LoadConstant(_) | LoadStack(_) => {
                self.stack.push(None)?;
            }
            StoreStack(_) => {
                self.stack.pop()?;
            }
            AdjustStack(slot) => {
                self.stack.adjust_to(slot)?;
            }
            AriUnaOp(_) | BitUnaOp(_) => (),
            AriBinOp(_) | BitBinOp(_) | RelBinOp(_) | StrBinOp(_) => {
                self.stack.pop()?;
            }
            Jump { .. } | Loop { .. } => (),
            JumpIf { .. } | LoopIf { .. } => (),
            TabCreate => {
                self.stack.push(None)?;
            }
            TabGet => {
                self.stack.pop()?;
                self.stack.pop()?;
                self.stack.push(None)?;
            }
            TabSet => {
                self.stack.pop()?;
                self.stack.pop()?;
                self.stack.pop()?;
            }
        }

        let id = self.opcodes.emit(opcode)?;

        Ok(id)
    }

    pub fn stack_top(&self) -> Result<StackSlot, StackStateError> {
        self.stack.top()
    }

    pub fn emit_loop_to(&mut self, target: InstrId) -> Result<InstrId, EmitError> {
        let offset = self.next_instr()?.checked_sub(target).unwrap() + 1;
        self.emit(OpCode::Loop { offset })
    }

    pub fn emit_adjust_to(&mut self, slot: StackSlot) -> Result<Option<InstrId>, EmitError> {
        let needs_adjustment = self.stack.adjust_to(slot)?;

        let instr_id = if needs_adjustment {
            let instr_id = self.opcodes.emit(OpCode::AdjustStack(slot))?;

            Some(instr_id)
        } else {
            None
        };

        Ok(instr_id)
    }

    pub fn backpatch_to_next(&mut self, index: InstrId) -> Result<(), BackpatchError> {
        let target = self.next_instr()?;
        let new_offset = target
            .checked_sub(index + 1)
            .ok_or(BackpatchError::IndexOutOfBounds)?;
        match self.get_mut(index) {
            Some(OpCode::JumpIf { offset, .. })
            | Some(OpCode::Jump { offset })
            | Some(OpCode::Loop { offset })
            | Some(OpCode::LoopIf { offset, .. }) => {
                *offset = new_offset;

                Ok(())
            }
            _ => Err(BackpatchError::IndexOutOfBounds),
        }
    }

    pub fn push_block(&mut self) -> Result<(), StackStateError> {
        self.stack.push_block()
    }

    pub fn pop_block(&mut self) -> Result<(), EmitError> {
        let current = self.stack.top()?;
        let slot = self.stack.pop_block()?;

        // Remove excessive temporaries upon exiting frame.
        if slot <= current {
            // Use raw emit: we already popped temporaries off the stack.
            self.opcodes.emit(OpCode::AdjustStack(slot))?;
        }

        Ok(())
    }

    pub fn pop_ghost_block(&mut self) -> Result<(), EmitError> {
        let current = self.stack.top()?;
        let slot = self.stack.pop_ghost_block()?;

        // Remove excessive temporaries upon exiting frame.
        if slot <= current {
            // Use raw emit: we already popped temporaries off the stack.
            self.opcodes.emit(OpCode::AdjustStack(slot))?;
        }

        Ok(())
    }

    pub fn push_stack(&mut self, name: Option<&'s str>) -> Result<StackSlot, StackStateError> {
        self.stack.push(name)
    }

    pub(super) fn lookup_local(&self, ident: &str) -> Option<StackSlot> {
        self.stack.lookup_local(ident)
    }

    pub fn name_local(&mut self, ident: &'s str) -> Result<(), StackStateError> {
        self.stack.pop()?;
        self.stack.push(Some(ident))?;

        Ok(())
    }

    pub fn resolve(mut self, height: u32) -> Result<Function, (EmitError, Self)> {
        if let Err(e) = self.emit_adjust_to(StackSlot(0)) {
            return Err((e, self));
        }

        let FunctionTracker { opcodes, stack: _ } = self;

        let fun = opcodes.resolve(height);
        Ok(fun)
    }
}
