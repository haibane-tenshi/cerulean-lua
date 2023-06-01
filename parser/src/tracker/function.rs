use std::collections::BTreeMap;
use thiserror::Error;

use repr::chunk::Function;
use repr::index::{InstrCountError, InstrId, StackSlot};
use repr::opcode::OpCode;

use super::{BlockId, OpCodeTracker, StackStateError, StackTracker};

#[derive(Debug, Error)]
pub enum BackpatchError {
    #[error(transparent)]
    InstrId(#[from] InstrCountError),

    #[error("there is no instruction under this index")]
    IndexOutOfBounds,
}

#[derive(Debug, Error)]
pub enum EmitError {
    #[error("reached erroneous stack state")]
    Stack(#[from] StackStateError),

    #[error("failed to emit instruction")]
    InstrId(#[from] InstrCountError),
}

#[derive(Debug, Default)]
struct BackpatchData {
    instructions: Vec<InstrId>,
    force_adjustment: bool,
}

#[derive(Debug, Default)]
pub struct FunctionTracker<'s> {
    opcodes: OpCodeTracker,
    stack: StackTracker<'s>,
    to_backpatch: BTreeMap<BlockId, BackpatchData>,
}

impl<'s> FunctionTracker<'s> {
    pub fn next_instr(&self) -> InstrId {
        self.opcodes.next()
    }

    // pub fn get(&self, index: InstrId) -> Option<&OpCode> {
    //     self.suspended.last()?.get(index)
    // }

    pub fn get_mut(&mut self, index: InstrId) -> Option<&mut OpCode> {
        self.opcodes.get_mut(index)
    }

    pub fn emit(&mut self, opcode: OpCode) -> Result<InstrId, EmitError> {
        self.emit_raw(opcode, true)
    }

    pub fn emit_raw(&mut self, opcode: OpCode, adjust_stack: bool) -> Result<InstrId, EmitError> {
        use OpCode::*;

        if adjust_stack {
            match opcode {
                Panic => {
                    // We leave current scope since panic can only be caught on function boundary.
                }
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
        }

        let id = self.opcodes.emit(opcode)?;

        Ok(id)
    }

    pub fn stack_top(&self) -> Result<StackSlot, StackStateError> {
        self.stack.top()
    }

    pub fn emit_loop_to(&mut self, target: InstrId) -> Result<InstrId, EmitError> {
        let offset = self.next_instr().checked_sub(target).unwrap() + 1;
        self.emit(OpCode::Loop { offset })
    }

    pub fn emit_jump_to_end_of(
        &mut self,
        target: BlockId,
        cond: Option<bool>,
    ) -> Result<InstrId, EmitError> {
        let opcode = match cond {
            Some(cond) => OpCode::JumpIf {
                cond,
                offset: Default::default(),
            },
            None => OpCode::Jump {
                offset: Default::default(),
            },
        };
        let instr_id = self.emit(opcode)?;

        let data = self.to_backpatch.entry(target).or_default();
        data.instructions.push(instr_id);
        data.force_adjustment |= self.stack.needs_adjustment_to_block_base(target)?;

        Ok(instr_id)
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
        let target = self.next_instr();
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

    pub fn start_block(&mut self) -> Result<BlockId, StackStateError> {
        self.stack.start_block()
    }

    pub fn finish_block(&mut self, block: BlockId) -> Result<StackSlot, EmitError> {
        let current = self.stack.top()?;
        let slot = self.stack.finish_block(block)?;

        // Stack have to be adjusted in two cases:
        // * current stack is above block base
        // * there is a jump to this point originating from stack position above block base
        let current_adjustment = slot < current;
        let jump_adjustment = self
            .to_backpatch
            .range(block..)
            .any(|data| data.1.force_adjustment);

        let jump_target = if current_adjustment || jump_adjustment {
            // Use raw emit: we already removed temporaries off the stack.
            self.emit_raw(OpCode::AdjustStack(slot), false)?
        } else {
            self.next_instr()
        };

        // Lastly we need to backpatch jumps from this block or any blocks above it.
        let to_backpatch = self
            .to_backpatch
            .split_off(&block)
            .into_values()
            .flat_map(|data| data.instructions);
        for index in to_backpatch {
            let new_offset = jump_target.checked_sub(index + 1).unwrap();

            match self.get_mut(index) {
                Some(OpCode::JumpIf { offset, .. })
                | Some(OpCode::Jump { offset })
                | Some(OpCode::Loop { offset })
                | Some(OpCode::LoopIf { offset, .. }) => {
                    *offset = new_offset;
                }
                _ => unreachable!(),
            }
        }

        Ok(slot)
    }

    pub fn push_stack(&mut self, name: Option<&'s str>) -> Result<StackSlot, StackStateError> {
        self.stack.push(name)
    }

    pub(crate) fn lookup_local(&self, ident: &str) -> Option<StackSlot> {
        self.stack.lookup_local(ident)
    }

    pub fn name_local(&mut self, slot: StackSlot, ident: &'s str) -> Result<(), StackStateError> {
        self.stack.name_local(slot, ident)
    }

    pub fn resolve(mut self, height: u32) -> Result<Function, (EmitError, Self)> {
        if let Err(e) = self.emit_adjust_to(StackSlot(0)) {
            return Err((e, self));
        }

        let FunctionTracker {
            opcodes,
            stack: _,
            to_backpatch: _,
        } = self;

        let fun = opcodes.resolve(height);
        Ok(fun)
    }
}
