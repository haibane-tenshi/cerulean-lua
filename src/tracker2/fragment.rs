use std::ops::Add;
use thiserror::Error;

use crate::opcode::{InstrCountError, InstrId, OpCode, StackSlot};
use crate::tracker2::function::{Function, FunctionView};
use crate::tracker2::stack::{BoundaryViolationError, PopError, PushError, StackView};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default)]
pub struct FragmentId(u32);

impl Add<u32> for FragmentId {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        let inner = self.0 + rhs;
        FragmentId(inner)
    }
}

pub struct Fragment<'s, 'fun, 'stack> {
    fun: FunctionView<'fun>,
    stack: StackView<'s, 'stack>,
}

impl<'s, 'fun, 'stack> Fragment<'s, 'fun, 'stack> {
    pub fn new(fun: &'fun mut Function, stack: StackView<'s, 'stack>) -> Self {
        let fun = FunctionView::new(fun);

        Fragment { fun, stack }
    }

    pub fn id(&self) -> FragmentId {
        self.fun.id()
    }

    pub fn stack(&self) -> &StackView<'s, 'stack> {
        &self.stack
    }

    pub fn stack_mut(&mut self) -> &mut StackView<'s, 'stack> {
        &mut self.stack
    }

    pub fn emit_raw(&mut self, instr: OpCode, adjust_stack: bool) -> Result<InstrId, EmitError> {
        if adjust_stack {
            match instr {
                // This opcode never returns, so stack manipulation is irrelevant.
                // Any opcodes after this one are either unreachable,
                // or in case we get there through jumps,
                // presumably this instruction should be the end of the fragment.
                // Anyway, stack space must be manually brought into consistent state.
                OpCode::Return(_) => (),
                // This opcode never returns, however it grabs the top value as panic message.
                OpCode::Panic => {
                    self.stack.pop()?;
                }
                OpCode::Invoke(slot) => {
                    if slot > self.stack.raw_top() {
                        return Err(EmitError::InvokeOutsideStackBoundary);
                    }

                    // Stack space at `slot` and above is consumed during invocation.
                    // Function returns are always variadic.
                    self.stack.adjust_to(slot)?;
                    self.stack.make_variadic();
                }

                OpCode::LoadConstant(_) | OpCode::LoadStack(_) | OpCode::TabCreate => {
                    self.stack.push()?;
                }
                OpCode::StoreStack(_) => {
                    self.stack.pop()?;
                }
                OpCode::AdjustStack(slot) => {
                    self.stack.adjust_to(slot)?;
                }
                OpCode::AriUnaOp(_) | OpCode::BitUnaOp(_) => {
                    self.stack.pop()?;
                    self.stack.push()?;
                }

                OpCode::AriBinOp(_)
                | OpCode::BitBinOp(_)
                | OpCode::RelBinOp(_)
                | OpCode::StrBinOp(_) => {
                    self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.push()?;
                }

                OpCode::Jump { .. }
                | OpCode::JumpIf { .. }
                | OpCode::Loop { .. }
                | OpCode::LoopIf { .. } => (),
                OpCode::TabGet => {
                    self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.push()?;
                }
                OpCode::TabSet => {
                    self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.pop()?;
                }
            }
        }

        self.fun.emit(instr).map_err(Into::into)
    }

    pub fn emit(&mut self, instr: OpCode) -> Result<InstrId, EmitError> {
        self.emit_raw(instr, true)
    }

    pub fn emit_adjust_to(&mut self, slot: StackSlot) -> Result<Option<InstrId>, EmitError> {
        let need_adjustment = self.stack.adjust_to(slot)?;

        let instr_id = if need_adjustment {
            let id = self.emit_raw(OpCode::AdjustStack(slot), false)?;
            Some(id)
        } else {
            None
        };

        Ok(instr_id)
    }

    pub fn emit_jump_to(
        &mut self,
        target: FragmentId,
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
        self.fun.register_jump(target, instr_id);

        Ok(instr_id)
    }

    pub fn emit_loop_to(&mut self) -> Result<(), EmitError> {
        self.emit_adjust_to(self.stack.boundary())?;
        let offset = self.fun.len() - self.fun.start() + 1;
        self.emit(OpCode::Loop { offset })?;

        Ok(())
    }

    pub fn get_mut(&mut self, instr_id: InstrId) -> Option<&mut OpCode> {
        self.fun.get_mut(instr_id)
    }

    pub fn len(&self) -> InstrId {
        self.fun.len()
    }

    pub fn new_fragment<'a>(&'a mut self) -> Fragment<'s, 'a, 'a> {
        let Fragment { fun, stack } = self;

        let fun = fun.new_block();
        let stack = stack.new_block();

        Fragment { fun, stack }
    }

    pub fn commit(self) {
        let Fragment { fun, stack } = self;

        fun.commit();
        stack.commit();
    }
}

#[derive(Debug, Error)]
#[error("failed to emit instruction")]
pub enum EmitError {
    PopStack(#[from] PopError),
    PushStack(#[from] PushError),
    AdjustStack(#[from] BoundaryViolationError),
    #[error("invoked pointing at stack slot above stack top")]
    InvokeOutsideStackBoundary,
    InstrCount(#[from] InstrCountError),
}
