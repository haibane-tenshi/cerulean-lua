use std::ops::Add;
use thiserror::Error;

use crate::opcode::{InstrCountError, InstrId, OpCode};
use crate::tracker2::function::{Function, FunctionView};
use crate::tracker2::stack::{BoundaryViolationError, PopError, PushError, StackView};

#[derive(Debug, Copy, Clone, Default)]
pub struct FragmentId(u32);

impl Add<u32> for FragmentId {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        let inner = self.0 + rhs;
        FragmentId(inner)
    }
}

pub struct Fragment<'s, 'fun, 'stack> {
    fragment_id: FragmentId,
    fun: FunctionView<'fun>,
    stack: StackView<'s, 'stack>,
}

impl<'s, 'fun, 'stack> Fragment<'s, 'fun, 'stack> {
    pub fn new(fun: &'fun mut Function, stack: StackView<'s, 'stack>) -> Self {
        let fun = FunctionView::new(fun);

        Fragment {
            fragment_id: Default::default(),
            fun,
            stack,
        }
    }

    pub fn emit_raw(&mut self, instr: OpCode, adjust_stack: bool) -> Result<InstrId, EmitError> {
        let instr = instr.into();

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
                    if slot > self.stack.top() {
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

    pub fn new_block<'a>(&'a mut self) -> Fragment<'s, 'a, 'a> {
        let Fragment {
            fragment_id,
            fun,
            stack,
        } = self;

        let fragment_id = *fragment_id + 1;
        let fun = fun.new_block();
        let stack = stack.new_block();

        Fragment {
            fragment_id,
            fun,
            stack,
        }
    }

    pub fn finalize(self) {
        let Fragment {
            fragment_id: _,
            fun,
            stack,
        } = self;

        fun.finalize();
        stack.finalize();
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
