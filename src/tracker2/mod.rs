pub(crate) mod stack;

use thiserror::Error;

use crate::index_vec::IndexVec;
use crate::opcode::{InstrCountError, InstrId, OpCode};

use stack::{BoundaryViolationError, PopError, PushError, StackView};

#[derive(Debug)]
pub enum Backpatch {}

#[derive(Debug)]
pub enum Instr {
    OpCode(OpCode),
    Unfinished(Backpatch),
}

impl From<OpCode> for Instr {
    fn from(value: OpCode) -> Self {
        Instr::OpCode(value)
    }
}

impl From<Backpatch> for Instr {
    fn from(value: Backpatch) -> Self {
        Instr::Unfinished(value)
    }
}

#[derive(Debug, Default)]
pub struct Fragment {
    opcodes: IndexVec<InstrId, Instr>,
}

#[derive(Debug)]
pub struct FragmentBuilder<'source, 'stack> {
    opcodes: IndexVec<InstrId, Instr>,
    stack: StackView<'source, 'stack>,
}

impl<'source, 'stack> FragmentBuilder<'source, 'stack> {
    pub fn new(stack: StackView<'source, 'stack>) -> Self {
        FragmentBuilder {
            opcodes: Default::default(),
            stack,
        }
    }

    pub fn emit_raw(
        &mut self,
        instr: impl Into<Instr>,
        adjust_stack: bool,
    ) -> Result<InstrId, EmitError> {
        let instr = instr.into();

        if adjust_stack {
            match instr {
                // This opcode never returns, so stack manipulation is irrelevant.
                // Any opcodes after this one are either unreachable,
                // or in case we get there through jumps,
                // presumably this instruction should be the end of the fragment.
                // Anyway, stack space must be manually brought into consistent state.
                Instr::OpCode(OpCode::Return(_)) => (),
                // This opcode never returns, however it grabs the top value as panic message.
                Instr::OpCode(OpCode::Panic) => {
                    self.stack.pop()?;
                }
                Instr::OpCode(OpCode::Invoke(slot)) => {
                    if slot > self.stack.top() {
                        return Err(EmitError::InvokeOutsideStackBoundary);
                    }

                    // Stack space at `slot` and above is consumed during invocation.
                    // Function returns are always variadic.
                    self.stack.adjust_to(slot)?;
                    self.stack.make_variadic();
                }
                Instr::OpCode(
                    OpCode::LoadConstant(_) | OpCode::LoadStack(_) | OpCode::TabCreate,
                ) => {
                    self.stack.push()?;
                }
                Instr::OpCode(OpCode::StoreStack(_)) => {
                    self.stack.pop()?;
                }
                Instr::OpCode(OpCode::AdjustStack(slot)) => {
                    self.stack.adjust_to(slot)?;
                }
                Instr::OpCode(OpCode::AriUnaOp(_) | OpCode::BitUnaOp(_)) => {
                    self.stack.pop()?;
                    self.stack.push()?;
                }
                Instr::OpCode(
                    OpCode::AriBinOp(_)
                    | OpCode::BitBinOp(_)
                    | OpCode::RelBinOp(_)
                    | OpCode::StrBinOp(_),
                ) => {
                    self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.push()?;
                }
                Instr::OpCode(
                    OpCode::Jump { .. }
                    | OpCode::JumpIf { .. }
                    | OpCode::Loop { .. }
                    | OpCode::LoopIf { .. },
                ) => (),
                Instr::OpCode(OpCode::TabGet) => {
                    self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.push()?;
                }
                Instr::OpCode(OpCode::TabSet) => {
                    self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.pop()?;
                }
                Instr::Unfinished(backpatch) => match backpatch {},
            }
        }

        self.opcodes.push(instr).map_err(Into::into)
    }

    pub fn emit(&mut self, instr: impl Into<Instr>) -> Result<InstrId, EmitError> {
        self.emit_raw(instr, true)
    }

    pub fn inline(&mut self, other: Fragment) -> Result<(), InstrCountError> {
        self.opcodes.extend(other.opcodes)
    }

    pub fn finalize(self) -> Fragment {
        let FragmentBuilder { opcodes, stack: _ } = self;

        Fragment { opcodes }
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
