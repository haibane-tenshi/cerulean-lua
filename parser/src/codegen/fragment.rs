use std::ops::Add;
use thiserror::Error;

use crate::codegen::const_table::ConstTableView;
use crate::codegen::func_table::FuncTableView;
use crate::codegen::function::FunctionView;
use crate::codegen::stack::{
    BoundaryViolationError, NewBlockAtError, PopError, PushError, StackView,
};
use repr::index::{ConstCapacityError, InstrCountError, InstrId, StackSlot};
use repr::literal::Literal;
use repr::opcode::OpCode;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default)]
pub struct FragmentId(u32);

impl Add<u32> for FragmentId {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        let inner = self.0 + rhs;
        FragmentId(inner)
    }
}

#[derive(Debug)]
pub struct Fragment<'s, 'origin> {
    func_table: FuncTableView<'origin>,
    const_table: ConstTableView<'origin>,
    fun: FunctionView<'origin>,
    stack: StackView<'s, 'origin>,
}

impl<'s, 'origin> Fragment<'s, 'origin> {
    pub fn new(
        func_table: FuncTableView<'origin>,
        const_table: ConstTableView<'origin>,
        fun: FunctionView<'origin>,
        stack: StackView<'s, 'origin>,
    ) -> Self {
        Fragment {
            func_table,
            const_table,
            fun,
            stack,
        }
    }

    pub fn id(&self) -> FragmentId {
        self.fun.id()
    }

    pub fn const_table_mut(&mut self) -> &mut ConstTableView<'origin> {
        &mut self.const_table
    }

    pub fn func_table_mut(&mut self) -> &mut FuncTableView<'origin> {
        &mut self.func_table
    }

    pub fn stack(&self) -> &StackView<'s, 'origin> {
        &self.stack
    }

    pub fn stack_mut(&mut self) -> &mut StackView<'s, 'origin> {
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
                    self.stack.try_pop()?;
                }
                OpCode::Invoke(slot) => {
                    if slot > self.stack.raw_top() {
                        return Err(EmitError::InvokeOutsideStackBoundary);
                    }

                    // Stack space at `slot` and above is consumed during invocation.
                    // Function returns are always variadic.
                    self.stack.try_adjust_to(slot)?;
                    self.stack.make_variadic();
                }
                OpCode::LoadConstant(_) | OpCode::LoadStack(_) | OpCode::TabCreate => {
                    self.stack.try_push()?;
                }
                OpCode::StoreStack(_) => {
                    self.stack.try_pop()?;
                }
                OpCode::AdjustStack(slot) => {
                    self.stack.try_adjust_to(slot)?;
                }
                OpCode::UnaOp(_) => {
                    self.stack.try_pop()?;
                    self.stack.try_push()?;
                }
                OpCode::BinOp(_) => {
                    self.stack.try_pop()?;
                    self.stack.try_pop()?;
                    self.stack.try_push()?;
                }
                OpCode::Jump { .. }
                | OpCode::JumpIf { .. }
                | OpCode::Loop { .. }
                | OpCode::LoopIf { .. } => (),
                OpCode::TabGet => {
                    self.stack.try_pop()?;
                    self.stack.try_pop()?;
                    self.stack.try_push()?;
                }
                OpCode::TabSet => {
                    self.stack.try_pop()?;
                    self.stack.try_pop()?;
                    self.stack.try_pop()?;
                }
            }
        }

        self.fun.emit(instr).map_err(Into::into)
    }

    pub fn try_emit(&mut self, instr: OpCode) -> Result<InstrId, EmitError> {
        self.emit_raw(instr, true)
    }

    pub fn emit(&mut self, instr: OpCode) -> InstrId {
        self.try_emit(instr).unwrap()
    }

    pub fn try_emit_adjust_to(&mut self, slot: StackSlot) -> Result<Option<InstrId>, EmitError> {
        let need_adjustment = self.stack.try_adjust_to(slot)?;

        let instr_id = if need_adjustment {
            let id = self.emit_raw(OpCode::AdjustStack(slot), false)?;
            Some(id)
        } else {
            None
        };

        Ok(instr_id)
    }

    pub fn emit_adjust_to(&mut self, slot: StackSlot) -> Option<InstrId> {
        self.try_emit_adjust_to(slot).unwrap()
    }

    pub fn try_emit_jump_to(
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

        let instr_id = self.try_emit(opcode)?;
        self.fun.register_jump(target, instr_id, self.stack.state());

        Ok(instr_id)
    }

    pub fn emit_jump_to(&mut self, target: FragmentId, cond: Option<bool>) -> InstrId {
        self.try_emit_jump_to(target, cond).unwrap()
    }

    pub fn try_emit_loop_to(&mut self) -> Result<(), EmitError> {
        self.try_emit_adjust_to(self.stack.boundary())?;
        let offset = self.fun.len() - self.fun.start() + 1;
        self.try_emit(OpCode::Loop { offset })?;

        Ok(())
    }

    pub fn emit_loop_to(&mut self) {
        self.try_emit_loop_to().unwrap()
    }

    pub fn try_emit_load_literal(
        &mut self,
        literal: Literal,
    ) -> Result<InstrId, EmitLoadLiteralError> {
        let const_id = self.const_table.try_insert(literal)?;
        let instr_id = self.try_emit(OpCode::LoadConstant(const_id))?;

        Ok(instr_id)
    }

    pub fn emit_load_literal(&mut self, literal: Literal) -> InstrId {
        self.try_emit_load_literal(literal).unwrap()
    }

    // pub fn get_mut(&mut self, instr_id: InstrId) -> Option<&mut OpCode> {
    //     self.fun.get_mut(instr_id)
    // }
    //
    // pub fn len(&self) -> InstrId {
    //     self.fun.len()
    // }

    pub fn new_function<'a>(&'a mut self, func: FunctionView<'a>) -> Fragment<'s, 'a> {
        let Fragment {
            func_table,
            const_table,
            fun: _,
            stack,
        } = self;

        let func_table = func_table.new_view();
        let const_table = const_table.new_view();
        let stack = stack.new_frame();

        Fragment {
            func_table,
            const_table,
            fun: func,
            stack,
        }
    }

    pub fn new_fragment(&mut self) -> Fragment<'s, '_> {
        let Fragment {
            func_table,
            const_table,
            fun,
            stack,
        } = self;

        let func_table = func_table.new_view();
        let const_table = const_table.new_view();
        let fun = fun.new_block();
        let stack = stack.new_block();

        Fragment {
            func_table,
            const_table,
            fun,
            stack,
        }
    }

    pub fn try_new_fragment_at(
        &mut self,
        slot: StackSlot,
    ) -> Result<Fragment<'s, '_>, NewBlockAtError> {
        let Fragment {
            func_table,
            const_table,
            fun,
            stack,
        } = self;

        let func_table = func_table.new_view();
        let const_table = const_table.new_view();
        let fun = fun.new_block();
        let stack = stack.try_new_block_at(slot)?;

        let r = Fragment {
            func_table,
            const_table,
            fun,
            stack,
        };

        Ok(r)
    }

    pub fn new_fragment_at(&mut self, slot: StackSlot) -> Fragment<'s, '_> {
        self.try_new_fragment_at(slot).unwrap()
    }

    pub fn new_fragment_at_boundary(&mut self) -> Fragment<'s, '_> {
        self.new_fragment_at(self.stack.boundary())
    }

    fn commit_impl(self, preserve_stack: bool) {
        let Fragment {
            func_table,
            const_table,
            fun,
            mut stack,
        } = self;

        func_table.commit();
        const_table.commit();

        let sequence_state = fun.reachable().then(|| stack.state());
        let jump_state = fun.commit();

        let final_state = match (sequence_state, jump_state) {
            (Some(a), Some(b)) => Some(a | b),
            (Some(a), _) | (_, Some(a)) => Some(a),
            (None, None) => None,
        };

        if let Some(state) = final_state {
            stack.apply(state);
        }

        stack.commit(preserve_stack);
    }

    pub fn commit_scope(self) {
        self.commit_impl(false)
    }

    pub fn commit(self) {
        self.commit_impl(true)
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

#[derive(Debug, Error)]
pub enum EmitLoadLiteralError {
    #[error(transparent)]
    Emit(#[from] EmitError),
    #[error(transparent)]
    Literal(#[from] ConstCapacityError),
}
