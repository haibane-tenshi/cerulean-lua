use std::ops::Add;
use thiserror::Error;

use crate::codegen::const_table::{ConstTable, ConstTableView};
use crate::codegen::func_table::{FuncTable, FuncTableView};
use crate::codegen::function::{Function, FunctionView};
use crate::codegen::jumps::{Jumps, JumpsView};
use crate::codegen::reachability::Reachability;
use crate::codegen::stack::{
    BoundaryViolationError, CommitKind, FragmentStackSlot, PopError, PushError, Stack, StackView,
};
use repr::chunk::Signature;
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
pub struct Core<'s, 'origin> {
    fragment_id: FragmentId,
    func_table: &'origin mut FuncTable,
    const_table: &'origin mut ConstTable,
    fun: &'origin mut Function,
    stack: &'origin mut Stack<'s>,
    jumps: &'origin mut Jumps,
    reachability: Reachability,
}

impl<'s, 'origin> Core<'s, 'origin> {
    pub fn fragment(self, kind: CommitKind) -> Fragment<'s, 'origin> {
        Fragment::new(self, kind)
    }

    pub fn scope(self) -> Fragment<'s, 'origin> {
        self.fragment(CommitKind::Scope)
    }

    pub fn expr(self) -> Fragment<'s, 'origin> {
        self.fragment(CommitKind::Expr)
    }

    pub fn decl(self) -> Fragment<'s, 'origin> {
        self.fragment(CommitKind::Decl)
    }

    pub fn frame(self, signature: Signature) -> Frame<'s, 'origin> {
        Frame::new(self, signature)
    }
}

#[derive(Debug)]
pub struct Fragment<'s, 'origin> {
    fragment_id: FragmentId,
    func_table: FuncTableView<'origin>,
    const_table: ConstTableView<'origin>,
    fun: FunctionView<'origin>,
    stack: StackView<'s, 'origin>,
    jumps: JumpsView<'origin>,
    reachability: Reachability,
    kind: CommitKind,
}

impl<'s, 'origin> Fragment<'s, 'origin> {
    pub fn new(core: Core<'s, 'origin>, kind: CommitKind) -> Self {
        let Core {
            fragment_id,
            func_table,
            const_table,
            fun,
            stack,
            jumps,
            reachability,
        } = core;

        let fragment_id = fragment_id + 1;
        let func_table = func_table.view();
        let const_table = const_table.view();
        let fun = fun.view();
        let stack = stack.view();
        let jumps = jumps.view(fragment_id, fun.len());

        Fragment {
            fragment_id,
            func_table,
            const_table,
            fun,
            stack,
            jumps,
            reachability,
            kind,
        }
    }

    pub fn new_at(core: Core<'s, 'origin>, kind: CommitKind, slot: FragmentStackSlot) -> Self {
        let Core {
            fragment_id,
            func_table,
            const_table,
            fun,
            stack,
            jumps,
            reachability,
        } = core;

        let fragment_id = fragment_id + 1;
        let func_table = func_table.view();
        let const_table = const_table.view();
        let fun = fun.view();
        let stack = stack.view_at(slot);
        let jumps = jumps.view(fragment_id, fun.len());

        Fragment {
            fragment_id,
            func_table,
            const_table,
            fun,
            stack,
            jumps,
            reachability,
            kind,
        }
    }

    pub fn id(&self) -> FragmentId {
        self.fragment_id
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

    pub fn fun_mut(&mut self) -> &mut FunctionView<'origin> {
        &mut self.fun
    }

    pub fn signature(&self) -> Signature {
        self.fun.signature()
    }

    pub fn stack_slot(&self, slot: FragmentStackSlot) -> StackSlot {
        self.stack.fragment_to_frame(slot)
    }

    pub fn try_emit(&mut self, instr: OpCode) -> Result<InstrId, EmitError> {
        self.stack.emit(&instr)?;
        self.reachability.emit(&instr);
        let r = self.fun.emit(instr)?;

        Ok(r)
    }

    pub fn emit(&mut self, instr: OpCode) -> InstrId {
        self.try_emit(instr).unwrap()
    }

    pub fn try_emit_adjust_to(
        &mut self,
        slot: FragmentStackSlot,
    ) -> Result<Option<InstrId>, EmitError> {
        let instr_id = if self.stack.need_adjustment_to(slot) {
            let slot = self.stack.fragment_to_frame(slot);
            let id = self.try_emit(OpCode::AdjustStack(slot))?;
            Some(id)
        } else {
            None
        };

        Ok(instr_id)
    }

    pub fn emit_adjust_to(&mut self, slot: FragmentStackSlot) -> Option<InstrId> {
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
        self.jumps
            .register_jump(target, instr_id, self.stack.state());

        Ok(instr_id)
    }

    pub fn emit_jump_to(&mut self, target: FragmentId, cond: Option<bool>) -> InstrId {
        self.try_emit_jump_to(target, cond).unwrap()
    }

    pub fn try_emit_loop_to(&mut self) -> Result<(), EmitError> {
        self.try_emit_adjust_to(FragmentStackSlot(0))?;
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

    pub fn new_core(&mut self) -> Core<'s, '_> {
        let Fragment {
            fragment_id,
            func_table,
            const_table,
            fun,
            stack,
            jumps,
            reachability,
            kind: _,
        } = self;

        let fragment_id = *fragment_id;
        let func_table = func_table.borrow();
        let const_table = const_table.borrow();
        let fun = fun.borrow();
        let stack = stack.borrow();
        let jumps = jumps.borrow();
        let reachability = *reachability;

        Core {
            fragment_id,
            func_table,
            const_table,
            fun,
            stack,
            jumps,
            reachability,
        }
    }

    pub fn new_frame(&mut self, signature: Signature) -> Frame<'s, '_> {
        Frame::new(self.new_core(), signature)
    }

    pub fn new_fragment(&mut self, kind: CommitKind) -> Fragment<'s, '_> {
        Fragment::new(self.new_core(), kind)
    }

    pub fn new_scope(&mut self) -> Fragment<'s, '_> {
        self.new_fragment(CommitKind::Scope)
    }

    pub fn new_expr(&mut self) -> Fragment<'s, '_> {
        self.new_fragment(CommitKind::Expr)
    }

    pub fn new_fragment_at(
        &mut self,
        kind: CommitKind,
        slot: FragmentStackSlot,
    ) -> Fragment<'s, '_> {
        Fragment::new_at(self.new_core(), kind, slot)
    }

    pub fn new_expr_at(&mut self, slot: FragmentStackSlot) -> Fragment<'s, '_> {
        self.new_fragment_at(CommitKind::Expr, slot)
    }

    pub fn new_fragment_at_boundary(&mut self, kind: CommitKind) -> Fragment<'s, '_> {
        self.new_fragment_at(kind, FragmentStackSlot(0))
    }

    pub fn commit(self) {
        let Fragment {
            fragment_id: _,
            func_table,
            const_table,
            mut fun,
            mut stack,
            jumps,
            reachability,
            kind,
        } = self;

        let is_reachable = reachability.commit();

        func_table.commit();
        const_table.commit();

        let sequence_state = is_reachable.then(|| stack.state());
        let jump_state = jumps.commit(&mut fun);

        let final_state = match (sequence_state, jump_state) {
            (Some(a), Some(b)) => Some(a | b),
            (Some(a), _) | (_, Some(a)) => Some(a),
            (None, None) => None,
        };

        if let Some(state) = final_state {
            stack.apply(state);
        }

        if matches!(kind, CommitKind::Scope)
            && final_state.is_some()
            && stack.need_adjustment_to(FragmentStackSlot(0))
        {
            let slot = stack.fragment_to_frame(FragmentStackSlot(0));
            fun.emit(OpCode::AdjustStack(slot)).unwrap();
        }

        fun.commit();
        stack.commit(kind);
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

#[derive(Debug)]
pub struct Frame<'s, 'origin> {
    func_table: FuncTableView<'origin>,
    const_table: ConstTableView<'origin>,
    stack: StackView<'s, 'origin>,
    fun: Function,
    jumps: Jumps,
}

impl<'s, 'origin> Frame<'s, 'origin> {
    pub fn new(core: Core<'s, 'origin>, signature: Signature) -> Self {
        let Core {
            func_table,
            const_table,
            stack,
            ..
        } = core;

        let func_table = func_table.view();
        let const_table = const_table.view();
        let stack = stack.frame();

        Frame {
            func_table,
            const_table,
            stack,
            fun: Function::new(signature),
            jumps: Jumps::new(),
        }
    }

    pub fn script(
        func_table: FuncTableView<'origin>,
        const_table: ConstTableView<'origin>,
        stack: StackView<'s, 'origin>,
        signature: Signature,
    ) -> Self {
        Frame {
            func_table,
            const_table,
            stack,
            fun: Function::new(signature),
            jumps: Jumps::new(),
        }
    }

    pub fn new_core(&mut self) -> Core<'s, '_> {
        let Frame {
            func_table,
            const_table,
            stack,
            fun,
            jumps,
        } = self;

        let fragment_id = FragmentId::default();
        let func_table = func_table.borrow();
        let const_table = const_table.borrow();
        let stack = stack.borrow();
        let reachability = Reachability::new();

        Core {
            fragment_id,
            func_table,
            const_table,
            fun,
            stack,
            jumps,
            reachability,
        }
    }

    pub fn commit(self) -> Function {
        let Frame {
            func_table,
            const_table,
            stack,
            fun,
            jumps: _,
        } = self;

        func_table.commit();
        const_table.commit();
        stack.commit(CommitKind::Scope);

        fun
    }
}
