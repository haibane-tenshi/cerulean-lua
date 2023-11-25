use std::ops::Add;
use thiserror::Error;
use tracing::trace;

use repr::index::{InstrId, StackSlot, UpvalueSlot};
use repr::literal::Literal;
use repr::opcode::OpCode;

use super::Ident;
use crate::codegen::const_table::{ConstTable, ConstTableView};
use crate::codegen::func_table::{FuncTable, FuncTableView};
use crate::codegen::function::{Function, FunctionView, Signature};
use crate::codegen::jumps::{Jumps, JumpsView};
use crate::codegen::labels::{Labels, LabelsView, PushLabelError};
use crate::codegen::loop_stack::LoopStack;
use crate::codegen::reachability::Reachability;
use crate::codegen::recipe_table::{RecipeTable, RecipeTableView};
use crate::codegen::stack::{
    BoundaryViolationError, CommitKind, FragmentStackSlot, PopError, PushError, Stack, StackView,
};
use crate::codegen::upvalues::{Upvalues, UpvaluesView};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default)]
pub struct FragmentId(u32);

impl Add<u32> for FragmentId {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        let inner = self.0 + rhs;
        FragmentId(inner)
    }
}

pub enum UpvalueSource {
    Temporary(StackSlot),
    Upvalue(UpvalueSlot),
}

impl From<UpvalueSource> for repr::chunk::UpvalueSource {
    fn from(value: UpvalueSource) -> repr::chunk::UpvalueSource {
        use repr::chunk::UpvalueSource::*;

        match value {
            UpvalueSource::Temporary(slot) => Temporary(slot),
            UpvalueSource::Upvalue(slot) => Upvalue(slot),
        }
    }
}

#[derive(Debug)]
pub struct Core<'s, 'origin> {
    fragment_id: FragmentId,
    func_table: &'origin mut FuncTable,
    const_table: &'origin mut ConstTable,
    recipe_table: &'origin mut RecipeTable,
    fun: &'origin mut Function,
    upvalues: &'origin mut Upvalues<'s>,
    stack: &'origin mut Stack<'s>,
    jumps: &'origin mut Jumps,
    labels: &'origin mut Labels<'s>,
    loop_stack: LoopStack,
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

    pub fn fragment_at(self, kind: CommitKind, slot: FragmentStackSlot) -> Fragment<'s, 'origin> {
        Fragment::new_at(self, kind, slot)
    }

    pub fn scope_at(self, slot: FragmentStackSlot) -> Fragment<'s, 'origin> {
        self.fragment_at(CommitKind::Scope, slot)
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
    recipe_table: RecipeTableView<'origin>,
    fun: FunctionView<'origin>,
    upvalues: UpvaluesView<'s, 'origin>,
    stack: StackView<'s, 'origin>,
    jumps: JumpsView<'origin>,
    labels: LabelsView<'s, 'origin>,
    loop_stack: LoopStack,
    reachability: Reachability,
    kind: CommitKind,
}

impl<'s, 'origin> Fragment<'s, 'origin> {
    pub fn new(core: Core<'s, 'origin>, kind: CommitKind) -> Self {
        let Core {
            fragment_id,
            func_table,
            const_table,
            recipe_table,
            fun,
            upvalues,
            stack,
            jumps,
            labels,
            loop_stack,
            reachability,
        } = core;

        let fragment_id = fragment_id + 1;
        let func_table = func_table.view();
        let const_table = const_table.view();
        let recipe_table = recipe_table.view();
        let fun = fun.view();
        let upvalues = upvalues.view();
        let stack = stack.view();
        let jumps = jumps.view(fragment_id, fun.len());
        let labels = if kind == CommitKind::Scope {
            labels.view_scope(fun.len())
        } else {
            labels.view_expr()
        };

        // trace!(?fragment_id, "construct");

        Fragment {
            fragment_id,
            func_table,
            const_table,
            recipe_table,
            fun,
            upvalues,
            stack,
            jumps,
            labels,
            loop_stack,
            reachability,
            kind,
        }
    }

    pub fn new_at(core: Core<'s, 'origin>, kind: CommitKind, slot: FragmentStackSlot) -> Self {
        let Core {
            fragment_id,
            func_table,
            const_table,
            recipe_table,
            fun,
            upvalues,
            stack,
            jumps,
            labels,
            loop_stack,
            reachability,
        } = core;

        let fragment_id = fragment_id + 1;
        let func_table = func_table.view();
        let const_table = const_table.view();
        let recipe_table = recipe_table.view();
        let fun = fun.view();
        let upvalues = upvalues.view();
        let stack = stack.view_at(slot);
        let jumps = jumps.view(fragment_id, fun.len());
        let labels = if kind == CommitKind::Scope {
            labels.view_scope(fun.len())
        } else {
            labels.view_expr()
        };

        // trace!(?fragment_id, "construct");

        Fragment {
            fragment_id,
            func_table,
            const_table,
            recipe_table,
            fun,
            upvalues,
            stack,
            jumps,
            labels,
            loop_stack,
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

    pub fn recipe_table_mut(&mut self) -> &mut RecipeTableView<'origin> {
        &mut self.recipe_table
    }

    pub fn stack(&self) -> &StackView<'s, 'origin> {
        &self.stack
    }

    pub fn capture_variable(&mut self, ident: Ident<'s>) -> Option<UpvalueSource> {
        use super::stack::NameLookup;

        match self.stack.lookup(ident) {
            NameLookup::Local(slot) => {
                trace!(fragment_id=?self.id(), %ident, ?slot, "lookup variable");

                Some(UpvalueSource::Temporary(slot))
            }
            NameLookup::Upvalue => {
                let slot = self.upvalues.register(ident);

                trace!(fragment_id=?self.id(), %ident, ?slot, "capture variable");

                Some(UpvalueSource::Upvalue(slot))
            }
            NameLookup::Global => None,
        }
    }

    pub fn capture_global_env(&mut self) -> Result<UpvalueSource, MissingGlobalEnvError> {
        self.capture_variable(Ident::env())
            .ok_or(MissingGlobalEnvError)
    }

    pub fn signature(&self) -> &Signature {
        self.fun.signature()
    }

    pub fn stack_slot(&self, slot: FragmentStackSlot) -> StackSlot {
        self.stack.fragment_to_frame(slot)
    }

    pub fn mark_as_loop(&mut self) {
        trace!(fragment_id=?self.id(), "mark as loop body");

        self.loop_stack.push(self.id())
    }

    pub fn try_emit(&mut self, opcode: OpCode) -> Result<InstrId, EmitError> {
        self.stack.emit(&opcode)?;
        self.reachability.emit(&opcode);
        let r = self.fun.emit(opcode);

        trace!(fragment_id=?self.id(), ?opcode, "emit opcode");

        Ok(r)
    }

    pub fn push_temporary(&mut self, name: Option<Ident<'s>>) -> FragmentStackSlot {
        if name.is_some() {
            let marker = self.fun.len();
            self.labels.push_last_binding(marker);
        }

        let r = self.stack.push(name);

        trace!(fragment_id=?self.id(), ?name, "push temporary");

        r
    }

    pub fn pop_temporary(&mut self) {
        self.stack.pop();

        trace!(fragment_id=?self.id(), "pop temporary");
    }

    pub fn adjust_stack_to(&mut self, height: FragmentStackSlot) -> bool {
        let r = self.stack.adjust_to(height);

        trace!(fragment_id=?self.id(), ?height, "adjust stack");

        r
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

        trace!(fragment_id=?self.id(), ?slot, "emitted optional AdjustStack");

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

        trace!(fragment_id=?self.id(), ?target, "emit jump to end of target fragment");

        Ok(instr_id)
    }

    pub fn emit_jump_to(&mut self, target: FragmentId, cond: Option<bool>) -> InstrId {
        self.try_emit_jump_to(target, cond).unwrap()
    }

    pub fn try_emit_loop_to(&mut self) -> Result<(), EmitError> {
        self.try_emit_adjust_to(FragmentStackSlot(0))?;
        let offset = self.fun.len() - self.fun.start() + 1;
        self.try_emit(OpCode::Loop { offset })?;

        trace!(fragment_id=?self.id(), "emit jump to start of current fragment");

        Ok(())
    }

    pub fn emit_loop_to(&mut self) {
        self.try_emit_loop_to().unwrap()
    }

    pub fn try_emit_load_literal(
        &mut self,
        literal: Literal,
    ) -> Result<InstrId, EmitLoadLiteralError> {
        let const_id = self.const_table.insert(literal);
        let instr_id = self.try_emit(OpCode::LoadConstant(const_id))?;

        Ok(instr_id)
    }

    pub fn emit_load_literal(&mut self, literal: Literal) -> InstrId {
        self.try_emit_load_literal(literal).unwrap()
    }

    pub fn try_emit_label(&mut self, label: Ident<'s>) -> Result<InstrId, PushLabelError> {
        use super::labels::Label;

        // Since it is possible to reach label from unknown future point,
        // stack is effectively in variadic state.
        // We need to forecefully adjust it.
        let stack_top = self.stack().len();
        self.stack.make_variadic();
        let instr_id = self.emit_adjust_to(stack_top).unwrap();

        let label = Label {
            name: label,
            target: instr_id,
        };
        self.labels.push_label(label, self.fun.borrow().view())?;

        // Unfortunally, we don't have a luxury to perform full codeflow analysis.
        // To keep things simple we assume that labels are always reachable.
        // This worsens codegen in some cases, but those should be rather rare.
        self.reachability.make_reachable();

        trace!(fragment_id=?self.id(), ?label, "emit label");

        Ok(instr_id)
    }

    pub fn try_emit_goto(&mut self, label: Ident<'s>) -> Result<InstrId, EmitError> {
        let target = self.fun.len();

        let opcode = self.labels.goto(label, target);
        let r = self.try_emit(opcode)?;

        trace!(fragment_id=?self.id(), ?label, "emit jump to label");

        Ok(r)
    }

    pub fn emit_goto(&mut self, label: Ident<'s>) -> InstrId {
        self.try_emit_goto(label).unwrap()
    }

    pub fn emit_break(&mut self) -> Result<InstrId, BreakOutsideLoopError> {
        let target = self
            .loop_stack
            .innermost_loop()
            .ok_or(BreakOutsideLoopError)?;
        let instr_id = self.emit_jump_to(target, None);

        trace!(fragment_id=?self.id(), "emit break statement");

        Ok(instr_id)
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
            recipe_table,
            fun,
            upvalues,
            stack,
            jumps,
            labels,
            loop_stack,
            reachability,
            kind: _,
        } = self;

        let fragment_id = *fragment_id;
        let func_table = func_table.borrow();
        let const_table = const_table.borrow();
        let recipe_table = recipe_table.borrow();
        let fun = fun.borrow();
        let upvalues = upvalues.borrow();
        let stack = stack.borrow();
        let jumps = jumps.borrow();
        let labels = labels.borrow();
        let loop_stack = *loop_stack;
        let reachability = *reachability;

        Core {
            fragment_id,
            func_table,
            const_table,
            recipe_table,
            fun,
            upvalues,
            stack,
            jumps,
            labels,
            loop_stack,
            reachability,
        }
    }

    // pub fn new_frame(&mut self, signature: Signature) -> Frame<'s, '_> {
    //     Frame::new(self.new_core(), signature)
    // }

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
            fragment_id,
            func_table,
            const_table,
            recipe_table,
            mut fun,
            upvalues,
            mut stack,
            jumps,
            labels,
            loop_stack: _,
            reachability,
            kind,
        } = self;

        let is_reachable = reachability.commit();

        func_table.commit();
        const_table.commit();
        recipe_table.commit();

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
            fun.emit(OpCode::AdjustStack(slot));
        }

        labels.commit(kind);
        fun.commit();
        upvalues.commit();
        stack.commit(kind);

        trace!(?fragment_id, "commit");
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
}

#[derive(Debug, Error)]
pub enum EmitLoadLiteralError {
    #[error(transparent)]
    Emit(#[from] EmitError),
}

#[derive(Debug)]
pub struct Frame<'s, 'origin> {
    func_table: FuncTableView<'origin>,
    const_table: ConstTableView<'origin>,
    recipe_table: RecipeTableView<'origin>,
    stack: StackView<'s, 'origin>,
    fun: Function,
    upvalues: Upvalues<'s>,
    jumps: Jumps,
    labels: Labels<'s>,
}

impl<'s, 'origin> Frame<'s, 'origin> {
    pub fn new(core: Core<'s, 'origin>, signature: Signature) -> Self {
        let Core {
            func_table,
            const_table,
            recipe_table,
            stack,
            ..
        } = core;

        let func_table = func_table.view();
        let const_table = const_table.view();
        let recipe_table = recipe_table.view();
        let stack = stack.frame();
        let fun = Function::new(signature);
        let upvalues = Upvalues::new();
        let jumps = Jumps::new();
        let labels = Labels::new(InstrId(0));

        Frame {
            func_table,
            const_table,
            recipe_table,
            stack,
            fun,
            upvalues,
            jumps,
            labels,
        }
    }

    pub fn script(
        func_table: FuncTableView<'origin>,
        const_table: ConstTableView<'origin>,
        recipe_table: RecipeTableView<'origin>,
        stack: StackView<'s, 'origin>,
        signature: Signature,
    ) -> Self {
        let mut upvalues = Upvalues::new();
        let mut upvalue_view = upvalues.view();
        upvalue_view.register(Ident::env());
        upvalue_view.commit();

        Frame {
            func_table,
            const_table,
            recipe_table,
            stack,
            fun: Function::new(signature),
            upvalues,
            jumps: Jumps::new(),
            labels: Labels::new(InstrId(0)),
        }
    }

    pub fn new_core(&mut self) -> Core<'s, '_> {
        let Frame {
            func_table,
            const_table,
            recipe_table,
            stack,
            fun,
            upvalues,
            jumps,
            labels,
        } = self;

        let fragment_id = FragmentId::default();
        let func_table = func_table.borrow();
        let const_table = const_table.borrow();
        let recipe_table = recipe_table.borrow();
        let stack = stack.borrow();
        let loop_stack = LoopStack::new();
        let reachability = Reachability::new();

        Core {
            fragment_id,
            func_table,
            const_table,
            recipe_table,
            fun,
            upvalues,
            stack,
            jumps,
            labels,
            loop_stack,
            reachability,
        }
    }

    pub fn commit(self) -> Result<(Function, Upvalues<'s>), UnresolvedGotoError> {
        let Frame {
            func_table,
            const_table,
            recipe_table,
            stack,
            fun,
            upvalues,
            jumps: _,
            labels,
        } = self;

        func_table.commit();
        const_table.commit();
        recipe_table.commit();
        stack.commit(CommitKind::Scope);

        if labels.is_resolved() {
            Ok((fun, upvalues))
        } else {
            Err(UnresolvedGotoError)
        }
    }
}

#[derive(Debug, Error)]
#[error("goto statement is missing its label")]
pub struct UnresolvedGotoError;

#[derive(Debug, Error)]
#[error("break statement is used outside a loop")]
pub struct BreakOutsideLoopError;

#[derive(Debug, Error)]
#[error("could not lookup global environment")]
pub struct MissingGlobalEnvError;
