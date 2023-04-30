use crate::lex::Lexer;
use crate::opcode::{ConstId, InstrId, StackSlot};
use crate::parser::{LexParseError, Optional, Require};
use crate::tracker::{ChunkTracker, EmitError, FunctionTracker};

#[derive(Debug, Copy, Clone)]
enum StackSlotOrConstId {
    StackSlot(StackSlot),
    ConstId(ConstId),
}

impl StackSlotOrConstId {
    fn load(self, tracker: &mut FunctionTracker) -> Result<InstrId, EmitError> {
        use crate::opcode::OpCode;
        use StackSlotOrConstId::*;

        match self {
            StackSlot(slot) => tracker.emit(OpCode::LoadStack(slot)),
            ConstId(const_id) => tracker.emit(OpCode::LoadConstant(const_id)),
        }
    }
}

pub(super) fn numerical_for<'s, 'a>(
    s: Lexer<'s>,
    tracker: &'a mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::opcode::{AriBinOp, OpCode, RelBinOp};
    use crate::parser::{block, expr_adjusted_to_1, identifier, match_token};
    use crate::value::Literal;

    let (s, ()) = match_token(s, Token::For)?;
    let (s, ident) = identifier(s).require()?;
    let (s, ()) = match_token(s, Token::Assign).require()?;

    tracker.current_mut()?.push_block()?;

    let loop_var = tracker.current()?.stack_top()?;
    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;
    tracker.current_mut()?.name_local(loop_var, ident)?;

    let (s, ()) = match_token(s, Token::Comma).require()?;

    let limit = tracker.current()?.stack_top()?;
    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;

    let mut maybe_step = |s: Lexer<'s>| -> Result<(Lexer<'s>, StackSlot), LexParseError> {
        let (s, ()) = match_token(s, Token::Comma).require()?;

        let step = tracker.current()?.stack_top()?;
        let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;

        Ok((s, step))
    };

    let (s, step) = maybe_step(s.clone()).optional(s);

    let step = match step {
        Some(slot) => StackSlotOrConstId::StackSlot(slot),
        None => {
            let const_id = tracker.insert_literal(Literal::Int(1))?;
            StackSlotOrConstId::ConstId(const_id)
        }
    };

    // Loop controls.

    let zero = tracker.insert_literal(Literal::Int(0))?;

    // Panic if increment is 0.
    if let StackSlotOrConstId::StackSlot(step) = step {
        let error_msg =
            tracker.insert_literal(Literal::String("loop increment is 0".to_string()))?;

        let current = tracker.current_mut()?;

        current.push_block()?;

        current.emit(OpCode::LoadStack(step))?;
        current.emit(OpCode::LoadConstant(zero))?;
        current.emit(OpCode::RelBinOp(RelBinOp::Eq))?;
        let jump = current.emit(OpCode::JumpIf {
            cond: false,
            offset: Default::default(),
        })?;

        // Panic path.
        current.emit(OpCode::LoadConstant(error_msg))?;
        current.emit(OpCode::Panic)?;

        // Happy path.
        current.backpatch_to_next(jump)?;
        current.pop_block()?;
    }

    let current = tracker.current_mut()?;
    let start = current.next_instr()?;
    let stack_start = current.stack_top()?;

    step.load(current)?;
    current.emit(OpCode::LoadConstant(zero))?;
    current.emit(OpCode::RelBinOp(RelBinOp::Gt))?;
    let neg_step_jump = current.emit(OpCode::JumpIf {
        cond: false,
        offset: Default::default(),
    })?;

    // Path: positive step.
    current.emit(OpCode::LoadStack(loop_var))?;
    current.emit(OpCode::LoadStack(limit))?;
    current.emit(OpCode::RelBinOp(RelBinOp::Le))?;
    let to_end_pos = current.emit(OpCode::JumpIf {
        cond: false,
        offset: Default::default(),
    })?;
    let to_block = current.emit(OpCode::Jump {
        offset: Default::default(),
    })?;

    // Path: negative step.
    // We assume total ordering for the variable.
    current.backpatch_to_next(neg_step_jump)?;
    current.emit(OpCode::LoadStack(loop_var))?;
    current.emit(OpCode::LoadStack(limit))?;
    current.emit(OpCode::RelBinOp(RelBinOp::Ge))?;
    let to_end_neg = current.emit(OpCode::JumpIf {
        cond: false,
        offset: Default::default(),
    })?;

    current.backpatch_to_next(to_block)?;
    current.emit(OpCode::AdjustStack(stack_start))?;

    let (s, ()) = match_token(s, Token::Do).require()?;
    let (s, ()) = block(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::End).require()?;

    let current = tracker.current_mut()?;

    // Increment control variable.
    current.emit(OpCode::LoadStack(loop_var))?;
    step.load(current)?;
    current.emit(OpCode::AriBinOp(AriBinOp::Add))?;
    current.emit(OpCode::StoreStack(loop_var))?;

    current.emit_loop_to(start)?;

    // Clean up the stack.
    current.backpatch_to_next(to_end_pos)?;
    current.backpatch_to_next(to_end_neg)?;
    current.pop_block()?;

    Ok((s, ()))
}
