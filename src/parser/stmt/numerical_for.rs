use crate::parser::prelude::*;
use crate::tracker::EmitError;

#[derive(Debug, Copy, Clone)]
enum StackSlotOrConstId {
    StackSlot(StackSlot),
    ConstId(ConstId),
}

impl StackSlotOrConstId {
    fn load(self, tracker: &mut FunctionTracker) -> Result<InstrId, EmitError> {
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
    use crate::parser::{block, expr_adjusted_to_1, identifier, match_token};

    let (s, ()) = match_token(s, Token::For)?;
    let (s, ident) = identifier(s).require()?;
    let (s, ()) = match_token(s, Token::Assign).require()?;

    let outer = tracker.current_mut()?.start_block()?;

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

        let zero_check = current.start_block()?;

        current.emit(OpCode::LoadStack(step))?;
        current.emit(OpCode::LoadConstant(zero))?;
        current.emit(OpCode::RelBinOp(RelBinOp::Eq))?;
        current.emit_jump_to_end_of(zero_check, Some(false))?;

        // Panic path.
        current.emit(OpCode::LoadConstant(error_msg))?;
        current.emit(OpCode::Panic)?;

        // Happy path.
        current.finish_block(zero_check)?;
    }

    let current = tracker.current_mut()?;
    let start = current.next_instr();
    let controls = current.start_block()?;
    let positive_step = current.start_block()?;

    step.load(current)?;
    current.emit(OpCode::LoadConstant(zero))?;
    current.emit(OpCode::RelBinOp(RelBinOp::Gt))?;
    current.emit_jump_to_end_of(positive_step, Some(false))?;

    // Path: positive step.
    current.emit(OpCode::LoadStack(loop_var))?;
    current.emit(OpCode::LoadStack(limit))?;
    current.emit(OpCode::RelBinOp(RelBinOp::Le))?;
    current.emit_jump_to_end_of(outer, Some(false))?;
    current.emit_jump_to_end_of(controls, None)?;

    current.finish_block(positive_step)?;

    // Path: negative step.
    // We assume total ordering for the variable.
    current.emit(OpCode::LoadStack(loop_var))?;
    current.emit(OpCode::LoadStack(limit))?;
    current.emit(OpCode::RelBinOp(RelBinOp::Ge))?;
    current.emit_jump_to_end_of(outer, Some(false))?;

    current.finish_block(controls)?;

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

    // Clean up.
    current.finish_block(outer)?;

    Ok((s, ()))
}
