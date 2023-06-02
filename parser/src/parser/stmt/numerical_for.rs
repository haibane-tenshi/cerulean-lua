use thiserror::Error;

use crate::codegen::fragment::EmitError;
use crate::parser::prelude::*;

#[derive(Debug, Copy, Clone)]
enum StackSlotOrConstId {
    StackSlot(StackSlot),
    ConstId(ConstId),
}

impl StackSlotOrConstId {
    fn load(self, frag: &mut Fragment) -> Result<InstrId, EmitError> {
        use StackSlotOrConstId::*;

        match self {
            StackSlot(slot) => frag.emit(OpCode::LoadStack(slot)),
            ConstId(const_id) => frag.emit(OpCode::LoadConstant(const_id)),
        }
    }
}

pub(crate) fn numerical_for<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut outer_frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    use crate::parser::block::block;
    use crate::parser::expr::expr_adjusted_to_1;
    use NumericalForFailure::*;

    let exit = outer_frag.id();

    let (s, _, Complete) = match_token(s, Token::For).map_parse(For)?;
    let (s, (ident, _), Complete) = identifier(s).map_parse(Ident)?;
    let (s, _, Complete) = match_token(s, Token::EqualsSign).map_parse(EqualsSign)?;

    let loop_var = outer_frag.stack().top()?;
    let (s, (), _) = expr_adjusted_to_1(s, chunk, outer_frag.new_fragment())
        .with_mode(FailureMode::Malformed)?;
    outer_frag.stack_mut().give_name(loop_var, ident)?;

    let (s, _, Complete) = match_token(s, Token::Comma).map_parse(Comma)?;

    let limit = outer_frag.stack().top()?;
    let (s, (), _) = expr_adjusted_to_1(s, chunk, outer_frag.new_fragment())
        .with_mode(FailureMode::Malformed)?;

    let mut maybe_step = |s: Lexer<'s>| -> Result<(Lexer<'s>, StackSlot, ()), ()> {
        let (s, _, Complete) = match_token(s, Token::Comma).map_err(|_| ())?;

        let step = outer_frag.stack().top().map_err(|_| ())?;
        let (s, (), _) = expr_adjusted_to_1(s, chunk, outer_frag.new_fragment()).map_err(|_| ())?;

        Ok((s, step, ()))
    };

    let (s, step, _) = maybe_step(s.clone()).optional(s);

    let step = match step {
        Some(slot) => StackSlotOrConstId::StackSlot(slot),
        None => {
            let const_id = chunk.constants.insert(Literal::Int(1))?;
            StackSlotOrConstId::ConstId(const_id)
        }
    };

    // Loop controls.

    let zero = chunk.constants.insert(Literal::Int(0))?;

    // Panic if increment is 0.
    if let StackSlotOrConstId::StackSlot(step) = step {
        let error_msg = chunk
            .constants
            .insert(Literal::String("loop increment is 0".to_string()))?;

        let mut zero_check = outer_frag.new_fragment();

        zero_check.emit(OpCode::LoadStack(step))?;
        zero_check.emit(OpCode::LoadConstant(zero))?;
        zero_check.emit(OpCode::RelBinOp(RelBinOp::Eq))?;
        zero_check.emit_jump_to(zero_check.id(), Some(false))?;

        // Panic path.
        zero_check.emit(OpCode::LoadConstant(error_msg))?;
        zero_check.emit(OpCode::Panic)?;

        // Happy path.
        zero_check.commit();
    }

    let mut frag = outer_frag.new_fragment();
    let mut controls = frag.new_fragment();
    let controls_id = controls.id();
    let mut positive_step = controls.new_fragment();

    step.load(&mut positive_step)?;
    positive_step.emit(OpCode::LoadConstant(zero))?;
    positive_step.emit(OpCode::RelBinOp(RelBinOp::Gt))?;
    positive_step.emit_jump_to(positive_step.id(), Some(false))?;

    // Path: positive step.
    positive_step.emit(OpCode::LoadStack(loop_var))?;
    positive_step.emit(OpCode::LoadStack(limit))?;
    positive_step.emit(OpCode::RelBinOp(RelBinOp::Le))?;
    positive_step.emit_jump_to(exit, Some(false))?;
    positive_step.emit_jump_to(controls_id, None)?;

    positive_step.commit();

    // Path: negative step.
    // We assume total ordering for the variable.
    controls.emit(OpCode::LoadStack(loop_var))?;
    controls.emit(OpCode::LoadStack(limit))?;
    controls.emit(OpCode::RelBinOp(RelBinOp::Ge))?;
    controls.emit_jump_to(exit, Some(false))?;

    controls.commit();

    let (s, _, Complete) = match_token(s, Token::Do).map_parse(Do)?;
    let (s, (), _) = block(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;
    let (s, _, status) = match_token(s, Token::End).map_parse(End)?;

    // Increment control variable.
    frag.emit(OpCode::LoadStack(loop_var))?;
    step.load(&mut frag)?;
    frag.emit(OpCode::AriBinOp(AriBinOp::Add))?;
    frag.emit(OpCode::StoreStack(loop_var))?;
    frag.emit_loop_to()?;

    // Clean up.
    frag.commit();
    outer_frag.commit_scope();

    Ok((s, (), status))
}

#[derive(Debug, Error)]
pub(crate) enum NumericalForFailure {
    #[error("missing `for` keyword")]
    For(#[source] TokenMismatch),
    #[error("missing identifier for control variable")]
    Ident(#[source] IdentMismatch),
    #[error("missing equals sign")]
    EqualsSign(#[source] TokenMismatch),
    #[error("missing comma separator between starting value and limit")]
    Comma(#[source] TokenMismatch),
    #[error("missing `do` keyword")]
    Do(#[source] TokenMismatch),
    #[error("missing `end` keyword")]
    End(#[source] TokenMismatch),
}

impl HaveFailureMode for NumericalForFailure {
    fn mode(&self) -> FailureMode {
        match self {
            NumericalForFailure::For(_) => FailureMode::Mismatch,
            NumericalForFailure::Ident(_) => FailureMode::Ambiguous,
            NumericalForFailure::EqualsSign(_) => FailureMode::Ambiguous,
            NumericalForFailure::Comma(_) => FailureMode::Malformed,
            NumericalForFailure::Do(_) => FailureMode::Malformed,
            NumericalForFailure::End(_) => FailureMode::Malformed,
        }
    }
}
