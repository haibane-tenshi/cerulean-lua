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

pub(crate) fn numerical_for<'s, 'origin>(
    mut outer_frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::block::block;
        use crate::parser::expr::expr_adjusted_to_1;

        let token_for = match_token(Token::For)
            .map_failure(|f| ParseFailure::from(NumericalForFailure::For(f)));
        let token_equals_sign = match_token(Token::EqualsSign)
            .map_failure(|f| ParseFailure::from(NumericalForFailure::EqualsSign(f)));
        let token_comma = match_token(Token::Comma)
            .map_failure(|f| ParseFailure::from(NumericalForFailure::Comma(f)));
        let token_do =
            match_token(Token::Do).map_failure(|f| ParseFailure::from(NumericalForFailure::Do(f)));
        let token_end = match_token(Token::End)
            .map_failure(|f| ParseFailure::from(NumericalForFailure::End(f)));
        let identifier = identifier
            .map_failure(|f| ParseFailure::from(NumericalForFailure::Ident(f)))
            .map_output(|(ident, _)| ident);

        let exit = outer_frag.id();

        let state = token_for
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and_replace(identifier)?
            .and_discard(token_equals_sign)?
            .with_mode(FailureMode::Malformed)
            .then(|ident| {
                |s| -> Result<_, FailFast> {
                    let loop_var = outer_frag.stack().top()?;
                    let status = expr_adjusted_to_1(outer_frag.new_fragment())
                        .parse_once(s)?
                        .map_output(|_| loop_var);
                    outer_frag.stack_mut().give_name(loop_var, ident)?;

                    Ok(status)
                }
            })?
            .and_discard(token_comma)?
            .and(|s| -> Result<_, FailFast> {
                let limit = outer_frag.stack().top()?;
                let status = expr_adjusted_to_1(outer_frag.new_fragment())
                    .parse_once(s)?
                    .map_output(|_| limit);

                Ok(status)
            })?
            .and(
                (|s| -> Result<_, FailFast> {
                    let step = outer_frag.stack().top()?;
                    let status = token_comma
                        .parse(s)?
                        .and(expr_adjusted_to_1(outer_frag.new_fragment()))?
                        .map_output(|_| step);

                    Ok(status)
                })
                .optional(),
            )?
            .try_map_output(|((loop_var, limit), step)| -> Result<_, CodegenError> {
                let step = match step {
                    Some(slot) => StackSlotOrConstId::StackSlot(slot),
                    None => {
                        let const_id = outer_frag.const_table_mut().insert(Literal::Int(1))?;
                        StackSlotOrConstId::ConstId(const_id)
                    }
                };

                Ok((loop_var, limit, step))
            })?
            .try_map_output(|(loop_var, limit, step)| -> Result<_, CodegenError> {
                // Loop controls.
                let zero = outer_frag.const_table_mut().insert(Literal::Int(0))?;

                // Panic if increment is 0.
                if let StackSlotOrConstId::StackSlot(step) = step {
                    let error_msg = outer_frag
                        .const_table_mut()
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
                Ok((loop_var, step, frag))
            })?
            .and_discard(token_do)?
            .then(|(loop_var, step, mut frag)| {
                move |s| -> Result<_, FailFast> {
                    let state = block(frag.new_fragment())
                        .parse_once(s)?
                        .map_output(|_| (loop_var, step, frag));

                    Ok(state)
                }
            })?
            .and_discard(token_end)?
            .try_map_output(|(loop_var, step, mut frag)| -> Result<_, CodegenError> {
                // Increment control variable.
                frag.emit(OpCode::LoadStack(loop_var))?;
                step.load(&mut frag)?;
                frag.emit(OpCode::AriBinOp(AriBinOp::Add))?;
                frag.emit(OpCode::StoreStack(loop_var))?;
                frag.emit_loop_to()?;

                // Clean up.
                frag.commit();
                Ok(())
            })?
            .collapse();

        // Clean up.
        let state = state.map_output(|_| outer_frag.commit());

        Ok(state)
    }
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
