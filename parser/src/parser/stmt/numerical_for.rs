use std::ops::Range;

use thiserror::Error;

use crate::parser::prelude::*;

#[derive(Debug, Clone)]
enum StackSlotOrConstId {
    StackSlot(StackSlot, Range<usize>),
    ConstId(ConstId),
}

pub(crate) fn numerical_for<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
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
        let identifier =
            identifier.map_failure(|f| ParseFailure::from(NumericalForFailure::Ident(f)));

        let mut envelope = core.scope();
        let envelope_id = envelope.id();

        let source = s.source();
        let _span = trace_span!("for_numeric").entered();

        let state = Source(s)
            .and(token_for)?
            .with_mode(FailureMode::Ambiguous)
            .map_output(Spanned::put_range)
            .and(identifier, keep_with_range)?
            .and(token_equals_sign, keep_range)?
            .with_mode(FailureMode::Malformed)
            .and(expr_adjusted_to_1(envelope.new_core()), discard)?
            .map_output(|output| {
                // Assign name.
                let (((for_span, ident, ident_span), eq_sign_span), span) = output.take();
                envelope.pop_temporary();
                envelope.push_temporary(Some(ident));

                span.put((for_span, ident_span, eq_sign_span))
            })
            .and(token_comma, discard)?
            .and(expr_adjusted_to_1(envelope.new_core()), keep_range)?
            .and(
                (|s| -> Result<_, FailFast> {
                    let status = Source(s)
                        .and(token_comma)?
                        .and(expr_adjusted_to_1(envelope.new_core()), replace_range)?;

                    Ok(status)
                })
                .optional(),
                opt_keep,
            )?
            .then(|output| {
                let ((((for_span, loop_var_span, _eq_sign_span), _limit_span), step), span) =
                    output.take();

                let debug_info = DebugInfo::NumericForLoop {
                    for_: for_span.clone(),
                    ident: loop_var_span,
                    step: step.clone(),
                };

                let loop_var = envelope.stack_slot(FragmentStackSlot(0));
                let limit = loop_var + 1;

                let step = match step {
                    Some(step_span) => StackSlotOrConstId::StackSlot(loop_var + 2, step_span),
                    None => {
                        let const_id = envelope.const_table_mut().insert(Literal::Int(1));
                        StackSlotOrConstId::ConstId(const_id)
                    }
                };

                // Emit loop controls.
                let mut loop_body = match step {
                    StackSlotOrConstId::StackSlot(step, ref _step_span) => {
                        let zero = envelope.const_table_mut().insert(Literal::Int(0));

                        // Panic if increment is 0.
                        let mut zero_check = envelope.new_expr();

                        zero_check.emit(OpCode::LoadStack(step), debug_info.clone());
                        zero_check.emit(OpCode::LoadConstant(zero), debug_info.clone());
                        zero_check.emit(RelBinOp::Eq.into(), debug_info.clone());
                        zero_check.emit_jump_to_end(Some(false), debug_info.clone());

                        // Technically, this is what we need to emit here.
                        // Currently this is disallowed.
                        // zero_check.emit(OpCode::AdjustStack(StackSlot(0)), debug_info.clone());
                        zero_check.emit(OpCode::Panic, debug_info.clone());

                        zero_check.commit();

                        let mut loop_body = envelope.new_scope();
                        loop_body.mark_as_loop();

                        let mut controls = loop_body.new_expr();
                        let controls_id = controls.id();

                        let mut positive_step = controls.new_expr();

                        positive_step.emit(OpCode::LoadStack(step), debug_info.clone());
                        positive_step.emit(OpCode::LoadConstant(zero), debug_info.clone());
                        positive_step.emit(RelBinOp::Gt.into(), debug_info.clone());
                        positive_step.emit_jump_to_end(Some(false), debug_info.clone());

                        // Path: positive step.
                        positive_step.emit(OpCode::LoadStack(loop_var), debug_info.clone());
                        positive_step.emit(OpCode::LoadStack(limit), debug_info.clone());
                        positive_step.emit(RelBinOp::LtEq.into(), debug_info.clone());
                        positive_step.emit_jump_to(envelope_id, Some(false), debug_info.clone());
                        positive_step.emit_jump_to(controls_id, None, debug_info.clone());

                        positive_step.commit();

                        // Path: negative step.
                        // We assume total ordering for the variable.
                        controls.emit(OpCode::LoadStack(loop_var), debug_info.clone());
                        controls.emit(OpCode::LoadStack(limit), debug_info.clone());
                        controls.emit(RelBinOp::GtEq.into(), debug_info.clone());
                        controls.emit_jump_to(envelope_id, Some(false), debug_info.clone());

                        controls.commit();

                        loop_body
                    }
                    StackSlotOrConstId::ConstId(_) => {
                        let mut loop_body = envelope.new_scope();
                        loop_body.mark_as_loop();

                        loop_body.emit(OpCode::LoadStack(loop_var), debug_info.clone());
                        loop_body.emit(OpCode::LoadStack(limit), debug_info.clone());
                        loop_body.emit(RelBinOp::LtEq.into(), debug_info.clone());
                        loop_body.emit_jump_to(envelope_id, Some(false), debug_info.clone());

                        loop_body
                    }
                };

                move |s| -> Result<_, FailFast> {
                    let state = Source(s)
                        .and(token_do)?
                        .and(block(loop_body.new_core()), opt_discard)?
                        .and(token_end, replace_range)?
                        .inspect(|output| {
                            let end_span = output.value.clone();

                            // Increment control variable.
                            loop_body.emit(OpCode::LoadStack(loop_var), debug_info.clone());
                            match step {
                                StackSlotOrConstId::StackSlot(slot, _) => {
                                    loop_body.emit(OpCode::LoadStack(slot), debug_info.clone())
                                }
                                StackSlotOrConstId::ConstId(const_id) => loop_body
                                    .emit(OpCode::LoadConstant(const_id), debug_info.clone()),
                            };
                            loop_body.emit(AriBinOp::Add.into(), debug_info.clone());
                            loop_body.emit(OpCode::StoreStack(loop_var), debug_info.clone());
                            loop_body.emit_loop_to(debug_info.clone());

                            // Clean up.
                            loop_body.commit(end_span.clone());
                        })
                        .map_output(|output| replace(span, output));

                    Ok(state)
                }
            })?
            .collapse()
            .map_output(|output| {
                let (end_span, span) = output.take();

                envelope.commit(end_span);

                trace!(span=?span.span(), str=&source[span.span()]);

                span
            });

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
