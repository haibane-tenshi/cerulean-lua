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
                let ((((for_span, _loop_var_span, eq_sign_span), _limit_span), step), span) =
                    output.take();

                let loop_var = envelope.stack_slot(FragmentStackSlot(0));
                let limit = loop_var + 1;

                let step = match step {
                    Some(step_span) => StackSlotOrConstId::StackSlot(loop_var + 2, step_span),
                    None => {
                        let const_id = envelope.const_table_mut().insert(Literal::Int(1));
                        StackSlotOrConstId::ConstId(const_id)
                    }
                };

                let generic_debug_info = debug_info::DebugInfo::Generic(for_span.clone());

                // Emit loop controls.
                let mut loop_body = match step {
                    StackSlotOrConstId::StackSlot(step, ref step_span) => {
                        let zero = envelope.const_table_mut().insert(Literal::Int(0));

                        let error_msg = envelope
                            .const_table_mut()
                            .insert(Literal::String("loop increment is 0".to_string()));

                        // Panic if increment is 0.
                        let mut zero_check = envelope.new_expr();

                        zero_check
                            .emit_with_debug(OpCode::LoadStack(step), generic_debug_info.clone());
                        zero_check.emit_with_debug(
                            OpCode::LoadConstant(zero),
                            generic_debug_info.clone(),
                        );
                        zero_check.emit(RelBinOp::Eq.into(), step_span.clone());
                        zero_check.emit_jump_to_end(Some(false), step_span.clone());

                        zero_check.emit_with_debug(
                            OpCode::LoadConstant(error_msg),
                            generic_debug_info.clone(),
                        );
                        zero_check.emit(OpCode::Panic, step_span.clone());

                        zero_check.commit();

                        let mut loop_body = envelope.new_scope();
                        loop_body.mark_as_loop();

                        let mut controls = loop_body.new_expr();
                        let controls_id = controls.id();

                        let mut positive_step = controls.new_expr();

                        positive_step
                            .emit_with_debug(OpCode::LoadStack(step), generic_debug_info.clone());
                        positive_step.emit_with_debug(
                            OpCode::LoadConstant(zero),
                            generic_debug_info.clone(),
                        );
                        positive_step.emit(RelBinOp::Gt.into(), for_span.clone());
                        positive_step.emit_jump_to_end(Some(false), for_span.clone());

                        // Path: positive step.
                        positive_step.emit_with_debug(
                            OpCode::LoadStack(loop_var),
                            generic_debug_info.clone(),
                        );
                        positive_step
                            .emit_with_debug(OpCode::LoadStack(limit), generic_debug_info.clone());
                        positive_step.emit(RelBinOp::LtEq.into(), for_span.clone());
                        positive_step.emit_jump_to(envelope_id, Some(false), for_span.clone());
                        positive_step.emit_jump_to(controls_id, None, for_span.clone());

                        positive_step.commit();

                        // Path: negative step.
                        // We assume total ordering for the variable.
                        controls.emit_with_debug(
                            OpCode::LoadStack(loop_var),
                            generic_debug_info.clone(),
                        );
                        controls
                            .emit_with_debug(OpCode::LoadStack(limit), generic_debug_info.clone());
                        controls.emit(RelBinOp::GtEq.into(), for_span.clone());
                        controls.emit_jump_to(envelope_id, Some(false), for_span.clone());

                        controls.commit();

                        loop_body
                    }
                    StackSlotOrConstId::ConstId(_) => {
                        let mut loop_body = envelope.new_scope();
                        loop_body.mark_as_loop();

                        loop_body.emit_with_debug(
                            OpCode::LoadStack(loop_var),
                            generic_debug_info.clone(),
                        );
                        loop_body
                            .emit_with_debug(OpCode::LoadStack(limit), generic_debug_info.clone());
                        loop_body.emit(RelBinOp::LtEq.into(), for_span.clone());
                        loop_body.emit_jump_to(envelope_id, Some(false), for_span.clone());

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
                            loop_body.emit_with_debug(
                                OpCode::LoadStack(loop_var),
                                generic_debug_info.clone(),
                            );
                            match step {
                                StackSlotOrConstId::StackSlot(slot, _) => loop_body
                                    .emit_with_debug(
                                        OpCode::LoadStack(slot),
                                        generic_debug_info.clone(),
                                    ),
                                StackSlotOrConstId::ConstId(const_id) => loop_body.emit_with_debug(
                                    OpCode::LoadConstant(const_id),
                                    generic_debug_info,
                                ),
                            };
                            loop_body.emit(AriBinOp::Add.into(), eq_sign_span.clone());
                            loop_body.emit_with_debug(
                                OpCode::StoreStack(loop_var),
                                debug_info::DebugInfo::Generic(eq_sign_span),
                            );
                            loop_body.emit_loop_to(end_span.clone());

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
