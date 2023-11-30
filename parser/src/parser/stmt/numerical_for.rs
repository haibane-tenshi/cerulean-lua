use thiserror::Error;

use crate::parser::prelude::*;

#[derive(Debug, Copy, Clone)]
enum StackSlotOrConstId {
    StackSlot(StackSlot),
    ConstId(ConstId),
}

impl StackSlotOrConstId {
    fn load(self, frag: &mut Fragment) -> InstrId {
        use StackSlotOrConstId::*;

        match self {
            StackSlot(slot) => frag.emit(OpCode::LoadStack(slot)),
            ConstId(const_id) => frag.emit(OpCode::LoadConstant(const_id)),
        }
    }
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
            .and(identifier, replace)?
            .and(token_equals_sign, discard)?
            .with_mode(FailureMode::Malformed)
            .then(|ident| {
                |s| -> Result<_, FailFast> {
                    let loop_var = envelope.stack_slot(envelope.stack().len());
                    let status = expr_adjusted_to_1(envelope.new_core())
                        .parse_once(s)?
                        .map_output(|output| {
                            // Assign name.
                            let (ident, span) = ident.take();
                            envelope.pop_temporary();
                            envelope.push_temporary(Some(ident));

                            replace(span, output).put(loop_var)
                        });

                    Ok(status)
                }
            })?
            .and(token_comma, discard)?
            .and(
                |s| -> Result<_, FailFast> {
                    let limit = envelope.stack_slot(envelope.stack().len());
                    let status = expr_adjusted_to_1(envelope.new_core())
                        .parse_once(s)?
                        .map_output(|span| span.put(limit));

                    Ok(status)
                },
                keep,
            )?
            .and(
                (|s| -> Result<_, FailFast> {
                    let step = envelope.stack_slot(envelope.stack().len());
                    let status = Source(s)
                        .and(token_comma)?
                        .and(expr_adjusted_to_1(envelope.new_core()), discard)?
                        .map_output(|span| span.put(step));

                    Ok(status)
                })
                .optional(),
                opt_keep,
            )?
            .then(|output| {
                let (((loop_var, limit), step), span) = output.take();

                let step = match step {
                    Some(slot) => StackSlotOrConstId::StackSlot(slot),
                    None => {
                        let const_id = envelope.const_table_mut().insert(Literal::Int(1));
                        StackSlotOrConstId::ConstId(const_id)
                    }
                };

                // Emit loop controls.
                let mut loop_body = match step {
                    StackSlotOrConstId::StackSlot(step) => {
                        let zero = envelope.const_table_mut().insert(Literal::Int(0));

                        let error_msg = envelope
                            .const_table_mut()
                            .insert(Literal::String("loop increment is 0".to_string()));

                        // Panic if increment is 0.
                        let mut zero_check = envelope.new_expr();

                        zero_check.emit(OpCode::LoadStack(step));
                        zero_check.emit(OpCode::LoadConstant(zero));
                        zero_check.emit(RelBinOp::Eq.into());
                        zero_check.emit_jump_to_end(Some(false));

                        zero_check.emit(OpCode::LoadConstant(error_msg));
                        zero_check.emit(OpCode::Panic);

                        zero_check.commit();

                        let mut loop_body = envelope.new_scope();
                        loop_body.mark_as_loop();

                        let mut controls = loop_body.new_expr();
                        let controls_id = controls.id();

                        let mut positive_step = controls.new_expr();

                        positive_step.emit(OpCode::LoadStack(step));
                        positive_step.emit(OpCode::LoadConstant(zero));
                        positive_step.emit(RelBinOp::Gt.into());
                        positive_step.emit_jump_to_end(Some(false));

                        // Path: positive step.
                        positive_step.emit(OpCode::LoadStack(loop_var));
                        positive_step.emit(OpCode::LoadStack(limit));
                        positive_step.emit(RelBinOp::Le.into());
                        positive_step.emit_jump_to(envelope_id, Some(false));
                        positive_step.emit_jump_to(controls_id, None);

                        positive_step.commit();

                        // Path: negative step.
                        // We assume total ordering for the variable.
                        controls.emit(OpCode::LoadStack(loop_var));
                        controls.emit(OpCode::LoadStack(limit));
                        controls.emit(RelBinOp::Ge.into());
                        controls.emit_jump_to(envelope_id, Some(false));

                        controls.commit();

                        loop_body
                    }
                    StackSlotOrConstId::ConstId(_) => {
                        let mut loop_body = envelope.new_scope();
                        loop_body.mark_as_loop();

                        loop_body.emit(OpCode::LoadStack(loop_var));
                        loop_body.emit(OpCode::LoadStack(limit));
                        loop_body.emit(RelBinOp::Le.into());
                        loop_body.emit_jump_to(envelope_id, Some(false));

                        loop_body
                    }
                };

                move |s| -> Result<_, FailFast> {
                    let state = Source(s)
                        .and(token_do)?
                        .and(block(loop_body.new_core()), opt_discard)?
                        .and(token_end, discard)?
                        .inspect(|_| {
                            // Increment control variable.
                            loop_body.emit(OpCode::LoadStack(loop_var));
                            step.load(&mut loop_body);
                            loop_body.emit(AriBinOp::Add.into());
                            loop_body.emit(OpCode::StoreStack(loop_var));
                            loop_body.emit_loop_to();

                            // Clean up.
                            loop_body.commit();
                        })
                        .map_output(|output| discard(span, output));

                    Ok(state)
                }
            })?
            .collapse()
            .inspect(|output| {
                envelope.commit();

                trace!(span=?output.span(), str=&source[output.span()]);
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
