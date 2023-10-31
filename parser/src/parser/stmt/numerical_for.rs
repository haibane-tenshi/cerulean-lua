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

        let mut outer_frag = core.scope();

        let exit = outer_frag.id();

        let state = token_for
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and(identifier, replace)?
            .and(token_equals_sign, discard)?
            .with_mode(FailureMode::Malformed)
            .then(|ident| {
                |s| -> Result<_, FailFast> {
                    let loop_var = outer_frag.stack_slot(outer_frag.stack().len());
                    let status = expr_adjusted_to_1(outer_frag.new_core())
                        .parse_once(s)?
                        .map_output(|output| {
                            // Assign name.
                            let (ident, span) = ident.take();
                            outer_frag.pop_temporary();
                            outer_frag.push_temporary(Some(ident));

                            replace(span, output.replace(loop_var).1)
                        });

                    Ok(status)
                }
            })?
            .and(token_comma, discard)?
            .and(
                |s| -> Result<_, FailFast> {
                    let limit = outer_frag.stack_slot(outer_frag.stack().len());
                    let status = expr_adjusted_to_1(outer_frag.new_core())
                        .parse_once(s)?
                        .map_output(|span| span.replace(limit).1);

                    Ok(status)
                },
                keep,
            )?
            .and(
                (|s| -> Result<_, FailFast> {
                    let step = outer_frag.stack_slot(outer_frag.stack().len());
                    let status = token_comma
                        .parse(s)?
                        .and(expr_adjusted_to_1(outer_frag.new_core()), discard)?
                        .map_output(|span| span.replace(step).1);

                    Ok(status)
                })
                .optional(),
                opt_keep,
            )?
            .map_output(|output| {
                let (((loop_var, limit), step), span) = output.take();

                let step = match step {
                    Some(slot) => StackSlotOrConstId::StackSlot(slot),
                    None => {
                        let const_id = outer_frag.const_table_mut().insert(Literal::Int(1));
                        StackSlotOrConstId::ConstId(const_id)
                    }
                };

                (loop_var, limit, step, span)
            })
            .map_output(|(loop_var, limit, step, span)| {
                // Emit loop controls.

                let frag = match step {
                    StackSlotOrConstId::StackSlot(step) => {
                        let zero = outer_frag.const_table_mut().insert(Literal::Int(0));

                        let error_msg = outer_frag
                            .const_table_mut()
                            .insert(Literal::String("loop increment is 0".to_string()));

                        // Panic if increment is 0.
                        let mut zero_check = outer_frag.new_expr();

                        zero_check.emit(OpCode::LoadStack(step));
                        zero_check.emit(OpCode::LoadConstant(zero));
                        zero_check.emit(RelBinOp::Eq.into());
                        zero_check.emit_jump_to(zero_check.id(), Some(false));

                        zero_check.emit(OpCode::LoadConstant(error_msg));
                        zero_check.emit(OpCode::Panic);

                        zero_check.commit();

                        let mut frag = outer_frag.new_scope();
                        frag.mark_as_loop();
                        let mut controls = frag.new_expr();
                        let controls_id = controls.id();
                        let mut positive_step = controls.new_expr();

                        positive_step.emit(OpCode::LoadStack(step));
                        positive_step.emit(OpCode::LoadConstant(zero));
                        positive_step.emit(RelBinOp::Gt.into());
                        positive_step.emit_jump_to(positive_step.id(), Some(false));

                        // Path: positive step.
                        positive_step.emit(OpCode::LoadStack(loop_var));
                        positive_step.emit(OpCode::LoadStack(limit));
                        positive_step.emit(RelBinOp::Le.into());
                        positive_step.emit_jump_to(exit, Some(false));
                        positive_step.emit_jump_to(controls_id, None);

                        positive_step.commit();

                        // Path: negative step.
                        // We assume total ordering for the variable.
                        controls.emit(OpCode::LoadStack(loop_var));
                        controls.emit(OpCode::LoadStack(limit));
                        controls.emit(RelBinOp::Ge.into());
                        controls.emit_jump_to(exit, Some(false));

                        controls.commit();

                        frag
                    }
                    StackSlotOrConstId::ConstId(_) => {
                        let mut frag = outer_frag.new_scope();
                        frag.mark_as_loop();

                        frag.emit(OpCode::LoadStack(loop_var));
                        frag.emit(OpCode::LoadStack(limit));
                        frag.emit(RelBinOp::Le.into());
                        frag.emit_jump_to(exit, Some(false));

                        frag
                    }
                };

                span.replace((loop_var, step, frag)).1
            })
            .and(token_do, discard)?
            .then(|output| {
                move |s| -> Result<_, FailFast> {
                    let ((loop_var, step, mut frag), span) = output.take();

                    let state = block(frag.new_core()).parse_once(s)?.map_output(|output| {
                        opt_discard(span, output).replace((loop_var, step, frag)).1
                    });

                    Ok(state)
                }
            })?
            .and(token_end, discard)?
            .map_output(|output| {
                let ((loop_var, step, mut frag), span) = output.take();

                // Increment control variable.
                frag.emit(OpCode::LoadStack(loop_var));
                step.load(&mut frag);
                frag.emit(AriBinOp::Add.into());
                frag.emit(OpCode::StoreStack(loop_var));
                frag.emit_loop_to();

                // Clean up.
                frag.commit();

                span
            })
            .collapse();

        // Clean up.
        let state = state.inspect(|_| outer_frag.commit());

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
