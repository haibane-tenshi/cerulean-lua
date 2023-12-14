use crate::parser::prefix_expr::{Place, PlaceFailure};
use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn assignment<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_list_adjusted_to;

        let token_equals_sign = match_token(Token::EqualsSign)
            .map_failure(|f| ParseFailure::from(AssignmentFailure::EqualsSign(f)));

        let mut frag = core.decl();
        let mut places_start = frag.stack().len();

        let source = s.source();
        let _span = trace_span!("assignment").entered();

        let state = places(frag.new_core())
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and(token_equals_sign, |prev, eq_sign| {
                let eq_sign_span = eq_sign.span();
                discard(prev, eq_sign).place(eq_sign_span)
            })?
            .with_mode(FailureMode::Malformed)
            .then(|places| {
                |s| -> Result<_, FailFast> {
                    let ((places, eq_sign_span), span) = places.take();

                    let count = places.len();
                    let expr_start = frag.stack().len();
                    let status = expr_list_adjusted_to(count, frag.new_core())
                        .parse_once(s)?
                        .inspect(|output| {
                            let expr_slots = (expr_start.0..).map(StackSlot);
                            for (expr_slot, place) in expr_slots.zip(places) {
                                match place {
                                    Place::Temporary(slot, ident) => {
                                        let debug_info = DebugInfo::StorePlace {
                                            place: DebugPlace::Temporary,
                                            ident,
                                            eq_sign: eq_sign_span.clone(),
                                            expr: output.span(),
                                        };

                                        frag.emit_with_debug(
                                            OpCode::LoadStack(expr_slot),
                                            debug_info.clone(),
                                        );
                                        frag.emit_with_debug(OpCode::StoreStack(slot), debug_info);
                                    }
                                    Place::Upvalue(slot, ident) => {
                                        let debug_info = DebugInfo::StorePlace {
                                            place: DebugPlace::Upvalue,
                                            ident,
                                            eq_sign: eq_sign_span.clone(),
                                            expr: output.span(),
                                        };

                                        frag.emit_with_debug(
                                            OpCode::LoadStack(expr_slot),
                                            debug_info.clone(),
                                        );
                                        frag.emit_with_debug(
                                            OpCode::StoreUpvalue(slot),
                                            debug_info,
                                        );
                                    }
                                    Place::TableField(debug_info) => {
                                        use crate::parser::prefix_expr::TableFieldDebugInfo;

                                        let debug_info = match debug_info {
                                            TableFieldDebugInfo::Local {
                                                table,
                                                index,
                                                indexing,
                                            } => DebugInfo::StoreTable {
                                                table,
                                                index,
                                                indexing,
                                                eq_sign: eq_sign_span.clone(),
                                                expr: output.span(),
                                            },
                                            TableFieldDebugInfo::Global(ident) => {
                                                DebugInfo::StorePlace {
                                                    place: DebugPlace::Global,
                                                    ident,
                                                    eq_sign: eq_sign_span.clone(),
                                                    expr: output.span(),
                                                }
                                            }
                                        };

                                        let table = frag.stack_slot(places_start);
                                        let field = table + 1;
                                        places_start += 2;

                                        frag.emit_with_debug(
                                            OpCode::LoadStack(table),
                                            debug_info.clone(),
                                        );
                                        frag.emit_with_debug(
                                            OpCode::LoadStack(field),
                                            debug_info.clone(),
                                        );
                                        frag.emit_with_debug(
                                            OpCode::LoadStack(expr_slot),
                                            debug_info.clone(),
                                        );
                                        frag.emit_with_debug(OpCode::TabSet, debug_info);
                                    }
                                }
                            }
                        })
                        .map_output(|output| discard(span, output));

                    Ok(status)
                }
            })?
            .inspect(|output| {
                frag.commit();

                trace!(span=?output.span(), str=&source[output.span()]);
            })
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum AssignmentFailure {
    #[error("expected place")]
    Place(#[from] PlaceFailure),
    #[error("missing equals sign")]
    EqualsSign(#[source] TokenMismatch),
    #[error("missing comma")]
    Comma(#[source] TokenMismatch),
}

fn places<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<Vec<Place>>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::prefix_expr::place;

        let token_comma = match_token(Token::Comma)
            .map_failure(|f| ParseFailure::from(AssignmentFailure::Comma(f)));

        let mut frag = core.decl();
        let mut result = Vec::new();

        let mut put_place = |output: Spanned<_>| {
            let (place, span) = output.take();
            result.push(place);
            span
        };

        let state = Source(s)
            .and(place(frag.new_core()).map_output(&mut put_place))?
            .and(
                (|s| -> Result<_, FailFast> {
                    let state = Source(s)
                        .and(token_comma)?
                        .and(place(frag.new_core()).map_output(&mut put_place), discard)?;

                    Ok(state)
                })
                .repeat_with(discard)
                .optional(),
                opt_discard,
            )?
            .map_output(|span| {
                frag.commit();
                span.put(result)
            });

        Ok(state)
    }
}
