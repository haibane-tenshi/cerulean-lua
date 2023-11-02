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

        let mut frag = core.scope();
        let mut places_start = frag.stack().len();

        let state = places(frag.new_core())
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and(token_equals_sign, discard)?
            .with_mode(FailureMode::Malformed)
            .then(|places| {
                |s| -> Result<_, FailFast> {
                    let (places, span) = places.take();

                    let count = places.len().try_into().unwrap();
                    let expr_start = frag.stack().len();
                    let status = expr_list_adjusted_to(count, frag.new_core())
                        .parse_once(s)?
                        .inspect(|_| {
                            let expr_slots = (expr_start.0..).map(StackSlot);
                            for (expr_slot, place) in expr_slots.zip(places) {
                                match place {
                                    Place::Temporary(slot) => {
                                        frag.emit(OpCode::LoadStack(expr_slot));
                                        frag.emit(OpCode::StoreStack(slot));
                                    }
                                    Place::TableField => {
                                        let table = frag.stack_slot(places_start);
                                        let field = table + 1;
                                        places_start += 2;

                                        frag.emit(OpCode::LoadStack(table));
                                        frag.emit(OpCode::LoadStack(field));
                                        frag.emit(OpCode::LoadStack(expr_slot));
                                        frag.emit(OpCode::TabSet);
                                    }
                                }
                            }
                        })
                        .map_output(|output| discard(span, output));

                    Ok(status)
                }
            })?
            .inspect(|_| frag.commit())
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

        let mut frag = core.scope();
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
