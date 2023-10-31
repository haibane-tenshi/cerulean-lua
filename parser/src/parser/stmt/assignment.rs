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
                |s| {
                    let (places, span) = places.take();

                    let count = places.len().try_into().unwrap();
                    let expr_start = frag.stack().len();
                    expr_list_adjusted_to(count, frag.new_core())
                        .map_output(|output| (expr_start, places, discard(span, output)))
                        .parse_once(s)
                }
            })?
            .map_output(move |(expr_start, places, span)| {
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

                frag.commit();

                span
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

        let mut frag = core.scope();

        let (first, state) = match place(frag.new_core()).parse_once(s)? {
            ParsingState::Success(s, output, success) => {
                let (place, span) = output.take();

                (place, ParsingState::Success(s, span, success))
            }
            ParsingState::Failure(failure) => return Ok(ParsingState::Failure(failure)),
        };

        let mut r = vec![first];

        let next = |s| -> Result<_, FailFast> {
            let token_comma = match_token(Token::Comma)
                .map_failure(|f| ParseFailure::from(AssignmentFailure::Comma(f)));

            let state = token_comma
                .parse_once(s)?
                .and(place(frag.new_core()), replace)?
                .map_output(|place| {
                    let (place, span) = place.take();
                    r.push(place);

                    span
                });

            Ok(state)
        };

        let state = state
            .and(next.repeat_with(discard).optional(), opt_discard)?
            .map_output(move |span| {
                frag.commit();

                span.replace(r).1
            });

        Ok(state)
    }
}
