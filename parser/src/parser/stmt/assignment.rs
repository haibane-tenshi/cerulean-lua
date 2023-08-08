use crate::parser::prefix_expr::{Place, PlaceFailure};
use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn assignment<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_list_adjusted_to;

        let token_equals_sign = match_token(Token::EqualsSign)
            .map_failure(|f| ParseFailure::from(AssignmentFailure::EqualsSign(f)));

        let mut places_start = frag.stack().top()?;

        let state = places(frag.new_fragment())
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and_discard(token_equals_sign)?
            .with_mode(FailureMode::Malformed)
            .then(|places| {
                |s| {
                    let count = places.len().try_into().unwrap();
                    let expr_start = frag.stack().top()?;
                    expr_list_adjusted_to(count, frag.new_fragment())
                        .map_output(|_| (expr_start, places))
                        .parse_once(s)
                }
            })?
            .try_map_output(move |(expr_start, places)| -> Result<_, CodegenError> {
                let expr_slots = (expr_start.0..).map(StackSlot);
                for (expr_slot, place) in expr_slots.zip(places) {
                    match place {
                        Place::Temporary(slot) => {
                            frag.emit(OpCode::LoadStack(expr_slot))?;
                            frag.emit(OpCode::StoreStack(slot))?;
                        }
                        Place::TableField => {
                            let table = places_start;
                            let field = places_start + 1;
                            places_start += 2;

                            frag.emit(OpCode::LoadStack(table))?;
                            frag.emit(OpCode::LoadStack(field))?;
                            frag.emit(OpCode::LoadStack(expr_slot))?;
                            frag.emit(OpCode::TabSet)?;
                        }
                    }
                }

                frag.commit();
                Ok(())
            })?
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
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Vec<Place>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::prefix_expr::place;

        let (first, state) = match place(frag.new_fragment()).parse_once(s)? {
            ParsingState::Success(s, output, success) => {
                (output, ParsingState::Success(s, (), success))
            }
            ParsingState::Failure(failure) => return Ok(ParsingState::Failure(failure)),
        };

        let mut r = vec![first];

        let next = |s| -> Result<_, FailFast> {
            let token_comma = match_token(Token::Comma)
                .map_failure(|f| ParseFailure::from(AssignmentFailure::Comma(f)));

            let state = token_comma
                .parse_once(s)?
                .and_with(place(frag.new_fragment()), |_, place| place)?
                .map_output(|place| {
                    r.push(place);
                });

            Ok(state)
        };

        let state = state.and(next.repeat())?.map_output(move |_| {
            frag.commit();
            r
        });

        Ok(state)
    }
}
