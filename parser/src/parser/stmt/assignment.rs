use crate::parser::expr::ExprListSuccess;
use crate::parser::prefix_expr::Place;
use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn assignment<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ExprListSuccess), Error<ParseFailure>> {
    use crate::parser::expr::expr_list_adjusted_to;
    use AssignmentFailure::*;

    let mut places_start = frag.stack().top()?;
    let (s, places, _) = places(s, chunk, frag.new_fragment())?;
    let count = places.len().try_into().unwrap();

    let (s, _, Complete) = match_token(s, Token::EqualsSign).map_parse(EqualsSign)?;

    let expr_start = frag.stack().top()?;
    let (s, (), status) = expr_list_adjusted_to(s, count, chunk, frag.new_fragment())
        .with_mode(FailureMode::Malformed)?;

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
    Ok((s, (), status))
}

#[derive(Debug, Error)]
pub(crate) enum AssignmentFailure {
    #[error("missing equals sign")]
    EqualsSign(#[source] TokenMismatch),
}

impl HaveFailureMode for AssignmentFailure {
    fn mode(&self) -> FailureMode {
        match self {
            AssignmentFailure::EqualsSign(_) => FailureMode::Ambiguous,
        }
    }
}

fn places<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, Vec<Place>, PlacesSuccess), Error<ParseFailure>> {
    use crate::parser::prefix_expr::place;

    let (mut s, first, _) = place(s, chunk, frag.new_fragment())?;

    let mut r = vec![first];

    let mut next_part = |s| -> Result<_, PlacesSuccess> {
        use PlacesSuccess::*;

        let (s, _, _) = match_token(s, Token::Comma).map_err(Comma)?;
        let (s, next, status) = place(s, chunk, frag.new_fragment()).map_err(Place)?;
        r.push(next);

        Ok((s, (), status))
    };

    let status = loop {
        s = match next_part(s.clone()) {
            Ok((s, _, _)) => s,
            Err(err) => break err,
        };
    };

    frag.commit();
    Ok((s, r, status))
}

enum PlacesSuccess {
    Comma(ParseError<TokenMismatch>),
    Place(Error<ParseFailure>),
}
