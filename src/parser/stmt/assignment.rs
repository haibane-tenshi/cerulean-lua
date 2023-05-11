use crate::parser::prefix_expr::Place;
use crate::parser::prelude::*;

pub(super) fn assignment<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::expr::expr_list_adjusted_to;

    let mut places_start = frag.stack().top()?;
    let (s, places) = places(s, chunk, frag.new_fragment())?;
    let count = places.len().try_into().unwrap();

    let (s, ()) = match_token(s, Token::EqualsSign).require()?;

    let expr_start = frag.stack().top()?;
    let (s, ()) = expr_list_adjusted_to(s, count, chunk, frag.new_fragment()).require()?;

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

    Ok((s, ()))
}

fn places<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, Vec<Place>), LexParseError> {
    use crate::parser::prefix_expr::place;

    let (mut s, first) = place(s, chunk, frag.new_fragment())?;

    let mut r = vec![first];

    let mut next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, Place), LexParseError> {
        let (s, ()) = match_token(s, Token::Comma)?;
        place(s, chunk, frag.new_fragment()).require()
    };

    while let Ok((ns, next)) = next_part(s.clone()) {
        s = ns;
        r.push(next);
    }

    frag.commit();

    Ok((s, r))
}
