use crate::parser::prefix_expr::Place;
use crate::parser::prelude::*;

pub(super) fn assignment<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::expr::expr_list_adjusted_to;

    let outer = tracker.current_mut()?.start_block()?;

    let mut places_start = tracker.current()?.stack_top()?;
    let (s, places) = places(s, tracker)?;
    let count = places.len().try_into().unwrap();

    let (s, ()) = match_token(s, Token::Assign).require()?;

    let expr_start = tracker.current()?.stack_top()?;
    let (s, ()) = expr_list_adjusted_to(s, tracker, count).require()?;

    let current = tracker.current_mut()?;

    let expr_slots = (expr_start.0..).map(StackSlot);
    for (expr_slot, place) in expr_slots.zip(places) {
        match place {
            Place::Temporary(slot) => {
                current.emit(OpCode::LoadStack(expr_slot))?;
                current.emit(OpCode::StoreStack(slot))?;
            }
            Place::TableField => {
                let table = places_start;
                let field = places_start + 1;
                places_start += 2;

                current.emit(OpCode::LoadStack(table))?;
                current.emit(OpCode::LoadStack(field))?;
                current.emit(OpCode::LoadStack(expr_slot))?;
                current.emit(OpCode::TabSet)?;
            }
        }
    }

    current.finish_block(outer)?;

    Ok((s, ()))
}

fn places<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, Vec<Place>), LexParseError> {
    use crate::parser::prefix_expr::place;

    let (mut s, first) = place(s, tracker)?;

    let mut r = vec![first];

    let mut next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, Place), LexParseError> {
        let (s, ()) = match_token(s, Token::Comma)?;
        place(s, tracker).require()
    };

    while let Ok((ns, next)) = next_part(s.clone()) {
        s = ns;
        r.push(next);
    }

    Ok((s, r))
}
