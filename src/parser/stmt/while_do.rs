use crate::parser::prelude::*;

pub(super) fn while_do<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::{block, expr_adjusted_to_1, match_token};

    let (s, ()) = match_token(s, Token::While)?;

    let current = tracker.current_mut()?;
    let start = current.next_instr();
    let outer = current.start_block()?;
    let inner = current.start_block()?;

    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::Do).require()?;

    tracker
        .current_mut()?
        .emit_jump_to_end_of(outer, Some(false))?;

    let (s, ()) = block(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::End).require()?;

    let current = tracker.current_mut()?;
    current.finish_block(inner)?;
    current.emit_loop_to(start)?;
    current.finish_block(outer)?;

    Ok((s, ()))
}
