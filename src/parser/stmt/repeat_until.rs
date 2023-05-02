use crate::lex::Lexer;

use crate::parser::{LexParseError, Require};
use crate::tracker::ChunkTracker;

pub(super) fn repeat_until<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::parser::{expr_adjusted_to_1, inner_block, match_token};

    let (s, ()) = match_token(s, Token::Repeat)?;

    let current = tracker.current_mut()?;
    let start = current.next_instr()?;
    let outer = current.start_block()?;
    let inner = current.start_block()?;

    let (s, ()) = inner_block(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::Until).require()?;
    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;

    let current = tracker.current_mut()?;
    current.emit_jump_to_end_of(outer, Some(true))?;
    current.finish_block(inner)?;
    current.emit_loop_to(start)?;
    current.finish_block(outer)?;

    Ok((s, ()))
}
