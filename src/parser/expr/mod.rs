mod expr;
mod function;
mod literal;
mod table;

use crate::parser::prelude::*;

pub(in crate::parser) use expr::expr;
pub(in crate::parser) use function::function;
pub(in crate::parser) use literal::literal;
pub(in crate::parser) use table::table;

pub fn expr_adjusted_to<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
    count: u32,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mark = tracker.current()?.stack_top()? + count;
    let r = expr(s, tracker)?;
    tracker.current_mut()?.emit_adjust_to(mark)?;

    Ok(r)
}

pub fn expr_adjusted_to_1<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    expr_adjusted_to(s, tracker, 1)
}

pub fn par_expr<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let (s, ()) = match_token(s, Token::ParL)?;
    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::ParR).require()?;

    Ok((s, ()))
}

pub fn expr_list<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mut mark = tracker.current()?.stack_top()?;

    let (mut s, ()) = expr(s, tracker)?;

    let mut next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        let (s, ()) = match_token(s, Token::Comma)?;

        // Expressions inside comma lists are adjusted to 1.
        mark += 1;
        tracker.current_mut()?.emit_adjust_to(mark)?;

        expr(s, tracker).require()
    };

    loop {
        s = match next_part(s.clone()) {
            Ok((s, ())) => s,
            Err(_) => break,
        }
    }

    Ok((s, ()))
}

pub fn expr_list_adjusted_to<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
    count: u32,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mark = tracker.current()?.stack_top()? + count;
    let r = expr_list(s, tracker)?;
    tracker.current_mut()?.emit_adjust_to(mark)?;

    Ok(r)
}
