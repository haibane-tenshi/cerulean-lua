use crate::lex::Lexer;

use crate::parser::{LexParseError, Optional, Require};
use crate::tracker::ChunkTracker;

pub(super) fn if_then<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::parser::{block, expr_adjusted_to_1, match_token};

    let (s, ()) = match_token(s, Token::If)?;

    let outer = tracker.current_mut()?.start_block()?;

    // This is a tricky bit.
    // We start the inner block only *after* condition is evaluated.
    // This causes expression result to be part of outer block, but not inner one.
    // The beneficial part is that now it doesn't need to be cleaned up as part of inner block,
    // sparing us from emitting extra AdjustStack instruction at the beginning of every branch.
    // The downside: condition values will accumulate and persist until the end of outer block.
    // In practice this is unlikely to ever cause problems,
    // unless one writes *very unreasonable* chain of `elseif`s.
    //
    // We implement the same thing around `elseif`s.

    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::Then).require()?;

    let current = tracker.current_mut()?;
    let mut inner = current.start_block()?;
    current.emit_jump_to_end_of(inner, Some(false))?;

    let (mut s, ()) = block(s, tracker).require()?;

    let mut else_if_clause = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        let (s, ()) = match_token(s, Token::ElseIf)?;

        let current = tracker.current_mut()?;
        current.emit_jump_to_end_of(outer, None)?;
        current.finish_block(inner)?;

        let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;

        let current = tracker.current_mut()?;
        inner = current.start_block()?;
        current.emit_jump_to_end_of(inner, Some(false))?;

        let (s, ()) = match_token(s, Token::Then).require()?;
        let (s, ()) = block(s, tracker).require()?;

        Ok((s, ()))
    };

    loop {
        s = match else_if_clause(s.clone()) {
            Ok((s, ())) => s,
            Err(_) => break,
        };
    }

    let mut else_clause = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        let (s, ()) = match_token(s, Token::Else)?;

        let current = tracker.current_mut()?;
        current.emit_jump_to_end_of(outer, None)?;
        current.finish_block(inner)?;

        inner = current.start_block()?;

        let (s, ()) = block(s, tracker).require()?;

        Ok((s, ()))
    };

    let (s, _) = else_clause(s.clone()).optional(s);
    let (s, ()) = match_token(s, Token::End).require()?;

    let current = tracker.current_mut()?;
    current.finish_block(inner)?;
    current.finish_block(outer)?;

    Ok((s, ()))
}
