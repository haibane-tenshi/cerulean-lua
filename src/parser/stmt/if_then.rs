use crate::lex::Lexer;

use super::super::tracker::ChunkTracker;
use crate::parser::{LexParseError, Optional, Require};

pub(super) fn if_then<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::opcode::OpCode;
    use crate::parser::{block, expr_adjusted_to_1, match_token};

    let (s, ()) = match_token(s, Token::If)?;

    tracker.current_mut()?.push_block()?;

    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::Then).require()?;

    let mut to_end = Vec::new();
    let mut to_next_block = tracker.current_mut()?.emit(OpCode::JumpIf {
        cond: false,
        offset: Default::default(),
    })?;

    let (mut s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

    let mut else_if_clause = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        let (s, ()) = match_token(s, Token::ElseIf)?;

        // Finish off the previous block.
        // This needs to jump to the very end of `if` statement.
        let to_patch = tracker.current_mut()?.emit(OpCode::Jump {
            offset: Default::default(),
        })?;
        to_end.push(to_patch);

        tracker.current_mut()?.backpatch_to_next(to_next_block)?;

        let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;

        to_next_block = tracker.current_mut()?.emit(OpCode::JumpIf {
            cond: false,
            offset: Default::default(),
        })?;

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

        to_next_block = {
            let r = tracker.current_mut()?.emit(OpCode::Jump {
                offset: Default::default(),
            })?;
            tracker.current_mut()?.backpatch_to_next(to_next_block)?;

            r
        };

        let (s, ()) = block(s, tracker).require()?;

        Ok((s, ()))
    };

    let (s, _) = else_clause(s.clone()).optional(s);
    let (s, ()) = match_token(s, Token::End).require()?;

    // Backpatch the last jump instruction and block ends.
    let current = tracker.current_mut()?;
    current.backpatch_to_next(to_next_block)?;
    for index in to_end {
        current.backpatch_to_next(index)?;
    }

    // Clean up stack if needed.
    current.pop_block()?;

    Ok((s, ()))
}
