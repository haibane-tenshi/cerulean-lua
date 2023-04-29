use crate::lex::{Lexer, Token};

use super::super::tracker::ChunkTracker;
use super::{block, expr_adjusted_to_1};
use super::{LexParseError, NextToken, ParseError};

pub(super) fn if_then<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    match s.next_token()? {
        Token::If => (),
        _ => return Err(ParseError.into()),
    }

    tracker.current_mut()?.push_block()?;
    let (mut s, ()) = expr_adjusted_to_1(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::Then => (),
        _ => return Err(ParseError.into()),
    }

    let mut to_end = Vec::new();

    let mut to_next_block = tracker.current_mut()?.emit(OpCode::JumpIf {
        cond: false,
        offset: Default::default(),
    })?;

    let (mut s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

    loop {
        let Ok(ns) = (|mut s: Lexer<'s>| -> Result<Lexer<'s>, LexParseError> {
            match s.next_token()? {
                Token::ElseIf => (),
                _ => return Err(ParseError.into()),
            }

            // Finish off the previous block.
            // This needs to jump to the very end of `if` statement.
            let to_patch = tracker.current_mut()?.emit(OpCode::Jump {offset: Default::default()})?;
            to_end.push(to_patch);

            tracker.current_mut()?.backpatch_to_next(to_next_block)?;

            let (mut s, ()) = expr_adjusted_to_1(s, tracker).map_err(LexParseError::eof_into_err)?;

            to_next_block = tracker.current_mut()?.emit(OpCode::JumpIf {
                cond: false,
                offset: Default::default(),
            })?;

            match s.next_required_token()? {
                Token::Then => (),
                _ => return Err(ParseError.into()),
            }

            let (s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

            Ok(s)
        })(s.clone()) else {
            break
        };

        s = ns;
    }

    let mut s = (|mut s: Lexer<'s>| -> Result<Lexer<'s>, LexParseError> {
        match s.next_token()? {
            Token::Else => (),
            _ => return Err(ParseError.into()),
        }

        to_next_block = {
            let r = tracker.current_mut()?.emit(OpCode::Jump {
                offset: Default::default(),
            })?;
            tracker.current_mut()?.backpatch_to_next(to_next_block)?;

            r
        };

        let (s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

        Ok(s)
    })(s.clone())
    .ok()
    .unwrap_or(s);

    match s.next_required_token()? {
        Token::End => (),
        _ => return Err(ParseError.into()),
    }

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
