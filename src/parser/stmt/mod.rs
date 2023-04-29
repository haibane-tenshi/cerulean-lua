mod assignment;
mod if_then;
mod local_assignment;
mod repeat_until;
mod while_do;

use crate::lex::{Lexer, Token};

use super::tracker::ChunkTracker;
use super::{block, expr_adjusted_to_1, inner_block};
use super::{LexParseError, NextToken, ParseError};

use assignment::assignment;
use if_then::if_then;
use local_assignment::local_assignment;
use repeat_until::repeat_until;
use while_do::while_do;

pub(super) fn statement<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    if let Ok(r) = semicolon(s.clone()) {
        Ok(r)
    } else if let Ok(r) = do_end(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = if_then(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = while_do(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = repeat_until(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = local_assignment(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = assignment(s.clone(), tracker) {
        Ok(r)
    } else {
        let mut s = s;
        let _ = s.next_token()?;
        Err(ParseError.into())
    }
}

fn semicolon(mut s: Lexer) -> Result<(Lexer, ()), LexParseError> {
    match s.next_token()? {
        Token::Semicolon => Ok((s, ())),
        _ => Err(ParseError.into()),
    }
}

fn do_end<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    match s.next_token()? {
        Token::Do => (),
        _ => return Err(ParseError.into()),
    };

    let (mut s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::End => (),
        _ => return Err(ParseError.into()),
    };

    Ok((s, ()))
}

pub(super) fn return_<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use super::expr_list;
    use crate::opcode::OpCode;

    match s.next_token()? {
        Token::Return => (),
        _ => return Err(ParseError.into()),
    }

    let slot = tracker.current()?.stack_top()?;

    let (s, ()) = expr_list(s, tracker).map_err(LexParseError::eof_into_err)?;

    let s = (|mut s: Lexer<'s>| match s.next_token().ok()? {
        Token::Semicolon => Some(s),
        _ => None,
    })(s.clone())
    .unwrap_or(s);

    tracker.current_mut()?.emit(OpCode::Return(slot))?;

    Ok((s, ()))
}
