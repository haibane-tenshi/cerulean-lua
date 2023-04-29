mod assignment;
mod if_then;
mod local_assignment;
mod repeat_until;
mod while_do;

use crate::lex::Lexer;

use super::block;
use super::tracker::ChunkTracker;
use crate::parser::{LexParseError, Optional, Require};

use assignment::assignment;
use if_then::if_then;
use local_assignment::local_assignment;
use repeat_until::repeat_until;
use while_do::while_do;

pub(super) fn statement<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::{NextToken, ParseError};

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

fn semicolon(s: Lexer) -> Result<(Lexer, ()), LexParseError> {
    use crate::lex::Token;
    use crate::parser::match_token;

    match_token(s, Token::Semicolon)
}

fn do_end<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::parser::match_token;

    let (s, ()) = match_token(s, Token::Do)?;
    let (s, ()) = block(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::End).require()?;

    Ok((s, ()))
}

pub(super) fn return_<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use super::expr_list;
    use crate::lex::Token;
    use crate::opcode::OpCode;
    use crate::parser::match_token;

    let (s, ()) = match_token(s, Token::Return)?;

    let slot = tracker.current()?.stack_top()?;

    let (s, ()) = expr_list(s, tracker).map_err(LexParseError::eof_into_err)?;
    let (s, _) = match_token(s.clone(), Token::Semicolon).optional(s);

    tracker.current_mut()?.emit(OpCode::Return(slot))?;

    Ok((s, ()))
}
