use crate::lex::{Lexer, Token};

use super::super::tracker::ChunkTracker;
use super::expr_adjusted_to_1;
use super::{LexParseError, NextToken, ParseError};

pub(super) fn assignment<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    let ident = match s.next_token()? {
        Token::Ident(ident) => ident,
        _ => return Err(ParseError.into()),
    };

    match s.next_required_token()? {
        Token::Assign => (),
        _ => return Err(ParseError.into()),
    }

    let (s, ()) = expr_adjusted_to_1(s, tracker).map_err(LexParseError::eof_into_err)?;

    // Otherwise try to store it inside known variable.
    let slot = tracker.lookup_local(ident).ok_or(ParseError)?;
    tracker.current_mut()?.emit(OpCode::StoreStack(slot))?;

    Ok((s, ()))
}
