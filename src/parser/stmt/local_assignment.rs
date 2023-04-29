use crate::lex::{Lexer, Token};

use super::super::tracker::ChunkTracker;
use super::expr_adjusted_to_1;
use super::{LexParseError, NextToken, ParseError};

pub(super) fn local_assignment<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    match s.next_token()? {
        Token::Local => (),
        _ => return Err(ParseError.into()),
    }

    let ident = match s.next_required_token()? {
        Token::Ident(ident) => ident,
        _ => return Err(ParseError.into()),
    };

    match s.next_required_token()? {
        Token::Assign => (),
        _ => return Err(ParseError.into()),
    }

    let (s, ()) = expr_adjusted_to_1(s, tracker).map_err(LexParseError::eof_into_err)?;

    tracker.current_mut()?.name_local(ident)?;

    Ok((s, ()))
}
