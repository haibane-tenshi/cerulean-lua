use crate::lex::{Lexer, Token};

use super::{ChunkTracker, LexParseError, NextToken, ParseError};

pub(super) fn statement<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let r = if let Ok(r) = semicolon(s.clone()) {
        Ok(r)
    } else if let Ok(r) = assignment(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = do_end(s.clone(), tracker) {
        Ok(r)
    } else {
        let mut s = s;
        let _ = s.next_token()?;
        Err(ParseError.into())
    };

    r
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

    let (mut s, ()) = super::block(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::End => (),
        _ => return Err(ParseError.into()),
    };

    Ok((s, ()))
}

fn assignment<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let tokens = [
        s.next_token()?,
        s.next_required_token()?,
        s.next_required_token()?,
    ];

    let ident = match tokens.as_slice() {
        [Token::Local, Token::Ident(ident), Token::Assign] => ident,
        _ => return Err(ParseError.into()),
    };

    let (s, ()) = super::expr::expr(s, tracker).map_err(LexParseError::eof_into_err)?;

    tracker.stack.pop();
    tracker.stack.push_named(ident);

    Ok((s, ()))
}
