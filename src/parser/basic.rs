use crate::parser::prelude::*;
use std::borrow::Cow;

pub fn match_token<'s>(
    mut s: Lexer<'s>,
    token: Token<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    if s.next_token()? == token {
        Ok((s, ()))
    } else {
        Err(ParseError.into())
    }
}

pub fn identifier(mut s: Lexer) -> Result<(Lexer, &str), LexParseError> {
    match s.next_token()? {
        Token::Ident(ident) => Ok((s, ident)),
        _ => Err(ParseError.into()),
    }
}

pub fn literal_str(mut s: Lexer) -> Result<(Lexer, Cow<str>), LexParseError> {
    match s.next_token()? {
        Token::ShortLiteralString(r) => Ok((s, r)),
        _ => Err(ParseError.into()),
    }
}
