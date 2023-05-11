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

pub fn literal(mut s: Lexer) -> Result<(Lexer, Literal), LexParseError> {
    let literal = match s.next_token()? {
        Token::Nil => Literal::Nil,
        Token::True => Literal::Bool(true),
        Token::False => Literal::Bool(false),
        Token::Numeral(Number::Int(value)) => Literal::Int(value),
        Token::Numeral(Number::Float(value)) => Literal::Float(value),
        Token::ShortLiteralString(value) => Literal::String(value.to_string()),
        _ => return Err(ParseError.into()),
    };

    Ok((s, literal))
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
