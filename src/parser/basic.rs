use std::borrow::Cow;

use logos::Span;
use thiserror::Error;

use crate::parser::prelude::*;
use crate::parser::NextTokenError;

pub(crate) fn match_token<'s>(
    mut s: Lexer<'s>,
    token: Token<'static>,
) -> Result<(Lexer<'s>, Span, Complete), ParseError<TokenMismatch>> {
    match s.next_token() {
        Ok(t) if t == token => {
            let span = s.span();
            Ok((s, span, Complete))
        }
        Ok(_) | Err(NextTokenError::Parse(Eof)) => {
            let err = TokenMismatch { expected: token };
            Err(err.into())
        }
        Err(NextTokenError::Lex(err)) => Err(err.into()),
    }
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected {expected:?}")]
pub struct TokenMismatch {
    expected: Token<'static>,
}

impl From<TokenMismatch> for ParseError<TokenMismatch> {
    fn from(value: TokenMismatch) -> Self {
        ParseError::Parse(value)
    }
}

pub(crate) fn identifier(
    mut s: Lexer,
) -> Result<(Lexer, (&str, Span), Complete), ParseError<IdentMismatch>> {
    match s.next_token() {
        Ok(Token::Ident(ident)) => {
            let span = s.span();
            Ok((s, (ident, span), Complete))
        }
        Ok(_) | Err(NextTokenError::Parse(Eof)) => Err(IdentMismatch.into()),
        Err(NextTokenError::Lex(lex)) => Err(lex.into()),
    }
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected identifier")]
pub struct IdentMismatch;

impl From<IdentMismatch> for ParseError<IdentMismatch> {
    fn from(value: IdentMismatch) -> Self {
        ParseError::Parse(value)
    }
}

pub(crate) fn literal(
    mut s: Lexer,
) -> Result<(Lexer, (Literal, Span), Complete), ParseError<LiteralMismatch>> {
    let literal = match s.next_token() {
        Ok(Token::Nil) => Literal::Nil,
        Ok(Token::True) => Literal::Bool(true),
        Ok(Token::False) => Literal::Bool(false),
        Ok(Token::Numeral(raw_number)) => match raw_number.parse()? {
            Number::Int(n) => Literal::Int(n),
            Number::Float(n) => Literal::Float(n),
        },
        Ok(Token::ShortLiteralString(raw_str)) => {
            let r = raw_str.unescape()?.to_string();
            Literal::String(r)
        }
        Ok(_) | Err(NextTokenError::Parse(Eof)) => return Err(LiteralMismatch.into()),
        Err(NextTokenError::Lex(lex)) => return Err(lex.into()),
    };
    let span = s.span();

    Ok((s, (literal, span), Complete))
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected literal")]
pub struct LiteralMismatch;

impl From<LiteralMismatch> for ParseError<LiteralMismatch> {
    fn from(value: LiteralMismatch) -> Self {
        ParseError::Parse(value)
    }
}

pub(crate) fn literal_str(
    mut s: Lexer,
) -> Result<(Lexer, (Cow<str>, Span), Complete), ParseError<LiteralStrMismatch>> {
    match s.next_token() {
        Ok(Token::ShortLiteralString(raw_str)) => {
            let r = raw_str.unescape()?;
            let span = s.span();
            Ok((s, (r, span), Complete))
        }
        Ok(_) | Err(NextTokenError::Parse(Eof)) => Err(LiteralStrMismatch.into()),
        Err(NextTokenError::Lex(lex)) => Err(lex.into()),
    }
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected literal string")]
pub struct LiteralStrMismatch;

impl From<LiteralStrMismatch> for ParseError<LiteralStrMismatch> {
    fn from(value: LiteralStrMismatch) -> Self {
        ParseError::Parse(value)
    }
}
