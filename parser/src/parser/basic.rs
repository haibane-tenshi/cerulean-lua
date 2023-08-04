use std::borrow::Cow;

use logos::Span;
use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn match_token<'s>(
    token: Token<'static>,
) -> impl Parse<
    Lexer<'s>,
    Output = Span,
    Success = Complete,
    Failure = TokenMismatch,
    FailFast = LexError,
> + Copy {
    move |mut s: Lexer<'s>| {
        let r = match s.next_token()? {
            Ok(t) if t == token => {
                let span = s.span();
                ParsingState::Success(s, span, Complete)
            }
            Ok(_) | Err(Eof) => {
                let err = TokenMismatch { expected: token };
                ParsingState::Failure(err)
            }
        };

        Ok(r)
    }
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected {expected:?}")]
pub struct TokenMismatch {
    expected: Token<'static>,
}

pub(crate) fn identifier(
    mut s: Lexer,
) -> Result<ParsingState<Lexer, (&str, Span), Complete, IdentMismatch>, LexError> {
    let r = match s.next_token()? {
        Ok(Token::Ident(ident)) => {
            let span = s.span();
            ParsingState::Success(s, (ident, span), Complete)
        }
        Ok(_) | Err(Eof) => ParsingState::Failure(IdentMismatch),
    };

    Ok(r)
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected identifier")]
pub struct IdentMismatch;

impl From<Never> for IdentMismatch {
    fn from(value: Never) -> Self {
        match value {}
    }
}

pub(crate) fn literal(
    mut s: Lexer,
) -> Result<ParsingState<Lexer, (Literal, Span), Complete, LiteralMismatch>, LexError> {
    let literal = match s.next_token()? {
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
        Ok(_) | Err(Eof) => return Ok(ParsingState::Failure(LiteralMismatch)),
    };
    let span = s.span();

    Ok(ParsingState::Success(s, (literal, span), Complete))
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected literal")]
pub struct LiteralMismatch;

pub(crate) fn literal_str(
    mut s: Lexer,
) -> Result<ParsingState<Lexer, (Cow<str>, Span), Complete, LiteralStrMismatch>, LexError> {
    let r = match s.next_token()? {
        Ok(Token::ShortLiteralString(raw_str)) => {
            let r = raw_str.unescape()?;
            let span = s.span();
            ParsingState::Success(s, (r, span), Complete)
        }
        Ok(_) | Err(Eof) => ParsingState::Failure(LiteralStrMismatch.into()),
    };

    Ok(r)
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected literal string")]
pub struct LiteralStrMismatch;
