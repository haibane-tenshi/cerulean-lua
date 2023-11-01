use std::borrow::Cow;

use logos::Span;
use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn match_token<'s>(
    token: Token<'static>,
) -> impl Parse<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = Complete,
    Failure = TokenMismatch,
    FailFast = LexError,
> + Copy {
    move |mut s: Lexer<'s>| {
        let r = match s.next_token()? {
            Ok(t) if t == token => {
                let r = Spanned {
                    value: (),
                    span: s.span(),
                };

                ParsingState::Success(s, r, Complete)
            }
            Ok(_) | Err(Eof) => {
                let err = TokenMismatch { span: s.span() };
                ParsingState::Failure((), err)
            }
        };

        Ok(r)
    }
}

#[derive(Debug, Error)]
#[error("encountered unexpected token at {span:?}")]
pub struct TokenMismatch {
    pub(crate) span: Span,
}

pub(crate) fn identifier(
    mut s: Lexer,
) -> Result<ParsingState<Lexer, (), Spanned<&str>, Complete, IdentMismatch>, LexError> {
    let r = match s.next_token()? {
        Ok(Token::Ident(ident)) => {
            let r = Spanned {
                value: ident,
                span: s.span(),
            };
            ParsingState::Success(s, r, Complete)
        }
        Ok(_) | Err(Eof) => {
            let span = s.span();
            ParsingState::Failure((), IdentMismatch { span })
        }
    };

    Ok(r)
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected identifier")]
pub struct IdentMismatch {
    pub(crate) span: Span,
}

impl From<Never> for IdentMismatch {
    fn from(value: Never) -> Self {
        match value {}
    }
}

pub(crate) fn literal(
    mut s: Lexer,
) -> Result<ParsingState<Lexer, (), Spanned<Literal>, Complete, LiteralMismatch>, LexError> {
    let literal = match s.next_token()? {
        Ok(Token::Nil) => Literal::Nil,
        Ok(Token::True) => Literal::Bool(true),
        Ok(Token::False) => Literal::Bool(false),
        Ok(Token::Numeral(raw_number)) => {
            match raw_number.parse().map_err(|_| LexError::Number(s.span()))? {
                Number::Int(n) => Literal::Int(n),
                Number::Float(n) => Literal::Float(n),
            }
        }
        Ok(Token::ShortLiteralString(raw_str)) => {
            let r = raw_str
                .unescape()
                .map_err(|_| LexError::Str(s.span()))?
                .to_string();
            Literal::String(r)
        }
        Ok(_) | Err(Eof) => return Ok(ParsingState::Failure((), LiteralMismatch)),
    };

    let r = Spanned {
        value: literal,
        span: s.span(),
    };

    Ok(ParsingState::Success(s, r, Complete))
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected literal")]
pub struct LiteralMismatch;

pub(crate) fn literal_str(
    mut s: Lexer,
) -> Result<ParsingState<Lexer, (), Spanned<Cow<str>>, Complete, LiteralStrMismatch>, LexError> {
    let r = match s.next_token()? {
        Ok(Token::ShortLiteralString(raw_str)) => {
            let literal_str = raw_str.unescape().map_err(|_| LexError::Str(s.span()))?;
            let r = Spanned {
                value: literal_str,
                span: s.span(),
            };
            ParsingState::Success(s, r, Complete)
        }
        Ok(_) | Err(Eof) => {
            let span = s.span();

            ParsingState::Failure((), LiteralStrMismatch { span })
        }
    };

    Ok(r)
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected literal string")]
pub struct LiteralStrMismatch {
    pub(crate) span: Span,
}
