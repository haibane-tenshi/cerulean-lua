use thiserror::Error;

use crate::parser2::prelude::*;
use crate::value::Literal;

#[derive(Debug, Error)]
#[error("expected a different token")]
pub struct TokenMismatch;

pub(crate) type Reason = FailureReason<TokenMismatch>;

impl From<TokenMismatch> for Reason {
    fn from(value: TokenMismatch) -> Self {
        Reason::Custom(value)
    }
}

pub(crate) fn token<'s>(token: Token) -> impl Parser<Lexer<'s>, Span, Complete, Reason> + '_ {
    move |mut s: Lexer<'s>| -> Result<(Lexer<'s>, _, _), _> {
        if s.next_token()? == token {
            let span = s.span();
            Ok((s, span, Complete))
        } else {
            Err(TokenMismatch.into())
        }
    }
}

pub(crate) fn literal<'s>() -> impl Parser<Lexer<'s>, (Literal, Span), Complete, Reason> {
    |mut s: Lexer<'s>| match s.next_token()? {
        Token::Nil => {
            let span = s.span();
            Ok((s, (Literal::Nil, span), Complete))
        }
        _ => Err(TokenMismatch.into()),
    }
}

pub(crate) fn identifier<'s>() -> impl Parser<Lexer<'s>, (&'s str, Span), Complete, Reason> {
    |mut s: Lexer<'s>| match s.next_token()? {
        Token::Ident(name) => {
            let span = s.span();
            Ok((s, (name, span), Complete))
        }
        _ => Err(TokenMismatch.into()),
    }
}
