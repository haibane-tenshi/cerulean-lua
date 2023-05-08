mod basic;
mod local_assignment;

use thiserror::Error;

use crate::lex::LexError as TokenizerError;
use crate::opcode::Chunk;
use crate::tracker2::const_::ConstTracker;
use crate::tracker2::stack::{Stack, StackView};
use prelude::*;

mod prelude {
    pub(crate) use crate::lex::{Lexer, Token};
    pub(crate) use crate::opcode::OpCode;
    pub(in crate::parser2) use crate::parser2::basic::{identifier, literal, token};
    pub(in crate::parser2) use crate::parser2::{Complete, FailureReason, NextToken, Parser};
    pub(crate) use crate::tracker2::const_::ConstTracker;
    pub(crate) use crate::tracker2::stack::StackView;
    pub(crate) use crate::tracker2::{Fragment, FragmentBuilder};
    pub(crate) use logos::Span;
}

trait NextToken<'s> {
    fn next_token(&mut self) -> Result<Token<'s>, LexError>;
}

impl<'s> NextToken<'s> for Lexer<'s> {
    fn next_token(&mut self) -> Result<Token<'s>, LexError> {
        match self.next() {
            Some(Ok(token)) => Ok(token),
            Some(Err(err)) => Err(err.into()),
            None => Err(Eof.into()),
        }
    }
}

#[derive(Debug, Error)]
#[error("reached the end of input")]
pub(crate) struct Eof;

#[derive(Debug, Error)]
pub(crate) enum LexError {
    #[error(transparent)]
    Eof(#[from] Eof),

    #[error(transparent)]
    Lex(#[from] TokenizerError),
}

pub(crate) trait Parser<Source, Value, Success, Failure> {
    fn parse(self, source: Source) -> Result<(Source, Value, Success), Failure>;
}

impl<F, Source, Value, Success, Failure> Parser<Source, Value, Success, Failure> for F
where
    F: FnOnce(Source) -> Result<(Source, Value, Success), Failure>,
{
    fn parse(self, source: Source) -> Result<(Source, Value, Success), Failure> {
        (self)(source)
    }
}

pub enum Error {}

pub fn chunk(s: Lexer) -> Result<Chunk, Error> {
    let mut stack = Stack::new();
    let mut constants = ConstTracker::default();

    let (_, frag, _) =
        local_assignment::local_assignment(StackView::new(&mut stack), &mut constants)
            .parse(s)
            .unwrap();

    let fun = frag.into_function(0);

    let functions = vec![fun].try_into().unwrap();
    let constants = constants.resolve();

    let chunk = Chunk {
        functions,
        constants,
    };

    Ok(chunk)
}

#[derive(Debug)]
pub(crate) struct Complete;

#[derive(Debug, Error)]
pub(crate) enum FailureReason<Custom> {
    #[error("there is no next token")]
    Eof(Eof),

    #[error("failed to tokenize input")]
    Lex(TokenizerError),

    #[error("parser expected a different sequence of tokens")]
    Custom(Custom),
}

impl<Custom> From<LexError> for FailureReason<Custom> {
    fn from(value: LexError) -> Self {
        match value {
            LexError::Eof(eof) => FailureReason::Eof(eof),
            LexError::Lex(err) => FailureReason::Lex(err),
        }
    }
}
