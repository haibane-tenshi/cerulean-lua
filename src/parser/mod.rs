mod basic;
mod block;
mod expr;
mod func_def;
mod prefix_expr;
mod prelude;
mod stmt;

use std::borrow::Cow;
use thiserror::Error;

use crate::lex::{LexError, Lexer, Token};
use crate::opcode::{
    Chunk, ConstCapacityError, FunctionCapacityError, FunctionId, InstrCountError,
};
use crate::tracker::{
    BackpatchError, ChunkTracker, EmitError, Error as CodegenError, FinishFnError, NoActiveFnError,
    StackStateError,
};

use expr::expr;
use prelude::*;
use stmt::{return_, statement};

#[derive(Debug, Error)]
#[error("parsing error")]
pub struct ParseError;

#[derive(Debug, Error)]
pub enum LexParseError {
    #[error(transparent)]
    Lex(#[from] LexError),

    #[error(transparent)]
    Parse(#[from] ParseError),

    #[error("failed to generate code")]
    Codegen(#[from] CodegenError),

    #[error("reached end of input")]
    Eof,
}

impl LexParseError {
    pub fn eof_into_err(self) -> Self {
        match self {
            LexParseError::Eof => LexParseError::Parse(ParseError),
            t => t,
        }
    }
}

impl From<ConstCapacityError> for LexParseError {
    fn from(value: ConstCapacityError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<NoActiveFnError> for LexParseError {
    fn from(value: NoActiveFnError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<EmitError> for LexParseError {
    fn from(value: EmitError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<StackStateError> for LexParseError {
    fn from(value: StackStateError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<InstrCountError> for LexParseError {
    fn from(value: InstrCountError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<FunctionCapacityError> for LexParseError {
    fn from(value: FunctionCapacityError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<FinishFnError> for LexParseError {
    fn from(value: FinishFnError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<BackpatchError> for LexParseError {
    fn from(value: BackpatchError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

trait NextToken {
    type Token;

    fn next_token(&mut self) -> Result<Self::Token, LexParseError>;
}

impl<'s> NextToken for Lexer<'s> {
    type Token = Token<'s>;

    fn next_token(&mut self) -> Result<Self::Token, LexParseError> {
        let r = self.next().ok_or(LexParseError::Eof)??;

        Ok(r)
    }
}

trait Require {
    fn require(self) -> Self;
}

impl<'s, T> Require for Result<(Lexer<'s>, T), LexParseError> {
    fn require(self) -> Self {
        self.map_err(LexParseError::eof_into_err)
    }
}

trait Optional<T> {
    type Source;

    fn optional(self, source: Self::Source) -> (Self::Source, Option<T>);
}

impl<'s, T> Optional<T> for Result<(Lexer<'s>, T), LexParseError> {
    type Source = Lexer<'s>;

    fn optional(self, source: Self::Source) -> (Self::Source, Option<T>) {
        match self {
            Ok((s, t)) => (s, Some(t)),
            Err(_) => (source, None),
        }
    }
}

pub fn chunk(s: Lexer) -> Result<Chunk, LexParseError> {
    use crate::parser::block::block;

    let mut tracker = ChunkTracker::empty();

    tracker.start_fn()?;

    match block(s, &mut tracker) {
        Ok((mut s, ())) => match s.next_token() {
            Err(LexParseError::Eof) => (),
            _ => return Err(ParseError.into()),
        },
        Err(LexParseError::Eof) => (),
        Err(err) => return Err(err),
    }

    tracker.finish_fn(0)?;
    tracker.resolve().map_err(|_| ParseError.into())
}
