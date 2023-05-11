mod basic;
mod block;
mod expr;
mod func_def;
mod prefix_expr;
mod prelude;
mod stmt;

use thiserror::Error;

use crate::lex::{Lexer, Token, UnrecognizedTokenError};
use crate::opcode::{Chunk, ConstCapacityError, FunctionCapacityError, InstrCountError};
use crate::tracker2::fragment::EmitError;
use crate::tracker2::stack::{
    BoundaryViolationError, GiveNameError, PopError, PushError, StackOverflowError,
    VariadicStackError,
};

use prelude::*;

#[derive(Debug, Error)]
#[error("parsing error")]
pub struct ParseError;

#[derive(Debug, Error)]
#[error("codegen error")]
pub struct CodegenError;

#[derive(Debug, Error)]
pub enum LexParseError {
    #[error(transparent)]
    Lex(#[from] UnrecognizedTokenError),

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

impl From<VariadicStackError> for LexParseError {
    fn from(_: VariadicStackError) -> Self {
        LexParseError::Codegen(CodegenError)
    }
}

impl From<StackOverflowError> for LexParseError {
    fn from(_: StackOverflowError) -> Self {
        LexParseError::Codegen(CodegenError)
    }
}

impl From<PushError> for LexParseError {
    fn from(_: PushError) -> Self {
        LexParseError::Codegen(CodegenError)
    }
}

impl From<PopError> for LexParseError {
    fn from(_: PopError) -> Self {
        LexParseError::Codegen(CodegenError)
    }
}

impl From<BoundaryViolationError> for LexParseError {
    fn from(_: BoundaryViolationError) -> Self {
        LexParseError::Codegen(CodegenError)
    }
}

impl From<GiveNameError> for LexParseError {
    fn from(_: GiveNameError) -> Self {
        LexParseError::Codegen(CodegenError)
    }
}

impl From<ConstCapacityError> for LexParseError {
    fn from(_: ConstCapacityError) -> Self {
        LexParseError::Codegen(CodegenError)
    }
}

impl From<EmitError> for LexParseError {
    fn from(_: EmitError) -> Self {
        LexParseError::Codegen(CodegenError)
    }
}

impl From<InstrCountError> for LexParseError {
    fn from(_: InstrCountError) -> Self {
        LexParseError::Codegen(CodegenError)
    }
}

impl From<FunctionCapacityError> for LexParseError {
    fn from(_: FunctionCapacityError) -> Self {
        LexParseError::Codegen(CodegenError)
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
    use crate::tracker2::chunk::Chunk;
    use crate::tracker2::function::Function;
    use crate::tracker2::stack::{Stack, StackView};

    let mut chunk = Chunk::with_script();
    let mut script = Function::new();
    let mut stack = Stack::new();

    let fragment = Fragment::new(&mut script, StackView::new(&mut stack));
    match block(s, &mut chunk, fragment) {
        Ok((mut s, ())) => match s.next_token() {
            Err(LexParseError::Eof) => (),
            _ => return Err(ParseError.into()),
        },
        Err(LexParseError::Eof) => (),
        Err(err) => return Err(err),
    }

    // Put script into where runtime expects it to find.
    let mut script = script.resolve(0);
    std::mem::swap(chunk.functions.first_mut().unwrap(), &mut script);
    let chunk = chunk.resolve();

    Ok(chunk)
}
