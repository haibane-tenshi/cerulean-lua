mod basic;
mod block;
mod error;
mod expr;
mod func_def;
mod prefix_expr;
mod prelude;
mod stmt;

use either::Either;
use std::ops::{BitOr, BitOrAssign};
use thiserror::Error;

use crate::codegen::fragment::EmitError;
use crate::codegen::stack::{
    BoundaryViolationError, GiveNameError, PopError, PushError, StackOverflowError,
    VariadicStackError,
};
use crate::lex::{Error as LexError, Lexer, Token};
use expr::function::FunctionFailure;
use expr::table::{TabBracketFailure, TabFailure, TabNameFailure};
use expr::ParExprFailure;
use func_def::FuncDefFailure;
use prefix_expr::{ArgsParExprFailure, FieldFailure, IndexFailure, VariableFailure};
use repr::chunk::Chunk;
use repr::index::{ConstCapacityError, FunctionCapacityError, InstrCountError};
use stmt::assignment::AssignmentFailure;
use stmt::do_end::DoEndFailure;
use stmt::generic_for::GenericForFailure;
use stmt::if_then::IfThenFailure;
use stmt::local_assignment::LocalAssignmentFailure;
use stmt::local_function::LocalFunctionFailure;
use stmt::numerical_for::NumericalForFailure;
use stmt::repeat_until::RepeatUntilFailure;
use stmt::while_do::WhileDoFailure;

use prelude::*;

pub(crate) trait MapParse<F> {
    type Output;

    fn map_parse(self, f: F) -> Self::Output;
}

impl<T, U, E, F> MapParse<F> for Result<T, ParseError<E>>
where
    F: FnOnce(E) -> U,
{
    type Output = Result<T, ParseError<U>>;

    fn map_parse(self, f: F) -> Self::Output {
        match self {
            Ok(t) => Ok(t),
            Err(ParseError::Parse(err)) => Err(ParseError::Parse(f(err))),
            Err(ParseError::Lex(err)) => Err(ParseError::Lex(err)),
        }
    }
}

impl<T, U, E, F> MapParse<F> for Result<T, Error<E>>
where
    F: FnOnce(E) -> U,
{
    type Output = Result<T, Error<U>>;

    fn map_parse(self, f: F) -> Self::Output {
        self.map_err(|err| err.map_parse(f))
    }
}

pub(crate) trait NextToken {
    type Token;

    fn next_token(&mut self) -> Result<Self::Token, NextTokenError>;
}

impl<'s> NextToken for Lexer<'s> {
    type Token = Token<'s>;

    fn next_token(&mut self) -> Result<Self::Token, NextTokenError> {
        let r = self.next().ok_or(NextTokenError::Parse(Eof))??;

        Ok(r)
    }
}

#[derive(Debug, Error)]
#[error("reached end of input")]
pub(crate) struct Eof;

type NextTokenError = ParseError<Eof>;

#[derive(Debug)]
pub(crate) struct Complete;

impl HaveFailureMode for Complete {
    fn mode(&self) -> FailureMode {
        FailureMode::Success
    }
}

pub(crate) trait Optional {
    type Source;
    type Value;
    type Success;
    type Failure;

    fn optional(
        self,
        source: Self::Source,
    ) -> (
        Self::Source,
        Option<Self::Value>,
        Either<Self::Success, Self::Failure>,
    );
}

impl<'s, T, Success, Failure> Optional for Result<(Lexer<'s>, T, Success), Failure> {
    type Source = Lexer<'s>;
    type Value = T;
    type Success = Success;
    type Failure = Failure;

    fn optional(self, source: Self::Source) -> (Self::Source, Option<T>, Either<Success, Failure>) {
        match self {
            Ok((s, t, success)) => (s, Some(t), Either::Left(success)),
            Err(failure) => (source, None, Either::Right(failure)),
        }
    }
}

pub fn chunk(s: Lexer) -> Result<Chunk, Error<ParseFailure>> {
    use crate::codegen::chunk::Chunk;
    use crate::codegen::function::Function;
    use crate::codegen::stack::{Stack, StackView};
    use crate::parser::block::block;

    let mut chunk = Chunk::with_script();
    let mut script = Function::new();
    let mut stack = Stack::new();

    let fragment = Fragment::new(&mut script, StackView::new(&mut stack));
    let _ = block(s, &mut chunk, fragment)?;

    // Put script into where runtime expects it to find.
    let mut script = script.resolve(0);
    std::mem::swap(chunk.functions.first_mut().unwrap(), &mut script);
    let chunk = chunk.resolve();

    Ok(chunk)
}
