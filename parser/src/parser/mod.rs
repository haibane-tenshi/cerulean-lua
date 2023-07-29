// We *are* using quite complex return types, but it is rather difficult to just typedef them:
// there are many moving/replaceable parts which are specific to individual parsers.
#![allow(clippy::type_complexity)]

mod basic;
mod block;
mod error;
mod expr;
mod func_def;
mod prefix_expr;
mod prelude;
mod stmt;

use repr::chunk::Chunk;
use thiserror::Error;

use crate::lex::{Lexer, Token};
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
    type Failure;

    fn optional(
        self,
        source: Self::Source,
    ) -> (Self::Source, Option<Self::Value>, Option<Self::Failure>);
}

impl<'s, T, Failure> Optional for Result<(Lexer<'s>, T), Failure> {
    type Source = Lexer<'s>;
    type Value = T;
    type Failure = Failure;

    fn optional(self, source: Self::Source) -> (Self::Source, Option<T>, Option<Failure>) {
        match self {
            Ok((s, t)) => (s, Some(t), None),
            Err(failure) => (source, None, Some(failure)),
        }
    }
}

pub fn chunk(s: Lexer) -> Result<Chunk, Error<ParseFailure>> {
    use crate::codegen::const_table::ConstTable;
    use crate::codegen::func_table::FuncTable;
    use crate::codegen::function::Function;
    use crate::codegen::stack::Stack;
    use crate::parser::block::block;

    // Reserve 0-th slot in table for the script itself.
    let mut func_table = FuncTable::with_script();
    let mut const_table = ConstTable::new();
    let mut script = Function::new();
    let mut stack = Stack::new();

    let fragment = Fragment::new(
        func_table.view(),
        const_table.view(),
        script.view(),
        stack.view(),
    );
    let (mut s, _) = block(s, fragment)?;

    if !matches!(s.next_token(), Err(NextTokenError::Parse(Eof))) {
        let err = ParseFailure {
            mode: FailureMode::Malformed,
            cause: ParseCause::ExpectedStatement,
        };

        return Err(Error::Parse(err));
    }

    let func_table = {
        let mut func_table = func_table.resolve();
        let mut script = script.resolve(0);

        // Put script where runtime expects it to find.
        std::mem::swap(func_table.first_mut().unwrap(), &mut script);

        func_table
    };
    let const_table = const_table.resolve();

    let chunk = Chunk {
        functions: func_table,
        constants: const_table,
    };

    Ok(chunk)
}
