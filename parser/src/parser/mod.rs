#![allow(clippy::type_complexity)]

mod basic;
mod block;
mod error;
mod expr;
mod func_def;
mod prefix_expr;
mod prelude;
mod stmt;
mod traits;

use repr::chunk::Chunk;
use thiserror::Error;

use crate::lex::{Lexer, Token};
use prelude::*;

pub(crate) trait NextToken {
    type Token;

    fn next_token(&mut self) -> Result<Result<Self::Token, Eof>, LexError>;
}

impl<'s> NextToken for Lexer<'s> {
    type Token = Token<'s>;

    fn next_token(&mut self) -> Result<Result<Self::Token, Eof>, LexError> {
        match self.next() {
            Some(Ok(token)) => Ok(Ok(token)),
            Some(Err(_err)) => Err(LexError::Token(self.span())),
            None => Ok(Err(Eof)),
        }
    }
}

#[derive(Debug, Error)]
#[error("reached end of input")]
pub(crate) struct Eof;

#[derive(Debug, Error)]
#[error(transparent)]
pub struct ParseError(#[from] ParseCause);

impl ParseError {
    pub fn into_diagnostic(self) -> codespan_reporting::diagnostic::Diagnostic<()> {
        self.0.into_diagnostic()
    }
}

impl From<ParseFailure> for ParseError {
    fn from(value: ParseFailure) -> Self {
        value.cause.into()
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Lex(#[from] LexError),
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error(transparent)]
    Codegen(#[from] CodegenError),
}

impl Error {
    pub fn into_diagnostic(self) -> codespan_reporting::diagnostic::Diagnostic<()> {
        match self {
            Error::Parse(err) => err.into_diagnostic(),
            Error::Lex(err) => err.into_diagnostic(),
            _ => todo!("{:?}", self),
        }
    }
}

impl From<FailFast> for Error {
    fn from(value: FailFast) -> Self {
        match value {
            FailFast::Lex(err) => Error::Lex(err),
            FailFast::Codegen(err) => Error::Codegen(err),
        }
    }
}

pub fn chunk(s: Lexer) -> Result<Chunk, Error> {
    use crate::codegen::const_table::ConstTable;
    use crate::codegen::fragment::Frame;
    use crate::codegen::func_table::FuncTable;
    use crate::codegen::stack::Stack;
    use crate::parser::block::block;
    use repr::chunk::Signature;

    // Reserve 0-th slot in table for the script itself.
    let mut func_table = FuncTable::with_script();
    let mut const_table = ConstTable::new();
    let mut stack = Stack::new();
    let signature = Signature {
        height: 0,
        is_variadic: true,
    };

    let mut frame = Frame::script(
        func_table.view(),
        const_table.view(),
        stack.view(),
        signature,
    );
    let state = block(frame.new_core()).parse_once(s)?;

    match state {
        ParsingState::Success(mut s, _, reason) => {
            if !matches!(s.next_token(), Ok(Err(Eof))) {
                match reason {
                    CompleteOr::Other(failure) => return Err(Error::Parse(failure.into())),
                    CompleteOr::Complete(_) => {
                        let err = ParseFailure {
                            mode: FailureMode::Malformed,
                            cause: ParseCause::ExpectedStmt(s.span()),
                        };

                        return Err(Error::Parse(err.into()));
                    }
                }
            }
        }
        ParsingState::Failure(failure) => match failure {},
    }

    let func_table = {
        let script = frame.commit().resolve();
        let mut func_table = func_table.resolve();

        // Put script where runtime expects it to find.
        let _ = std::mem::replace(func_table.first_mut().unwrap(), script);

        func_table
    };
    let const_table = const_table.resolve();

    let chunk = Chunk {
        functions: func_table,
        constants: const_table,
    };

    Ok(chunk)
}
