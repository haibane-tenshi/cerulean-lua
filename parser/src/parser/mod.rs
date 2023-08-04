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
            Some(Err(err)) => Err(LexError::Token(err)),
            None => Ok(Err(Eof)),
        }
    }
}

#[derive(Debug, Error)]
#[error("reached end of input")]
pub(crate) struct Eof;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Lex(#[from] LexError),
    #[error(transparent)]
    Parse(#[from] ParseFailure),
    #[error(transparent)]
    Codegen(#[from] CodegenError),
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
    let state = block(fragment).parse_once(s)?;

    match state {
        ParsingState::Success(mut s, _, reason) => {
            if !matches!(s.next_token(), Ok(Err(Eof))) {
                match reason {
                    CompleteOr::Other(failure) => return Err(Error::Parse(failure)),
                    CompleteOr::Complete(_) => {
                        let err = ParseFailure {
                            mode: FailureMode::Malformed,
                            cause: ParseCause::ExpectedStatement,
                        };

                        return Err(Error::Parse(err));
                    }
                }
            }
        }
        ParsingState::Failure(failure) => match failure {},
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
