mod expr;
mod stmt;
mod tracker;

use thiserror::Error;

use crate::lex::{LexError, Lexer, Token};
use crate::opcode::Chunk;
use tracker::ChunkTracker;

#[derive(Debug, Error)]
#[error("parsing error")]
pub struct ParseError;

#[derive(Debug, Error)]
pub enum LexParseError {
    #[error(transparent)]
    Lex(#[from] LexError),

    #[error(transparent)]
    Parse(#[from] ParseError),

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

trait NextToken {
    type Token;

    fn next_token(&mut self) -> Result<Self::Token, LexParseError>;

    fn next_required_token(&mut self) -> Result<Self::Token, LexParseError> {
        self.next_token().map_err(LexParseError::eof_into_err)
    }
}

impl<'s> NextToken for Lexer<'s> {
    type Token = Token<'s>;

    fn next_token(&mut self) -> Result<Self::Token, LexParseError> {
        let r = self.next().ok_or(LexParseError::Eof)??;

        Ok(r)
    }
}

pub fn chunk(s: Lexer) -> Result<Chunk, LexParseError> {
    let mut tracker = ChunkTracker::empty();

    match block(s, &mut tracker) {
        Ok((mut s, ())) => match s.next_token() {
            Err(LexParseError::Eof) => (),
            _ => return Err(ParseError.into()),
        },
        Err(LexParseError::Eof) => (),
        Err(err) => return Err(err),
    }

    tracker.resolve().map_err(|_| ParseError.into())
}

fn block<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    tracker.push_block();

    let r = inner_block(s, tracker);

    tracker.pop_block().unwrap();

    r
}

fn inner_block<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    loop {
        s = match stmt::statement(s.clone(), tracker) {
            Ok((s, ())) => s,
            Err(_) => break,
        };
    }

    Ok((s, ()))
}
