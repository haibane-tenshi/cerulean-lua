mod expr;

use std::collections::HashMap;

use thiserror::Error;

use crate::lex::{LexError, Lexer, Token};
use crate::opcode::{Chunk, ConstId, OpCode};
use crate::value::Literal;

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

#[derive(Debug, Default)]
struct ConstStorage {
    constants: Vec<Literal>,
    backlinks: HashMap<Literal, ConstId>,
}

impl ConstStorage {
    pub fn insert(&mut self, value: Literal) -> ConstId {
        *self.backlinks.entry(value).or_insert_with(|| {
            let index = self.constants.len().try_into().unwrap();
            self.constants.push(value);

            ConstId(index)
        })
    }

    pub fn resolve(self) -> Vec<Literal> {
        self.constants
    }
}

#[derive(Debug, Default)]
struct Storages {
    constants: ConstStorage,
    codes: Vec<OpCode>,
}

pub fn chunk(mut s: Lexer) -> Result<Chunk, LexParseError> {
    let mut storages = Storages::default();

    loop {
        s = match assignment(s, &mut storages) {
            Ok((s, ())) => s,
            Err(LexParseError::Eof) => break,
            Err(err) => return Err(err),
        };
    }

    let Storages { codes, constants } = storages;

    let constants = constants.resolve();

    let chunk = Chunk {
        codes,
        constants,
        lines: Default::default(),
    };

    Ok(chunk)
}

fn assignment<'s>(
    mut s: Lexer<'s>,
    storages: &mut Storages,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let tokens = [
        s.next_token()?,
        s.next_required_token()?,
        s.next_required_token()?,
    ];

    let _ = match tokens.as_slice() {
        [Token::Local, Token::Ident(ident), Token::Assign] => ident,
        _ => return Err(ParseError.into()),
    };

    let (s, ()) = expr::expr(s, storages).map_err(LexParseError::eof_into_err)?;

    Ok((s, ()))
}
