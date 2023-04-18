use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;

use crate::lex::Token;
use crate::opcode::{Chunk, ConstId, OpCode};
use crate::value::Literal;

#[derive(Debug)]
pub struct ParseError;

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "parsing error")
    }
}

impl Error for ParseError {}

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

fn next<'a, 's>(s: &'a [Token<'s>]) -> Result<(Token<'s>, &'a [Token<'s>]), ParseError> {
    let (token, s) = s.split_first().ok_or(ParseError)?;
    Ok((*token, s))
}

pub fn chunk(mut s: &[Token]) -> Result<Chunk, ParseError> {
    let mut storages = Storages::default();

    while !s.is_empty() {
        (s, _) = assignment(s, &mut storages)?;
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

fn assignment<'a, 's>(
    s: &'a [Token<'s>],
    storages: &mut Storages,
) -> Result<(&'a [Token<'s>], ()), ParseError> {
    let (_, s) = match s {
        [Token::Local, Token::Ident(ident), Token::Assign, rest @ ..] => (ident, rest),
        _ => return Err(ParseError),
    };

    let (s, const_id) = literal(s, &mut storages.constants)?;

    storages.codes.push(OpCode::LoadConstant(const_id));

    Ok((s, ()))
}

fn literal<'a, 's>(
    s: &'a [Token<'s>],
    const_storage: &mut ConstStorage,
) -> Result<(&'a [Token<'s>], ConstId), ParseError> {
    use crate::lex::Number;

    let (token, s) = next(s)?;

    let literal = match token {
        Token::Nil => Literal::Nil,
        Token::True => Literal::Bool(true),
        Token::False => Literal::Bool(false),
        Token::Numeral(Number::Uint(value)) => Literal::Uint(value),
        Token::Numeral(Number::Float(value)) => Literal::Float(value),
        _ => return Err(ParseError),
    };

    let id = const_storage.insert(literal);

    Ok((s, id))
}
