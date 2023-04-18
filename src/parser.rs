use std::collections::HashMap;

use crate::lex::Token;
use crate::opcode::{Chunk, ConstId, OpCode};
use crate::value::Literal;

pub struct Error;

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

fn next<'a, 's>(s: &'a [Token<'s>]) -> Result<(Token<'s>, &'a [Token<'s>]), Error> {
    let (token, s) = s.split_first().ok_or(Error)?;
    Ok((*token, s))
}

pub fn chunk(mut s: &[Token]) -> Result<Chunk, Error> {
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
) -> Result<(&'a [Token<'s>], ()), Error> {
    let (_, s) = match s {
        [Token::Local, Token::Ident(ident), Token::Assign, rest @ ..] => (ident, rest),
        _ => return Err(Error),
    };

    let (s, const_id) = literal(s, &mut storages.constants)?;

    storages.codes.push(OpCode::LoadConstant(const_id));

    Ok((s, ()))
}

fn literal<'a, 's>(
    s: &'a [Token<'s>],
    const_storage: &mut ConstStorage,
) -> Result<(&'a [Token<'s>], ConstId), Error> {
    use crate::lex::Number;

    let (token, s) = next(s)?;

    let literal = match token {
        Token::Nil => Literal::Nil,
        Token::True => Literal::Bool(true),
        Token::False => Literal::Bool(false),
        Token::Numeral(Number::Uint(value)) => Literal::Uint(value),
        Token::Numeral(Number::Float(value)) => Literal::Float(value),
        _ => return Err(Error),
    };

    let id = const_storage.insert(literal);

    Ok((s, id))
}
