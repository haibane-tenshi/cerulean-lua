use std::collections::HashMap;

use crate::lex::Token;
use crate::opcode::{Chunk, ConstId};
use crate::value::Literal;

pub struct Error;

#[derive(Debug, Default)]
struct ConstStorage {
    constants: Vec<Literal>,
    backlinks: HashMap<Literal, ConstId>,
}

impl ConstStorage {
    pub fn new() -> Self {
        Default::default()
    }

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

fn next<'a, 's>(s: &'a [Token<'s>]) -> Result<(Token<'s>, &'a [Token<'s>]), Error> {
    let (token, s) = s.split_first().ok_or(Error)?;
    Ok((*token, s))
}

pub fn chunk(s: &[Token]) -> Result<Chunk, Error> {
    todo!()
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
