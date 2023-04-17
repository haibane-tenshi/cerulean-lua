use crate::lex::Token;
use crate::opcode::Chunk;
use crate::value::Literal;

pub struct Error;

fn next<'a, 's>(s: &'a [Token<'s>]) -> Result<(Token<'s>, &'a [Token<'s>]), Error> {
    let (token, s) = s.split_first().ok_or(Error)?;
    Ok((*token, s))
}

pub fn chunk(s: &[Token]) -> Result<Chunk, Error> {
    todo!()
}

fn literal<'a, 's>(s: &'a [Token<'s>]) -> Result<(&'a [Token<'s>], Literal), Error> {
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

    Ok((s, literal))
}
