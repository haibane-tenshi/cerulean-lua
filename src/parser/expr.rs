use super::{next, ParseError, Storages};
use crate::lex::Token;
use crate::opcode::BinaryOp;

fn literal<'a, 's>(
    s: &'a [Token<'s>],
    storage: &mut Storages,
) -> Result<(&'a [Token<'s>], ()), ParseError> {
    use crate::lex::Number;
    use crate::opcode::OpCode;
    use crate::value::Literal;

    let (token, s) = next(s)?;

    let literal = match token {
        Token::Nil => Literal::Nil,
        Token::True => Literal::Bool(true),
        Token::False => Literal::Bool(false),
        Token::Numeral(Number::Uint(value)) => Literal::Uint(value),
        Token::Numeral(Number::Float(value)) => Literal::Float(value),
        _ => return Err(ParseError),
    };

    let id = storage.constants.insert(literal);
    storage.codes.push(OpCode::LoadConstant(id));

    Ok((s, ()))
}

pub(super) fn expr<'a, 's>(
    s: &'a [Token<'s>],
    storages: &mut Storages,
) -> Result<(&'a [Token<'s>], ()), ParseError> {
    expr_bp(s, 0, storages)
}

fn expr_bp<'a, 's>(
    s: &'a [Token<'s>],
    min_bp: u64,
    storages: &mut Storages,
) -> Result<(&'a [Token<'s>], ()), ParseError> {
    use crate::opcode::OpCode;

    let (mut s, ()) = expr_atom(s, storages)?;

    loop {
        let Ok((ns, op)) = bin_op(s) else {
            break
        };

        let (lhs_bp, rhs_bp) = infix_binding_power(op);

        if lhs_bp < min_bp {
            break;
        }

        (s, _) = expr_bp(ns, rhs_bp, storages)?;

        storages.codes.push(OpCode::BinaryOp(op))
    }

    Ok((s, ()))
}

fn expr_atom<'a, 's>(
    s: &'a [Token<'s>],
    storages: &mut Storages,
) -> Result<(&'a [Token<'s>], ()), ParseError> {
    literal(s, storages)
}

fn bin_op<'a, 's>(s: &'a [Token<'s>]) -> Result<(&'a [Token<'s>], BinaryOp), ParseError> {
    let (token, s) = next(s)?;

    let op = match token {
        Token::Plus => BinaryOp::Add,
        Token::Minus => BinaryOp::Sub,
        Token::Asterisk => BinaryOp::Mul,
        Token::Slash => BinaryOp::Div,
        Token::DoubleSlash => BinaryOp::FloorDiv,
        Token::Percent => BinaryOp::Rem,
        Token::Caret => BinaryOp::Exp,
        _ => return Err(ParseError),
    };

    Ok((s, op))
}

pub fn infix_binding_power(op: BinaryOp) -> (u64, u64) {
    use BinaryOp::*;

    match op {
        Add | Sub => (19, 20),
        Mul | Div | FloorDiv | Rem => (21, 22),
        Exp => (25, 26),
    }
}
