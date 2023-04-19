use super::{LexParseError, NextToken, ParseError, Storages};
use crate::lex::{Lexer, Token};
use crate::opcode::{BinaryOp, UnaryOp};

fn literal<'s>(mut s: Lexer<'s>, storage: &mut Storages) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Number;
    use crate::opcode::OpCode;
    use crate::value::Literal;

    let token = s.next_token()?;

    let literal = match token {
        Token::Nil => Literal::Nil,
        Token::True => Literal::Bool(true),
        Token::False => Literal::Bool(false),
        Token::Numeral(Number::Int(value)) => Literal::Int(value),
        Token::Numeral(Number::Float(value)) => Literal::Float(value),
        _ => return Err(ParseError.into()),
    };

    let id = storage.constants.insert(literal);
    storage.codes.push(OpCode::LoadConstant(id));

    Ok((s, ()))
}

pub(super) fn expr<'s>(
    s: Lexer<'s>,
    storages: &mut Storages,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    expr_bp(s, 0, storages)
}

fn expr_bp<'s>(
    s: Lexer<'s>,
    min_bp: u64,
    storages: &mut Storages,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    let mut s = if let Ok((s, op)) = unary_op(s.clone()) {
        let ((), rhs_bp) = prefix_binding_power(op);
        let (s, ()) = expr_bp(s, rhs_bp, storages)?;

        storages.codes.push(OpCode::UnaryOp(UnaryOp::Neg));

        s
    } else {
        let (s, ()) = expr_atom(s, storages)?;

        s
    };

    loop {
        let Ok((ns, op)) = bin_op(s.clone()) else {
            break
        };

        let (lhs_bp, rhs_bp) = infix_binding_power(op);

        if lhs_bp < min_bp {
            break;
        }

        (s, _) = expr_bp(ns, rhs_bp, storages).map_err(LexParseError::eof_into_err)?;

        storages.codes.push(OpCode::BinaryOp(op))
    }

    Ok((s, ()))
}

fn expr_atom<'s>(s: Lexer<'s>, storages: &mut Storages) -> Result<(Lexer<'s>, ()), LexParseError> {
    literal(s, storages)
}

fn unary_op(mut s: Lexer) -> Result<(Lexer, UnaryOp), LexParseError> {
    let token = s.next_token()?;

    let op = match token {
        Token::Minus => UnaryOp::Neg,
        _ => return Err(ParseError.into()),
    };

    Ok((s, op))
}

fn bin_op(mut s: Lexer) -> Result<(Lexer, BinaryOp), LexParseError> {
    let token = s.next_token()?;

    let op = match token {
        Token::Plus => BinaryOp::Add,
        Token::Minus => BinaryOp::Sub,
        Token::Asterisk => BinaryOp::Mul,
        Token::Slash => BinaryOp::Div,
        Token::DoubleSlash => BinaryOp::FloorDiv,
        Token::Percent => BinaryOp::Rem,
        Token::Caret => BinaryOp::Exp,
        _ => return Err(ParseError.into()),
    };

    Ok((s, op))
}

fn infix_binding_power(op: BinaryOp) -> (u64, u64) {
    use BinaryOp::*;

    match op {
        Add | Sub => (19, 20),
        Mul | Div | FloorDiv | Rem => (21, 22),
        Exp => (25, 26),
    }
}

fn prefix_binding_power(op: UnaryOp) -> ((), u64) {
    match op {
        UnaryOp::Neg => ((), 24),
    }
}
