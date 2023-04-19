use super::{LexParseError, NextToken, ParseError, Storages};
use crate::lex::{Lexer, Token};
use crate::opcode::{AriBinOp, AriUnaOp, BitBinOp, BitUnaOp};

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

    let mut s = if let Ok((s, op)) = prefix_op(s.clone()) {
        let ((), rhs_bp) = op.binding_power();
        let (s, ()) = expr_bp(s, rhs_bp, storages)?;

        let opcode = match op {
            Prefix::AriUnaOp(op) => OpCode::AriUnaOp(op),
            Prefix::BitUnaOp(op) => OpCode::BitUnaOp(op),
        };

        storages.codes.push(opcode);

        s
    } else {
        let (s, ()) = expr_atom(s, storages)?;

        s
    };

    loop {
        let Ok((ns, op)) = infix_op(s.clone()) else {
            break
        };

        let (lhs_bp, rhs_bp) = op.binding_power();

        if lhs_bp < min_bp {
            break;
        }

        (s, _) = expr_bp(ns, rhs_bp, storages).map_err(LexParseError::eof_into_err)?;

        let opcode = match op {
            Infix::AriBinOp(op) => OpCode::AriBinOp(op),
            Infix::BitBinOp(op) => OpCode::BitBinOp(op),
        };

        storages.codes.push(opcode)
    }

    Ok((s, ()))
}

fn expr_atom<'s>(s: Lexer<'s>, storages: &mut Storages) -> Result<(Lexer<'s>, ()), LexParseError> {
    literal(s, storages)
}

fn prefix_op(mut s: Lexer) -> Result<(Lexer, Prefix), LexParseError> {
    let token = s.next_token()?;

    let op = match token {
        Token::Minus => Prefix::AriUnaOp(AriUnaOp::Neg),
        Token::Tilde => Prefix::BitUnaOp(BitUnaOp::Not),
        _ => return Err(ParseError.into()),
    };

    Ok((s, op))
}

fn infix_op(mut s: Lexer) -> Result<(Lexer, Infix), LexParseError> {
    let token = s.next_token()?;

    let op = match token {
        Token::Plus => Infix::AriBinOp(AriBinOp::Add),
        Token::Minus => Infix::AriBinOp(AriBinOp::Sub),
        Token::Asterisk => Infix::AriBinOp(AriBinOp::Mul),
        Token::Slash => Infix::AriBinOp(AriBinOp::Div),
        Token::DoubleSlash => Infix::AriBinOp(AriBinOp::FloorDiv),
        Token::Percent => Infix::AriBinOp(AriBinOp::Rem),
        Token::Caret => Infix::AriBinOp(AriBinOp::Exp),
        Token::Ampersand => Infix::BitBinOp(BitBinOp::And),
        Token::Pipe => Infix::BitBinOp(BitBinOp::Or),
        Token::Tilde => Infix::BitBinOp(BitBinOp::Xor),
        Token::DoubleAngL => Infix::BitBinOp(BitBinOp::ShL),
        Token::DoubleAngR => Infix::BitBinOp(BitBinOp::ShR),
        _ => return Err(ParseError.into()),
    };

    Ok((s, op))
}

#[derive(Debug, Copy, Clone)]
enum Prefix {
    AriUnaOp(AriUnaOp),
    BitUnaOp(BitUnaOp),
}

impl Prefix {
    fn binding_power(self) -> ((), u64) {
        match self {
            Prefix::AriUnaOp(AriUnaOp::Neg) => ((), 24),
            Prefix::BitUnaOp(BitUnaOp::Not) => ((), 24),
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Infix {
    AriBinOp(AriBinOp),
    BitBinOp(BitBinOp),
}

impl Infix {
    fn binding_power(self) -> (u64, u64) {
        match self {
            Infix::AriBinOp(op) => {
                use AriBinOp::*;

                match op {
                    Add | Sub => (19, 20),
                    Mul | Div | FloorDiv | Rem => (21, 22),
                    Exp => (25, 26),
                }
            }
            Infix::BitBinOp(op) => {
                use BitBinOp::*;

                match op {
                    Or => (7, 8),
                    Xor => (9, 10),
                    And => (11, 12),
                    ShL | ShR => (13, 14),
                }
            }
        }
    }
}
