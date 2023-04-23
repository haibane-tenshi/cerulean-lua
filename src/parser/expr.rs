use super::tracker::ChunkTracker;
use super::{func_body, prefix_expr, LexParseError, NextToken, ParseError};
use crate::lex::{Lexer, Token};
use crate::opcode::{AriBinOp, AriUnaOp, BitBinOp, BitUnaOp, RelBinOp, StrBinOp};

fn literal<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker,
) -> Result<(Lexer<'s>, ()), LexParseError> {
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
        Token::ShortLiteralString(value) => Literal::String(value.to_string()),
        _ => return Err(ParseError.into()),
    };

    let id = tracker.insert_literal(literal);
    tracker.push(OpCode::LoadConstant(id));

    Ok((s, ()))
}

fn function<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;
    use crate::value::Literal;

    match s.next_token()? {
        Token::Function => (),
        _ => return Err(ParseError.into()),
    }

    let (s, func_id) = func_body(s, tracker).map_err(LexParseError::eof_into_err)?;

    let const_id = tracker.insert_literal(Literal::Function(func_id));
    tracker.push(OpCode::LoadConstant(const_id));

    Ok((s, ()))
}

pub(super) fn par_expr<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    match s.next_token()? {
        Token::ParL => (),
        _ => return Err(ParseError.into()),
    }

    let (mut s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::ParR => (),
        _ => return Err(ParseError.into()),
    }

    Ok((s, ()))
}

pub(super) fn expr<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    expr_bp(s, 0, tracker)
}

fn expr_bp<'s>(
    s: Lexer<'s>,
    min_bp: u64,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    let mut s = if let Ok((s, op)) = prefix_op(s.clone()) {
        let ((), rhs_bp) = op.binding_power();
        let (s, ()) = expr_bp(s, rhs_bp, tracker)?;

        let opcode = match op {
            Prefix::Ari(op) => OpCode::AriUnaOp(op),
            Prefix::Bit(op) => OpCode::BitUnaOp(op),
        };

        tracker.push(opcode);

        s
    } else {
        let (s, ()) = atom(s, tracker)?;

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

        (s, _) = expr_bp(ns, rhs_bp, tracker).map_err(LexParseError::eof_into_err)?;

        let opcode = match op {
            Infix::Ari(op) => OpCode::AriBinOp(op),
            Infix::Bit(op) => OpCode::BitBinOp(op),
            Infix::Rel(op) => OpCode::RelBinOp(op),
            Infix::Str(op) => OpCode::StrBinOp(op),
        };

        // Stack effect: pop 2 -> push 1
        tracker.push(opcode);
    }

    Ok((s, ()))
}

fn atom<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    if let Ok(r) = literal(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = prefix_expr(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = function(s, tracker) {
        Ok(r)
    } else {
        Err(ParseError.into())
    }
}

fn prefix_op(mut s: Lexer) -> Result<(Lexer, Prefix), LexParseError> {
    let token = s.next_token()?;

    let op = match token {
        Token::Minus => Prefix::Ari(AriUnaOp::Neg),
        Token::Tilde => Prefix::Bit(BitUnaOp::Not),
        _ => return Err(ParseError.into()),
    };

    Ok((s, op))
}

fn infix_op(mut s: Lexer) -> Result<(Lexer, Infix), LexParseError> {
    let token = s.next_token()?;

    let op = match token {
        Token::Plus => Infix::Ari(AriBinOp::Add),
        Token::Minus => Infix::Ari(AriBinOp::Sub),
        Token::Asterisk => Infix::Ari(AriBinOp::Mul),
        Token::Slash => Infix::Ari(AriBinOp::Div),
        Token::DoubleSlash => Infix::Ari(AriBinOp::FloorDiv),
        Token::Percent => Infix::Ari(AriBinOp::Rem),
        Token::Caret => Infix::Ari(AriBinOp::Exp),
        Token::Ampersand => Infix::Bit(BitBinOp::And),
        Token::Pipe => Infix::Bit(BitBinOp::Or),
        Token::Tilde => Infix::Bit(BitBinOp::Xor),
        Token::DoubleAngL => Infix::Bit(BitBinOp::ShL),
        Token::DoubleAngR => Infix::Bit(BitBinOp::ShR),
        Token::DoubleEqual => Infix::Rel(RelBinOp::Eq),
        Token::TildeEqual => Infix::Rel(RelBinOp::Neq),
        Token::AngL => Infix::Rel(RelBinOp::Le),
        Token::AngLEqual => Infix::Rel(RelBinOp::Lt),
        Token::AngR => Infix::Rel(RelBinOp::Ge),
        Token::AngREqual => Infix::Rel(RelBinOp::Gt),
        Token::DoubleDot => Infix::Str(StrBinOp::Concat),
        _ => return Err(ParseError.into()),
    };

    Ok((s, op))
}

#[derive(Debug, Copy, Clone)]
enum Prefix {
    Ari(AriUnaOp),
    Bit(BitUnaOp),
}

impl Prefix {
    fn binding_power(self) -> ((), u64) {
        match self {
            Prefix::Ari(AriUnaOp::Neg) => ((), 24),
            Prefix::Bit(BitUnaOp::Not) => ((), 24),
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Infix {
    Ari(AriBinOp),
    Bit(BitBinOp),
    Rel(RelBinOp),
    Str(StrBinOp),
}

impl Infix {
    fn binding_power(self) -> (u64, u64) {
        match self {
            Infix::Ari(op) => {
                use AriBinOp::*;

                match op {
                    Add | Sub => (19, 20),
                    Mul | Div | FloorDiv | Rem => (21, 22),
                    Exp => (25, 26),
                }
            }
            Infix::Bit(op) => {
                use BitBinOp::*;

                match op {
                    Or => (7, 8),
                    Xor => (9, 10),
                    And => (11, 12),
                    ShL | ShR => (13, 14),
                }
            }
            Infix::Rel(_) => (5, 6),
            Infix::Str(_) => (15, 16),
        }
    }
}
