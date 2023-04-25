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

    let id = tracker.insert_literal(literal)?;
    tracker.current_mut()?.emit(OpCode::LoadConstant(id))?;

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

    let const_id = tracker.insert_literal(Literal::Function(func_id))?;
    tracker
        .current_mut()?
        .emit(OpCode::LoadConstant(const_id))?;

    Ok((s, ()))
}

fn table<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    match s.next_token()? {
        Token::CurlyL => (),
        _ => return Err(ParseError.into()),
    }

    let table_slot = tracker.current()?.stack_top()?;
    tracker.current_mut()?.emit(OpCode::TabCreate)?;

    let mut field_list = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        let mut index = 1;

        let mut field = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
            use crate::value::Literal;

            let bracket = |mut s: Lexer<'s>,
                           tracker: &mut ChunkTracker<'s>|
             -> Result<(Lexer<'s>, ()), LexParseError> {
                match s.next_token()? {
                    Token::BracketL => (),
                    _ => return Err(ParseError.into()),
                }

                tracker.current_mut()?.emit(OpCode::LoadStack(table_slot))?;

                let top = tracker.current()?.stack_top()?.next();
                let (mut s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;
                tracker.current_mut()?.emit_adjust_to(top)?;

                match s.next_required_token()? {
                    Token::BracketR => (),
                    _ => return Err(ParseError.into()),
                }

                match s.next_required_token()? {
                    Token::Assign => (),
                    _ => return Err(ParseError.into()),
                }

                let top = top.next();
                let (s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;
                tracker.current_mut()?.emit_adjust_to(top)?;

                tracker.current_mut()?.emit(OpCode::TabSet)?;

                Ok((s, ()))
            };

            let name = |mut s: Lexer<'s>,
                        tracker: &mut ChunkTracker<'s>|
             -> Result<(Lexer<'s>, ()), LexParseError> {
                let ident = match s.next_token()? {
                    Token::Ident(ident) => ident,
                    _ => return Err(ParseError.into()),
                };

                tracker.current_mut()?.emit(OpCode::LoadStack(table_slot))?;

                let const_id = tracker.insert_literal(Literal::String(ident.to_string()))?;
                tracker
                    .current_mut()?
                    .emit(OpCode::LoadConstant(const_id))?;

                match s.next_required_token()? {
                    Token::Assign => (),
                    _ => return Err(ParseError.into()),
                }

                let top = tracker.current()?.stack_top()?.next();
                let (s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;
                tracker.current_mut()?.emit_adjust_to(top)?;

                tracker.current_mut()?.emit(OpCode::TabSet)?;

                Ok((s, ()))
            };

            let mut index = |s: Lexer<'s>,
                             tracker: &mut ChunkTracker<'s>|
             -> Result<(Lexer<'s>, ()), LexParseError> {
                let start = tracker.current()?.stack_top()?;
                let r = expr(s, tracker)?;
                tracker.current_mut()?.emit_adjust_to(start.next())?;

                tracker.current_mut()?.emit(OpCode::LoadStack(table_slot))?;

                let const_id = tracker.insert_literal(Literal::Int(index))?;
                tracker
                    .current_mut()?
                    .emit(OpCode::LoadConstant(const_id))?;
                index += 1;

                tracker.current_mut()?.emit(OpCode::LoadStack(start))?;
                tracker.current_mut()?.emit(OpCode::TabSet)?;
                tracker.current_mut()?.emit_adjust_to(start)?;

                Ok(r)
            };

            if let Ok(r) = bracket(s.clone(), tracker) {
                Ok(r)
            } else if let Ok(r) = name(s.clone(), tracker) {
                Ok(r)
            } else if let Ok(r) = index(s, tracker) {
                Ok(r)
            } else {
                Err(ParseError.into())
            }
        };

        let field_sep = |mut s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
            match s.next_token()? {
                Token::Comma | Token::Semicolon => (),
                _ => return Err(ParseError.into()),
            };

            Ok((s, ()))
        };

        let (mut s, ()) = field(s)?;

        let mut next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
            let (s, ()) = field_sep(s)?;
            field(s)
        };

        loop {
            s = match next_part(s.clone()) {
                Ok((s, ())) => s,
                Err(_) => break,
            }
        }

        let s = field_sep(s.clone()).ok().map(|(s, _)| s).unwrap_or(s);

        Ok((s, ()))
    };

    let mut s = match field_list(s.clone()) {
        Ok((s, ())) => s,
        Err(_) => s,
    };

    match s.next_required_token()? {
        Token::CurlyR => (),
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

        tracker.current_mut()?.emit(opcode)?;

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

        tracker.current_mut()?.emit(opcode)?;
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
    } else if let Ok(r) = function(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = table(s, tracker) {
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
