mod function;
mod literal;
mod table;

use crate::parser::prelude::*;

pub(in crate::parser) use function::function;
pub(in crate::parser) use literal::literal;
pub(in crate::parser) use table::table;

enum OpCodeOrJump {
    OpCode(OpCode),
    Jump(InstrId),
}

pub(in crate::parser) fn expr<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    expr_impl(s, 0, tracker)
}

fn expr_impl<'s>(
    s: Lexer<'s>,
    min_bp: u64,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    // This should always point at where the first value of previous expressions should be.
    // We need this to implement short-circuiting of `and` and `or` ops.
    let mut top = tracker.current()?.stack_top()?;

    let mut s = if let Ok((s, op)) = prefix_op(s.clone()) {
        let ((), rhs_bp) = op.binding_power();
        let (s, ()) = expr_impl(s, rhs_bp, tracker)?;

        let opcode = match op {
            Prefix::Ari(op) => OpCode::AriUnaOp(op),
            Prefix::Bit(op) => OpCode::BitUnaOp(op),
        };

        tracker.current_mut()?.emit_adjust_to(top + 1)?;
        tracker.current_mut()?.emit(opcode)?;

        s
    } else {
        let (s, ()) = atom(s, tracker)?;

        s
    };

    // At this point there can be variadic number of values on stack.
    // This is OK: it can happen when there are no operators in current expression.
    // We cannot trim it yet, outside context might expect those values.

    loop {
        let Ok((ns, op)) = infix_op(s.clone()) else {
            break
        };

        let (lhs_bp, rhs_bp) = op.binding_power();

        if lhs_bp < min_bp {
            break;
        }

        // At this point we 100% know that we are parsing an infix operator.
        // It implies that we HAVE to adjust both operands to 1 value before doing op itself.

        // Adjust left operand.
        tracker.current_mut()?.emit_adjust_to(top + 1)?;

        let maybe_opcode = match op {
            Infix::Ari(op) => OpCodeOrJump::OpCode(OpCode::AriBinOp(op)),
            Infix::Bit(op) => OpCodeOrJump::OpCode(OpCode::BitBinOp(op)),
            Infix::Rel(op) => OpCodeOrJump::OpCode(OpCode::RelBinOp(op)),
            Infix::Str(op) => OpCodeOrJump::OpCode(OpCode::StrBinOp(op)),
            Infix::Logical(op) => {
                let cond = match op {
                    Logical::Or => true,
                    Logical::And => false,
                };

                let instr_id = tracker.current_mut()?.emit(OpCode::JumpIf {
                    cond,
                    offset: Default::default(),
                })?;

                // Discard left operand when entering the other branch.
                tracker.current_mut()?.emit_adjust_to(top)?;

                OpCodeOrJump::Jump(instr_id)
            }
        };

        let rhs_top = tracker.current()?.stack_top()? + 1;
        (s, _) = expr_impl(ns, rhs_bp, tracker).map_err(LexParseError::eof_into_err)?;

        // Adjust right operand.
        tracker.current_mut()?.emit_adjust_to(rhs_top)?;

        match maybe_opcode {
            OpCodeOrJump::OpCode(opcode) => {
                tracker.current_mut()?.emit(opcode)?;
            }
            OpCodeOrJump::Jump(index) => {
                tracker.current_mut()?.backpatch_to_next(index)?;
            }
        }

        // Make sure that top points at the result of current op.
        top = tracker.current()?.stack_top()?.prev().unwrap();
    }

    Ok((s, ()))
}

fn atom<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::prefix_expr::prefix_expr;

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
    let op = match s.next_token()? {
        Token::Minus => Prefix::Ari(AriUnaOp::Neg),
        Token::Tilde => Prefix::Bit(BitUnaOp::Not),
        _ => return Err(ParseError.into()),
    };

    Ok((s, op))
}

fn infix_op(mut s: Lexer) -> Result<(Lexer, Infix), LexParseError> {
    let op = match s.next_token()? {
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
        Token::AngL => Infix::Rel(RelBinOp::Lt),
        Token::AngLEqual => Infix::Rel(RelBinOp::Le),
        Token::AngR => Infix::Rel(RelBinOp::Gt),
        Token::AngREqual => Infix::Rel(RelBinOp::Ge),
        Token::DoubleDot => Infix::Str(StrBinOp::Concat),
        Token::Or => Infix::Logical(Logical::Or),
        Token::And => Infix::Logical(Logical::And),
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
    Logical(Logical),
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
            Infix::Logical(op) => match op {
                Logical::Or => (1, 2),
                Logical::And => (3, 4),
            },
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Logical {
    And,
    Or,
}
