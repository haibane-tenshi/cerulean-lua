use crate::parser::expr::ExprSuccessReason;
use crate::parser::prelude::*;

enum OpCodeOrJump {
    OpCode(OpCode),
    Jump(InstrId),
}

pub(in crate::parser) fn expr<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ExprSuccessReason), Error<ParseFailure>> {
    expr_impl(s, 0, chunk, frag)
}

fn expr_impl<'s>(
    s: Lexer<'s>,
    min_bp: u64,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ExprSuccessReason), Error<ParseFailure>> {
    // This should always point at where the first value of previous expressions should be.
    // We need this to implement short-circuiting of `and` and `or` ops.
    let mut top = frag.stack().top()?;

    let mut s = if let Ok((s, op)) = prefix_op(s.clone()) {
        let ((), rhs_bp) = op.binding_power();
        let (s, (), _) = expr_impl(s, rhs_bp, chunk, frag.new_fragment())?;

        let opcode = match op {
            Prefix::Ari(op) => OpCode::AriUnaOp(op),
            Prefix::Bit(op) => OpCode::BitUnaOp(op),
        };

        frag.emit_adjust_to(top + 1)?;
        frag.emit(opcode)?;

        s
    } else {
        let (s, (), _) = atom(s, chunk, frag.new_fragment())?;

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
        frag.emit_adjust_to(top + 1)?;

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

                let instr_id = frag.emit(OpCode::JumpIf {
                    cond,
                    offset: Default::default(),
                })?;

                // Discard left operand when entering the other branch.
                frag.emit_adjust_to(top)?;

                OpCodeOrJump::Jump(instr_id)
            }
        };

        let rhs_top = frag.stack().top()? + 1;
        (s, _, _) = expr_impl(ns, rhs_bp, chunk, frag.new_fragment())?;

        // Adjust right operand.
        frag.emit_adjust_to(rhs_top)?;

        match maybe_opcode {
            OpCodeOrJump::OpCode(opcode) => {
                frag.emit(opcode)?;
            }
            OpCodeOrJump::Jump(index) => {
                let next_instr = frag.len();

                match frag.get_mut(index) {
                    Some(OpCode::JumpIf { offset, .. }) => {
                        *offset = next_instr - index - 1;
                    }
                    _ => unreachable!(),
                }
            }
        }

        // Make sure that top points at the result of current op.
        top = frag.stack().top()? - repr::opcode::StackOffset(1);
    }

    frag.commit();

    Ok((s, (), ExprSuccessReason))
}

fn atom<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ()), Error<ParseFailure>> {
    use super::{function, literal, table};
    use crate::parser::prefix_expr::prefix_expr;

    let mut inner = || {
        let mut err = match literal(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), _status)) => return Ok((s, (), ())),
            Err(err) => err.map_parse(|_| ParseFailure {
                mode: FailureMode::Mismatch,
                cause: ParseCause::ExpectedExpr,
            }),
        };

        err |= match prefix_expr(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status)),
            Err(err) => err,
        };

        err |= match table(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), _status)) => return Ok((s, (), ())),
            Err(err) => err,
        };

        err |= match function(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), _status)) => return Ok((s, (), ())),
            Err(err) => err,
        };

        Err(err)
    };

    let r = inner()?;
    frag.commit();

    Ok(r)
}

fn prefix_op(mut s: Lexer) -> Result<(Lexer, Prefix), ParseError<PrefixMismatchError>> {
    let token = s.next_token().map_parse(|_| PrefixMismatchError)?;

    let op = match token {
        Token::MinusSign => Prefix::Ari(AriUnaOp::Neg),
        Token::Tilde => Prefix::Bit(BitUnaOp::Not),
        _ => return Err(ParseError::Parse(PrefixMismatchError)),
    };

    Ok((s, op))
}

struct PrefixMismatchError;

fn infix_op(mut s: Lexer) -> Result<(Lexer, Infix), ParseError<InfixMismatchError>> {
    let token = s.next_token().map_parse(|_| InfixMismatchError)?;

    let op = match token {
        Token::PlusSign => Infix::Ari(AriBinOp::Add),
        Token::MinusSign => Infix::Ari(AriBinOp::Sub),
        Token::Asterisk => Infix::Ari(AriBinOp::Mul),
        Token::Slash => Infix::Ari(AriBinOp::Div),
        Token::DoubleSlash => Infix::Ari(AriBinOp::FloorDiv),
        Token::PercentSign => Infix::Ari(AriBinOp::Rem),
        Token::Circumflex => Infix::Ari(AriBinOp::Exp),
        Token::Ampersand => Infix::Bit(BitBinOp::And),
        Token::Pipe => Infix::Bit(BitBinOp::Or),
        Token::Tilde => Infix::Bit(BitBinOp::Xor),
        Token::DoubleAngleL => Infix::Bit(BitBinOp::ShL),
        Token::DoubleAngleR => Infix::Bit(BitBinOp::ShR),
        Token::DoubleEqualsSign => Infix::Rel(RelBinOp::Eq),
        Token::TildeEqualsSign => Infix::Rel(RelBinOp::Neq),
        Token::AngleL => Infix::Rel(RelBinOp::Lt),
        Token::AngleLEqualsSign => Infix::Rel(RelBinOp::Le),
        Token::AngleR => Infix::Rel(RelBinOp::Gt),
        Token::AngleREqualsSign => Infix::Rel(RelBinOp::Ge),
        Token::DoubleDot => Infix::Str(StrBinOp::Concat),
        Token::Or => Infix::Logical(Logical::Or),
        Token::And => Infix::Logical(Logical::And),
        _ => return Err(ParseError::Parse(InfixMismatchError)),
    };

    Ok((s, op))
}

struct InfixMismatchError;

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
