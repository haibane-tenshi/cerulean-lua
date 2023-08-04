use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn expr<'s, 'origin>(
    frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let r = expr_impl(0, frag)
            .parse_once(s)?
            .map_success(|success| match success {
                ExprSuccess::LessTightlyBound => {
                    unreachable!("there should be no ops with binding power below 0")
                }
                ExprSuccess::Parsing(success) => success,
            });

        Ok(r)
    }
}

#[derive(Debug, Error)]
pub(crate) enum ExprFailure {
    #[error("failed to parse prefix")]
    Prefix(PrefixMismatchError),
    #[error("failed to parse infix")]
    Infix(InfixMismatchError),
}

impl HaveFailureMode for ExprFailure {
    fn mode(&self) -> FailureMode {
        match self {
            ExprFailure::Prefix(_) => FailureMode::Mismatch,
            ExprFailure::Infix(_) => FailureMode::Ambiguous,
        }
    }
}

fn expr_impl<'s, 'origin>(
    min_bp: u64,
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ExprSuccess,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let stack_start = frag.stack().top().map_err(Into::<CodegenError>::into)?;

        let prefix = |s: Lexer<'s>| -> Result<_, FailFast> {
            let frag = &mut frag;
            let r = prefix_op(s.clone())?
                .map_failure(|f| ParseFailure::from(ExprFailure::Prefix(f)))
                .then(|op| {
                    move |s: Lexer<'s>| -> Result<_, FailFast> {
                        let ((), rhs_bp) = op.binding_power();

                        let r = expr_impl(rhs_bp, frag.new_fragment())
                            .parse_once(s)?
                            .try_map_output(|_| -> Result<_, CodegenError> {
                                let opcode = match op {
                                    Prefix::Ari(op) => OpCode::AriUnaOp(op),
                                    Prefix::Bit(op) => OpCode::BitUnaOp(op),
                                };

                                frag.emit_adjust_to(stack_start + 1)?;
                                frag.emit(opcode)?;

                                Ok(())
                            })?;

                        Ok(r)
                    }
                })?;

            Ok(r)
        };

        let state = prefix
            .parse_once(s.clone())?
            .map_success(CompleteOr::Other)
            .or(s, atom(frag.new_fragment()))?;

        // At this point there can be variadic number of values on stack.
        // This is OK: it can happen when there are no operators in current expression.
        // We cannot trim it yet, outside context might expect those values.

        let next_part = |s: Lexer<'s>| -> Result<
            ParsingState<Lexer<'s>, (), ExprSuccess, ExprSuccess>,
            FailFast,
        > {
            let mut frag = frag.new_fragment();
            let r = infix_op(s)?
                .map_failure(|failure| ExprSuccess::Parsing(ExprFailure::Infix(failure).into()))
                .transform(|op| {
                    let (lhs_bp, _rhs_bp) = op.binding_power();

                    if lhs_bp < min_bp {
                        Err(ExprSuccess::LessTightlyBound)
                    } else {
                        Ok(op)
                    }
                })
                .try_map_output(|op| -> Result<_, CodegenError> {
                    // At this point we 100% know that we are parsing an infix operator.
                    // It implies that we HAVE to adjust both operands to 1 value before doing op itself.

                    // Adjust left operand.
                    frag.emit_adjust_to(stack_start + 1)?;

                    let maybe_opcode = match op {
                        Infix::Ari(op) => Some(OpCode::AriBinOp(op)),
                        Infix::Bit(op) => Some(OpCode::BitBinOp(op)),
                        Infix::Rel(op) => Some(OpCode::RelBinOp(op)),
                        Infix::Str(op) => Some(OpCode::StrBinOp(op)),
                        Infix::Logical(op) => {
                            let cond = match op {
                                Logical::Or => true,
                                Logical::And => false,
                            };

                            frag.emit_jump_to(frag.id(), Some(cond))?;

                            // Discard left operand when entering the other branch.
                            frag.emit_adjust_to(stack_start)?;

                            None
                        }
                    };

                    let rhs_top = frag.stack().top()? + 1;

                    Ok((maybe_opcode, rhs_top, op))
                })?
                .then(|(maybe_opcode, rhs_top, op)| {
                    let frag = &mut frag;
                    move |s: Lexer<'s>| -> Result<_, FailFast> {
                        let r = expr_impl(op.binding_power().1, frag.new_fragment())
                            .parse_once(s)?
                            .map_output(|_| (maybe_opcode, rhs_top));

                        Ok(r)
                    }
                })?
                .try_map_output(move |(maybe_opcode, rhs_top)| -> Result<_, CodegenError> {
                    // Adjust right operand.
                    frag.emit_adjust_to(rhs_top)?;

                    if let Some(opcode) = maybe_opcode {
                        frag.emit(opcode)?;
                    }

                    frag.commit();
                    Ok(())
                })?;

            Ok(r)
        };

        let r = state.and(next_part.repeat())?.map_output(|_| {
            frag.commit();
        });

        Ok(r)
    }
}

enum ExprSuccess {
    LessTightlyBound,
    Parsing(ParseFailure),
}

impl From<CompleteOr<ParseFailure>> for CompleteOr<ExprSuccess> {
    fn from(value: CompleteOr<ParseFailure>) -> Self {
        match value {
            CompleteOr::Complete(value) => CompleteOr::Complete(value),
            CompleteOr::Other(value) => CompleteOr::Other(ExprSuccess::Parsing(value)),
        }
    }
}

impl Combine<ExprSuccess> for ExprSuccess {
    type Output = Self;

    fn combine(self, other: ExprSuccess) -> Self::Output {
        use ExprSuccess::*;

        match (self, other) {
            (Parsing(lhs), Parsing(rhs)) => Parsing(lhs.combine(rhs)),
            (Parsing(failure), _) | (_, Parsing(failure)) => Parsing(failure),
            (LessTightlyBound, LessTightlyBound) => LessTightlyBound,
        }
    }
}

impl Combine<ParseFailure> for ExprSuccess {
    type Output = Self;

    fn combine(self, other: ParseFailure) -> Self::Output {
        match self {
            ExprSuccess::LessTightlyBound => ExprSuccess::LessTightlyBound,
            ExprSuccess::Parsing(failure) => ExprSuccess::Parsing(failure.combine(other)),
        }
    }
}

impl Combine<Never> for ExprSuccess {
    type Output = Never;

    fn combine(self, other: Never) -> Self::Output {
        other
    }
}

impl Combine<ExprSuccess> for CompleteOr<ExprSuccess> {
    type Output = ExprSuccess;

    fn combine(self, other: ExprSuccess) -> Self::Output {
        match self {
            CompleteOr::Complete(_) => other,
            CompleteOr::Other(value) => value.combine(other),
        }
    }
}

impl<T> From<T> for ExprSuccess
where
    T: Into<ParseFailure>,
{
    fn from(value: T) -> Self {
        ExprSuccess::Parsing(value.into())
    }
}

fn atom<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailureOrComplete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use super::{function::function, literal::literal, table::table};
        use crate::parser::prefix_expr::prefix_expr;

        let r = literal(frag.new_fragment())
            .parse_once(s.clone())?
            .map_failure(|_| ParseFailure {
                mode: FailureMode::Mismatch,
                cause: ParseCause::ExpectedExpr,
            })
            .map_success(ParseFailureOrComplete::Complete)
            .or(s.clone(), prefix_expr(frag.new_fragment()))?
            .or(s.clone(), table(frag.new_fragment()))?
            .or(s, function(frag.new_fragment()))?
            .map_output(|_| {
                frag.commit();
            });

        Ok(r)
    }
}

fn prefix_op(
    mut s: Lexer,
) -> Result<ParsingState<Lexer, Prefix, Complete, PrefixMismatchError>, LexError> {
    let op = match s.next_token()? {
        Ok(Token::MinusSign) => Prefix::Ari(AriUnaOp::Neg),
        Ok(Token::Tilde) => Prefix::Bit(BitUnaOp::Not),
        _ => return Ok(ParsingState::Failure(PrefixMismatchError)),
    };

    Ok(ParsingState::Success(s, op, Complete))
}

#[derive(Debug, Error)]
#[error("expected prefix op")]
pub(crate) struct PrefixMismatchError;

impl HaveFailureMode for PrefixMismatchError {
    fn mode(&self) -> FailureMode {
        FailureMode::Mismatch
    }
}

fn infix_op(
    mut s: Lexer,
) -> Result<ParsingState<Lexer, Infix, Complete, InfixMismatchError>, LexError> {
    let op = match s.next_token()? {
        Ok(Token::PlusSign) => Infix::Ari(AriBinOp::Add),
        Ok(Token::MinusSign) => Infix::Ari(AriBinOp::Sub),
        Ok(Token::Asterisk) => Infix::Ari(AriBinOp::Mul),
        Ok(Token::Slash) => Infix::Ari(AriBinOp::Div),
        Ok(Token::DoubleSlash) => Infix::Ari(AriBinOp::FloorDiv),
        Ok(Token::PercentSign) => Infix::Ari(AriBinOp::Rem),
        Ok(Token::Circumflex) => Infix::Ari(AriBinOp::Exp),
        Ok(Token::Ampersand) => Infix::Bit(BitBinOp::And),
        Ok(Token::Pipe) => Infix::Bit(BitBinOp::Or),
        Ok(Token::Tilde) => Infix::Bit(BitBinOp::Xor),
        Ok(Token::DoubleAngleL) => Infix::Bit(BitBinOp::ShL),
        Ok(Token::DoubleAngleR) => Infix::Bit(BitBinOp::ShR),
        Ok(Token::DoubleEqualsSign) => Infix::Rel(RelBinOp::Eq),
        Ok(Token::TildeEqualsSign) => Infix::Rel(RelBinOp::Neq),
        Ok(Token::AngleL) => Infix::Rel(RelBinOp::Lt),
        Ok(Token::AngleLEqualsSign) => Infix::Rel(RelBinOp::Le),
        Ok(Token::AngleR) => Infix::Rel(RelBinOp::Gt),
        Ok(Token::AngleREqualsSign) => Infix::Rel(RelBinOp::Ge),
        Ok(Token::DoubleDot) => Infix::Str(StrBinOp::Concat),
        Ok(Token::Or) => Infix::Logical(Logical::Or),
        Ok(Token::And) => Infix::Logical(Logical::And),
        _ => return Ok(ParsingState::Failure(InfixMismatchError)),
    };

    Ok(ParsingState::Success(s, op, Complete))
}

#[derive(Debug, Error)]
#[error("expected infix op")]
pub(crate) struct InfixMismatchError;

impl HaveFailureMode for InfixMismatchError {
    fn mode(&self) -> FailureMode {
        FailureMode::Mismatch
    }
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
