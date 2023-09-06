use thiserror::Error;

use crate::parser::prelude::*;
use repr::opcode::{BinOp, UnaOp};

pub(crate) fn expr<'s, 'origin>(
    frag: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |mut s: Lexer<'s>| {
        let r = expr_impl(0, frag)
            .parse_once(s.clone())?
            .map_failure(|failure| {
                let _ = s.next();
                let err = ParseFailure::from(ParseCause::ExpectedExpr(s.span()));

                failure.arrow(err)
            })
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
    #[error("failed to parse opening parenthesis")]
    ParL(TokenMismatch),
    #[error("failed to parse closing parenthesis")]
    ParR(TokenMismatch),
}

fn expr_impl<'s, 'origin>(
    min_bp: u64,
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ExprSuccess,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.expr();
        let stack_start = frag.stack().len();

        let prefix = |s: Lexer<'s>| -> Result<_, FailFast> {
            let mut frag = frag.new_expr();
            let r = prefix_op(s)?
                .map_failure(|f| ParseFailure::from(ExprFailure::Prefix(f)))
                .then(|op| {
                    let frag = &mut frag;
                    move |s: Lexer<'s>| -> Result<_, FailFast> {
                        let ((), rhs_bp) = op.binding_power();

                        let r = expr_impl(rhs_bp, frag.new_core())
                            .parse_once(s)?
                            .map_output(|_| {
                                let opcode = OpCode::UnaOp(op.0);

                                frag.emit_adjust_to(stack_start + 1);
                                frag.emit(opcode);
                            });

                        Ok(r)
                    }
                })?
                .map_output(|_| {
                    frag.commit();
                });

            Ok(r)
        };

        let state = prefix
            .parse_once(s.clone())?
            .map_success(CompleteOr::Other)
            .or_else(|| (s, atom(frag.new_core())))?
            .with_mode(FailureMode::Ambiguous);

        // At this point there can be variadic number of values on stack.
        // This is OK: it can happen when there are no operators in current expression.
        // We cannot trim it yet, outside context might expect those values.

        let next_part = |s: Lexer<'s>| -> Result<
            ParsingState<Lexer<'s>, (), ExprSuccess, ExprSuccess>,
            FailFast,
        > {
            let mut frag = frag.new_expr_at(stack_start);
            let r = infix_op(s)?
                .map_failure(|failure| ExprSuccess::Parsing(ExprFailure::Infix(failure).into()))
                .with_mode(FailureMode::Malformed)
                .transform(|op| {
                    let (lhs_bp, _rhs_bp) = op.binding_power();

                    if lhs_bp < min_bp {
                        Err(ExprSuccess::LessTightlyBound)
                    } else {
                        Ok(op)
                    }
                })
                .map_output(|op| {
                    // At this point we 100% know that we are parsing an infix operator.
                    // It implies that we HAVE to adjust both operands to 1 value before doing op itself.

                    // Adjust left operand.
                    frag.emit_adjust_to(stack_start + 1);

                    let maybe_opcode = match op {
                        Infix::BinOp(op) => Some(OpCode::BinOp(op)),
                        Infix::Logical(op) => {
                            let cond = match op {
                                Logical::Or => true,
                                Logical::And => false,
                            };

                            frag.emit_jump_to(frag.id(), Some(cond));

                            // Discard left operand when entering the other branch.
                            frag.emit_adjust_to(stack_start);

                            None
                        }
                    };

                    let rhs_top = frag.stack().len() + 1;

                    (maybe_opcode, rhs_top, op)
                })
                .then(|(maybe_opcode, rhs_top, op)| {
                    let frag = &mut frag;
                    move |s: Lexer<'s>| -> Result<_, FailFast> {
                        let r = expr_impl(op.binding_power().1, frag.new_core())
                            .parse_once(s)?
                            .map_output(|_| (maybe_opcode, rhs_top));

                        Ok(r)
                    }
                })?
                .map_output(move |(maybe_opcode, rhs_top)| {
                    // Adjust right operand.
                    frag.emit_adjust_to(rhs_top);

                    if let Some(opcode) = maybe_opcode {
                        frag.emit(opcode);
                    }

                    frag.commit();
                })
                .collapse();

            Ok(r)
        };

        let r = state
            .and(next_part.repeat())?
            .map_output(|_| {
                frag.commit();
            })
            .collapse();

        Ok(r)
    }
}

enum ExprSuccess {
    LessTightlyBound,
    Parsing(ParseFailure),
}

impl WithMode for ExprSuccess {
    fn with_mode(self, mode: FailureMode) -> Self {
        match self {
            ExprSuccess::LessTightlyBound => ExprSuccess::LessTightlyBound,
            ExprSuccess::Parsing(err) => ExprSuccess::Parsing(err.with_mode(mode)),
        }
    }
}

impl From<CompleteOr<ParseFailure>> for CompleteOr<ExprSuccess> {
    fn from(value: CompleteOr<ParseFailure>) -> Self {
        match value {
            CompleteOr::Complete(value) => CompleteOr::Complete(value),
            CompleteOr::Other(value) => CompleteOr::Other(ExprSuccess::Parsing(value)),
        }
    }
}

impl Arrow<ExprSuccess> for ExprSuccess {
    type Output = Self;

    fn arrow(self, other: ExprSuccess) -> Self::Output {
        match (self, other) {
            (ExprSuccess::Parsing(lhs), ExprSuccess::Parsing(rhs)) => {
                ExprSuccess::Parsing(lhs.arrow(rhs))
            }
            (_, r) => r,
        }
    }
}

impl Arrow<ExprSuccess> for Complete {
    type Output = ExprSuccess;

    fn arrow(self, other: ExprSuccess) -> Self::Output {
        other
    }
}

impl Arrow<ExprSuccess> for CompleteOr<ExprSuccess> {
    type Output = ExprSuccess;

    fn arrow(self, other: ExprSuccess) -> Self::Output {
        match self {
            CompleteOr::Complete(_) => other,
            CompleteOr::Other(lhs) => lhs.arrow(other),
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
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailureOrComplete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use super::{function::function, literal::literal, table::table, variadic::variadic};
        use crate::parser::prefix_expr::prefix_expr;

        let mut frag = core.expr();

        let r = literal(frag.new_core())
            .parse_once(s.clone())?
            // Discard failure, we should never observe this one as an error.
            .map_failure(|_| Complete)
            .map_success(ParseFailureOrComplete::Complete)
            .or_else(|| {
                (
                    s.clone(),
                    variadic(frag.new_core()).map_failure(|_| Complete),
                )
            })?
            .or_else(|| (s.clone(), prefix_expr(frag.new_core())))?
            .or_else(|| (s.clone(), table(frag.new_core())))?
            .or_else(|| (s, function(frag.new_core())))?
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
        Ok(Token::MinusSign) => Prefix(UnaOp::AriNeg),
        Ok(Token::Tilde) => Prefix(UnaOp::BitNot),
        Ok(Token::Hash) => Prefix(UnaOp::StrLen),
        Ok(Token::Not) => Prefix(UnaOp::LogNot),
        _ => {
            let span = s.span();
            return Ok(ParsingState::Failure(PrefixMismatchError { span }));
        }
    };

    Ok(ParsingState::Success(s, op, Complete))
}

#[derive(Debug, Error)]
#[error("expected prefix op")]
pub(crate) struct PrefixMismatchError {
    pub(crate) span: logos::Span,
}

fn infix_op(
    mut s: Lexer,
) -> Result<ParsingState<Lexer, Infix, Complete, InfixMismatchError>, LexError> {
    let op = match s.next_token()? {
        Ok(Token::PlusSign) => Infix::BinOp(BinOp::Ari(AriBinOp::Add)),
        Ok(Token::MinusSign) => Infix::BinOp(BinOp::Ari(AriBinOp::Sub)),
        Ok(Token::Asterisk) => Infix::BinOp(BinOp::Ari(AriBinOp::Mul)),
        Ok(Token::Slash) => Infix::BinOp(BinOp::Ari(AriBinOp::Div)),
        Ok(Token::DoubleSlash) => Infix::BinOp(BinOp::Ari(AriBinOp::FloorDiv)),
        Ok(Token::PercentSign) => Infix::BinOp(BinOp::Ari(AriBinOp::Rem)),
        Ok(Token::Circumflex) => Infix::BinOp(BinOp::Ari(AriBinOp::Exp)),
        Ok(Token::Ampersand) => Infix::BinOp(BinOp::Bit(BitBinOp::And)),
        Ok(Token::Pipe) => Infix::BinOp(BinOp::Bit(BitBinOp::Or)),
        Ok(Token::Tilde) => Infix::BinOp(BinOp::Bit(BitBinOp::Xor)),
        Ok(Token::DoubleAngleL) => Infix::BinOp(BinOp::Bit(BitBinOp::ShL)),
        Ok(Token::DoubleAngleR) => Infix::BinOp(BinOp::Bit(BitBinOp::ShR)),
        Ok(Token::DoubleEqualsSign) => Infix::BinOp(BinOp::Rel(RelBinOp::Eq)),
        Ok(Token::TildeEqualsSign) => Infix::BinOp(BinOp::Rel(RelBinOp::Neq)),
        Ok(Token::AngleL) => Infix::BinOp(BinOp::Rel(RelBinOp::Lt)),
        Ok(Token::AngleLEqualsSign) => Infix::BinOp(BinOp::Rel(RelBinOp::Le)),
        Ok(Token::AngleR) => Infix::BinOp(BinOp::Rel(RelBinOp::Gt)),
        Ok(Token::AngleREqualsSign) => Infix::BinOp(BinOp::Rel(RelBinOp::Ge)),
        Ok(Token::DoubleDot) => Infix::BinOp(BinOp::Str(StrBinOp::Concat)),
        Ok(Token::Or) => Infix::Logical(Logical::Or),
        Ok(Token::And) => Infix::Logical(Logical::And),
        _ => {
            let span = s.span();
            return Ok(ParsingState::Failure(InfixMismatchError { span }));
        }
    };

    Ok(ParsingState::Success(s, op, Complete))
}

#[derive(Debug, Error)]
#[error("expected infix op")]
pub(crate) struct InfixMismatchError {
    pub(crate) span: logos::Span,
}

#[derive(Debug, Copy, Clone)]
struct Prefix(UnaOp);

impl Prefix {
    fn binding_power(self) -> ((), u64) {
        ((), 24)
    }
}

#[derive(Debug, Copy, Clone)]
enum Infix {
    BinOp(BinOp),
    Logical(Logical),
}

impl Infix {
    fn binding_power(self) -> (u64, u64) {
        match self {
            Infix::BinOp(BinOp::Ari(op)) => {
                use AriBinOp::*;

                match op {
                    Add | Sub => (19, 20),
                    Mul | Div | FloorDiv | Rem => (21, 22),
                    Exp => (25, 26),
                }
            }
            Infix::BinOp(BinOp::Bit(op)) => {
                use BitBinOp::*;

                match op {
                    Or => (7, 8),
                    Xor => (9, 10),
                    And => (11, 12),
                    ShL | ShR => (13, 14),
                }
            }
            Infix::BinOp(BinOp::Rel(_)) => (5, 6),
            Infix::BinOp(BinOp::Str(_)) => (15, 16),
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
