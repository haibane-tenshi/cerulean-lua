use thiserror::Error;

use crate::parser::prelude::*;
use repr::opcode::{BinOp, UnaOp};

pub(crate) fn expr<'s, 'origin>(
    frag: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |mut s: Lexer<'s>| {
        let source = s.source();
        let _span = trace_span!("expr").entered();

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
            })
            .inspect(|output| {
                trace!(span=?output.span(), str=&source[output.span()]);
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
    Output = Spanned<()>,
    Success = ExprSuccess,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use repr::index::InstrId;

        let mut frag = core.expr();
        let stack_start = frag.stack().len();

        let mut total_span = 0..0;

        let state = Source(s)
            .or(head_expr(frag.new_core()).map_success(CompleteOr::Other))?
            .or(atom(frag.new_core()))?
            .with_mode(FailureMode::Ambiguous)
            .inspect(|output| total_span = output.span());

        // At this point there can be variadic number of values on stack.
        // This is OK: it can happen when there are no operators in current expression.
        // We cannot trim it yet, outside context might expect those values.

        enum MaybeOpcode {
            OpCode(BinOp),
            ToBackpatch(InstrId),
        }

        let next_part = |s: Lexer<'s>| -> Result<
            ParsingState<Lexer<'s>, (), Spanned<()>, ExprSuccess, ExprSuccess>,
            FailFast,
        > {
            let mut frag = frag.new_expr_at(stack_start);
            let state = infix_op(s)?
                .map_failure(|failure| ExprSuccess::Parsing(ExprFailure::Infix(failure).into()))
                .with_mode(FailureMode::Malformed)
                .transform(|op| {
                    let (lhs_bp, _rhs_bp) = op.value.binding_power();

                    if lhs_bp < min_bp {
                        Err(ExprSuccess::LessTightlyBound)
                    } else {
                        Ok(op)
                    }
                })
                .map_output(|op| {
                    // At this point we 100% know that we are parsing an infix operator.
                    // It implies that we HAVE to adjust both operands to 1 value before doing op itself.

                    let (op, span) = op.take();

                    // Adjust left operand.
                    // Debug info backpatched later.
                    let lhs_adjust =
                        frag.emit_adjust_to(FragmentStackSlot(1), DebugInfo::Generic(span.span()));

                    let maybe_opcode = match op {
                        Infix::BinOp(op) => MaybeOpcode::OpCode(op),
                        Infix::Logical(op) => {
                            let cond = match op {
                                Logical::Or => true,
                                Logical::And => false,
                            };

                            // Emit dummy debug info.
                            // We backpatch it later.
                            let debug_info = DebugInfo::Generic(span.span());

                            let instr_id =
                                frag.emit_load_stack(FragmentStackSlot(0), debug_info.clone());
                            frag.emit_jump_to_end(Some(cond), debug_info.clone());

                            // Discard left operand when entering the other branch.
                            frag.emit_adjust_to(FragmentStackSlot(0), debug_info);

                            MaybeOpcode::ToBackpatch(instr_id)
                        }
                    };

                    let rhs_top = frag.stack().len() + 1;

                    (maybe_opcode, lhs_adjust, rhs_top, op, span.span(), span)
                })
                .then(|(maybe_opcode, lhs_adjust, rhs_top, op, op_span, span)| {
                    let frag = &mut frag;
                    move |s: Lexer<'s>| -> Result<_, FailFast> {
                        let state = expr_impl(op.binding_power().1, frag.new_core())
                            .parse_once(s)?
                            .map_output(|output| {
                                (
                                    maybe_opcode,
                                    lhs_adjust,
                                    rhs_top,
                                    op_span,
                                    output.span(),
                                    discard(span, output),
                                )
                            });

                        Ok(state)
                    }
                })?
                .map_output(
                    |(maybe_opcode, lhs_adjust, rhs_top, op_span, rhs_span, output)| {
                        let debug_info = DebugInfo::BinOp {
                            op: op_span,
                            lhs: total_span.clone(),
                            rhs: rhs_span,
                        };

                        // Adjust right operand.
                        frag.emit_adjust_to(rhs_top, debug_info.clone());

                        match maybe_opcode {
                            MaybeOpcode::OpCode(op) => {
                                frag.emit_with_debug(OpCode::BinOp(op), debug_info.clone());
                            }
                            MaybeOpcode::ToBackpatch(instr_id) => {
                                for id in [instr_id, instr_id + 1, instr_id + 2] {
                                    if let Some(info) = frag.get_debug_info_mut(id) {
                                        *info = debug_info.clone();
                                    }
                                }
                            }
                        }

                        if let Some(instr_id) = lhs_adjust {
                            if let Some(info) = frag.get_debug_info_mut(instr_id) {
                                *info = debug_info;
                            }
                        }

                        frag.commit();

                        total_span = total_span.start..output.span.end;

                        output
                    },
                )
                .collapse();

            Ok(state)
        };

        let r = state
            .and(next_part.repeat_with(discard).optional(), opt_discard)?
            .inspect(|_| {
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

fn head_expr<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = ExprSuccess,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    |s: Lexer<'s>| {
        let mut frag = core.expr();

        let state = prefix_op(s)?
            .map_failure(|f| ParseFailure::from(ExprFailure::Prefix(f)))
            .then(|op| {
                let (op, span) = op.take();

                let frag = &mut frag;
                move |s: Lexer<'s>| -> Result<_, FailFast> {
                    let ((), rhs_bp) = op.binding_power();
                    let stack_start = frag.stack().len();

                    let r = expr_impl(rhs_bp, frag.new_core())
                        .parse_once(s)?
                        .map_output(|output| {
                            let opcode = OpCode::UnaOp(op.0);

                            let debug_info = DebugInfo::UnaOp {
                                op: span.span(),
                                arg: output.span(),
                            };

                            frag.emit_adjust_to(stack_start + 1, debug_info.clone());
                            frag.emit_with_debug(opcode, debug_info);

                            discard(span, output)
                        });

                    Ok(r)
                }
            })?
            .inspect(|_| {
                frag.commit();
            });

        Ok(state)
    }
}

fn atom<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = ParseFailureOrComplete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use super::{function::function, literal::literal, table::table, variadic::variadic};
        use crate::parser::prefix_expr::prefix_expr;

        let mut frag = core.expr();

        let r = Source(s)
            .or(literal(frag.new_core()))?
            // Discard failure, we should never observe this one as an error.
            .map_failure(|_| Complete)
            .map_success(ParseFailureOrComplete::Complete)
            .or(variadic(frag.new_core()).map_failure(|_| Complete))?
            .or(prefix_expr(frag.new_core()))?
            .or(table(frag.new_core()))?
            .or(function(frag.new_core()))?
            .map_fsource(|_| ())
            .inspect(|_| {
                frag.commit();
            });

        Ok(r)
    }
}

fn prefix_op(
    mut s: Lexer,
) -> Result<ParsingState<Lexer, (), Spanned<Prefix>, Complete, PrefixMismatchError>, LexError> {
    let op = match s.next_token()? {
        Ok(Token::MinusSign) => Prefix(UnaOp::AriNeg),
        Ok(Token::Tilde) => Prefix(UnaOp::BitNot),
        Ok(Token::Hash) => Prefix(UnaOp::StrLen),
        Ok(Token::Not) => Prefix(UnaOp::LogNot),
        _ => {
            let span = s.span();
            return Ok(ParsingState::Failure((), PrefixMismatchError { span }));
        }
    };

    let r = Spanned {
        value: op,
        span: s.span(),
    };

    Ok(ParsingState::Success(s, r, Complete))
}

#[derive(Debug, Error)]
#[error("expected prefix op")]
pub(crate) struct PrefixMismatchError {
    pub(crate) span: logos::Span,
}

fn infix_op(
    mut s: Lexer,
) -> Result<ParsingState<Lexer, (), Spanned<Infix>, Complete, InfixMismatchError>, LexError> {
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
        Ok(Token::AngleLEqualsSign) => Infix::BinOp(BinOp::Rel(RelBinOp::LtEq)),
        Ok(Token::AngleR) => Infix::BinOp(BinOp::Rel(RelBinOp::Gt)),
        Ok(Token::AngleREqualsSign) => Infix::BinOp(BinOp::Rel(RelBinOp::GtEq)),
        Ok(Token::DoubleDot) => Infix::BinOp(BinOp::Str(StrBinOp::Concat)),
        Ok(Token::Or) => Infix::Logical(Logical::Or),
        Ok(Token::And) => Infix::Logical(Logical::And),
        _ => {
            let span = s.span();
            return Ok(ParsingState::Failure((), InfixMismatchError { span }));
        }
    };

    let r = Spanned {
        value: op,
        span: s.span(),
    };

    Ok(ParsingState::Success(s, r, Complete))
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
