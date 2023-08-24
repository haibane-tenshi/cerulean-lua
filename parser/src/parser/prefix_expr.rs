use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn prefix_expr<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let state = prefix_expr_impl(frag.new_fragment())
            .parse_once(s)?
            .map_output(|output| {
                // Eagerly evaluate place.
                if let PrefixExpr::Place(place) = output {
                    place.eval(&mut frag);
                }

                frag.commit();
            });

        Ok(state)
    }
}

pub(crate) fn place<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Place,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let start = s.span().start;

        let state = prefix_expr_impl(frag.new_fragment()).parse_once(s)?;

        let state = match state {
            ParsingState::Success(s, output, success) => {
                if let PrefixExpr::Place(place) = output {
                    frag.commit();

                    ParsingState::Success(s, place, success)
                } else {
                    use super::stmt::assignment::AssignmentFailure;

                    let end = s.span().end;
                    let failure = PlaceFailure { span: start..end };

                    let err = ParseFailure {
                        mode: FailureMode::Mismatch,
                        cause: ParseCause::from(AssignmentFailure::from(failure)),
                    };

                    ParsingState::Failure(err)
                }
            }
            ParsingState::Failure(failure) => ParsingState::Failure(failure),
        };

        Ok(state)
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum Place {
    Temporary(StackSlot),
    TableField,
}

impl Place {
    pub fn eval(self, frag: &mut Fragment) -> InstrId {
        let opcode = match self {
            Place::Temporary(slot) => OpCode::LoadStack(slot),
            Place::TableField => OpCode::TabGet,
        };

        frag.emit(opcode)
    }
}

#[derive(Debug, Error)]
#[error("expected expression that evaluates to place")]
pub(crate) struct PlaceFailure {
    pub(crate) span: logos::Span,
}

fn prefix_expr_impl<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = PrefixExpr,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::par_expr;

        let stack_start = frag.stack().top();

        let prefix = |s: Lexer<'s>| -> Result<_, FailFast> {
            let state = variable(&frag).parse(s.clone())?.map_failure(|_| Complete);

            let state = state
                .map_output(|slot| PrefixExpr::Place(Place::Temporary(slot)))
                .or(
                    s,
                    par_expr(frag.new_fragment()).map_output(|_| PrefixExpr::Expr),
                )?;

            Ok(state)
        };

        let (mut output, state) = match prefix.parse_once(s)? {
            ParsingState::Success(s, output, success) => {
                (output, ParsingState::Success(s, (), success))
            }
            ParsingState::Failure(failure) => return Ok(ParsingState::Failure(failure)),
        };

        let next = |s: Lexer<'s>| -> Result<_, FailFast> {
            let mut frag = frag.new_fragment_at(stack_start);

            // Evaluate place.
            if let PrefixExpr::Place(place) = output {
                place.eval(&mut frag);
            }

            // Adjust stack.
            // Prefix expressions (except the very last one) always evaluate to 1 value.
            // We are reusing the same stack slot to store it.
            frag.emit_adjust_to(stack_start + 1);

            let state = func_call(stack_start, frag.new_fragment_at_boundary())
                .parse_once(s.clone())?
                .map_output(|_| PrefixExpr::FnCall)
                .or(
                    s.clone(),
                    field(frag.new_fragment()).map_output(|_| PrefixExpr::Place(Place::TableField)),
                )?
                .or(
                    s,
                    index(frag.new_fragment()).map_output(|_| PrefixExpr::Place(Place::TableField)),
                )?
                .map_output(|expr| {
                    output = expr;
                    frag.commit();
                });

            Ok(state)
        };

        let state = state.and(next.repeat())?.map_output(move |_| {
            frag.commit();
            output
        });

        Ok(state)
    }
}

#[derive(Debug, Clone, Copy)]
enum PrefixExpr {
    Expr,
    Place(Place),
    FnCall,
}

fn func_call<'s, 'origin>(
    invoke_target: StackSlot,
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let state = args_par_expr(frag.new_fragment())
            .parse_once(s.clone())?
            .or(s.clone(), args_table(frag.new_fragment()))?
            .or(
                s,
                args_str(frag.new_fragment()).map_failure(|_| ParseFailure {
                    mode: FailureMode::Mismatch,
                    cause: ParseCause::FunctionCall,
                }),
            )?
            .map_output(move |_| {
                frag.emit(OpCode::Invoke(invoke_target));

                frag.commit();
            });

        Ok(state)
    }
}

fn args_par_expr<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_list;

        let token_par_l = match_token(Token::ParL)
            .map_failure(|f| ParseFailure::from(FnArgsParExprFailure::ParL(f)));
        let token_par_r = match_token(Token::ParR)
            .map_failure(|f| ParseFailure::from(FnArgsParExprFailure::ParR(f)));

        let state = token_par_l
            .parse_once(s)?
            .and(expr_list(frag.new_fragment()).optional())?
            .and(token_par_r)?
            .map_output(move |_| {
                frag.commit();
            });

        Ok(state)
    }
}

#[derive(Debug, Error)]
#[error("failed to parse function arguments")]
pub enum FnArgsParExprFailure {
    ParL(TokenMismatch),
    ParR(TokenMismatch),
}

fn args_str<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = LiteralStrMismatch,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let state = literal_str(s)?.map_output(move |(value, _)| {
            frag.emit_load_literal(Literal::String(value.into_owned()));

            frag.commit();
        });

        Ok(state)
    }
}

use crate::parser::expr::table::table as args_table;

fn variable<'s, 'a>(
    frag: &'a Fragment<'s, '_>,
) -> impl Parse<
    Lexer<'s>,
    Output = StackSlot,
    Success = Complete,
    Failure = IdentMismatch,
    FailFast = FailFast,
> + 'a {
    move |s: Lexer<'s>| {
        let state =
            identifier(s)?.try_map_output(|(ident, _)| match frag.stack().lookup(ident) {
                NameLookup::Local(slot) => Ok(slot),
                NameLookup::Upvalue => Err(VariableFailure::UnsupportedUpvalue),
                NameLookup::Global => Err(VariableFailure::UnsupportedGlobal),
            })?;

        Ok(state)
    }
}

#[derive(Debug, Error)]
#[error("failed to parse variable")]
pub(crate) enum VariableFailure {
    #[error("upvalues are not yet supported")]
    UnsupportedUpvalue,
    #[error("globals are not yet supported")]
    UnsupportedGlobal,
}

fn field<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let token_dot =
            match_token(Token::Dot).map_failure(|f| ParseFailure::from(FieldFailure::Dot(f)));
        let identifier = identifier.map_failure(|f| ParseFailure::from(FieldFailure::Ident(f)));

        let state = token_dot
            .parse_once(s)?
            .and_with(identifier, |_, (ident, _)| ident)?
            .map_output(move |ident| {
                frag.emit_load_literal(Literal::String(ident.to_string()));

                frag.commit();
            });

        Ok(state)
    }
}

#[derive(Debug, Error)]
#[error("failed to access table's field")]
pub(crate) enum FieldFailure {
    Dot(TokenMismatch),
    Ident(IdentMismatch),
}

fn index<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_adjusted_to_1;

        let token_bracket_l = match_token(Token::BracketL)
            .map_failure(|f| ParseFailure::from(IndexFailure::BracketL(f)));
        let token_bracket_r = match_token(Token::BracketR)
            .map_failure(|f| ParseFailure::from(IndexFailure::BracketR(f)));

        let state = token_bracket_l
            .parse_once(s)?
            .and(expr_adjusted_to_1(frag.new_fragment()))?
            .and(token_bracket_r)?
            .map_output(move |_| {
                frag.commit();
            });

        Ok(state)
    }
}

#[derive(Debug, Error)]
#[error("failed to parse table index")]
pub(crate) enum IndexFailure {
    BracketL(TokenMismatch),
    BracketR(TokenMismatch),
}
