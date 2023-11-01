use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn prefix_expr<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.expr();

        let state = prefix_expr_impl(frag.new_core())
            .parse_once(s)?
            .map_output(|output| {
                // Eagerly evaluate place.
                let (expr_type, span) = output.take();
                if let PrefixExpr::Place(place) = expr_type {
                    place.eval(&mut frag);
                }
                frag.commit();

                span
            });

        Ok(state)
    }
}

pub(crate) fn place<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<Place>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let start = s.span().start;

        let mut frag = core.expr();

        let state = prefix_expr_impl(frag.new_core()).parse_once(s)?;

        let state = match state {
            ParsingState::Success(s, output, success) => {
                let (expr_type, span) = output.take();

                if let PrefixExpr::Place(place) = expr_type {
                    frag.commit();

                    ParsingState::Success(s, span.replace(place).1, success)
                } else {
                    use super::stmt::assignment::AssignmentFailure;

                    let end = s.span().end;
                    let failure = PlaceFailure { span: start..end };

                    let err = ParseFailure {
                        mode: FailureMode::Mismatch,
                        cause: ParseCause::from(AssignmentFailure::from(failure)),
                    };

                    ParsingState::Failure((), err)
                }
            }
            ParsingState::Failure(s, failure) => ParsingState::Failure(s, failure),
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

pub(crate) fn func_call<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| -> Result<_, FailFast> {
        let mut frag = core.scope();

        let state = prefix_expr_impl(frag.new_core())
            .parse_once(s)?
            .transform(|output| {
                let (expr_type, span) = output.take();

                if let PrefixExpr::FnCall = expr_type {
                    Ok(span)
                } else {
                    let err = FnCallFailure { span: span.span };

                    Err(err.into())
                }
            });

        // Function calls leave stack in variadic state, so we need to scope it when it is used as statement.
        frag.commit();

        Ok(state)
    }
}

#[derive(Debug, Error)]
#[error("failed to parse function call")]
pub(crate) struct FnCallFailure {
    pub(crate) span: logos::Span,
}

fn prefix_expr_impl<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<PrefixExpr>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.expr();

        let stack_start = frag.stack().len();

        let (mut expr_type, state) = match head(frag.new_core()).parse_once(s)? {
            ParsingState::Success(s, output, success) => {
                let (expr_type, span) = output.take();
                (
                    expr_type,
                    ParsingState::<_, (), _, _, _>::Success(s, span, success),
                )
            }
            ParsingState::Failure(s, failure) => return Ok(ParsingState::Failure(s, failure)),
        };

        let next = |s: Lexer<'s>| -> Result<_, FailFast> {
            use crate::codegen::stack::CommitKind;
            let mut frag = frag.new_fragment_at_boundary(CommitKind::Expr);

            // Evaluate place.
            if let PrefixExpr::Place(place) = expr_type {
                place.eval(&mut frag);
            }

            // Adjust stack.
            // Prefix expressions (except the very last one) always evaluate to 1 value.
            // We are reusing the same stack slot to store it.
            frag.emit_adjust_to(stack_start + 1);

            let state = Source(s)
                .and(tail_segment(stack_start, frag.new_core()))?
                .map_output(|output| {
                    let (expr, span) = output.take();
                    expr_type = expr;
                    frag.commit();
                    span
                });

            Ok(state)
        };

        let state = state
            .and(next.repeat_with(discard).optional(), opt_discard)?
            .map_output(move |span| {
                frag.commit();
                span.put(expr_type)
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

fn head<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<PrefixExpr>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::par_expr;

        let mut frag = core.expr();

        let state = Source(s)
            .or(variable(&frag).map_failure(|_| Complete))?
            .or(par_expr(frag.new_core())
                .map_output(|span: Spanned<_>| span.map(|_| PrefixExpr::Expr)))?
            .map_fsource(|_| ());

        Ok(state)
    }
}

fn tail_segment<'s, 'origin>(
    stack_start: FragmentStackSlot,
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<PrefixExpr>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.expr();

        let state = Source(s)
            .or(func_args(stack_start, frag.new_core()))?
            .or(field(frag.new_core()))?
            .or(index(frag.new_core()))?
            .or(tab_call(stack_start, frag.new_core()))?
            .map_fsource(|_| ());

        Ok(state)
    }
}

fn func_args<'s, 'origin>(
    invoke_target: FragmentStackSlot,
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<PrefixExpr>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::codegen::stack::CommitKind;

        let mut frag = Fragment::new_at(core, CommitKind::Expr, invoke_target);

        let state = Source(s)
            .or(args_par_expr(frag.new_core()))?
            .or(args_table(frag.new_core()))?
            .or(args_str(frag.new_core()))?
            .map_fsource(|_| ())
            .inspect(move |_| {
                frag.emit(OpCode::Invoke(frag.stack_slot(FragmentStackSlot(0))));

                frag.commit();
            })
            .map_output(|span| span.put(PrefixExpr::FnCall));

        Ok(state)
    }
}

fn args_par_expr<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_list;

        let token_par_l =
            match_token(Token::ParL).map_failure(|f| ParseFailure::from(FnArgsFailure::ParL(f)));
        let token_par_r =
            match_token(Token::ParR).map_failure(|f| ParseFailure::from(FnArgsFailure::ParR(f)));

        let mut frag = core.expr();

        let state = token_par_l
            .parse_once(s)?
            .and(expr_list(frag.new_core()).optional(), opt_discard)?
            .and(token_par_r, discard)?
            .inspect(move |_| {
                frag.commit();
            });

        Ok(state)
    }
}

#[derive(Debug, Error)]
#[error("failed to parse function arguments")]
pub enum FnArgsFailure {
    ParL(TokenMismatch),
    ParR(TokenMismatch),
    String(LiteralStrMismatch),
}

fn args_str<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.expr();

        let state = literal_str(s)?
            .map_failure(|f| ParseFailure::from(FnArgsFailure::String(f)))
            .map_output(move |output| {
                let (value, span) = output.take();
                frag.emit_load_literal(Literal::String(value.into_owned()));

                frag.commit();
                span
            });

        Ok(state)
    }
}

use crate::parser::expr::table::table as args_table;

fn variable<'s, 'a>(
    frag: &'a Fragment<'s, '_>,
) -> impl Parse<
    Lexer<'s>,
    Output = Spanned<PrefixExpr>,
    Success = Complete,
    Failure = IdentMismatch,
    FailFast = FailFast,
> + 'a {
    move |s: Lexer<'s>| {
        let state = identifier(s)?.try_map_output(|output| {
            let (ident, span) = output.take();
            match frag.stack().lookup(ident) {
                NameLookup::Local(slot) => Ok(span.put(PrefixExpr::Place(Place::Temporary(slot)))),
                NameLookup::Upvalue => Err(VariableFailure::UnsupportedUpvalue),
                NameLookup::Global => Err(VariableFailure::UnsupportedGlobal),
            }
        })?;

        Ok(state)
    }
}

#[derive(Debug, Error)]
#[error("failed to parse variable")]
pub enum VariableFailure {
    #[error("upvalues are not yet supported")]
    UnsupportedUpvalue,
    #[error("globals are not yet supported")]
    UnsupportedGlobal,
}

fn field<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<PrefixExpr>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let token_dot =
            match_token(Token::Dot).map_failure(|f| ParseFailure::from(FieldFailure::Dot(f)));
        let identifier = identifier.map_failure(|f| ParseFailure::from(FieldFailure::Ident(f)));

        let mut frag = core.expr();

        let state = token_dot
            .parse_once(s)?
            .and(identifier, replace)?
            .map_output(move |output| {
                let (ident, span) = output.take();
                frag.emit_load_literal(Literal::String(ident.to_string()));

                frag.commit();
                span.put(PrefixExpr::Place(Place::TableField))
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
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<PrefixExpr>,
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

        let mut frag = core.expr();

        let state = token_bracket_l
            .parse_once(s)?
            .and(expr_adjusted_to_1(frag.new_core()), discard)?
            .and(token_bracket_r, discard)?
            .inspect(move |_| {
                frag.commit();
            })
            .map_output(|span| span.put(PrefixExpr::Place(Place::TableField)));

        Ok(state)
    }
}

#[derive(Debug, Error)]
#[error("failed to parse table index")]
pub(crate) enum IndexFailure {
    BracketL(TokenMismatch),
    BracketR(TokenMismatch),
}

fn tab_call<'s, 'origin>(
    table: FragmentStackSlot,
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<PrefixExpr>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::codegen::stack::CommitKind;

        let token_colon =
            match_token(Token::Colon).map_failure(|f| ParseFailure::from(TabCallFailure::Colon(f)));
        let ident = identifier.map_failure(|f| ParseFailure::from(TabCallFailure::Ident(f)));

        let mut frag = Fragment::new_at(core, CommitKind::Expr, table);

        let state = token_colon
            .parse(s)?
            .and(ident, replace)?
            .map_output(|output| {
                let (ident, span) = output.take();
                let invoke_target = frag.stack().len();
                let table = frag.stack_slot(FragmentStackSlot(0));

                frag.emit(OpCode::LoadStack(table));
                frag.emit_load_literal(Literal::String(ident.into()));
                frag.emit(OpCode::TabGet);

                // Pass table itself as the first argument.
                frag.emit(OpCode::LoadStack(table));

                (invoke_target, span)
            })
            .then(|(invoke_target, span)| {
                func_args(invoke_target, frag.new_core()).map_output(|output| discard(span, output))
            })?
            .inspect(move |_| {
                frag.commit();
            })
            .map_output(|span| span.put(PrefixExpr::FnCall));

        Ok(state)
    }
}

#[derive(Debug, Error)]
#[error("failure to parse table method call")]
pub(crate) enum TabCallFailure {
    Colon(TokenMismatch),
    Ident(IdentMismatch),
}
