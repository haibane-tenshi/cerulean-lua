use std::ops::Range;
use thiserror::Error;

use crate::codegen::fragment::UpvalueSource;
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

        let source = s.source();
        let _span = trace_span!("prefix_expr").entered();

        let state = prefix_expr_impl(frag.new_core())
            .parse_once(s)?
            .map_output(|output| {
                // Eagerly evaluate place.
                let (expr_type, span) = output.take();
                if let PrefixExpr::Place(place) = expr_type {
                    let (opcode, debug_info) = place.into_opcode();
                    frag.emit_with_debug(opcode, debug_info);
                }
                frag.commit();

                trace!(span=?span.span(), str=&source[span.span()]);

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
        let mut frag = core.expr();

        let source = s.source();
        let _span = trace_span!("place").entered();

        let state = prefix_expr_impl(frag.new_core())
            .parse_once(s)?
            .transform(|output| {
                let (expr_type, span) = output.take();

                if let PrefixExpr::Place(place) = expr_type {
                    frag.commit();

                    Ok(span.put(place))
                } else {
                    use super::stmt::assignment::AssignmentFailure;

                    let failure = PlaceFailure { span: span.span };

                    let err = ParseFailure {
                        mode: FailureMode::Mismatch,
                        cause: ParseCause::from(AssignmentFailure::from(failure)),
                    };

                    Err(err)
                }
            })
            .inspect(|output| {
                trace!(span=?output.span(), str=&source[output.span()]);
            });

        Ok(state)
    }
}

trait IntoOpcode {
    fn into_opcode(self) -> OpCode;
}

#[derive(Debug, Clone)]
pub(crate) enum Place {
    Temporary(StackSlot, Range<usize>),
    Upvalue(UpvalueSlot, Range<usize>),
    TableField(debug_info::TabGet),
}

impl Place {
    fn into_opcode(self) -> (OpCode, debug_info::DebugInfo) {
        match self {
            Place::Temporary(slot, span) => (
                OpCode::LoadStack(slot),
                debug_info::LoadStack::Local(span).into(),
            ),
            Place::Upvalue(slot, span) => (
                OpCode::LoadUpvalue(slot),
                debug_info::LoadUpvalue::Global(span).into(),
            ),
            Place::TableField(debug_info) => (OpCode::TabGet, debug_info.into()),
        }
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
        // Function calls leave stack in variadic state, so we need to scope it when it is used as statement.
        let mut frag = core.scope();

        let source = s.source();
        let _span = trace_span!("fn_call").entered();

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
            })
            .inspect(|output| {
                frag.commit(output.span());

                trace!(span=?output.span(), str=&source[output.span()]);
            });

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
        // Need this as workaround because repeat is unable to use output values to construct parsers.
        let mut expr_type = PrefixExpr::Expr;
        let mut total_span = 0..0;

        let state = Source(s)
            .and(head(frag.new_core()).map_output(|output: Spanned<_>| {
                let (expr, span) = output.take();
                expr_type = expr;
                total_span = span.span();
                span
            }))?
            .and(
                (|s: Lexer<'s>| -> Result<_, FailFast> {
                    let mut frag = frag.new_expr_at(FragmentStackSlot(0));

                    // Evaluate place.
                    if let PrefixExpr::Place(place) = expr_type.clone() {
                        let (opcode, debug_info) = place.into_opcode();
                        frag.emit_with_debug(opcode, debug_info);
                    }

                    // Adjust stack.
                    // Prefix expressions (except the very last one) always evaluate to 1 value.
                    // We are reusing the same stack slot to store it.
                    // Required since previous part could have been a function invocation.
                    frag.emit_adjust_to(FragmentStackSlot(1), total_span.clone());

                    let state = Source(s)
                        .and(
                            tail_segment(frag.new_core(), total_span.clone()).map_output(
                                |output: Spanned<_>| {
                                    let (expr, span) = output.take();
                                    expr_type = expr;
                                    total_span = total_span.start..span.span().end;
                                    span
                                },
                            ),
                        )?
                        .inspect(|_| frag.commit());

                    Ok(state)
                })
                .repeat_with(discard)
                .optional(),
                opt_discard,
            )?
            .inspect(|_| frag.commit())
            .map_output(|span| span.put(expr_type));

        Ok(state)
    }
}

#[derive(Debug, Clone)]
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
            .or(variable(frag.new_core()).map_failure(|_| Complete))?
            .or(par_expr(frag.new_core())
                .map_output(|span: Spanned<_>| span.map(|_| PrefixExpr::Expr)))?
            .map_fsource(|_| ())
            .inspect(|_| frag.commit());

        Ok(state)
    }
}

fn tail_segment<'s, 'origin>(
    core: Core<'s, 'origin>,
    span: Range<usize>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<PrefixExpr>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        // Grab previous part of expression.
        let mut frag = core.expr_at(FragmentStackSlot(0));

        // There should always be exactly one value
        assert_eq!(frag.stack().len(), FragmentStackSlot(1));

        let state = Source(s)
            .or(func_invocation(frag.new_core(), span.clone()))?
            .or(field(frag.new_core(), span.clone()))?
            .or(index(frag.new_core(), span.clone()))?
            .or(tab_call(frag.new_core(), span))?
            .map_fsource(|_| ())
            .inspect(|_| {
                frag.commit();
            });

        Ok(state)
    }
}

fn func_invocation<'s, 'origin>(
    core: Core<'s, 'origin>,
    callable_span: Range<usize>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<PrefixExpr>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.expr_at(FragmentStackSlot(0));

        let state = Source(s).and(func_args(frag.new_core()))?.inspect(|span| {
            let args = frag.stack_slot(FragmentStackSlot(0));
            frag.emit_with_debug(
                OpCode::Invoke(args),
                debug_info::Invoke::Call {
                    callable: callable_span,
                    args: span.span(),
                }
                .into(),
            );
            frag.commit();
        });

        Ok(state)
    }
}

fn func_args<'s, 'origin>(
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
            .or(args_par_expr(frag.new_core()))?
            .or(args_table(frag.new_core()))?
            .or(args_str(frag.new_core()))?
            .map_fsource(|_| ())
            .inspect(move |_| {
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

        let state = Source(s)
            .and(token_par_l)?
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
                frag.emit_load_literal(Literal::String(value.into_owned()), span.span());

                frag.commit();
                span
            });

        Ok(state)
    }
}

use crate::parser::expr::table::table as args_table;

fn variable<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<PrefixExpr>,
    Success = Complete,
    Failure = IdentMismatch,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.expr();

        let state = identifier(s)?
            .try_map_output(|output| -> Result<_, CodegenError> {
                let (ident, span) = output.take();
                let place = match frag.capture_variable(ident) {
                    Some(UpvalueSource::Temporary(slot)) => Place::Temporary(slot, span.span()),
                    Some(UpvalueSource::Upvalue(slot)) => Place::Upvalue(slot, span.span()),
                    None => {
                        let (opcode, info) = match frag.capture_global_env()? {
                            UpvalueSource::Temporary(slot) => (
                                OpCode::LoadStack(slot),
                                debug_info::LoadStack::Global(span.span()).into(),
                            ),
                            UpvalueSource::Upvalue(slot) => (
                                OpCode::LoadUpvalue(slot),
                                debug_info::LoadUpvalue::Global(span.span()).into(),
                            ),
                        };
                        frag.emit_with_debug(opcode, info);
                        frag.emit_load_literal(Literal::String(ident.to_string()), span.span());

                        let debug_info = debug_info::TabGet::GlobalEnv { ident: span.span() };

                        Place::TableField(debug_info)
                    }
                };

                Ok(span.put(PrefixExpr::Place(place)))
            })?
            .inspect(|_| frag.commit());

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
    table_span: Range<usize>,
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

        let state = Source(s)
            .and(token_dot)?
            .and(identifier, replace_with_range)?
            .map_output(move |output| {
                let ((ident, ident_span), span) = output.take();
                frag.emit_load_literal(Literal::String(ident.to_string()), ident_span.clone());
                frag.commit();

                let debug_info = debug_info::TabGet::Local {
                    table: table_span,
                    index: ident_span,
                    indexing: span.span(),
                };

                span.put(PrefixExpr::Place(Place::TableField(debug_info)))
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
    table_span: Range<usize>,
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

        let state = Source(s)
            .and(token_bracket_l)?
            .and(expr_adjusted_to_1(frag.new_core()), replace_range)?
            .and(token_bracket_r, discard)?
            .inspect(move |_| {
                frag.commit();
            })
            .map_output(|output| {
                let (index, span) = output.take();

                let debug_info = debug_info::TabGet::Local {
                    table: table_span,
                    index,
                    indexing: span.span(),
                };

                span.put(PrefixExpr::Place(Place::TableField(debug_info)))
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

fn tab_call<'s, 'origin>(
    core: Core<'s, 'origin>,
    table_span: Range<usize>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<PrefixExpr>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let token_colon =
            match_token(Token::Colon).map_failure(|f| ParseFailure::from(TabCallFailure::Colon(f)));
        let ident = identifier.map_failure(|f| ParseFailure::from(TabCallFailure::Ident(f)));

        let mut frag = core.expr_at(FragmentStackSlot(0));

        let state = Source(s)
            .and(token_colon)?
            .map_output(Spanned::put_range)
            .and(ident, keep_with_range)?
            .map_output(|output| {
                let ((colon_span, ident, ident_span), span) = output.take();

                // Acquire function.
                frag.emit_load_stack(FragmentStackSlot(0), table_span.clone());
                frag.emit_load_literal(Literal::String(ident.to_string()), ident_span.clone());
                frag.emit_with_debug(
                    OpCode::TabGet,
                    debug_info::TabGet::Local {
                        table: table_span.clone(),
                        index: ident_span.clone(),
                        indexing: colon_span.start..ident_span.end,
                    }
                    .into(),
                );

                // Pass table itself as the first argument.
                frag.emit_load_stack(FragmentStackSlot(0), table_span.clone());

                span.put_range()
            })
            .and(func_args(frag.new_core()), keep_range)?
            .map_output(move |output| {
                let ((colon_ident_span, args_span), span) = output.take();

                let args = frag.stack_slot(FragmentStackSlot(1));
                frag.emit_with_debug(
                    OpCode::Invoke(args),
                    debug_info::Invoke::Call {
                        callable: table_span.start..colon_ident_span.end,
                        args: args_span,
                    }
                    .into(),
                );
                frag.commit();

                span.put(PrefixExpr::FnCall)
            });

        Ok(state)
    }
}

#[derive(Debug, Error)]
#[error("failure to parse table method call")]
pub(crate) enum TabCallFailure {
    Colon(TokenMismatch),
    Ident(IdentMismatch),
}
