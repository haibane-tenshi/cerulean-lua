use std::ops::Range;
use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn table<'s, 'frag>(
    core: Core<'s, 'frag>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'frag {
    move |s: Lexer<'s>| {
        let curly_l =
            match_token(Token::CurlyL).map_failure(|f| ParseFailure::from(TableFailure::CurlyL(f)));
        let curly_r =
            match_token(Token::CurlyR).map_failure(|f| ParseFailure::from(TableFailure::CurlyR(f)));

        let mut frag = core.expr();

        let state = Source(s)
            .and(curly_l)?
            .with_mode(FailureMode::Malformed)
            .then(|span| {
                frag.emit_with_debug(
                    OpCode::TabCreate,
                    DebugInfo::CreateTable { table: span.span() },
                );

                field_list(frag.new_core(), span.span())
                    .optional()
                    .map_output(|output| opt_discard(span, output))
            })?
            .and(curly_r, discard)?
            .inspect(move |_| {
                frag.commit();
            })
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum TableFailure {
    #[error("expected opening curly brace")]
    CurlyL(TokenMismatch),
    #[error("failed to parse bracket setter")]
    BracketSetter(#[from] BracketFailure),
    #[error("failed to parse name setter")]
    NameSetter(#[from] NameFailure),
    #[error("expected table field separator")]
    Sep(FieldSepMismatchError),
    #[error("expected closing curly brace")]
    CurlyR(TokenMismatch),
}

fn field_list<'s, 'origin>(
    core: Core<'s, 'origin>,
    table_span: Range<usize>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = CompleteOr<ParseFailure>,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let field_sep = field_sep.map_failure(|f| ParseFailure::from(TableFailure::Sep(f)));

        let mut frag = core.expr_at(FragmentStackSlot(0));
        let mut next_index = 1;

        let state = Source(s)
            .and(field(frag.new_core(), next_index, table_span.clone()))?
            .map_output(|output| {
                let (field_type, span) = output.take();
                if let FieldType::Index = field_type {
                    next_index += 1;
                }

                span
            })
            .and(
                (|s| -> Result<_, FailFast> {
                    let state = Source(s)
                        .and(field_sep)?
                        .and(
                            field(frag.new_core(), next_index, table_span.clone()),
                            replace,
                        )?
                        .inspect(|output| {
                            if let FieldType::Index = output.value {
                                next_index += 1;
                            }
                        });

                    Ok(state)
                })
                .repeat_with(discard)
                .optional(),
                opt_discard,
            )?
            .and(
                field_sep.map_success(CompleteOr::Complete).optional(),
                opt_discard,
            )?
            .inspect(|_| frag.commit());

        Ok(state)
    }
}

fn field<'s, 'origin>(
    core: Core<'s, 'origin>,
    next_index: i64,
    table_span: Range<usize>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<FieldType>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.expr_at(FragmentStackSlot(0));

        let state = Source(s)
            .or(bracket(frag.new_core(), table_span.clone()))?
            .or(name(frag.new_core(), table_span.clone()))?
            .or(index(frag.new_core(), next_index, table_span))?
            .map_fsource(|_| ())
            .inspect(move |_| {
                frag.commit();
            });

        Ok(state)
    }
}

enum FieldType {
    Index,
    Bracket,
    Name,
}

fn field_sep(
    mut s: Lexer,
) -> Result<ParsingState<Lexer, (), Spanned<()>, Complete, FieldSepMismatchError>, LexError> {
    let r = match s.next_token()? {
        Ok(Token::Comma | Token::Semicolon) => {
            let r = Spanned {
                value: (),
                span: s.span(),
            };
            ParsingState::Success(s, r, Complete)
        }
        Ok(_) | Err(Eof) => ParsingState::Failure((), FieldSepMismatchError),
    };

    Ok(r)
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected table field separator `,` or ';`")]
pub(crate) struct FieldSepMismatchError;

fn bracket<'s, 'origin>(
    core: Core<'s, 'origin>,
    table_span: Range<usize>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<FieldType>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_adjusted_to_1;
        use BracketFailure::*;
        use TableFailure::BracketSetter;

        let bracket_l = match_token(Token::BracketL)
            .map_failure(|f| ParseFailure::from(BracketSetter(BracketL(f))));
        let bracket_r = match_token(Token::BracketR)
            .map_failure(|f| ParseFailure::from(BracketSetter(BracketR(f))));
        let equals_sign = match_token(Token::EqualsSign)
            .map_failure(|f| ParseFailure::from(BracketSetter(EqualsSign(f))));

        let mut frag = core.expr_at(FragmentStackSlot(0));

        let state = Source(s)
            .and(bracket_l)?
            .with_mode(FailureMode::Malformed)
            .inspect(|output| {
                frag.emit_load_stack(FragmentStackSlot(0), output.span());
            })
            .and(expr_adjusted_to_1(frag.new_core()), replace_range)?
            .and(bracket_r, discard)?
            .map_output(|output| {
                let span = output.span();
                output.place(span)
            })
            .and(equals_sign, keep_range)?
            .and(expr_adjusted_to_1(frag.new_core()), discard)?
            .map_output(move |output| {
                let (((index_span, indexing_span), eq_sign_span), span) = output.take();

                frag.emit_with_debug(
                    OpCode::TabSet,
                    DebugInfo::ConstructTable {
                        table: table_span,
                        entry: DebugTabEntry::Index {
                            index: index_span,
                            indexing: indexing_span,
                            eq_sign: eq_sign_span,
                        },
                    },
                );
                frag.commit();

                span.put(FieldType::Bracket)
            })
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum BracketFailure {
    #[error("expected opening bracket")]
    BracketL(TokenMismatch),
    #[error("expected closing bracket")]
    BracketR(TokenMismatch),
    #[error("expected equals sign")]
    EqualsSign(TokenMismatch),
}

fn name<'s, 'origin>(
    core: Core<'s, 'origin>,
    table_span: Range<usize>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<FieldType>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_adjusted_to_1;
        use NameFailure::*;
        use TableFailure::NameSetter;

        let ident = identifier.map_failure(|f| ParseFailure::from(NameSetter(Ident(f))));
        let equals_sign = match_token(Token::EqualsSign)
            .map_failure(|f| ParseFailure::from(NameSetter(EqualsSign(f))));

        let mut frag = core.expr_at(FragmentStackSlot(0));

        let state = Source(s)
            .and(ident)?
            .with_mode(FailureMode::Ambiguous)
            .map_output(|r| {
                let (ident, span) = r.take();

                frag.emit_load_stack(FragmentStackSlot(0), span.span());
                frag.emit_load_literal(
                    Literal::String(ident.to_string()),
                    DebugInfo::Literal(span.span()),
                );

                span.put_range()
            })
            .and(equals_sign, keep_range)?
            .with_mode(FailureMode::Malformed)
            .and(expr_adjusted_to_1(frag.new_core()), discard)?
            .map_output(move |output| {
                let ((ident_span, eq_sign_span), span) = output.take();

                frag.emit_with_debug(
                    OpCode::TabSet,
                    DebugInfo::ConstructTable {
                        table: table_span,
                        entry: DebugTabEntry::Field {
                            ident: ident_span,
                            eq_sign: eq_sign_span,
                        },
                    },
                );
                frag.commit();

                span.put(FieldType::Name)
            })
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum NameFailure {
    #[error("expected identifier")]
    Ident(IdentMismatch),
    #[error("expected equals sign")]
    EqualsSign(TokenMismatch),
}

fn index<'s, 'origin>(
    core: Core<'s, 'origin>,
    index: i64,
    table_span: Range<usize>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<FieldType>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_adjusted_to_1;

        let mut frag = core.expr_at(FragmentStackSlot(0));

        let value_slot = frag.stack().len();
        let r = expr_adjusted_to_1(frag.new_core())
            .parse_once(s)?
            .inspect(move |output| {
                let debug_info = DebugInfo::ConstructTable {
                    table: table_span,
                    entry: DebugTabEntry::Value {
                        value: output.span(),
                    },
                };

                frag.emit_load_stack(FragmentStackSlot(0), output.span());
                frag.emit_load_literal(Literal::Int(index), debug_info.clone());
                frag.emit_load_stack(value_slot, output.span());
                frag.emit_with_debug(OpCode::TabSet, debug_info.clone());
                frag.emit_adjust_to(value_slot, output.span());

                frag.commit();
            })
            .map_output(|span| span.put(FieldType::Index));

        Ok(r)
    }
}
