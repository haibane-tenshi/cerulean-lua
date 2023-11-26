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
                frag.emit(OpCode::TabCreate);

                field_list(frag.new_core())
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
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = CompleteOr<ParseFailure>,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let field_sep = field_sep.map_failure(|f| ParseFailure::from(TableFailure::Sep(f)));

        let mut frag = core.scope_at(FragmentStackSlot(0));
        let mut next_index = 1;

        let state = Source(s)
            .and(field(next_index, frag.new_core()))?
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
                        .and(field(next_index, frag.new_core()), replace)?
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
    next_index: i64,
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<FieldType>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.scope_at(FragmentStackSlot(0));

        let state = Source(s)
            .or(bracket(frag.new_core()))?
            .or(name(frag.new_core()))?
            .or(index(next_index, frag.new_core()))?
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

        let mut frag = core.scope_at(FragmentStackSlot(0));

        let state = Source(s)
            .and(bracket_l)?
            .with_mode(FailureMode::Malformed)
            .inspect(|_| {
                frag.emit(OpCode::LoadStack(frag.stack_slot(FragmentStackSlot(0))));
            })
            .and(expr_adjusted_to_1(frag.new_core()), discard)?
            .and(bracket_r, discard)?
            .and(equals_sign, discard)?
            .and(expr_adjusted_to_1(frag.new_core()), discard)?
            .inspect(move |_| {
                frag.emit(OpCode::TabSet);

                frag.commit();
            })
            .collapse()
            .map_output(|span| span.put(FieldType::Bracket));

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

        let mut frag = core.scope_at(FragmentStackSlot(0));

        let state = Source(s)
            .and(ident)?
            .with_mode(FailureMode::Ambiguous)
            .map_output(|r| {
                let (ident, r) = r.take();

                frag.emit(OpCode::LoadStack(frag.stack_slot(FragmentStackSlot(0))));
                frag.emit_load_literal(Literal::String(ident.to_string()));

                r
            })
            .and(equals_sign, discard)?
            .with_mode(FailureMode::Malformed)
            .and(expr_adjusted_to_1(frag.new_core()), discard)?
            .inspect(move |_| {
                frag.emit(OpCode::TabSet);

                frag.commit();
            })
            .collapse()
            .map_output(|span| span.put(FieldType::Name));

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
    index: i64,
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<FieldType>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_adjusted_to_1;

        let mut frag = core.scope_at(FragmentStackSlot(0));

        let start = frag.stack().len();
        let r = expr_adjusted_to_1(frag.new_core())
            .parse_once(s)?
            .inspect(move |_| {
                frag.emit(OpCode::LoadStack(frag.stack_slot(FragmentStackSlot(0))));
                frag.emit_load_literal(Literal::Int(index));
                frag.emit(OpCode::LoadStack(frag.stack_slot(start)));
                frag.emit(OpCode::TabSet);
                frag.emit_adjust_to(start);

                frag.commit();
            })
            .map_output(|span| span.put(FieldType::Index));

        Ok(r)
    }
}
