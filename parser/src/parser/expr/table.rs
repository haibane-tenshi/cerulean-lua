use logos::Span;
use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn table<'s, 'frag>(
    core: Core<'s, 'frag>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
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

        let state = curly_l
            .parse(s)?
            .with_mode(FailureMode::Malformed)
            .map_output(|_| {
                let table_slot = frag.stack().len();
                frag.emit(OpCode::TabCreate);

                table_slot
            })
            .then(|table_slot| field_list(table_slot, frag.new_core()).optional())?
            .and(curly_r)?
            .map_output(move |_| {
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
    table_slot: FragmentStackSlot,
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = CompleteOr<ParseFailure>,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.scope();

        let mut next_index = 1;
        let first_field = |s: Lexer<'s>| -> Result<_, FailFast> {
            let r = field(table_slot, next_index, frag.new_core()).parse_once(s)?;

            if let ParsingState::Success(_, FieldType::Index, _) = &r {
                next_index += 1
            }

            Ok(r)
        };

        let field_sep = field_sep.map_failure(|f| ParseFailure::from(TableFailure::Sep(f)));

        let state = first_field.parse_once(s)?;

        let next = |s: Lexer<'s>| {
            let field = |s: Lexer<'s>| -> Result<_, FailFast> {
                let r = field(table_slot, next_index, frag.new_core()).parse_once(s)?;

                if let ParsingState::Success(_, FieldType::Index, _) = &r {
                    next_index += 1
                }

                Ok(r)
            };

            field_sep.parse(s)?.and(field)
        };

        let r = state
            .and(next.repeat())?
            .and(field_sep.map_success(CompleteOr::Complete).optional())?
            .map_output(|_| {
                frag.commit();
            });

        Ok(r)
    }
}

fn field<'s, 'origin>(
    table_slot: FragmentStackSlot,
    next_index: i64,
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = FieldType,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.scope();

        let state = bracket(table_slot, frag.new_core())
            .parse_once(s.clone())?
            .map_output(|_| FieldType::Bracket)
            .or_else(|| {
                let p = name(table_slot, frag.new_core()).map_output(|_| FieldType::Name);

                (s.clone(), p)
            })?
            .or_else(|| {
                let p =
                    index(table_slot, next_index, frag.new_core()).map_output(|_| FieldType::Index);

                (s, p)
            })?
            .map_output(move |r| {
                frag.commit();
                r
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
) -> Result<ParsingState<Lexer, Span, Complete, FieldSepMismatchError>, LexError> {
    let r = match s.next_token()? {
        Ok(Token::Comma | Token::Semicolon) => {
            let span = s.span();
            ParsingState::Success(s, span, Complete)
        }
        Ok(_) | Err(Eof) => ParsingState::Failure(FieldSepMismatchError),
    };

    Ok(r)
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected table field separator `,` or ';`")]
pub(crate) struct FieldSepMismatchError;

fn bracket<'s, 'origin>(
    table_slot: FragmentStackSlot,
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
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

        let mut frag = core.scope();

        let state = bracket_l
            .parse(s)?
            .with_mode(FailureMode::Malformed)
            .map_output(|_| {
                frag.emit(OpCode::LoadStack(frag.stack_slot(table_slot)));
            })
            .and(expr_adjusted_to_1(frag.new_core()))?
            .and(bracket_r)?
            .and(equals_sign)?
            .and(expr_adjusted_to_1(frag.new_core()))?
            .map_output(move |_| {
                frag.emit(OpCode::TabSet);

                frag.commit();
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
    table_slot: FragmentStackSlot,
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
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

        let mut frag = core.scope_at(table_slot);

        let state = ident
            .parse(s)?
            .with_mode(FailureMode::Ambiguous)
            .map_output(|(ident, _)| {
                frag.emit(OpCode::LoadStack(frag.stack_slot(FragmentStackSlot(0))));
                frag.emit_load_literal(Literal::String(ident.to_string()));
            })
            .and(equals_sign)?
            .with_mode(FailureMode::Malformed)
            .and(expr_adjusted_to_1(frag.new_core()))?
            .map_output(move |_| {
                frag.emit(OpCode::TabSet);

                frag.commit();
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
    table_slot: FragmentStackSlot,
    index: i64,
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_adjusted_to_1;

        let mut frag = core.scope();

        let start = frag.stack().len();
        let r = expr_adjusted_to_1(frag.new_core())
            .parse_once(s)?
            .map_output(move |_| {
                frag.emit(OpCode::LoadStack(frag.stack_slot(table_slot)));
                frag.emit_load_literal(Literal::Int(index));
                frag.emit(OpCode::LoadStack(frag.stack_slot(start)));
                frag.emit(OpCode::TabSet);
                frag.emit_adjust_to(start);

                frag.commit();
            });

        Ok(r)
    }
}
