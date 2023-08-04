use logos::Span;
use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn table<'s, 'frag>(
    mut frag: Fragment<'s, 'frag>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'frag {
    move |s: Lexer<'s>| {
        use TabFailure::*;

        let curly_l = |s| -> Result<_, LexError> {
            let r = match_token(Token::CurlyL)
                .parse(s)?
                .map_failure(CurlyL)
                .map_failure(Into::<ParseFailure>::into);
            Ok(r)
        };

        let curly_r = |s| -> Result<_, LexError> {
            let r = match_token(Token::CurlyR)
                .parse(s)?
                .map_failure(CurlyR)
                .map_failure(Into::<ParseFailure>::into);
            Ok(r)
        };

        let r = curly_l
            .parse(s)?
            .try_map_output(|_| -> Result<_, CodegenError> {
                let table_slot = frag.stack().top()?;
                frag.emit(OpCode::TabCreate)?;

                Ok(table_slot)
            })?
            .then(|table_slot| field_list(table_slot, frag.new_fragment()))?
            .and(curly_r)?
            .map_output(move |_| {
                frag.commit();
            });

        Ok(r)
    }
}

#[derive(Debug, Error)]
pub(crate) enum TabFailure {
    #[error("expected opening curly brace")]
    CurlyL(TokenMismatch),
    #[error("expected closing curly brace")]
    CurlyR(TokenMismatch),
}

impl HaveFailureMode for TabFailure {
    fn mode(&self) -> FailureMode {
        match self {
            TabFailure::CurlyL(_) => FailureMode::Mismatch,
            TabFailure::CurlyR(_) => FailureMode::Malformed,
        }
    }
}

fn field_list<'s, 'origin>(
    table_slot: StackSlot,
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut next_index = 1;
        let first_field = |s: Lexer<'s>| -> Result<_, FailFast> {
            let r = field(table_slot, next_index, frag.new_fragment()).parse_once(s)?;

            if let ParsingState::Success(_, FieldType::Index, _) = &r {
                next_index += 1
            }

            Ok(r)
        };

        let state = first_field.parse_once(s)?;

        let next = |s: Lexer<'s>| {
            let field = |s: Lexer<'s>| -> Result<_, FailFast> {
                let r = field(table_slot, next_index, frag.new_fragment()).parse_once(s)?;

                if let ParsingState::Success(_, FieldType::Index, _) = &r {
                    next_index += 1
                }

                Ok(r)
            };

            field_sep(s)?
                .map_failure(Into::<ParseFailure>::into)
                .and(field)
        };

        let r = state.and(next.repeat())?.map_output(|_| {
            frag.commit();
        });

        Ok(r)
    }
}

fn field<'s, 'origin>(
    table_slot: StackSlot,
    next_index: i64,
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = FieldType,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        // let bracket = |

        let r = bracket(table_slot, frag.new_fragment())
            .parse_once(s.clone())?
            .map_output(|_| FieldType::Bracket)
            .or(
                s.clone(),
                name(table_slot, frag.new_fragment()).map_output(|_| FieldType::Name),
            )?
            .or(
                s,
                index(table_slot, next_index, frag.new_fragment()).map_output(|_| FieldType::Index),
            )?
            .map_output(move |r| {
                frag.commit();
                r
            });

        Ok(r)
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
#[error("encountered unexpected token, expected table field separator")]
pub(crate) struct FieldSepMismatchError;

impl HaveFailureMode for FieldSepMismatchError {
    fn mode(&self) -> FailureMode {
        FailureMode::Mismatch
    }
}

fn bracket<'s, 'origin>(
    table_slot: StackSlot,
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_adjusted_to_1;
        use TabBracketFailure::*;

        let bracket_l = |s: Lexer<'s>| -> Result<_, FailFast> {
            Ok(match_token(Token::BracketL)
                .parse(s)?
                .map_failure(BracketL)
                .map_failure(Into::<ParseFailure>::into))
        };

        let bracket_r = |s: Lexer<'s>| -> Result<_, FailFast> {
            Ok(match_token(Token::BracketR)
                .parse(s)?
                .map_failure(BracketR)
                .map_failure(Into::<ParseFailure>::into))
        };

        let equals_sign = |s: Lexer<'s>| -> Result<_, FailFast> {
            Ok(match_token(Token::EqualsSign)
                .parse(s)?
                .map_failure(EqualsSign)
                .map_failure(Into::<ParseFailure>::into))
        };

        let r = bracket_l(s)?
            .try_map_output(|_| -> Result<_, CodegenError> {
                frag.emit(OpCode::LoadStack(table_slot))?;
                Ok(())
            })?
            .and(expr_adjusted_to_1(frag.new_fragment()))?
            .and(bracket_r)?
            .and(equals_sign)?
            .and(expr_adjusted_to_1(frag.new_fragment()))?
            .try_map_output(move |_| -> Result<_, CodegenError> {
                frag.emit(OpCode::TabSet)?;

                frag.commit();
                Ok(())
            })?;

        Ok(r)
    }
}

#[derive(Debug, Error)]
pub(crate) enum TabBracketFailure {
    #[error("expected opening bracket")]
    BracketL(TokenMismatch),
    #[error("expected closing bracket")]
    BracketR(TokenMismatch),
    #[error("expected equals sign")]
    EqualsSign(TokenMismatch),
}

impl HaveFailureMode for TabBracketFailure {
    fn mode(&self) -> FailureMode {
        match self {
            TabBracketFailure::BracketL(_) => FailureMode::Mismatch,
            TabBracketFailure::BracketR(_) => FailureMode::Malformed,
            TabBracketFailure::EqualsSign(_) => FailureMode::Malformed,
        }
    }
}

fn name<'s, 'origin>(
    table_slot: StackSlot,
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_adjusted_to_1;
        use TabNameFailure::*;

        let ident = |s: Lexer<'s>| -> Result<_, FailFast> {
            Ok(identifier(s)?
                .map_failure(Ident)
                .map_failure(Into::<ParseFailure>::into))
        };

        let equals_sign = |s: Lexer<'s>| -> Result<_, FailFast> {
            Ok(match_token(Token::EqualsSign)
                .parse(s)?
                .map_failure(EqualsSign))
        };

        let r = ident(s)?
            .try_map_output(|(ident, _)| -> Result<_, CodegenError> {
                frag.emit(OpCode::LoadStack(table_slot))?;
                frag.emit_load_literal(Literal::String(ident.to_string()))?;

                Ok(())
            })?
            .and(equals_sign)?
            .and(expr_adjusted_to_1(frag.new_fragment()))?
            .try_map_output(move |_| -> Result<_, CodegenError> {
                frag.emit(OpCode::TabSet)?;

                frag.commit();
                Ok(())
            })?;

        Ok(r)
    }
}

#[derive(Debug, Error)]
pub(crate) enum TabNameFailure {
    #[error("expected identifier")]
    Ident(IdentMismatch),
    #[error("expected equals sign")]
    EqualsSign(TokenMismatch),
}

impl HaveFailureMode for TabNameFailure {
    fn mode(&self) -> FailureMode {
        match self {
            TabNameFailure::Ident(_) => FailureMode::Mismatch,
            TabNameFailure::EqualsSign(_) => FailureMode::Malformed,
        }
    }
}

fn index<'s, 'origin>(
    table_slot: StackSlot,
    index: i64,
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_adjusted_to_1;

        let start = frag.stack().top().map_err(Into::<CodegenError>::into)?;
        let r = expr_adjusted_to_1(frag.new_fragment())
            .parse_once(s)?
            .try_map_output(move |_| -> Result<_, CodegenError> {
                frag.emit(OpCode::LoadStack(table_slot))?;
                frag.emit_load_literal(Literal::Int(index))?;
                frag.emit(OpCode::LoadStack(start))?;
                frag.emit(OpCode::TabSet)?;
                frag.emit_adjust_to(start)?;

                frag.commit();
                Ok(())
            })?;

        Ok(r)
    }
}
