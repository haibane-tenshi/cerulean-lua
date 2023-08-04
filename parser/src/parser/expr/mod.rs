#[allow(clippy::module_inception)]
pub(crate) mod expr;
pub(crate) mod function;
pub(crate) mod literal;
pub(crate) mod table;

use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) use expr::expr;

pub(crate) fn expr_adjusted_to<'s, 'origin>(
    count: u32,
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mark = frag.stack().top().map_err(Into::<CodegenError>::into)? + count;
        let r = expr(frag.new_fragment()).parse_once(s)?.try_map_output(
            move |_| -> Result<_, CodegenError> {
                frag.emit_adjust_to(mark)?;

                frag.commit();
                Ok(())
            },
        )?;

        Ok(r)
    }
}

pub(crate) fn expr_adjusted_to_1<'s, 'origin>(
    frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| expr_adjusted_to(1, frag).parse_once(s)
}

pub(crate) fn par_expr<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use ParExprFailure::*;

        let par_l = |s: Lexer<'s>| -> Result<_, FailFast> {
            Ok(match_token(Token::ParL)
                .parse(s)?
                .map_failure(ParL)
                .map_failure(Into::<ParseFailure>::into))
        };

        let par_r = |s: Lexer<'s>| -> Result<_, FailFast> {
            Ok(match_token(Token::ParR)
                .parse(s)?
                .map_failure(ParR)
                .map_failure(Into::<ParseFailure>::into))
        };

        let r = par_l
            .parse(s)?
            .and(expr_adjusted_to_1(frag.new_fragment()))?
            .and(par_r)?
            .map_output(|_| {
                frag.commit();
            });

        Ok(r)
    }
}

#[derive(Debug, Error)]
#[error("failed to parse parenthesised expression")]
pub enum ParExprFailure {
    ParL(TokenMismatch),
    ParR(TokenMismatch),
}

impl HaveFailureMode for ParExprFailure {
    fn mode(&self) -> FailureMode {
        match self {
            ParExprFailure::ParL(_) => FailureMode::Mismatch,
            ParExprFailure::ParR(_) => FailureMode::Malformed,
        }
    }
}

pub(crate) fn expr_list<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut mark = frag.stack().top()? + 1;

        let state = expr(frag.new_fragment()).parse_once(s)?;

        let next_part = |s: Lexer<'s>| -> Result<_, FailFast> {
            let token_comma = match_token(Token::Comma)
                .map_failure(|f| ParseFailure::from(ExprListError::Comma(f)));

            let r = token_comma
                .parse(s)?
                .try_map_output(|_| -> Result<_, CodegenError> {
                    // Expressions inside comma lists are adjusted to 1.
                    frag.emit_adjust_to(mark)?;
                    Ok(())
                })?
                .and(expr(frag.new_fragment_at(mark).unwrap()))?
                .map_output(|_| {
                    mark += 1;
                });

            Ok(r)
        };

        let r = state
            .and(next_part.repeat())?
            .map_output(move |_| frag.commit());

        Ok(r)
    }
}

#[derive(Debug, Error)]
pub(crate) enum ExprListError {
    #[error("expected comma")]
    Comma(TokenMismatch),
}

impl HaveFailureMode for ExprListError {
    fn mode(&self) -> FailureMode {
        FailureMode::Mismatch
    }
}

pub(crate) fn expr_list_adjusted_to<'s, 'origin>(
    count: u32,
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mark = frag.stack().top()? + count;
        let r = expr_list(frag.new_fragment())
            .parse_once(s)?
            .try_map_output(|_| -> Result<_, CodegenError> {
                frag.emit_adjust_to(mark)?;

                frag.commit();
                Ok(())
            })?;

        Ok(r)
    }
}
