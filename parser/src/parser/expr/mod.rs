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
        let mark = frag.stack().top() + count;
        let r = expr(frag.new_fragment())
            .parse_once(s)?
            .map_output(move |_| {
                frag.emit_adjust_to(mark);

                frag.commit();
            });

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
        use expr::ExprFailure;

        let par_l =
            match_token(Token::ParL).map_failure(|f| ParseFailure::from(ExprFailure::ParL(f)));

        let par_r =
            match_token(Token::ParR).map_failure(|f| ParseFailure::from(ExprFailure::ParR(f)));

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
pub enum ParExprFailure {}

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
        let mut mark = frag.stack().top() + 1;

        let state = expr(frag.new_fragment()).parse_once(s)?;

        let next_part = |s: Lexer<'s>| -> Result<_, FailFast> {
            let token_comma = match_token(Token::Comma)
                .map_failure(|f| ParseFailure::from(ExprListError::Comma(f)));

            let r = token_comma
                .parse(s)?
                .map_output(|_| {
                    // Expressions inside comma lists are adjusted to 1.
                    frag.emit_adjust_to(mark);
                })
                .and(expr(frag.new_fragment_at(mark)))?
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
        let mark = frag.stack().top() + count;
        let r = expr_list(frag.new_fragment())
            .parse_once(s)?
            .map_output(|_| {
                frag.emit_adjust_to(mark);

                frag.commit();
            });

        Ok(r)
    }
}
