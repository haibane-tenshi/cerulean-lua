#[allow(clippy::module_inception)]
pub(crate) mod expr;
pub(crate) mod function;
pub(crate) mod literal;
pub(crate) mod table;
pub(crate) mod variadic;

use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) use expr::expr;

pub(crate) fn expr_adjusted_to<'s, 'origin>(
    count: usize,
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

        let mark = frag.stack().len() + count;
        let r = expr(frag.new_core()).parse_once(s)?.inspect(move |_| {
            frag.emit_adjust_to(mark);

            frag.commit();
        });

        Ok(r)
    }
}

pub(crate) fn expr_adjusted_to_1<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| expr_adjusted_to(1, core).parse_once(s)
}

pub(crate) fn par_expr<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
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

        let mut frag = core.expr();

        let state = Source(s)
            .and(par_l)?
            .and(expr_adjusted_to_1(frag.new_core()), discard)?
            .and(par_r, discard)?
            .inspect(|_| {
                frag.commit();
            });

        Ok(state)
    }
}

#[derive(Debug, Error)]
#[error("failed to parse parenthesised expression")]
pub enum ParExprFailure {}

pub(crate) fn expr_list<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let token_comma =
            match_token(Token::Comma).map_failure(|f| ParseFailure::from(ExprListError::Comma(f)));

        let mut frag = core.expr();
        let mut mark = frag.stack().len() + 1;

        let state = Source(s)
            .and(expr(frag.new_core()))?
            .and(
                (|s: Lexer<'s>| -> Result<_, FailFast> {
                    let state = Source(s)
                        .and(token_comma)?
                        .inspect(|_| {
                            // Expressions inside comma lists are adjusted to 1.
                            frag.emit_adjust_to(mark);
                        })
                        .and(expr(frag.new_core()), discard)?
                        .inspect(|_| {
                            mark += 1;
                        });

                    Ok(state)
                })
                .repeat_with(discard)
                .optional(),
                opt_discard,
            )?
            .inspect(move |_| frag.commit());

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum ExprListError {
    #[error("expected comma")]
    Comma(TokenMismatch),
}

pub(crate) fn expr_list_adjusted_to<'s, 'origin>(
    count: usize,
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

        let mark = frag.stack().len() + count;
        let r = Source(s).and(expr_list(frag.new_core()))?.inspect(|_| {
            frag.emit_adjust_to(mark);

            frag.commit();
        });

        Ok(r)
    }
}
