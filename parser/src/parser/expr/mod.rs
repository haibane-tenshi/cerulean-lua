#[allow(clippy::module_inception)]
mod expr;
pub(crate) mod function;
pub(crate) mod literal;
pub(crate) mod table;

use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) use expr::{expr, ExprSuccess};

pub(crate) fn expr_adjusted_to<'s>(
    s: Lexer<'s>,
    count: u32,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ExprSuccess), Error<ParseFailure>> {
    let mark = frag.stack().top()? + count;
    let r = expr(s, chunk, frag.new_fragment())?;
    frag.emit_adjust_to(mark)?;

    frag.commit();
    Ok(r)
}

pub(crate) fn expr_adjusted_to_1<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ExprSuccess), Error<ParseFailure>> {
    expr_adjusted_to(s, 1, chunk, frag)
}

pub(crate) fn par_expr<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    use ParExprFailure::*;

    let (s, _, Complete) = match_token(s, Token::ParL).map_parse(ParL)?;
    let (s, (), _) =
        expr_adjusted_to_1(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;
    let (s, _, status) = match_token(s, Token::ParR).map_parse(ParR)?;

    frag.commit();
    Ok((s, (), status))
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

pub(crate) fn expr_list<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ExprListSuccess), Error<ParseFailure>> {
    let mut mark = frag.stack().top()?;

    let (mut s, (), _) = expr(s, chunk, frag.new_fragment())?;

    let mut next_part = |s| -> Result<_, ExprListSuccess> {
        use ExprListSuccessInner::*;

        let (s, _, _) = match_token(s, Token::Comma).map_parse(Comma)?;

        // Expressions inside comma lists are adjusted to 1.
        // TODO: bug, fixme
        // Adjustment will persist even if we fail to parse next expression
        // This needs to be wrapped in fragment, but we cannot do it right now since we cannot
        // touch outside stack.
        mark += 1;
        frag.emit_adjust_to(mark)?;

        let (s, _, status) = expr(s, chunk, frag.new_fragment()).map_parse(Expr)?;

        Ok((s, (), status))
    };

    let status = loop {
        s = match next_part(s.clone()) {
            Ok((s, _, _)) => s,
            Err(err) => break err,
        };
    };

    frag.commit();
    Ok((s, (), status))
}

pub(crate) type ExprListSuccess = Error<ExprListSuccessInner>;

#[derive(Debug)]
pub(crate) enum ExprListSuccessInner {
    Comma(TokenMismatch),
    Expr(ParseFailure),
}

pub(crate) fn expr_list_adjusted_to<'s>(
    s: Lexer<'s>,
    count: u32,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ExprListSuccess), Error<ParseFailure>> {
    let mark = frag.stack().top()? + count;
    let r = expr_list(s, chunk, frag.new_fragment())?;
    frag.emit_adjust_to(mark)?;

    frag.commit();
    Ok(r)
}
