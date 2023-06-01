#[allow(clippy::module_inception)]
mod expr;
pub(crate) mod function;
mod literal;
pub(crate) mod table;

use thiserror::Error;

use crate::parser::prelude::*;

pub(in crate::parser) use expr::expr;
pub(in crate::parser) use function::function;
pub(in crate::parser) use literal::literal;
pub(in crate::parser) use table::table;

pub(crate) fn expr_adjusted_to<'s>(
    s: Lexer<'s>,
    count: u32,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ExprSuccessReason), Error<ParseFailure>> {
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
) -> Result<(Lexer<'s>, (), ExprSuccessReason), Error<ParseFailure>> {
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
) -> Result<(Lexer<'s>, (), ()), Error<ParseFailure>> {
    let mut mark = frag.stack().top()?;

    let (mut s, (), _) = expr(s, chunk, frag.new_fragment())?;

    let mut next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, (), _), ()> {
        let (s, _, Complete) = match_token(s, Token::Comma).map_err(|_| ())?;

        // Expressions inside comma lists are adjusted to 1.
        mark += 1;
        frag.emit_adjust_to(mark).map_err(|_| ())?;

        expr(s, chunk, frag.new_fragment()).map_err(|_| ())
    };

    let status = loop {
        s = match next_part(s.clone()) {
            Ok((s, (), _)) => s,
            Err(err) => break err,
        }
    };

    frag.commit();

    Ok((s, (), status))
}

enum ExprListSuccess {}

pub(crate) fn expr_list_adjusted_to<'s>(
    s: Lexer<'s>,
    count: u32,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ()), Error<ParseFailure>> {
    let mark = frag.stack().top()? + count;
    let r = expr_list(s, chunk, frag.new_fragment())?;
    frag.emit_adjust_to(mark)?;

    frag.commit();

    Ok(r)
}

#[derive(Debug)]
pub struct ExprSuccessReason;
