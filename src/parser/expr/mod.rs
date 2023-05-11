#[allow(clippy::module_inception)]
mod expr;
mod function;
mod literal;
mod table;

use crate::parser::prelude::*;

pub(in crate::parser) use expr::expr;
pub(in crate::parser) use function::function;
pub(in crate::parser) use literal::literal;
pub(in crate::parser) use table::table;

pub fn expr_adjusted_to<'s>(
    s: Lexer<'s>,
    count: u32,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mark = frag.stack().top()? + count;
    let r = expr(s, chunk, frag.new_fragment())?;
    frag.emit_adjust_to(mark)?;

    frag.commit();

    Ok(r)
}

pub fn expr_adjusted_to_1<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    expr_adjusted_to(s, 1, chunk, frag)
}

pub fn par_expr<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let (s, ()) = match_token(s, Token::ParL)?;
    let (s, ()) = expr_adjusted_to_1(s, chunk, frag.new_fragment()).require()?;
    let (s, ()) = match_token(s, Token::ParR).require()?;

    frag.commit();

    Ok((s, ()))
}

pub fn expr_list<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mut mark = frag.stack().top()?;

    let (mut s, ()) = expr(s, chunk, frag.new_fragment())?;

    let mut next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        let (s, ()) = match_token(s, Token::Comma)?;

        // Expressions inside comma lists are adjusted to 1.
        mark += 1;
        frag.emit_adjust_to(mark)?;

        expr(s, chunk, frag.new_fragment()).require()
    };

    loop {
        s = match next_part(s.clone()) {
            Ok((s, ())) => s,
            Err(_) => break,
        }
    }

    frag.commit();

    Ok((s, ()))
}

pub fn expr_list_adjusted_to<'s>(
    s: Lexer<'s>,
    count: u32,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mark = frag.stack().top()? + count;
    let r = expr_list(s, chunk, frag.new_fragment())?;
    frag.emit_adjust_to(mark)?;

    frag.commit();

    Ok(r)
}
