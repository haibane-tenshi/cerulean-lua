use crate::parser::prelude::*;

pub(super) fn if_then<'s, 'fun, 'stack>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut outer_frag: Fragment<'s, 'fun, 'stack>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::block::block;
    use crate::parser::expr::expr_adjusted_to_1;

    let outer = outer_frag.id();

    let (s, ()) = match_token(s, Token::If)?;
    let (s, ()) = expr_adjusted_to_1(s, chunk, outer_frag.new_fragment()).require()?;
    let (s, ()) = match_token(s, Token::Then).require()?;

    let mut frag = outer_frag.new_fragment();
    frag.emit_jump_to(frag.id(), Some(false))?;

    let (mut s, ()) = block(s, chunk, frag.new_fragment()).require()?;

    if match_token(s.clone(), Token::End).is_err() {
        frag.emit_jump_to(outer, None)?;
    }
    frag.commit();

    let mut else_if_clause = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        let (s, ()) = match_token(s, Token::ElseIf)?;

        let mut frag = outer_frag.new_fragment();

        let (s, ()) = expr_adjusted_to_1(s, chunk, frag.new_fragment()).require()?;
        let (s, ()) = match_token(s, Token::Then).require()?;
        frag.emit_jump_to(frag.id(), Some(false))?;

        let (s, ()) = block(s, chunk, frag.new_fragment()).require()?;

        if match_token(s.clone(), Token::End).is_err() {
            frag.emit_jump_to(outer, None)?;
        }
        frag.commit();

        Ok((s, ()))
    };

    loop {
        s = match else_if_clause(s.clone()) {
            Ok((s, ())) => s,
            Err(_) => break,
        };
    }

    let mut else_clause = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        let (s, ()) = match_token(s, Token::Else)?;
        let (s, ()) = block(s, chunk, outer_frag.new_fragment()).require()?;

        Ok((s, ()))
    };

    let (s, _) = else_clause(s.clone()).optional(s);
    let (s, ()) = match_token(s, Token::End).require()?;

    outer_frag.commit();

    Ok((s, ()))
}
