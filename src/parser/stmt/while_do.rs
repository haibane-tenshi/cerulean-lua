use crate::parser::prelude::*;

pub(super) fn while_do<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut outer_frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::block::block;
    use crate::parser::expr::expr_adjusted_to_1;

    let (s, ()) = match_token(s, Token::While)?;

    let mut frag = outer_frag.new_fragment();

    let (s, ()) = expr_adjusted_to_1(s, chunk, frag.new_fragment()).require()?;
    let (s, ()) = match_token(s, Token::Do).require()?;

    frag.emit_jump_to(frag.id(), Some(false))?;

    let (s, ()) = block(s, chunk, frag.new_fragment()).require()?;
    let (s, ()) = match_token(s, Token::End).require()?;

    frag.emit_loop_to()?;
    frag.commit();
    outer_frag.commit();

    Ok((s, ()))
}
