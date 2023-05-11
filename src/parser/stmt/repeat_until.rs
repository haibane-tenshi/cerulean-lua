use crate::parser::prelude::*;

pub(super) fn repeat_until<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut outer_frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::block::inner_block;
    use crate::parser::expr::expr_adjusted_to_1;

    let (s, ()) = match_token(s, Token::Repeat)?;

    let mut frag = outer_frag.new_fragment();

    let (s, ()) = inner_block(s, chunk, frag.new_fragment()).require()?;
    let (s, ()) = match_token(s, Token::Until).require()?;
    let (s, ()) = expr_adjusted_to_1(s, chunk, frag.new_fragment()).require()?;

    frag.emit_jump_to(frag.id(), Some(true))?;
    frag.emit_loop_to()?;
    frag.commit();
    outer_frag.commit();

    Ok((s, ()))
}
