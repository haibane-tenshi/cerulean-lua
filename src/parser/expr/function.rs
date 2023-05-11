use crate::parser::prelude::*;

pub(in crate::parser) fn function<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::func_def::func_body;

    let (s, ()) = match_token(s, Token::Function)?;
    let (s, func_id) = func_body(s, chunk, frag.new_fragment()).require()?;

    let const_id = chunk.constants.insert(Literal::Function(func_id))?;
    frag.emit(OpCode::LoadConstant(const_id))?;

    frag.commit();

    Ok((s, ()))
}
