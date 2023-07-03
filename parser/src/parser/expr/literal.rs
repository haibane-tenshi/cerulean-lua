use crate::parser::prelude::*;

pub(crate) fn literal<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), Error<LiteralMismatch>> {
    let (s, (literal, _)) = crate::parser::basic::literal(s)?;

    let id = chunk.constants.insert(literal)?;
    frag.emit(OpCode::LoadConstant(id))?;

    frag.commit();
    Ok((s, ()))
}
