use crate::parser::prelude::*;

pub(in crate::parser) fn literal<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<LiteralMismatch>> {
    let (s, (literal, _), status) = crate::parser::basic::literal(s)?;

    let id = chunk.constants.insert(literal)?;
    frag.emit(OpCode::LoadConstant(id))?;

    frag.commit();

    Ok((s, (), status))
}
