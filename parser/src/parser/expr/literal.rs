use crate::parser::prelude::*;

pub(crate) fn literal<'s>(
    s: Lexer<'s>,
    mut frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, ()), Error<LiteralMismatch>> {
    let (s, (literal, _)) = crate::parser::basic::literal(s)?;

    let id = frag.const_table_mut().insert(literal)?;
    frag.emit(OpCode::LoadConstant(id))?;

    frag.commit();
    Ok((s, ()))
}
