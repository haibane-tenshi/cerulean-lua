use crate::parser::prelude::*;

pub(crate) fn literal<'s>(
    s: Lexer<'s>,
    mut frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, ()), Error<LiteralMismatch>> {
    let (s, (literal, _)) = crate::parser::basic::literal(s)?;

    frag.emit_load_literal(literal)?;

    frag.commit();
    Ok((s, ()))
}
