use crate::parser::prelude::*;

pub(crate) fn literal<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = Complete,
    Failure = LiteralMismatch,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let r = crate::parser::basic::literal(s)?.map_output(move |r| {
            let (literal, r) = r.take();

            let mut frag = core.expr();

            frag.emit_load_literal(literal);
            frag.commit();

            r
        });

        Ok(r)
    }
}
