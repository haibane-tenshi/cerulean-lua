use crate::parser::prelude::*;

pub(crate) fn literal<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = LiteralMismatch,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let r = crate::parser::basic::literal(s)?.map_output(move |(literal, _)| {
            frag.emit_load_literal(literal);

            frag.commit_expr();
        });

        Ok(r)
    }
}
