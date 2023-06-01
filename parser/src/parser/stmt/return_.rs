use crate::parser::prelude::*;

pub(crate) fn return_<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ()), Error<ReturnFailure>> {
    use super::semicolon;
    use crate::parser::expr::expr_list;
    use ReturnFailure::*;

    let (s, _, Complete) = match_token(s, Token::Return).map_parse(Return)?;

    let slot = frag.stack().top()?;

    let (s, (), _) = expr_list(s, chunk, frag.new_fragment())
        .with_mode(FailureMode::Malformed)
        .map_parse(Expr)?;
    let (s, _, status) = semicolon(s.clone()).optional(s);

    frag.emit(OpCode::Return(slot))?;
    frag.commit();

    Ok((s, (), ()))
}

pub(crate) enum ReturnFailure {
    Return(TokenMismatch),
    Expr(ParseFailure),
}
