use crate::parser::prelude::*;
use either::Either;

pub(crate) fn return_<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ReturnSuccess), Error<ReturnFailure>> {
    use crate::parser::expr::expr_list;
    use ReturnFailure::*;

    let (s, _, Complete) = match_token(s, Token::Return).map_parse(Return)?;

    let slot = frag.stack().top()?;

    let (s, (), _) = expr_list(s, chunk, frag.new_fragment())
        .with_mode(FailureMode::Malformed)
        .map_parse(Expr)?;
    let (s, _, status) = match_token(s.clone(), Token::Semicolon).optional(s);

    frag.emit(OpCode::Return(slot))?;
    frag.commit();

    Ok((s, (), ReturnSuccess(status)))
}

pub(crate) struct ReturnSuccess(Either<Complete, ParseError<TokenMismatch>>);

pub(crate) enum ReturnFailure {
    Return(TokenMismatch),
    Expr(ParseFailure),
}
