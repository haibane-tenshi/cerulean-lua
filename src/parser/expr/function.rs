use crate::parser::prelude::*;

pub(in crate::parser) fn function<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::{func_body, match_token};

    let (s, ()) = match_token(s, Token::Function)?;
    let (s, func_id) = func_body(s, tracker).require()?;

    let const_id = tracker.insert_literal(Literal::Function(func_id))?;
    tracker
        .current_mut()?
        .emit(OpCode::LoadConstant(const_id))?;

    Ok((s, ()))
}
