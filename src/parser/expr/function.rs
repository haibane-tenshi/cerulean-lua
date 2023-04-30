use super::{LexParseError, Require};
use crate::lex::Lexer;
use crate::parser::tracker::ChunkTracker;

pub(super) fn function<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::opcode::OpCode;
    use crate::parser::{func_body, match_token};
    use crate::value::Literal;

    let (s, ()) = match_token(s, Token::Function)?;
    let (s, func_id) = func_body(s, tracker).require()?;

    let const_id = tracker.insert_literal(Literal::Function(func_id))?;
    tracker
        .current_mut()?
        .emit(OpCode::LoadConstant(const_id))?;

    Ok((s, ()))
}
