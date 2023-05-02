use crate::lex::Lexer;
use crate::parser::{LexParseError, Require};
use crate::tracker::ChunkTracker;

pub(super) fn local_function<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::opcode::OpCode;
    use crate::parser::{func_body, identifier, match_token};
    use crate::value::Literal;

    let (s, ()) = match_token(s, Token::Local)?;
    let (s, ()) = match_token(s, Token::Function).require()?;
    let (s, ident) = identifier(s).require()?;

    // Lua disambiguates this case by introducing local variable first and assigning to it later.
    // This is relevant for recursive functions.
    // We only need to introduce the name:
    // it will get assigned to after function body is parsed.
    tracker.current_mut()?.push_stack(Some(ident))?;

    let (s, func_id) = func_body(s, tracker).require()?;

    let const_id = tracker.insert_literal(Literal::Function(func_id))?;
    let current = tracker.current_mut()?;

    // Stack is already adjusted, we just need to silently write to correct slot here.
    current.emit_raw(OpCode::LoadConstant(const_id), false)?;

    Ok((s, ()))
}
