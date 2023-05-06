use crate::parser::prelude::*;

pub(super) fn assignment<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::{expr_adjusted_to_1, identifier, match_token};

    let (s, ident) = identifier(s)?;
    let (s, ()) = match_token(s, Token::Assign).require()?;
    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;

    // Try to store it inside known variable.
    let slot = tracker.lookup_local(ident).ok_or(ParseError)?;
    tracker.current_mut()?.emit(OpCode::StoreStack(slot))?;

    Ok((s, ()))
}
