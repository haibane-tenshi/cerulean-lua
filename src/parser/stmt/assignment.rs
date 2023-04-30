use crate::lex::Lexer;

use crate::parser::{LexParseError, Require};
use crate::tracker::ChunkTracker;

pub(super) fn assignment<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::opcode::OpCode;
    use crate::parser::{expr_adjusted_to_1, identifier, match_token, ParseError};

    let (s, ident) = identifier(s)?;
    let (s, ()) = match_token(s, Token::Assign).require()?;
    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;

    // Try to store it inside known variable.
    let slot = tracker.lookup_local(ident).ok_or(ParseError)?;
    tracker.current_mut()?.emit(OpCode::StoreStack(slot))?;

    Ok((s, ()))
}
