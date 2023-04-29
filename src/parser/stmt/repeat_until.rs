use crate::lex::{Lexer, Token};

use super::super::tracker::ChunkTracker;
use super::{expr_adjusted_to_1, inner_block};
use super::{LexParseError, NextToken, ParseError};

pub(super) fn repeat_until<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    match s.next_token()? {
        Token::Repeat => (),
        _ => return Err(ParseError.into()),
    }

    let current = tracker.current_mut()?;
    let start = current.next_instr()?;
    let stack_start = current.stack_top()?;
    tracker.current_mut()?.push_block()?;

    let (mut s, ()) = inner_block(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::Until => (),
        _ => return Err(ParseError.into()),
    }

    let (s, ()) = expr_adjusted_to_1(s, tracker).map_err(LexParseError::eof_into_err)?;

    // Handle controls of this loop.
    // Jump to cleanup code when condition is true.
    let to_end = tracker.current_mut()?.emit(OpCode::JumpIf {
        cond: true,
        offset: Default::default(),
    })?;

    let current = tracker.current_mut()?;
    current.pop_block()?;
    current.emit_loop_to(start)?;

    // Cleanup after loop is exited.
    current.backpatch_to_next(to_end)?;

    // JumpIf leaves condition on stack, let's fix it.
    current.emit(OpCode::AdjustStack(stack_start))?;

    Ok((s, ()))
}
