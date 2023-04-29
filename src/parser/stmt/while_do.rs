use crate::lex::{Lexer, Token};

use super::super::tracker::ChunkTracker;
use super::{block, expr_adjusted_to_1};
use super::{LexParseError, NextToken, ParseError};

pub(super) fn while_do<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    match s.next_token()? {
        Token::While => (),
        _ => return Err(ParseError.into()),
    }

    let current = tracker.current_mut()?;
    let stack_top = current.stack_top()?;
    let start = current.next_instr()?;
    current.push_block()?;

    let (mut s, ()) = expr_adjusted_to_1(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::Do => (),
        _ => return Err(ParseError.into()),
    }

    let cond = tracker.current_mut()?.emit(OpCode::JumpIf {
        cond: false,
        offset: Default::default(),
    })?;

    let (mut s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::End => (),
        _ => return Err(ParseError.into()),
    }

    let current = tracker.current_mut()?;
    current.pop_block()?;
    current.emit_loop_to(start)?;

    // Loop exit branch.
    current.backpatch_to_next(cond)?;

    // JumpIf leaves condition on stack, let's fix it.
    current.emit(OpCode::AdjustStack(stack_top))?;

    Ok((s, ()))
}
