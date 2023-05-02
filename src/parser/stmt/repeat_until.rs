use crate::lex::Lexer;

use crate::parser::{LexParseError, Require};
use crate::tracker::ChunkTracker;

pub(super) fn repeat_until<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::opcode::OpCode;
    use crate::parser::{expr_adjusted_to_1, inner_block, match_token};

    let (s, ()) = match_token(s, Token::Repeat)?;

    let current = tracker.current_mut()?;
    let start = current.next_instr()?;
    let stack_start = current.stack_top()?;
    let inner = current.start_block()?;

    let (s, ()) = inner_block(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::Until).require()?;
    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;

    // Handle controls of this loop.
    // Jump to cleanup code when condition is true.
    let to_end = tracker.current_mut()?.emit(OpCode::JumpIf {
        cond: true,
        offset: Default::default(),
    })?;

    let current = tracker.current_mut()?;
    current.finish_block(inner)?;
    current.emit_loop_to(start)?;

    // Cleanup after loop is exited.
    current.backpatch_to_next(to_end)?;

    // JumpIf leaves condition on stack, let's fix it.
    current.emit(OpCode::AdjustStack(stack_start))?;

    Ok((s, ()))
}
