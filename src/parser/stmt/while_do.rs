use crate::lex::Lexer;

use crate::parser::{LexParseError, Require};
use crate::tracker::ChunkTracker;

pub(super) fn while_do<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::opcode::OpCode;
    use crate::parser::{block, expr_adjusted_to_1, match_token};

    let (s, ()) = match_token(s, Token::While)?;

    let current = tracker.current_mut()?;
    let stack_top = current.stack_top()?;
    let start = current.next_instr()?;
    current.push_block()?;

    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::Do).require()?;

    let cond = tracker.current_mut()?.emit(OpCode::JumpIf {
        cond: false,
        offset: Default::default(),
    })?;

    let (s, ()) = block(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::End).require()?;

    let current = tracker.current_mut()?;
    current.pop_block()?;
    current.emit_loop_to(start)?;

    // Loop exit branch.
    current.backpatch_to_next(cond)?;

    // JumpIf leaves condition on stack, let's fix it.
    current.emit(OpCode::AdjustStack(stack_top))?;

    Ok((s, ()))
}
