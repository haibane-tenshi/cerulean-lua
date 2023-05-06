use crate::parser::prelude::*;

pub(super) fn generic_for<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::{block, expr_list_adjusted_to, match_token};

    let (s, ()) = match_token(s, Token::For)?;
    let (s, names) = name_list(s).require()?;
    let (s, ()) = match_token(s, Token::In).require()?;

    let outer = tracker.current_mut()?.start_block()?;

    let top = tracker.current()?.stack_top()?;
    let (s, ()) = expr_list_adjusted_to(s, tracker, 4).require()?;

    let iter = top;
    let state = top + 1;
    let control = top + 2;
    // Currently unimplemented.
    let _close = top + 3;

    let nil = tracker.insert_literal(Literal::Nil)?;
    let current = tracker.current_mut()?;
    let new_control = current.stack_top()?;
    let start = current.next_instr();

    let inner = current.start_block()?;

    current.emit(OpCode::LoadStack(iter))?;
    current.emit(OpCode::LoadStack(state))?;
    current.emit(OpCode::LoadStack(control))?;
    current.emit(OpCode::Invoke(new_control))?;

    let count: u32 = names.len().try_into().unwrap();
    current.emit_adjust_to(new_control + count)?;

    let condition = current.start_block()?;

    current.emit(OpCode::LoadStack(new_control))?;
    current.emit(OpCode::LoadConstant(nil))?;
    current.emit(OpCode::RelBinOp(RelBinOp::Eq))?;
    current.emit_jump_to_end_of(outer, Some(true))?;

    current.emit(OpCode::LoadStack(new_control))?;
    current.emit(OpCode::StoreStack(control))?;

    current.finish_block(condition)?;

    // Assign names
    for (name, slot) in names.into_iter().zip((new_control.0..).map(StackSlot)) {
        current.name_local(slot, name)?;
    }

    let (s, ()) = match_token(s, Token::Do).require()?;
    let (s, ()) = block(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::End).require()?;

    let current = tracker.current_mut()?;

    current.finish_block(inner)?;
    current.emit_loop_to(start)?;
    current.finish_block(outer)?;

    Ok((s, ()))
}

fn name_list<'s>(s: Lexer<'s>) -> Result<(Lexer<'s>, Vec<&'s str>), LexParseError> {
    use crate::parser::{identifier, match_token};

    let (mut s, ident) = identifier(s)?;
    let mut r = vec![ident];

    let next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, &'s str), LexParseError> {
        let (s, ()) = match_token(s, Token::Comma)?;
        identifier(s).require()
    };

    while let Ok((ns, ident)) = next_part(s.clone()) {
        s = ns;
        r.push(ident);
    }

    Ok((s, r))
}
