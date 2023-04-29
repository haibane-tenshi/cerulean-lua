use crate::lex::{Lexer, Token};
use crate::opcode::InstrId;

use super::tracker::ChunkTracker;
use super::{block, expr_adjusted_to_1, inner_block};
use super::{LexParseError, NextToken, ParseError};

pub(super) fn statement<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let r = if let Ok(r) = semicolon(s.clone()) {
        Ok(r)
    } else if let Ok(r) = assignment(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = do_end(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = if_then(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = while_do(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = repeat_until(s.clone(), tracker) {
        Ok(r)
    } else {
        let mut s = s;
        let _ = s.next_token()?;
        Err(ParseError.into())
    };

    r
}

fn semicolon(mut s: Lexer) -> Result<(Lexer, ()), LexParseError> {
    match s.next_token()? {
        Token::Semicolon => Ok((s, ())),
        _ => Err(ParseError.into()),
    }
}

fn do_end<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    match s.next_token()? {
        Token::Do => (),
        _ => return Err(ParseError.into()),
    };

    let (mut s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::End => (),
        _ => return Err(ParseError.into()),
    };

    Ok((s, ()))
}

fn assignment<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    let (mut s, local) = {
        let maybe_local = |mut s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
            match s.next_token()? {
                Token::Local => (),
                _ => return Err(ParseError.into()),
            }

            Ok((s, ()))
        };

        match maybe_local(s.clone()) {
            Ok((s, ())) => (s, Some(())),
            Err(_) => (s, None),
        }
    };

    let ident = {
        let ident = s.next_token().map_err(|err| {
            if local.is_some() {
                err.eof_into_err()
            } else {
                err
            }
        })?;

        match ident {
            Token::Ident(ident) => ident,
            _ => return Err(ParseError.into()),
        }
    };

    match s.next_required_token()? {
        Token::Assign => (),
        _ => return Err(ParseError.into()),
    }

    let (s, ()) = expr_adjusted_to_1(s, tracker).map_err(LexParseError::eof_into_err)?;

    match local {
        Some(()) => {
            // If we have local keyword, introduce new local variable.
            tracker.current_mut()?.name_local(ident)?;
        }
        None => {
            // Otherwise try to store it inside known variable.
            let slot = tracker.lookup_local(ident).ok_or(ParseError)?;
            tracker.current_mut()?.emit(OpCode::StoreStack(slot))?;
        }
    }

    Ok((s, ()))
}

fn if_then<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    match s.next_token()? {
        Token::If => (),
        _ => return Err(ParseError.into()),
    }

    let (mut s, ()) = expr_adjusted_to_1(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::Then => (),
        _ => return Err(ParseError.into()),
    }

    let mut to_end = Vec::new();

    let mut to_next_block = tracker.current_mut()?.emit(OpCode::JumpIf {
        cond: false,
        offset: Default::default(),
    })?;

    let (mut s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

    loop {
        let Ok(ns) = (|mut s: Lexer<'s>| -> Result<Lexer<'s>, LexParseError> {
            match s.next_token()? {
                Token::ElseIf => (),
                _ => return Err(ParseError.into()),
            }

            // Finish off the previous block.
            // This needs to jump to the very end of `if` statement.
            let to_patch = tracker.current_mut()?.emit(OpCode::Jump {offset: Default::default()})?;
            to_end.push(to_patch);

            tracker.current_mut()?.backpatch_to_next(to_next_block)?;

            let (mut s, ()) = expr_adjusted_to_1(s, tracker).map_err(LexParseError::eof_into_err)?;

            to_next_block = tracker.current_mut()?.emit(OpCode::JumpIf {
                cond: false,
                offset: Default::default(),
            })?;

            match s.next_required_token()? {
                Token::Then => (),
                _ => return Err(ParseError.into()),
            }

            let (s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

            Ok(s)
        })(s.clone()) else {
            break
        };

        s = ns;
    }

    let mut s = (|mut s: Lexer<'s>| -> Result<Lexer<'s>, LexParseError> {
        match s.next_token()? {
            Token::Else => (),
            _ => return Err(ParseError.into()),
        }

        to_next_block = {
            let r = tracker.current_mut()?.emit(OpCode::Jump {
                offset: Default::default(),
            })?;
            tracker.current_mut()?.backpatch_to_next(to_next_block)?;

            r
        };

        let (s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

        Ok(s)
    })(s.clone())
    .ok()
    .unwrap_or(s);

    match s.next_required_token()? {
        Token::End => (),
        _ => return Err(ParseError.into()),
    }

    // Backpatch the last jump instruction and block ends.
    let current = tracker.current_mut()?;
    current.backpatch_to_next(to_next_block)?;
    for index in to_end {
        current.backpatch_to_next(index)?;
    }

    Ok((s, ()))
}

fn while_do<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    match s.next_token()? {
        Token::While => (),
        _ => return Err(ParseError.into()),
    }

    let start = tracker.current_mut()?.next_instr()?;

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

    tracker.current_mut()?.emit_loop_to(start)?;
    tracker.current_mut()?.backpatch_to_next(cond)?;

    Ok((s, ()))
}

fn repeat_until<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    match s.next_token()? {
        Token::Repeat => (),
        _ => return Err(ParseError.into()),
    }

    tracker.current_mut()?.push_block()?;
    let start = tracker.current_mut()?.next_instr()?;

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

    tracker.current_mut()?.pop_ghost_block()?;
    tracker.current_mut()?.emit_loop_to(start)?;

    // Cleanup stack after loop is exited.
    tracker.current_mut()?.backpatch_to_next(to_end)?;
    tracker.current_mut()?.pop_block()?;

    Ok((s, ()))
}

pub(super) fn return_<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use super::expr_list;
    use crate::opcode::OpCode;

    match s.next_token()? {
        Token::Return => (),
        _ => return Err(ParseError.into()),
    }

    let slot = tracker.current()?.stack_top()?;

    let (s, ()) = expr_list(s, tracker).map_err(LexParseError::eof_into_err)?;

    let s = (|mut s: Lexer<'s>| match s.next_token().ok()? {
        Token::Semicolon => Some(s),
        _ => None,
    })(s.clone())
    .unwrap_or(s);

    tracker.current_mut()?.emit(OpCode::Return(slot))?;

    Ok((s, ()))
}
