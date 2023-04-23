use crate::lex::{Lexer, Token};

use super::expr::expr;
use super::tracker::{ChunkTracker, InstrId};
use super::{block, inner_block};
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

    let top = tracker.stack_top().unwrap();
    let (s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;
    tracker.emit_adjust_to(top.next()).unwrap();

    match local {
        Some(()) => {
            // If we have local keyword, introduce new local variable.
            tracker.name_local(ident).map_err(|_| ParseError)?;
        }
        None => {
            // Otherwise try to store it inside known variable.
            let slot = tracker.lookup_local(ident).ok_or(ParseError)?;
            tracker.emit(OpCode::StoreStack(slot));
        }
    }

    Ok((s, ()))
}

fn backpatch_to_current(index: InstrId, tracker: &mut ChunkTracker) {
    use crate::opcode::OpCode;

    let target = tracker.next_instr();
    let new_offset = target.0 - index.0 - 1;
    match tracker.get_mut(index) {
        Some(OpCode::JumpIf { offset, .. }) => {
            *offset = new_offset;
        }
        Some(OpCode::Jump { offset }) => *offset = new_offset,
        _ => unreachable!(),
    };
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

    let (mut s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::Then => (),
        _ => return Err(ParseError.into()),
    }

    let mut to_end = Vec::new();

    let mut to_next_block = tracker.emit(OpCode::JumpIf {
        cond: false,
        offset: 0,
    });

    let (mut s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

    loop {
        let Ok(ns) = (|mut s: Lexer<'s>| -> Result<Lexer<'s>, LexParseError> {
            match s.next_token()? {
                Token::ElseIf => (),
                _ => return Err(ParseError.into()),
            }

            // Finish off the previous block.
            // This needs to jump to the very end of `if` statement.
            let to_patch = tracker.emit(OpCode::Jump {offset: 0});
            to_end.push(to_patch);

            backpatch_to_current(to_next_block, tracker);

            let (mut s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;

            to_next_block = tracker.emit(OpCode::JumpIf {
                cond: false,
                offset: 0,
            });

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
            let r = tracker.emit(OpCode::Jump { offset: 0 });
            backpatch_to_current(to_next_block, tracker);

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
    backpatch_to_current(to_next_block, tracker);
    for index in to_end {
        backpatch_to_current(index, tracker);
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

    let start = tracker.next_instr();

    let (mut s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::Do => (),
        _ => return Err(ParseError.into()),
    }

    let cond = tracker.emit(OpCode::JumpIf {
        cond: false,
        offset: 0,
    });

    let (mut s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::End => (),
        _ => return Err(ParseError.into()),
    }

    tracker.emit_loop_to(start);
    backpatch_to_current(cond, tracker);

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

    tracker.push_block().unwrap();
    let start = tracker.next_instr();

    let (mut s, ()) = inner_block(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::Until => (),
        _ => return Err(ParseError.into()),
    }

    let (s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;

    // Handle controls of this loop.

    // Jump to cleanup code when condition is true.
    let to_end = tracker.emit(OpCode::JumpIf {
        cond: true,
        offset: 0,
    });

    tracker.pop_ghost_block().unwrap();
    tracker.emit_loop_to(start);

    // Cleanup stack after loop is exited.
    backpatch_to_current(to_end, tracker);
    tracker.pop_block().unwrap();

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

    let slot = tracker.stack_top().unwrap();

    let (s, ()) = expr_list(s, tracker).map_err(LexParseError::eof_into_err)?;

    let s = (|mut s: Lexer<'s>| match s.next_token().ok()? {
        Token::Semicolon => Some(s),
        _ => None,
    })(s.clone())
    .unwrap_or(s);

    tracker.emit(OpCode::Return(slot));

    Ok((s, ()))
}
