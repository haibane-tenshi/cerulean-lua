use crate::lex::{Lexer, Token};

use super::expr::expr;
use super::{block, inner_block};
use super::{ChunkTracker, LexParseError, NextToken, ParseError};

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
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let tokens = [
        s.next_token()?,
        s.next_required_token()?,
        s.next_required_token()?,
    ];

    let ident = match tokens.as_slice() {
        [Token::Local, Token::Ident(ident), Token::Assign] => ident,
        _ => return Err(ParseError.into()),
    };

    let (s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;

    tracker.stack.pop();
    tracker.stack.push_named(ident);

    Ok((s, ()))
}

fn backpatch_to_current(index: u32, tracker: &mut ChunkTracker) {
    use crate::opcode::OpCode;

    let target = tracker.codes.next();
    let new_offset = target - index - 1;
    match tracker.codes.get_mut(index) {
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

    let mut to_next_block = tracker.codes.push(OpCode::JumpIf {
        cond: false,
        offset: 0,
    });
    tracker.stack.pop();

    let (mut s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

    loop {
        let Ok(ns) = (|mut s: Lexer<'s>| -> Result<Lexer<'s>, LexParseError> {
            match s.next_token()? {
                Token::ElseIf => (),
                _ => return Err(ParseError.into()),
            }

            // Finish off the previous block.
            // This needs to jump to the very end of `if` statement.
            let to_patch = tracker.codes.push(OpCode::Jump {offset: 0});
            to_end.push(to_patch);

            backpatch_to_current(to_next_block, tracker);

            let (mut s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;

            to_next_block = tracker.codes.push(OpCode::JumpIf {
                cond: false,
                offset: 0,
            });
            tracker.stack.pop();

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
            let r = tracker.codes.push(OpCode::Jump { offset: 0 });
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

    let start = tracker.codes.next();

    let (mut s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::Do => (),
        _ => return Err(ParseError.into()),
    }

    let cond = tracker.codes.push(OpCode::JumpIf {
        cond: false,
        offset: 0,
    });
    tracker.stack.pop();

    let (mut s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::End => (),
        _ => return Err(ParseError.into()),
    }

    let offset = tracker.codes.next() - start + 1;
    tracker.codes.push(OpCode::Loop { offset });
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

    tracker.stack.push_frame();
    let start = tracker.codes.next();

    let (mut s, ()) = inner_block(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::Until => (),
        _ => return Err(ParseError.into()),
    }

    let (s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;

    // Handle controls of this loop.

    // Jump to cleanup code when condition is true.
    let to_end = tracker.codes.push(OpCode::JumpIf {
        cond: true,
        offset: 0,
    });
    tracker.stack.pop();

    let count = tracker.stack.pop_frame().unwrap();
    let pop_frame = count.try_into().ok().map(OpCode::PopStack);

    // Otherwise cleanup stack and loop to start.
    if let Some(opcode) = pop_frame {
        tracker.codes.push(opcode);
    }

    let offset = tracker.codes.next() - start + 1;
    tracker.codes.push(OpCode::Loop { offset });

    // Cleanup stack after loop is exited.
    backpatch_to_current(to_end, tracker);

    if let Some(opcode) = pop_frame {
        tracker.codes.push(opcode);
    }

    Ok((s, ()))
}
