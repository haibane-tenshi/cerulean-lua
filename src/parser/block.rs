use crate::parser::prelude::*;

pub fn block<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let block = tracker.current_mut()?.start_block()?;

    let r = inner_block(s, tracker);

    tracker.current_mut()?.finish_block(block)?;

    r
}

pub fn inner_block<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::stmt::{return_, statement};

    loop {
        s = match statement(s.clone(), tracker) {
            Ok((s, ())) => s,
            Err(_) => break,
        };
    }

    let s = match return_(s.clone(), tracker) {
        Ok((s, ())) => s,
        Err(_) => s,
    };

    Ok((s, ()))
}
