use crate::parser::prelude::*;

pub fn block<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    inner_block(s, chunk, frag)
}

pub fn inner_block<'s>(
    mut s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::stmt::{return_, statement};

    loop {
        s = match statement(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, ())) => s,
            Err(_) => break,
        };
    }

    let s = match return_(s.clone(), chunk, frag.new_fragment()) {
        Ok((s, ())) => s,
        Err(_) => s,
    };

    frag.commit();

    Ok((s, ()))
}
