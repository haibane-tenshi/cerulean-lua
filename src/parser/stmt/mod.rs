mod assignment;
mod generic_for;
mod if_then;
mod local_assignment;
mod local_function;
mod numerical_for;
mod repeat_until;
mod while_do;

use crate::parser::prelude::*;

use assignment::assignment;
use generic_for::generic_for;
use if_then::if_then;
use local_assignment::local_assignment;
use local_function::local_function;
use numerical_for::numerical_for;
use repeat_until::repeat_until;
use while_do::while_do;

pub(in crate::parser) fn statement<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    if let Ok(r) = semicolon(s.clone()) {
        Ok(r)
    } else if let Ok(r) = do_end(s.clone(), chunk, frag.new_fragment()) {
        Ok(r)
    } else if let Ok(r) = if_then(s.clone(), chunk, frag.new_fragment()) {
        Ok(r)
    } else if let Ok(r) = while_do(s.clone(), chunk, frag.new_fragment()) {
        Ok(r)
    } else if let Ok(r) = repeat_until(s.clone(), chunk, frag.new_fragment()) {
        Ok(r)
    } else if let Ok(r) = numerical_for(s.clone(), chunk, frag.new_fragment()) {
        Ok(r)
    } else if let Ok(r) = generic_for(s.clone(), chunk, frag.new_fragment()) {
        Ok(r)
    } else if let Ok(r) = local_assignment(s.clone(), chunk, frag.new_fragment()) {
        Ok(r)
    } else if let Ok(r) = local_function(s.clone(), chunk, frag.new_fragment()) {
        Ok(r)
    } else if let Ok(r) = assignment(s.clone(), chunk, frag.new_fragment()) {
        Ok(r)
    } else {
        let mut s = s;
        let _ = s.next_token()?;
        Err(ParseError.into())
    }
}

fn semicolon(s: Lexer) -> Result<(Lexer, ()), LexParseError> {
    match_token(s, Token::Semicolon)
}

fn do_end<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::block::block;

    let (s, ()) = match_token(s, Token::Do)?;
    let (s, ()) = block(s, chunk, frag.new_fragment()).require()?;
    let (s, ()) = match_token(s, Token::End).require()?;

    frag.commit();

    Ok((s, ()))
}

pub(in crate::parser) fn return_<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use super::expr::expr_list;

    let (s, ()) = match_token(s, Token::Return)?;

    let slot = frag.stack().top()?;

    let (s, ()) = expr_list(s, chunk, frag.new_fragment()).map_err(LexParseError::eof_into_err)?;
    let (s, _) = semicolon(s.clone()).optional(s);

    frag.emit(OpCode::Return(slot))?;
    frag.commit();

    Ok((s, ()))
}
