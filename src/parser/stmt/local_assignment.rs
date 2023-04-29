use crate::lex::{Lexer, Token};

use super::super::expr_list;
use super::super::tracker::ChunkTracker;
use super::{LexParseError, NextToken, ParseError};

pub(super) fn local_assignment<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::StackSlot;

    match s.next_token()? {
        Token::Local => (),
        _ => return Err(ParseError.into()),
    }

    let stack_start = tracker.current()?.stack_top()?;
    let (mut s, idents) = ident_list(s)?;

    match s.next_required_token()? {
        Token::Assign => (),
        _ => return Err(ParseError.into()),
    }

    let (s, ()) = expr_list(s, tracker).map_err(LexParseError::eof_into_err)?;

    let current = tracker.current_mut()?;
    let count = idents.len().try_into().unwrap();
    current.emit_adjust_to(stack_start + count)?;

    for (ident, slot) in idents.into_iter().zip((stack_start.0..).map(StackSlot)) {
        current.name_local(slot, ident)?;
    }

    Ok((s, ()))
}

fn ident_list<'s>(mut s: Lexer<'s>) -> Result<(Lexer<'s>, Vec<&'s str>), LexParseError> {
    let ident = match s.next_token() {
        Ok(Token::Ident(ident)) => ident,
        _ => return Err(ParseError.into()),
    };

    let mut r = vec![ident];

    let next_part = |mut s: Lexer<'s>| -> Result<(Lexer<'s>, &'s str), LexParseError> {
        match s.next_token()? {
            Token::Comma => (),
            _ => return Err(ParseError.into()),
        }

        let ident = match s.next_required_token()? {
            Token::Ident(ident) => ident,
            _ => return Err(ParseError.into()),
        };

        Ok((s, ident))
    };

    while let Ok((ns, ident)) = next_part(s.clone()) {
        s = ns;
        r.push(ident);
    }

    Ok((s, r))
}
