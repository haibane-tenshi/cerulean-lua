use crate::parser::prelude::*;

pub(super) fn local_assignment<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::expr::expr_list;

    let (s, ()) = match_token(s, Token::Local)?;

    let stack_start = tracker.current()?.stack_top()?;
    let (s, idents) = ident_list(s).require()?;
    let (s, ()) = match_token(s, Token::Assign).require()?;

    let (s, ()) = expr_list(s, tracker).map_err(LexParseError::eof_into_err)?;

    let current = tracker.current_mut()?;
    let count: u32 = idents.len().try_into().unwrap();
    current.emit_adjust_to(stack_start + count)?;

    for (ident, slot) in idents.into_iter().zip((stack_start.0..).map(StackSlot)) {
        current.name_local(slot, ident)?;
    }

    Ok((s, ()))
}

fn ident_list<'s>(s: Lexer<'s>) -> Result<(Lexer<'s>, Vec<&'s str>), LexParseError> {
    let (mut s, ident) = identifier(s)?;

    let mut r = vec![ident];

    let next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, &'s str), LexParseError> {
        let (s, ()) = match_token(s, Token::Comma)?;
        let (s, ident) = identifier(s).require()?;

        Ok((s, ident))
    };

    while let Ok((ns, ident)) = next_part(s.clone()) {
        s = ns;
        r.push(ident);
    }

    Ok((s, r))
}
