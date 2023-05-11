use crate::parser::prelude::*;

pub(super) fn local_assignment<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::expr::expr_list;

    let (s, ()) = match_token(s, Token::Local)?;

    let stack_start = frag.stack().top()?;

    let (s, idents) = ident_list(s).require()?;
    let (s, ()) = match_token(s, Token::Assign).require()?;
    let (s, ()) = expr_list(s, chunk, frag.new_fragment()).map_err(LexParseError::eof_into_err)?;

    let count: u32 = idents.len().try_into().unwrap();
    frag.emit_adjust_to(stack_start + count)?;

    for (ident, slot) in idents.into_iter().zip((stack_start.0..).map(StackSlot)) {
        frag.stack_mut().give_name(slot, ident)?;
    }

    frag.commit();

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
