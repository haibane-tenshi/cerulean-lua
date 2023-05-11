use crate::parser::prelude::*;

pub(super) fn local_function<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::func_def::func_body;

    let (s, ()) = match_token(s, Token::Local)?;
    let (s, ()) = match_token(s, Token::Function).require()?;
    let (s, ident) = identifier(s).require()?;

    // Lua disambiguates this case by introducing local variable first and assigning to it later.
    // This is relevant for recursive functions.
    // We only need to introduce the name:
    // it will get assigned to after function body is parsed.
    let slot = frag.stack_mut().push()?;
    frag.stack_mut().give_name(slot, ident)?;

    let (s, func_id) = func_body(s, chunk, frag.new_fragment()).require()?;

    let const_id = chunk.constants.insert(Literal::Function(func_id))?;

    // Stack is already adjusted, we just need to silently write to correct slot here.
    frag.emit_raw(OpCode::LoadConstant(const_id), false)?;
    frag.commit();

    Ok((s, ()))
}
