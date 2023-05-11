use crate::parser::prelude::*;

pub(super) fn generic_for<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut outer_frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::block::block;
    use crate::parser::expr::expr_list_adjusted_to;

    let (s, ()) = match_token(s, Token::For)?;
    let (s, names) = name_list(s).require()?;
    let (s, ()) = match_token(s, Token::In).require()?;

    let top = outer_frag.stack().top()?;
    let (s, ()) = expr_list_adjusted_to(s, 4, chunk, outer_frag.new_fragment()).require()?;

    let iter = top;
    let state = top + 1;
    let control = top + 2;
    // Currently unimplemented.
    let _close = top + 3;

    let nil = chunk.constants.insert(Literal::Nil)?;

    let mut frag = outer_frag.new_fragment();
    let new_control = frag.stack().top()?;

    frag.emit(OpCode::LoadStack(iter))?;
    frag.emit(OpCode::LoadStack(state))?;
    frag.emit(OpCode::LoadStack(control))?;
    frag.emit(OpCode::Invoke(new_control))?;

    let count: u32 = names.len().try_into().unwrap();
    frag.emit_adjust_to(new_control + count)?;

    frag.emit(OpCode::LoadStack(new_control))?;
    frag.emit(OpCode::LoadConstant(nil))?;
    frag.emit(OpCode::RelBinOp(RelBinOp::Eq))?;
    frag.emit_jump_to(frag.id(), Some(true))?;

    frag.emit(OpCode::LoadStack(new_control))?;
    frag.emit(OpCode::StoreStack(control))?;

    // Assign names
    for (name, slot) in names.into_iter().zip((new_control.0..).map(StackSlot)) {
        frag.stack_mut().give_name(slot, name)?;
    }

    let (s, ()) = match_token(s, Token::Do).require()?;
    let (s, ()) = block(s, chunk, frag.new_fragment()).require()?;
    let (s, ()) = match_token(s, Token::End).require()?;

    frag.emit_loop_to()?;
    frag.commit_scope();
    outer_frag.commit();

    Ok((s, ()))
}

fn name_list<'s>(s: Lexer<'s>) -> Result<(Lexer<'s>, Vec<&'s str>), LexParseError> {
    let (mut s, ident) = identifier(s)?;
    let mut r = vec![ident];

    let next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, &'s str), LexParseError> {
        let (s, ()) = match_token(s, Token::Comma)?;
        identifier(s).require()
    };

    while let Ok((ns, ident)) = next_part(s.clone()) {
        s = ns;
        r.push(ident);
    }

    Ok((s, r))
}
