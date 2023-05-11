use crate::parser::prelude::*;

pub(in crate::parser) fn table<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let (s, ()) = match_token(s, Token::CurlyL)?;

    let table_slot = frag.stack().top()?;
    frag.emit(OpCode::TabCreate)?;

    let (s, _) = field_list(s.clone(), table_slot, chunk, frag.new_fragment()).optional(s);
    let (s, ()) = match_token(s, Token::CurlyR)?;

    frag.commit();

    Ok((s, ()))
}

fn field_list<'s>(
    s: Lexer<'s>,
    table_slot: StackSlot,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mut next_index = 1;

    let mut field = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        if let Ok(r) = bracket(s.clone(), table_slot, chunk, frag.new_fragment()) {
            Ok(r)
        } else if let Ok(r) = name(s.clone(), table_slot, chunk, frag.new_fragment()) {
            Ok(r)
        } else if let Ok(r) = index(s, table_slot, next_index, chunk, frag.new_fragment()) {
            next_index += 1;
            Ok(r)
        } else {
            Err(ParseError.into())
        }
    };

    let (mut s, ()) = field(s)?;

    let mut next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        let (s, ()) = field_sep(s)?;
        field(s).require()
    };

    loop {
        s = match next_part(s.clone()) {
            Ok((s, ())) => s,
            Err(_) => break,
        }
    }

    let (s, _) = field_sep(s.clone()).optional(s);

    frag.commit();

    Ok((s, ()))
}

fn field_sep(mut s: Lexer) -> Result<(Lexer, ()), LexParseError> {
    match s.next_token()? {
        Token::Comma | Token::Semicolon => (),
        _ => return Err(ParseError.into()),
    };

    Ok((s, ()))
}

fn bracket<'s>(
    s: Lexer<'s>,
    table_slot: StackSlot,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::expr::expr_adjusted_to_1;

    let (s, ()) = match_token(s, Token::BracketL)?;

    frag.emit(OpCode::LoadStack(table_slot))?;

    let (s, ()) = expr_adjusted_to_1(s, chunk, frag.new_fragment()).require()?;
    let (s, ()) = match_token(s, Token::BracketR).require()?;
    let (s, ()) = match_token(s, Token::EqualsSign).require()?;
    let (s, ()) = expr_adjusted_to_1(s, chunk, frag.new_fragment()).require()?;

    frag.emit(OpCode::TabSet)?;
    frag.commit();

    Ok((s, ()))
}

fn name<'s>(
    s: Lexer<'s>,
    table_slot: StackSlot,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::expr::expr_adjusted_to_1;

    let (s, ident) = identifier(s)?;

    frag.emit(OpCode::LoadStack(table_slot))?;

    let const_id = chunk.constants.insert(Literal::String(ident.to_string()))?;
    frag.emit(OpCode::LoadConstant(const_id))?;

    let (s, ()) = match_token(s, Token::EqualsSign).require()?;
    let (s, ()) = expr_adjusted_to_1(s, chunk, frag.new_fragment()).require()?;

    frag.emit(OpCode::TabSet)?;
    frag.commit();

    Ok((s, ()))
}

fn index<'s>(
    s: Lexer<'s>,
    table_slot: StackSlot,
    index: i64,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::expr::expr_adjusted_to_1;

    let start = frag.stack().top()?;
    let r = expr_adjusted_to_1(s, chunk, frag.new_fragment())?;

    frag.emit(OpCode::LoadStack(table_slot))?;

    let const_id = chunk.constants.insert(Literal::Int(index))?;
    frag.emit(OpCode::LoadConstant(const_id))?;

    frag.emit(OpCode::LoadStack(start))?;
    frag.emit(OpCode::TabSet)?;
    frag.emit_adjust_to(start)?;

    frag.commit();

    Ok(r)
}
