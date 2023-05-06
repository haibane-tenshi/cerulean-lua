use crate::parser::prelude::*;

pub(in crate::parser) fn table<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::match_token;

    let (s, ()) = match_token(s, Token::CurlyL)?;

    let table_slot = tracker.current()?.stack_top()?;
    tracker.current_mut()?.emit(OpCode::TabCreate)?;

    let (s, _) = field_list(s.clone(), tracker, table_slot).optional(s);
    let (s, ()) = match_token(s, Token::CurlyR)?;

    Ok((s, ()))
}

fn field_list<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
    table_slot: StackSlot,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mut next_index = 1;

    let mut field = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        if let Ok(r) = bracket(s.clone(), tracker, table_slot) {
            Ok(r)
        } else if let Ok(r) = name(s.clone(), tracker, table_slot) {
            Ok(r)
        } else if let Ok(r) = index(s, tracker, table_slot, next_index) {
            next_index += 1;
            Ok(r)
        } else {
            Err(ParseError.into())
        }
    };

    let field_sep = |mut s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        match s.next_token()? {
            Token::Comma | Token::Semicolon => (),
            _ => return Err(ParseError.into()),
        };

        Ok((s, ()))
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

    Ok((s, ()))
}

fn bracket<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
    table_slot: StackSlot,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::{expr_adjusted_to_1, match_token};

    let (s, ()) = match_token(s, Token::BracketL)?;

    tracker.current_mut()?.emit(OpCode::LoadStack(table_slot))?;

    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::BracketR).require()?;
    let (s, ()) = match_token(s, Token::Assign).require()?;
    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;

    tracker.current_mut()?.emit(OpCode::TabSet)?;

    Ok((s, ()))
}

fn name<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
    table_slot: StackSlot,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::{expr_adjusted_to_1, identifier, match_token};

    let (s, ident) = identifier(s)?;

    tracker.current_mut()?.emit(OpCode::LoadStack(table_slot))?;

    let const_id = tracker.insert_literal(Literal::String(ident.to_string()))?;
    tracker
        .current_mut()?
        .emit(OpCode::LoadConstant(const_id))?;

    let (s, ()) = match_token(s, Token::Assign).require()?;
    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;

    tracker.current_mut()?.emit(OpCode::TabSet)?;

    Ok((s, ()))
}

fn index<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
    table_slot: StackSlot,
    index: i64,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::expr_adjusted_to_1;

    let start = tracker.current()?.stack_top()?;
    let r = expr_adjusted_to_1(s, tracker)?;

    tracker.current_mut()?.emit(OpCode::LoadStack(table_slot))?;

    let const_id = tracker.insert_literal(Literal::Int(index))?;
    tracker
        .current_mut()?
        .emit(OpCode::LoadConstant(const_id))?;

    tracker.current_mut()?.emit(OpCode::LoadStack(start))?;
    tracker.current_mut()?.emit(OpCode::TabSet)?;
    tracker.current_mut()?.emit_adjust_to(start)?;

    Ok(r)
}
