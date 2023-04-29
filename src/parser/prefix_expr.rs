use super::tracker::ChunkTracker;
use super::{expr_adjusted_to_1, expr_list, par_expr, LexParseError, NextToken, ParseError};
use crate::lex::{Lexer, Token};

fn variable<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    let ident = match s.next_token()? {
        Token::Ident(ident) => ident,
        _ => return Err(ParseError.into()),
    };

    let slot = tracker.lookup_local(ident).ok_or(ParseError)?;
    tracker.current_mut()?.emit(OpCode::LoadStack(slot))?;

    Ok((s, ()))
}

pub(super) fn prefix_expr<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mut s = if let Ok((s, ())) = variable(s.clone(), tracker) {
        s
    } else if let Ok((s, ())) = par_expr(s, tracker) {
        s
    } else {
        return Err(ParseError.into());
    };

    loop {
        s = if let Ok((s, ())) = func_args(s.clone(), tracker) {
            s
        } else if let Ok((s, ())) = field(s.clone(), tracker) {
            s
        } else if let Ok((s, ())) = index(s.clone(), tracker) {
            s
        } else {
            break;
        }
    }

    Ok((s, ()))
}

fn func_args<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    match s.next_required_token()? {
        Token::ParL => (),
        _ => return Err(ParseError.into()),
    }

    let invoke_target = tracker.current()?.stack_top().unwrap().prev().unwrap();

    s = match expr_list(s.clone(), tracker) {
        Ok((s, ())) => s,
        _ => s,
    };

    match s.next_required_token()? {
        Token::ParR => (),
        _ => return Err(ParseError.into()),
    }

    tracker.current_mut()?.emit(OpCode::Invoke(invoke_target))?;

    Ok((s, ()))
}

fn field<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;
    use crate::value::Literal;

    match s.next_token()? {
        Token::Dot => (),
        _ => return Err(ParseError.into()),
    }

    let ident = match s.next_required_token()? {
        Token::Ident(ident) => ident,
        _ => return Err(ParseError.into()),
    };

    let const_id = tracker.insert_literal(Literal::String(ident.to_string()))?;
    let fun = tracker.current_mut()?;
    fun.emit(OpCode::LoadConstant(const_id))?;
    fun.emit(OpCode::TabGet)?;

    Ok((s, ()))
}

fn index<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;

    match s.next_token()? {
        Token::BracketL => (),
        _ => return Err(ParseError.into()),
    }

    let (mut s, ()) = expr_adjusted_to_1(s, tracker)?;
    tracker.current_mut()?.emit(OpCode::TabGet)?;

    match s.next_required_token()? {
        Token::BracketR => (),
        _ => return Err(ParseError.into()),
    }

    Ok((s, ()))
}
