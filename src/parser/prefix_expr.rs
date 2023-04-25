use super::tracker::ChunkTracker;
use super::{expr_list, par_expr, LexParseError, NextToken, ParseError};
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

    while let Ok((ns, ())) = func_args(s.clone(), tracker) {
        s = ns;
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
