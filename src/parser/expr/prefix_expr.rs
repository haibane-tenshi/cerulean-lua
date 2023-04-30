use crate::lex::Lexer;
use crate::parser::{LexParseError, Optional, Require};
use crate::tracker::ChunkTracker;

fn variable<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;
    use crate::parser::{identifier, ParseError};

    let (s, ident) = identifier(s)?;
    let slot = tracker.lookup_local(ident).ok_or(ParseError)?;
    tracker.current_mut()?.emit(OpCode::LoadStack(slot))?;

    Ok((s, ()))
}

pub(in crate::parser) fn prefix_expr<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::{par_expr, ParseError};

    let mut s = if let Ok((s, ())) = variable(s.clone(), tracker) {
        s
    } else if let Ok((s, ())) = par_expr(s, tracker) {
        s
    } else {
        return Err(ParseError.into());
    };

    loop {
        s = if let Ok((s, ())) = func_call(s.clone(), tracker) {
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

fn func_call<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;
    use crate::parser::ParseError;

    let invoke_target = tracker.current()?.stack_top()?.prev().unwrap();

    let s = if let Ok((s, ())) = args_par_expr(s.clone(), tracker) {
        s
    } else if let Ok((s, ())) = args_table(s.clone(), tracker) {
        s
    } else if let Ok((s, ())) = args_str(s, tracker) {
        s
    } else {
        return Err(ParseError.into());
    };

    tracker.current_mut()?.emit(OpCode::Invoke(invoke_target))?;

    Ok((s, ()))
}

fn args_par_expr<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::parser::{expr_list, match_token};

    let (s, ()) = match_token(s, Token::ParL).require()?;
    let (s, _) = expr_list(s.clone(), tracker).optional(s);
    let (s, ()) = match_token(s, Token::ParR).require()?;

    Ok((s, ()))
}

fn args_str<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::opcode::OpCode;
    use crate::parser::literal_str;
    use crate::value::Literal;

    let (s, val) = literal_str(s)?;

    let const_id = tracker.insert_literal(Literal::String(val.into_owned()))?;
    tracker
        .current_mut()?
        .emit(OpCode::LoadConstant(const_id))?;

    Ok((s, ()))
}

fn args_table<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    super::table(s, tracker)
}

fn field<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::opcode::OpCode;
    use crate::parser::{identifier, match_token};
    use crate::value::Literal;

    let (s, ()) = match_token(s, Token::Dot)?;
    let (s, ident) = identifier(s).require()?;

    let const_id = tracker.insert_literal(Literal::String(ident.to_string()))?;
    let fun = tracker.current_mut()?;
    fun.emit(OpCode::LoadConstant(const_id))?;
    fun.emit(OpCode::TabGet)?;

    Ok((s, ()))
}

fn index<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::Token;
    use crate::opcode::OpCode;
    use crate::parser::{expr_adjusted_to_1, match_token};

    let (s, ()) = match_token(s, Token::BracketL)?;
    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::BracketR).require()?;

    tracker.current_mut()?.emit(OpCode::TabGet)?;

    Ok((s, ()))
}
