use crate::parser::prelude::*;
use crate::tracker::EmitError;

pub(in crate::parser) fn prefix_expr<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let (s, r) = prefix_expr_impl(s, tracker)?;

    // Eagerly evaluate place.
    if let PrefixExpr::Place(place) = r {
        place.eval(tracker.current_mut()?)?;
    }

    Ok((s, ()))
}

pub(in crate::parser) fn place<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, Place), LexParseError> {
    let (s, r) = prefix_expr_impl(s, tracker)?;

    if let PrefixExpr::Place(place) = r {
        Ok((s, place))
    } else {
        Err(ParseError.into())
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Place {
    Temporary(StackSlot),
    TableField,
}

impl Place {
    pub fn eval(self, tracker: &mut FunctionTracker) -> Result<InstrId, EmitError> {
        let opcode = match self {
            Place::Temporary(slot) => OpCode::LoadStack(slot),
            Place::TableField => OpCode::TabGet,
        };

        tracker.emit(opcode)
    }
}

enum PrefixExpr {
    Expr,
    Place(Place),
    FnCall,
}

fn prefix_expr_impl<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, PrefixExpr), LexParseError> {
    use crate::parser::expr::par_expr;

    let stack_top = tracker.current()?.stack_top()? + 1;

    let (mut s, mut r) = if let Ok((s, slot)) = variable(s.clone(), tracker) {
        (s, PrefixExpr::Place(Place::Temporary(slot)))
    } else if let Ok((s, ())) = par_expr(s, tracker) {
        (s, PrefixExpr::Expr)
    } else {
        return Err(ParseError.into());
    };

    #[allow(clippy::while_let_loop)]
    loop {
        // Peek to determine if any of the follow-up expressions are expected.
        // Note that we effectively start codegen in subparsers after encountering
        // any of these tokens, so even if no parsers properly evaluate,
        // we must error out anyway since we end up in bad state.
        match s.clone().next_token() {
            Ok(
                Token::ParL
                | Token::CurlyL
                | Token::ShortLiteralString(_)
                | Token::BracketL
                | Token::Dot,
            ) => (),
            _ => break,
        }

        // Evaluate place.
        if let PrefixExpr::Place(place) = r {
            place.eval(tracker.current_mut()?)?;
        }

        // Adjust stack.
        // Prefix expressions (except the very last one) always evaluate to 1 value.
        // We are reusing the same stack slot to store it.
        tracker.current_mut()?.emit_adjust_to(stack_top)?;

        (s, r) = if let Ok((s, ())) = func_call(s.clone(), tracker) {
            (s, PrefixExpr::FnCall)
        } else if let Ok((s, ())) = field(s.clone(), tracker) {
            (s, PrefixExpr::Place(Place::TableField))
        } else if let Ok((s, ())) = index(s.clone(), tracker) {
            (s, PrefixExpr::Place(Place::TableField))
        } else {
            return Err(ParseError.into());
        };
    }

    Ok((s, r))
}

fn func_call<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
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
    use crate::parser::expr::expr_list;

    let (s, ()) = match_token(s, Token::ParL).require()?;
    let (s, _) = expr_list(s.clone(), tracker).optional(s);
    let (s, ()) = match_token(s, Token::ParR).require()?;

    Ok((s, ()))
}

fn args_str<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
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
    crate::parser::expr::table(s, tracker)
}

fn variable<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker,
) -> Result<(Lexer<'s>, StackSlot), LexParseError> {
    let (s, ident) = identifier(s)?;
    let slot = tracker.lookup_local(ident).ok_or(ParseError)?;

    Ok((s, slot))
}

fn field<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let (s, ()) = match_token(s, Token::Dot)?;
    let (s, ident) = identifier(s).require()?;

    let const_id = tracker.insert_literal(Literal::String(ident.to_string()))?;
    let fun = tracker.current_mut()?;
    fun.emit(OpCode::LoadConstant(const_id))?;

    Ok((s, ()))
}

fn index<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::expr::expr_adjusted_to_1;

    let (s, ()) = match_token(s, Token::BracketL)?;
    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::BracketR).require()?;

    Ok((s, ()))
}
