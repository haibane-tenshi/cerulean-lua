use crate::parser::prelude::*;
use crate::tracker2::fragment::EmitError;

pub(in crate::parser) fn prefix_expr<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let (s, r) = prefix_expr_impl(s, chunk, frag.new_fragment())?;

    // Eagerly evaluate place.
    if let PrefixExpr::Place(place) = r {
        place.eval(&mut frag)?;
    }

    frag.commit();

    Ok((s, ()))
}

pub(in crate::parser) fn place<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, Place), LexParseError> {
    let (s, r) = prefix_expr_impl(s, chunk, frag.new_fragment())?;

    if let PrefixExpr::Place(place) = r {
        frag.commit();
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
    pub fn eval(self, frag: &mut Fragment) -> Result<InstrId, EmitError> {
        let opcode = match self {
            Place::Temporary(slot) => OpCode::LoadStack(slot),
            Place::TableField => OpCode::TabGet,
        };

        frag.emit(opcode)
    }
}

#[derive(Debug)]
enum PrefixExpr {
    Expr,
    Place(Place),
    FnCall,
}

fn prefix_expr_impl<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, PrefixExpr), LexParseError> {
    use crate::parser::expr::par_expr;

    let stack_top = frag.stack().top()? + 1;

    let (mut s, mut r) = if let Ok((s, slot)) = variable(s.clone(), &frag) {
        (s, PrefixExpr::Place(Place::Temporary(slot)))
    } else if let Ok((s, ())) = par_expr(s, chunk, frag.new_fragment()) {
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
            place.eval(&mut frag)?;
        }

        // Adjust stack.
        // Prefix expressions (except the very last one) always evaluate to 1 value.
        // We are reusing the same stack slot to store it.
        frag.emit_adjust_to(stack_top)?;

        (s, r) = if let Ok((s, ())) = func_call(s.clone(), chunk, frag.new_fragment()) {
            (s, PrefixExpr::FnCall)
        } else if let Ok((s, ())) = field(s.clone(), chunk, frag.new_fragment()) {
            (s, PrefixExpr::Place(Place::TableField))
        } else if let Ok((s, ())) = index(s.clone(), chunk, frag.new_fragment()) {
            (s, PrefixExpr::Place(Place::TableField))
        } else {
            return Err(ParseError.into());
        };
    }

    frag.commit();

    Ok((s, r))
}

fn func_call<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let invoke_target = frag.stack().top()?.prev().unwrap();

    let s = if let Ok((s, ())) = args_par_expr(s.clone(), chunk, frag.new_fragment()) {
        s
    } else if let Ok((s, ())) = args_table(s.clone(), chunk, frag.new_fragment()) {
        s
    } else if let Ok((s, ())) = args_str(s, chunk, frag.new_fragment()) {
        s
    } else {
        return Err(ParseError.into());
    };

    frag.emit(OpCode::Invoke(invoke_target))?;
    frag.commit();

    Ok((s, ()))
}

fn args_par_expr<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::expr::expr_list;

    let (s, ()) = match_token(s, Token::ParL).require()?;
    let (s, _) = expr_list(s.clone(), chunk, frag.new_fragment()).optional(s);
    let (s, ()) = match_token(s, Token::ParR).require()?;

    frag.commit();

    Ok((s, ()))
}

fn args_str<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let (s, val) = literal_str(s)?;

    let const_id = chunk.constants.insert(Literal::String(val.into_owned()))?;
    frag.emit(OpCode::LoadConstant(const_id))?;

    frag.commit();

    Ok((s, ()))
}

fn args_table<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    crate::parser::expr::table(s, chunk, frag)
}

fn variable<'s>(
    s: Lexer<'s>,
    frag: &Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, StackSlot), LexParseError> {
    let (s, ident) = identifier(s)?;
    match frag.stack().lookup(ident) {
        NameLookup::Local(slot) => Ok((s, slot)),
        NameLookup::Upvalue | NameLookup::Global => Err(ParseError.into()),
    }
}

fn field<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let (s, ()) = match_token(s, Token::Dot)?;
    let (s, ident) = identifier(s).require()?;

    let const_id = chunk.constants.insert(Literal::String(ident.to_string()))?;
    frag.emit(OpCode::LoadConstant(const_id))?;

    frag.commit();

    Ok((s, ()))
}

fn index<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::expr::expr_adjusted_to_1;

    let (s, ()) = match_token(s, Token::BracketL)?;
    let (s, ()) = expr_adjusted_to_1(s, chunk, frag.new_fragment()).require()?;
    let (s, ()) = match_token(s, Token::BracketR).require()?;

    frag.commit();

    Ok((s, ()))
}
