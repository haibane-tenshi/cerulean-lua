use thiserror::Error;

use crate::codegen::fragment::EmitError;
use crate::parser::prelude::*;

pub(crate) fn prefix_expr<'s>(
    s: Lexer<'s>,
    mut frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    let (s, r) = prefix_expr_impl(s, frag.new_fragment())?;

    // Eagerly evaluate place.
    if let PrefixExpr::Place(place) = r {
        place.eval(&mut frag)?;
    }

    frag.commit();

    Ok((s, ()))
}

pub(crate) fn place<'s>(
    s: Lexer<'s>,
    mut frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, Place), Error<ParseFailure>> {
    let (s, r) = prefix_expr_impl(s, frag.new_fragment())?;

    if let PrefixExpr::Place(place) = r {
        frag.commit();

        Ok((s, place))
    } else {
        let err = ParseFailure {
            mode: FailureMode::Malformed,
            cause: ParseCause::ExpectedPlace,
        };

        Err(err.into())
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum Place {
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

fn prefix_expr_impl<'s>(
    s: Lexer<'s>,
    mut frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, PrefixExpr), Error<ParseFailure>> {
    use crate::parser::expr::par_expr;

    let stack_start = frag.stack().top()?;

    let prefix = || {
        let mut err: Error<ParseFailure> = match variable(s.clone(), &frag) {
            Ok((s, slot)) => return Ok((s, PrefixExpr::Place(Place::Temporary(slot)))),
            Err(err) => err.into(),
        };

        err |= match par_expr(s, frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, PrefixExpr::Expr)),
            Err(err) => err,
        };

        Err(err)
    };

    let (mut s, mut r) = prefix()?;
    let stack_top = stack_start + 1;

    let mut next_part = |s: Lexer<'s>,
                         mut frag: Fragment<'s, '_>|
     -> Result<_, Error<ParseFailure>> {
        // Evaluate place.
        if let PrefixExpr::Place(place) = r {
            place.eval(&mut frag)?;
        }

        // Adjust stack.
        // Prefix expressions (except the very last one) always evaluate to 1 value.
        // We are reusing the same stack slot to store it.
        frag.emit_adjust_to(stack_top)?;

        let tail = || {
            let mut err = match func_call(s.clone(), stack_start, frag.new_fragment_at_boundary()) {
                Ok((s, ())) => return Ok((s, PrefixExpr::FnCall)),
                Err(err) => err,
            };

            err |= match field(s.clone(), frag.new_fragment()) {
                Ok((s, ())) => return Ok((s, PrefixExpr::Place(Place::TableField))),
                Err(err) => err,
            };

            err |= match index(s, frag.new_fragment()) {
                Ok((s, ())) => return Ok((s, PrefixExpr::Place(Place::TableField))),
                Err(err) => err,
            };

            Err(err)
        };

        let (s, nr) = tail()?;
        r = nr;

        frag.commit();
        Ok((s, ()))
    };

    let _ = loop {
        s = match next_part(s.clone(), frag.new_fragment_at(stack_start).unwrap()) {
            Ok((s, _)) => s,
            Err(err) => break err,
        }
    };

    frag.commit();
    Ok((s, r))
}

#[derive(Debug)]
enum PrefixExpr {
    Expr,
    Place(Place),
    FnCall,
}

fn func_call<'s>(
    s: Lexer<'s>,
    invoke_target: StackSlot,
    mut frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    let choice = || {
        let mut err = match args_par_expr(s.clone(), frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err,
        };

        err |= match args_table(s.clone(), frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err,
        };

        err |= match args_str(s, frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err.map_parse(|_| ParseFailure {
                mode: FailureMode::Mismatch,
                cause: ParseCause::FunctionCall,
            }),
        };

        Err(err)
    };

    let (s, ()) = choice()?;

    frag.emit(OpCode::Invoke(invoke_target))?;

    frag.commit();
    Ok((s, ()))
}

fn args_par_expr<'s>(
    s: Lexer<'s>,
    mut frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    use crate::parser::expr::expr_list;
    use ArgsParExprFailure::*;

    let (s, _) = match_token(s, Token::ParL).map_parse(ParL)?;
    let (s, _, _) = expr_list(s.clone(), frag.new_fragment()).optional(s);
    let (s, _) = match_token(s, Token::ParR).map_parse(ParR)?;

    frag.commit();
    Ok((s, ()))
}

#[derive(Debug, Error)]
#[error("failed to parse function arguments")]
pub enum ArgsParExprFailure {
    ParL(TokenMismatch),
    ParR(TokenMismatch),
}

impl HaveFailureMode for ArgsParExprFailure {
    fn mode(&self) -> FailureMode {
        use ArgsParExprFailure::*;

        match self {
            ParL(_) => FailureMode::Mismatch,
            ParR(_) => FailureMode::Malformed,
        }
    }
}

fn args_str<'s>(
    s: Lexer<'s>,
    mut frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, ()), Error<LiteralStrMismatch>> {
    let (s, (val, _)) = literal_str(s)?;

    let const_id = frag
        .const_table_mut()
        .insert(Literal::String(val.into_owned()))?;
    frag.emit(OpCode::LoadConstant(const_id))?;

    frag.commit();
    Ok((s, ()))
}

fn args_table<'s>(
    s: Lexer<'s>,
    frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    crate::parser::expr::table::table(s, frag)
}

fn variable<'s>(
    s: Lexer<'s>,
    frag: &Fragment<'s, '_>,
) -> Result<(Lexer<'s>, StackSlot), ParseError<VariableFailure>> {
    use VariableFailure::*;

    let (s, (ident, _)) = identifier(s).map_parse(Ident)?;
    match frag.stack().lookup(ident) {
        NameLookup::Local(slot) => Ok((s, slot)),
        NameLookup::Upvalue => Err(ParseError::Parse(UnsupportedUpvalue)),
        NameLookup::Global => Err(ParseError::Parse(UnsupportedGlobal)),
    }
}

#[derive(Debug, Error)]
#[error("failed to parse variable")]
pub(crate) enum VariableFailure {
    Ident(IdentMismatch),
    #[error("upvalues are not yet supported")]
    UnsupportedUpvalue,
    #[error("globals are not yet supported")]
    UnsupportedGlobal,
}

impl HaveFailureMode for VariableFailure {
    fn mode(&self) -> FailureMode {
        match self {
            VariableFailure::Ident(_) => FailureMode::Mismatch,
            _ => FailureMode::Malformed,
        }
    }
}

fn field<'s>(
    s: Lexer<'s>,
    mut frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    use FieldFailure::*;

    let (s, _) = match_token(s, Token::Dot).map_parse(Dot)?;
    let (s, (ident, _)) = identifier(s).map_parse(Ident)?;

    let const_id = frag
        .const_table_mut()
        .insert(Literal::String(ident.to_string()))?;
    frag.emit(OpCode::LoadConstant(const_id))?;

    frag.commit();
    Ok((s, ()))
}

#[derive(Debug, Error)]
#[error("failed to access table's field")]
pub(crate) enum FieldFailure {
    Dot(TokenMismatch),
    Ident(IdentMismatch),
}

impl HaveFailureMode for FieldFailure {
    fn mode(&self) -> FailureMode {
        match self {
            FieldFailure::Dot(_) => FailureMode::Mismatch,
            FieldFailure::Ident(_) => FailureMode::Malformed,
        }
    }
}

fn index<'s>(
    s: Lexer<'s>,
    mut frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    use crate::parser::expr::expr_adjusted_to_1;
    use IndexFailure::*;

    let (s, _) = match_token(s, Token::BracketL).map_parse(BracketL)?;
    let (s, ()) = expr_adjusted_to_1(s, frag.new_fragment())
        .map_err(|err| err.with_mode(FailureMode::Malformed))?;
    let (s, _) = match_token(s, Token::BracketR).map_parse(BracketR)?;

    frag.commit();
    Ok((s, ()))
}

#[derive(Debug, Error)]
#[error("failed to parse table index")]
pub(crate) enum IndexFailure {
    BracketL(TokenMismatch),
    BracketR(TokenMismatch),
}

impl HaveFailureMode for IndexFailure {
    fn mode(&self) -> FailureMode {
        match self {
            IndexFailure::BracketL(_) => FailureMode::Mismatch,
            IndexFailure::BracketR(_) => FailureMode::Malformed,
        }
    }
}
