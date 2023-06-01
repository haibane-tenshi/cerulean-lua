use thiserror::Error;

use crate::codegen::fragment::EmitError;
use crate::parser::prelude::*;

pub(in crate::parser) fn prefix_expr<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ()), Error<ParseFailure>> {
    let (s, r, status) = prefix_expr_impl(s, chunk, frag.new_fragment())?;

    // Eagerly evaluate place.
    if let PrefixExpr::Place(place) = r {
        place.eval(&mut frag)?;
    }

    frag.commit();

    Ok((s, (), status))
}

pub(in crate::parser) fn place<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, Place, ()), Error<ParseFailure>> {
    let (s, r, status) = prefix_expr_impl(s, chunk, frag.new_fragment())?;

    if let PrefixExpr::Place(place) = r {
        frag.commit();

        Ok((s, place, status))
    } else {
        let err = ParseFailure {
            mode: FailureMode::Malformed,
            cause: ParseCause::ExpectedPlace,
        };

        Err(err.into())
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
) -> Result<(Lexer<'s>, PrefixExpr, ()), Error<ParseFailure>> {
    use crate::parser::expr::par_expr;

    let stack_top = frag.stack().top()? + 1;

    let prefix = || {
        let mut err: Error<ParseFailure> = match variable(s.clone(), &frag) {
            Ok((s, slot, status)) => {
                return Ok((s, PrefixExpr::Place(Place::Temporary(slot)), status))
            }
            Err(err) => err.into(),
        };

        err |= match par_expr(s, chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, PrefixExpr::Expr, status)),
            Err(err) => err,
        };

        Err(err)
    };

    let (mut s, mut r, _) = prefix()?;

    #[allow(clippy::while_let_loop)]
    loop {
        // Peek to determine if any of the follow-up expressions are expected.
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
        // TODO: properly wrap it in fragment instead of peeking
        if let PrefixExpr::Place(place) = r {
            place.eval(&mut frag)?;
        }

        // Adjust stack.
        // Prefix expressions (except the very last one) always evaluate to 1 value.
        // We are reusing the same stack slot to store it.
        frag.emit_adjust_to(stack_top)?;

        let mut tail = || {
            let mut err = match func_call(s.clone(), chunk, frag.new_fragment()) {
                Ok((s, (), status)) => return Ok((s, PrefixExpr::FnCall, status)),
                Err(err) => err,
            };

            err |= match field(s.clone(), chunk, frag.new_fragment()) {
                Ok((s, (), status)) => {
                    return Ok((s, PrefixExpr::Place(Place::TableField), status))
                }
                Err(err) => err,
            };

            err |= match index(s.clone(), chunk, frag.new_fragment()) {
                Ok((s, (), status)) => {
                    return Ok((s, PrefixExpr::Place(Place::TableField), status))
                }
                Err(err) => err,
            };

            Err(err)
        };

        (s, r, _) = tail()?;
    }

    frag.commit();

    Ok((s, r, ()))
}

fn func_call<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    let invoke_target = frag.stack().top()? - repr::index::StackOffset(1);

    let choice = || {
        let mut err = match args_par_expr(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status)),
            Err(err) => err,
        };

        err |= match args_table(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status)),
            Err(err) => err,
        };

        err |= match args_str(s, chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status)),
            Err(err) => err.map_parse(|_| ParseFailure {
                mode: FailureMode::Mismatch,
                cause: ParseCause::FunctionCall,
            }),
        };

        Err(err)
    };

    let (s, (), Complete) = choice()?;

    frag.emit(OpCode::Invoke(invoke_target))?;
    frag.commit();

    Ok((s, (), Complete))
}

fn args_par_expr<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    use crate::parser::expr::expr_list;
    use ArgsParExprFailure::*;

    let (s, _, _) = match_token(s, Token::ParL).map_parse(ParL)?;
    let (s, _, _) = expr_list(s.clone(), chunk, frag.new_fragment()).optional(s);
    let (s, _, status) = match_token(s, Token::ParR).map_parse(ParR)?;

    frag.commit();

    Ok((s, (), status))
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
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<LiteralStrMismatch>> {
    let (s, (val, _), _) = literal_str(s)?;

    let const_id = chunk.constants.insert(Literal::String(val.into_owned()))?;
    frag.emit(OpCode::LoadConstant(const_id))?;

    frag.commit();

    Ok((s, (), Complete))
}

fn args_table<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    crate::parser::expr::table(s, chunk, frag)
}

fn variable<'s>(
    s: Lexer<'s>,
    frag: &Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, StackSlot, Complete), ParseError<VariableFailure>> {
    use VariableFailure::*;

    let (s, (ident, _), _) = identifier(s).map_parse(Ident)?;
    match frag.stack().lookup(ident) {
        NameLookup::Local(slot) => Ok((s, slot, Complete)),
        NameLookup::Upvalue => Err(ParseError::Parse(UnsupportedUpvalue)),
        NameLookup::Global => Err(ParseError::Parse(UnsupportedGlobal)),
    }
}

#[derive(Debug, Error)]
#[error("failed to parse variable")]
pub enum VariableFailure {
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
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    use FieldFailure::*;

    let (s, _, _) = match_token(s, Token::Dot).map_parse(Dot)?;
    let (s, (ident, _), status) = identifier(s).map_parse(Ident)?;

    let const_id = chunk.constants.insert(Literal::String(ident.to_string()))?;
    frag.emit(OpCode::LoadConstant(const_id))?;

    frag.commit();

    Ok((s, (), status))
}

#[derive(Debug, Error)]
#[error("failed to access table's field")]
pub enum FieldFailure {
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
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    use crate::parser::expr::expr_adjusted_to_1;
    use IndexFailure::*;

    let (s, _, _) = match_token(s, Token::BracketL).map_parse(BracketL)?;
    let (s, (), _) = expr_adjusted_to_1(s, chunk, frag.new_fragment())
        .map_err(|err| err.with_mode(FailureMode::Malformed))?;
    let (s, _, status) = match_token(s, Token::BracketR).map_parse(BracketR)?;

    frag.commit();

    Ok((s, (), status))
}

#[derive(Debug, Error)]
#[error("failed to parse table index")]
pub enum IndexFailure {
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
