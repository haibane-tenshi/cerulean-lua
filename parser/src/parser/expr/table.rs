use logos::Span;
use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn table<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    use TabFailure::*;

    let (s, _) = match_token(s, Token::CurlyL).map_parse(CurlyL)?;

    let table_slot = frag.stack().top()?;
    frag.emit(OpCode::TabCreate)?;

    let (s, _, _) = field_list(s.clone(), table_slot, chunk, frag.new_fragment()).optional(s);
    let (s, _) = match_token(s, Token::CurlyR).map_parse(CurlyR)?;

    frag.commit();

    Ok((s, ()))
}

#[derive(Debug, Error)]
pub(crate) enum TabFailure {
    #[error("expected opening curly brace")]
    CurlyL(TokenMismatch),
    #[error("expected closing curly brace")]
    CurlyR(TokenMismatch),
}

impl HaveFailureMode for TabFailure {
    fn mode(&self) -> FailureMode {
        match self {
            TabFailure::CurlyL(_) => FailureMode::Mismatch,
            TabFailure::CurlyR(_) => FailureMode::Malformed,
        }
    }
}

fn field_list<'s>(
    s: Lexer<'s>,
    table_slot: StackSlot,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    let mut next_index = 1;
    let mut field = |s: Lexer<'s>| -> Result<_, Error<ParseFailure>> {
        let (s, field_type) = field(
            s.clone(),
            table_slot,
            next_index,
            chunk,
            frag.new_fragment(),
        )?;

        if let FieldType::Index = field_type {
            next_index += 1
        }

        Ok((s, ()))
    };

    let (mut s, ()) = field(s)?;

    loop {
        s = match field_sep(s.clone()) {
            Ok((s, _)) => s,
            Err(_erf) => break,
        };

        s = match field(s.clone()) {
            Ok((s, ())) => s,
            Err(_err) => break,
        }
    }

    frag.commit();
    Ok((s, ()))
}

#[derive(Debug)]
enum FieldListSuccess {
    FieldSep(ParseError<FieldSepMismatchError>),
    Expr(Error<ParseFailure>),
}

impl From<ParseError<FieldSepMismatchError>> for FieldListSuccess {
    fn from(value: ParseError<FieldSepMismatchError>) -> Self {
        FieldListSuccess::FieldSep(value)
    }
}

impl From<Error<ParseFailure>> for FieldListSuccess {
    fn from(value: Error<ParseFailure>) -> Self {
        FieldListSuccess::Expr(value)
    }
}

fn field<'s>(
    s: Lexer<'s>,
    table_slot: StackSlot,
    next_index: i64,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, FieldType), Error<ParseFailure>> {
    let inner = || {
        let mut err = match bracket(s.clone(), table_slot, chunk, frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, FieldType::Bracket)),
            Err(err) => err,
        };

        err |= match name(s.clone(), table_slot, chunk, frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, FieldType::Name)),
            Err(err) => err,
        };

        err |= match index(s, table_slot, next_index, chunk, frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, FieldType::Index)),
            Err(err) => err,
        };

        Err(err)
    };

    let r = inner()?;

    frag.commit();
    Ok(r)
}

enum FieldType {
    Index,
    Bracket,
    Name,
}

fn field_sep(mut s: Lexer) -> Result<(Lexer, Span), ParseError<FieldSepMismatchError>> {
    use crate::parser::NextTokenError;

    match s.next_token() {
        Ok(Token::Comma | Token::Semicolon) => {
            let span = s.span();
            Ok((s, span))
        }
        Ok(_) | Err(NextTokenError::Parse(Eof)) => Err(ParseError::Parse(FieldSepMismatchError)),
        Err(NextTokenError::Lex(lex)) => Err(lex.into()),
    }
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected table field separator")]
pub(crate) struct FieldSepMismatchError;

fn bracket<'s>(
    s: Lexer<'s>,
    table_slot: StackSlot,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    use crate::parser::expr::expr_adjusted_to_1;
    use TabBracketFailure::*;

    let (s, _) = match_token(s, Token::BracketL).map_parse(BracketL)?;

    frag.emit(OpCode::LoadStack(table_slot))?;

    let (s, ()) =
        expr_adjusted_to_1(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;
    let (s, _) = match_token(s, Token::BracketR).map_parse(BracketR)?;
    let (s, _) = match_token(s, Token::EqualsSign).map_parse(EqualsSign)?;
    let (s, ()) =
        expr_adjusted_to_1(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;

    frag.emit(OpCode::TabSet)?;

    frag.commit();
    Ok((s, ()))
}

#[derive(Debug, Error)]
pub(crate) enum TabBracketFailure {
    #[error("expected opening bracket")]
    BracketL(TokenMismatch),
    #[error("expected closing bracket")]
    BracketR(TokenMismatch),
    #[error("expected equals sign")]
    EqualsSign(TokenMismatch),
}

impl HaveFailureMode for TabBracketFailure {
    fn mode(&self) -> FailureMode {
        match self {
            TabBracketFailure::BracketL(_) => FailureMode::Mismatch,
            TabBracketFailure::BracketR(_) => FailureMode::Malformed,
            TabBracketFailure::EqualsSign(_) => FailureMode::Malformed,
        }
    }
}

fn name<'s>(
    s: Lexer<'s>,
    table_slot: StackSlot,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    use crate::parser::expr::expr_adjusted_to_1;
    use TabNameFailure::*;

    let (s, (ident, _)) = identifier(s).map_parse(Ident)?;

    frag.emit(OpCode::LoadStack(table_slot))?;

    let const_id = chunk.constants.insert(Literal::String(ident.to_string()))?;
    frag.emit(OpCode::LoadConstant(const_id))?;

    let (s, _) = match_token(s, Token::EqualsSign).map_parse(EqualsSign)?;
    let (s, ()) =
        expr_adjusted_to_1(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;

    frag.emit(OpCode::TabSet)?;

    frag.commit();
    Ok((s, ()))
}

#[derive(Debug, Error)]
pub(crate) enum TabNameFailure {
    #[error("expected identifier")]
    Ident(IdentMismatch),
    #[error("expected equals sign")]
    EqualsSign(TokenMismatch),
}

impl HaveFailureMode for TabNameFailure {
    fn mode(&self) -> FailureMode {
        match self {
            TabNameFailure::Ident(_) => FailureMode::Mismatch,
            TabNameFailure::EqualsSign(_) => FailureMode::Malformed,
        }
    }
}

fn index<'s>(
    s: Lexer<'s>,
    table_slot: StackSlot,
    index: i64,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
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
