use either::Either;
use logos::Span;
use std::ops::BitOr;
use thiserror::Error;

use crate::parser::expr::ExprSuccessReason;
use crate::parser::prelude::*;

pub(in crate::parser) fn table<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    use TabFailure::*;

    let (s, _, _) = match_token(s, Token::CurlyL).map_parse(CurlyL)?;

    let table_slot = frag.stack().top()?;
    frag.emit(OpCode::TabCreate)?;

    let (s, _, _) = field_list(s.clone(), table_slot, chunk, frag.new_fragment()).optional(s);
    let (s, _, status) = match_token(s, Token::CurlyR).map_parse(CurlyR)?;

    frag.commit();

    Ok((s, (), status))
}

#[derive(Debug, Error)]
pub enum TabFailure {
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
) -> Result<(Lexer<'s>, (), FieldListSuccessReason), Error<ParseFailure>> {
    let mut next_index = 1;
    let mut field = |s: Lexer<'s>| -> Result<_, Error<ParseFailure>> {
        let (s, field_type, status) = field(
            s.clone(),
            table_slot,
            next_index,
            chunk,
            frag.new_fragment(),
        )?;
        match field_type {
            FieldType::Index => next_index += 1,
            _ => (),
        }

        Ok((s, (), status))
    };

    let (mut s, (), _) = field(s)?;

    let mut next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, (), _), FieldListSuccessReason> {
        let (s, _, _) = field_sep(s)?;
        field(s).map_err(Into::into)
    };

    let err = loop {
        s = match next_part(s.clone()) {
            Ok((s, (), _)) => s,
            Err(err) => break err,
        }
    };

    let (s, _, status) = field_sep(s.clone()).optional(s);

    frag.commit();
    let status = err | status.into();

    Ok((s, (), status))
}

#[derive(Debug)]
enum FieldListSuccessReason {
    Complete(Complete),
    FieldSep(ParseError<FieldSepMismatchError>),
    Expr(Error<ParseFailure>),
}

impl BitOr for FieldListSuccessReason {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        use FieldListSuccessReason::*;

        match (self, rhs) {
            (r @ Expr(_), _) => r,
            (_, r @ Expr(_)) => r,
            (r @ FieldSep(_), _) => r,
            (_, r @ FieldSep(_)) => r,
            (r, _) => r,
        }
    }
}

impl From<Complete> for FieldListSuccessReason {
    fn from(value: Complete) -> Self {
        FieldListSuccessReason::Complete(value)
    }
}

impl From<ParseError<FieldSepMismatchError>> for FieldListSuccessReason {
    fn from(value: ParseError<FieldSepMismatchError>) -> Self {
        FieldListSuccessReason::FieldSep(value)
    }
}

impl From<Error<ParseFailure>> for FieldListSuccessReason {
    fn from(value: Error<ParseFailure>) -> Self {
        FieldListSuccessReason::Expr(value)
    }
}

impl From<Either<Complete, ParseError<FieldSepMismatchError>>> for FieldListSuccessReason {
    fn from(value: Either<Complete, ParseError<FieldSepMismatchError>>) -> Self {
        match value {
            Either::Left(value) => FieldListSuccessReason::Complete(value),
            Either::Right(value) => FieldListSuccessReason::FieldSep(value),
        }
    }
}

fn field<'s>(
    s: Lexer<'s>,
    table_slot: StackSlot,
    next_index: i64,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, FieldType, ExprSuccessReason), Error<ParseFailure>> {
    let inner = || {
        let mut err = match bracket(s.clone(), table_slot, chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, FieldType::Bracket, status)),
            Err(err) => err,
        };

        err |= match name(s.clone(), table_slot, chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, FieldType::Name, status)),
            Err(err) => err,
        };

        err |= match index(s, table_slot, next_index, chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, FieldType::Index, status)),
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

fn field_sep(mut s: Lexer) -> Result<(Lexer, Span, Complete), ParseError<FieldSepMismatchError>> {
    use crate::parser::NextTokenError;

    match s.next_token() {
        Ok(Token::Comma | Token::Semicolon) => {
            let span = s.span();
            Ok((s, span, Complete))
        }
        Ok(_) | Err(NextTokenError::Parse(Eof)) => Err(ParseError::Parse(FieldSepMismatchError)),
        Err(NextTokenError::Lex(lex)) => Err(lex.into()),
    }
}

#[derive(Debug, Error)]
#[error("encountered unexpected token, expected table field separator")]
pub struct FieldSepMismatchError;

fn bracket<'s>(
    s: Lexer<'s>,
    table_slot: StackSlot,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ExprSuccessReason), Error<ParseFailure>> {
    use crate::parser::expr::expr_adjusted_to_1;
    use TabBracketFailure::*;

    let (s, _, Complete) = match_token(s, Token::BracketL).map_parse(BracketL)?;

    frag.emit(OpCode::LoadStack(table_slot))?;

    let (s, (), _) =
        expr_adjusted_to_1(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;
    let (s, _, Complete) = match_token(s, Token::BracketR).map_parse(BracketR)?;
    let (s, _, Complete) = match_token(s, Token::EqualsSign).map_parse(EqualsSign)?;
    let (s, (), status) =
        expr_adjusted_to_1(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;

    frag.emit(OpCode::TabSet)?;
    frag.commit();

    Ok((s, (), status))
}

#[derive(Debug, Error)]
pub enum TabBracketFailure {
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
) -> Result<(Lexer<'s>, (), ExprSuccessReason), Error<ParseFailure>> {
    use crate::parser::expr::expr_adjusted_to_1;
    use TabNameFailure::*;

    let (s, (ident, _), Complete) = identifier(s).map_parse(Ident)?;

    frag.emit(OpCode::LoadStack(table_slot))?;

    let const_id = chunk.constants.insert(Literal::String(ident.to_string()))?;
    frag.emit(OpCode::LoadConstant(const_id))?;

    let (s, _, Complete) = match_token(s, Token::EqualsSign).map_parse(EqualsSign)?;
    let (s, (), status) = expr_adjusted_to_1(s, chunk, frag.new_fragment())
        .map_err(|err| err.with_mode(FailureMode::Malformed))?;

    frag.emit(OpCode::TabSet)?;
    frag.commit();

    Ok((s, (), status))
}

#[derive(Debug, Error)]
pub enum TabNameFailure {
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
) -> Result<(Lexer<'s>, (), ExprSuccessReason), Error<ParseFailure>> {
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
