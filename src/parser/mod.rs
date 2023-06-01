mod basic;
mod block;
mod expr;
mod func_def;
mod prefix_expr;
mod prelude;
mod stmt;

use either::Either;
use std::ops::{BitOr, BitOrAssign};
use thiserror::Error;

use crate::lex::{Error as LexError, Lexer, Token};
use crate::opcode::{Chunk, ConstCapacityError, FunctionCapacityError, InstrCountError};
use crate::tracker2::fragment::EmitError;
use crate::tracker2::stack::{
    BoundaryViolationError, GiveNameError, PopError, PushError, StackOverflowError,
    VariadicStackError,
};
use expr::function::FunctionFailure;
use expr::table::{TabBracketFailure, TabFailure, TabNameFailure};
use expr::ParExprFailure;
use func_def::FuncDefFailure;
use prefix_expr::{ArgsParExprFailure, FieldFailure, IndexFailure, VariableFailure};
use stmt::assignment::AssignmentFailure;
use stmt::do_end::DoEndFailure;
use stmt::generic_for::GenericForFailure;
use stmt::if_then::IfThenFailure;
use stmt::local_assignment::LocalAssignmentFailure;
use stmt::local_function::LocalFunctionFailure;
use stmt::numerical_for::NumericalForFailure;
use stmt::repeat_until::RepeatUntilFailure;
use stmt::while_do::WhileDoFailure;

use prelude::*;

#[derive(Debug, Error)]
pub enum ParseError<P> {
    #[error(transparent)]
    Lex(LexError),

    #[error(transparent)]
    Parse(P),
}

impl<T, P> From<T> for ParseError<P>
where
    T: Into<LexError>,
{
    fn from(value: T) -> Self {
        ParseError::Lex(value.into())
    }
}

#[derive(Debug, Error)]
#[error("codegen error")]
pub struct CodegenError;

#[derive(Debug, Error)]
pub enum Error<P> {
    #[error(transparent)]
    Lex(#[from] LexError),

    #[error(transparent)]
    Parse(P),

    #[error("failed to generate code")]
    Codegen(#[from] CodegenError),
}

impl<P> Error<P> {
    pub fn map_parse<F, U>(self, f: F) -> Error<U>
    where
        F: FnOnce(P) -> U,
    {
        match self {
            Error::Lex(t) => Error::Lex(t),
            Error::Codegen(t) => Error::Codegen(t),
            Error::Parse(t) => Error::Parse(f(t)),
        }
    }
}

impl Error<ParseFailure> {
    pub fn with_mode(self, mode: FailureMode) -> Self {
        match self {
            Error::Parse(t) => Error::Parse(t.with_mode(mode)),
            t => t,
        }
    }
}

impl<T, P> From<ParseError<T>> for Error<P>
where
    T: Into<P>,
{
    fn from(value: ParseError<T>) -> Self {
        match value {
            ParseError::Lex(err) => Error::Lex(err),
            ParseError::Parse(err) => Error::Parse(err.into()),
        }
    }
}

impl<P> From<VariadicStackError> for Error<P> {
    fn from(_: VariadicStackError) -> Self {
        Error::Codegen(CodegenError)
    }
}

impl<P> From<StackOverflowError> for Error<P> {
    fn from(_: StackOverflowError) -> Self {
        Error::Codegen(CodegenError)
    }
}

impl<P> From<PushError> for Error<P> {
    fn from(_: PushError) -> Self {
        Error::Codegen(CodegenError)
    }
}

impl<P> From<PopError> for Error<P> {
    fn from(_: PopError) -> Self {
        Error::Codegen(CodegenError)
    }
}

impl<P> From<BoundaryViolationError> for Error<P> {
    fn from(_: BoundaryViolationError) -> Self {
        Error::Codegen(CodegenError)
    }
}

impl<P> From<GiveNameError> for Error<P> {
    fn from(_: GiveNameError) -> Self {
        Error::Codegen(CodegenError)
    }
}

impl<P> From<ConstCapacityError> for Error<P> {
    fn from(_: ConstCapacityError) -> Self {
        Error::Codegen(CodegenError)
    }
}

impl<P> From<EmitError> for Error<P> {
    fn from(_: EmitError) -> Self {
        Error::Codegen(CodegenError)
    }
}

impl<P> From<InstrCountError> for Error<P> {
    fn from(_: InstrCountError) -> Self {
        Error::Codegen(CodegenError)
    }
}

impl<P> From<FunctionCapacityError> for Error<P> {
    fn from(_: FunctionCapacityError) -> Self {
        Error::Codegen(CodegenError)
    }
}

impl<P> From<P> for Error<ParseFailure>
where
    P: Into<ParseFailure>,
{
    fn from(value: P) -> Self {
        Error::Parse(value.into())
    }
}

impl<P> BitOrAssign for Error<P>
where
    P: BitOrAssign,
{
    fn bitor_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (Error::Lex(_), _) => (),
            (t, value @ Error::Lex(_)) => *t = value,
            (Error::Codegen(_), _) => (),
            (t, value @ Error::Codegen(_)) => *t = value,
            (Error::Parse(t), Error::Parse(value)) => *t |= value,
        }
    }
}

impl<P> BitOr for Error<P>
where
    Self: BitOrAssign,
{
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        self |= rhs;
        self
    }
}

#[derive(Debug, Error)]
#[error("failed to parse input")]
pub struct ParseFailure {
    mode: FailureMode,
    #[source]
    cause: ParseCause,
}

impl ParseFailure {
    pub fn with_mode(self, mode: FailureMode) -> Self {
        ParseFailure { mode, ..self }
    }
}

impl BitOrAssign for ParseFailure {
    fn bitor_assign(&mut self, rhs: Self) {
        if self.mode < rhs.mode {
            *self = rhs
        }
    }
}

impl BitOr for ParseFailure {
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        self |= rhs;
        self
    }
}

impl<T> From<T> for ParseFailure
where
    T: HaveFailureMode + Into<ParseCause>,
{
    fn from(value: T) -> Self {
        let mode = value.mode();
        let cause = value.into();

        ParseFailure { mode, cause }
    }
}

#[derive(Debug, Error)]
enum ParseCause {
    #[error("failed to parse named table setter")]
    TabName(#[from] TabNameFailure),
    #[error("failed to parse bracketed table setter")]
    TabBracket(#[from] TabBracketFailure),
    #[error("failed to parse table constructor")]
    Tab(#[from] TabFailure),
    #[error("expected place")]
    ExpectedPlace,
    #[error("expected expression")]
    ExpectedExpr,
    #[error("failed to parse parenthesised expression")]
    ParExpr(#[from] ParExprFailure),
    #[error("failed to parse function expression")]
    FunctionExpr(#[from] FunctionFailure),
    #[error("failed to parse function definition")]
    FunctionDef(#[from] FuncDefFailure),
    #[error("failed to parse variable")]
    Variable(#[from] VariableFailure),
    #[error("failed to access table field")]
    TabField(#[from] FieldFailure),
    #[error("failed to index into table")]
    TabIndex(#[from] IndexFailure),
    #[error("expected function call")]
    FunctionCall,
    #[error("failed to parse function call arguments")]
    FunctionArgs(#[from] ArgsParExprFailure),
    #[error("failed to parse variable assignment")]
    Assignment(#[from] AssignmentFailure),
    #[error("failed to parse generic-for statement")]
    GenericFor(#[from] GenericForFailure),
    #[error("failed to parse if-then statement")]
    IfThen(#[from] IfThenFailure),
    #[error("failed to parse local assignment")]
    LocalAssignment(#[from] LocalAssignmentFailure),
    #[error("failed to parse local function declaration")]
    LocalFunction(#[from] LocalFunctionFailure),
    #[error("failed to parse numerical-for statement")]
    NumericalFor(#[from] NumericalForFailure),
    #[error("failed to parse repeat-until statement")]
    RepeatUntil(#[from] RepeatUntilFailure),
    #[error("failed to parse while-do statement")]
    WhileDo(#[from] WhileDoFailure),
    #[error("expected statement")]
    ExpectedStatement,
    #[error("failed to parse do-end block")]
    DoEnd(#[from] DoEndFailure),
}

trait MapParse<F> {
    type Output;

    fn map_parse(self, f: F) -> Self::Output;
}

impl<T, U, E, F> MapParse<F> for Result<T, ParseError<E>>
where
    F: FnOnce(E) -> U,
{
    type Output = Result<T, ParseError<U>>;

    fn map_parse(self, f: F) -> Self::Output {
        match self {
            Ok(t) => Ok(t),
            Err(ParseError::Parse(err)) => Err(ParseError::Parse(f(err))),
            Err(ParseError::Lex(err)) => Err(ParseError::Lex(err)),
        }
    }
}

impl<T, U, E, F> MapParse<F> for Result<T, Error<E>>
where
    F: FnOnce(E) -> U,
{
    type Output = Result<T, Error<U>>;

    fn map_parse(self, f: F) -> Self::Output {
        self.map_err(|err| err.map_parse(f))
    }
}

trait WithMode {
    fn with_mode(self, mode: FailureMode) -> Self;
}

impl<T> WithMode for Result<T, Error<ParseFailure>> {
    fn with_mode(self, mode: FailureMode) -> Self {
        self.map_err(|err| err.with_mode(mode))
    }
}

trait NextToken {
    type Token;

    fn next_token(&mut self) -> Result<Self::Token, NextTokenError>;
}

impl<'s> NextToken for Lexer<'s> {
    type Token = Token<'s>;

    fn next_token(&mut self) -> Result<Self::Token, NextTokenError> {
        let r = self.next().ok_or(NextTokenError::Parse(Eof))??;

        Ok(r)
    }
}

#[derive(Debug, Error)]
#[error("reached end of input")]
struct Eof;

type NextTokenError = ParseError<Eof>;

#[derive(Debug)]
pub(crate) struct Complete;

impl HaveFailureMode for Complete {
    fn mode(&self) -> FailureMode {
        FailureMode::Success
    }
}

trait Optional {
    type Source;
    type Value;
    type Success;
    type Failure;

    fn optional(
        self,
        source: Self::Source,
    ) -> (
        Self::Source,
        Option<Self::Value>,
        Either<Self::Success, Self::Failure>,
    );
}

impl<'s, T, Success, Failure> Optional for Result<(Lexer<'s>, T, Success), Failure> {
    type Source = Lexer<'s>;
    type Value = T;
    type Success = Success;
    type Failure = Failure;

    fn optional(self, source: Self::Source) -> (Self::Source, Option<T>, Either<Success, Failure>) {
        match self {
            Ok((s, t, success)) => (s, Some(t), Either::Left(success)),
            Err(failure) => (source, None, Either::Right(failure)),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum FailureMode {
    Success,
    Mismatch,
    Ambiguous,
    Malformed,
    Codegen,
}

pub trait HaveFailureMode {
    fn mode(&self) -> FailureMode;
}

pub fn chunk(s: Lexer) -> Result<Chunk, Error<ParseFailure>> {
    use crate::parser::block::block;
    use crate::tracker2::chunk::Chunk;
    use crate::tracker2::function::Function;
    use crate::tracker2::stack::{Stack, StackView};

    let mut chunk = Chunk::with_script();
    let mut script = Function::new();
    let mut stack = Stack::new();

    let fragment = Fragment::new(&mut script, StackView::new(&mut stack));
    let _ = block(s, &mut chunk, fragment)?;

    // Put script into where runtime expects it to find.
    let mut script = script.resolve(0);
    std::mem::swap(chunk.functions.first_mut().unwrap(), &mut script);
    let chunk = chunk.resolve();

    Ok(chunk)
}
