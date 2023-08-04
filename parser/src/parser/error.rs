use std::ops::{BitOr, BitOrAssign};

use repr::index::{ConstCapacityError, FunctionCapacityError, InstrCountError};
use thiserror::Error;

use super::expr::expr::ExprFailure;
use super::expr::function::FunctionFailure;
use super::expr::table::{TabBracketFailure, TabFailure, TabNameFailure};
use super::expr::{ExprListError, ParExprFailure};
use super::func_def::FuncDefFailure;
use super::prefix_expr::{FieldFailure, FnArgsParExprFailure, IndexFailure, VariableFailure};
use super::stmt::assignment::AssignmentFailure;
use super::stmt::do_end::DoEndFailure;
use super::stmt::generic_for::GenericForFailure;
use super::stmt::if_then::IfThenFailure;
use super::stmt::local_assignment::LocalAssignmentFailure;
use super::stmt::local_function::LocalFunctionFailure;
use super::stmt::numerical_for::NumericalForFailure;
use super::stmt::repeat_until::RepeatUntilFailure;
use super::stmt::return_::ReturnFailure;
use super::stmt::while_do::WhileDoFailure;
use crate::codegen::fragment::{EmitError, EmitLoadLiteralError};
use crate::codegen::stack::{
    BoundaryViolationError, GiveNameError, PopError, PushError, StackOverflowError,
    VariadicStackError,
};

pub use crate::lex::Error as LexError;
pub use std::convert::Infallible as Never;

// use prelude::*;

#[derive(Debug, Error)]
#[error("codegen error")]
pub struct CodegenError;

impl From<VariadicStackError> for CodegenError {
    fn from(_: VariadicStackError) -> Self {
        CodegenError
    }
}

impl From<StackOverflowError> for CodegenError {
    fn from(_: StackOverflowError) -> Self {
        CodegenError
    }
}

impl From<PushError> for CodegenError {
    fn from(_: PushError) -> Self {
        CodegenError
    }
}

impl From<PopError> for CodegenError {
    fn from(_: PopError) -> Self {
        CodegenError
    }
}

impl From<BoundaryViolationError> for CodegenError {
    fn from(_: BoundaryViolationError) -> Self {
        CodegenError
    }
}

impl From<GiveNameError> for CodegenError {
    fn from(_: GiveNameError) -> Self {
        CodegenError
    }
}

impl From<ConstCapacityError> for CodegenError {
    fn from(_: ConstCapacityError) -> Self {
        CodegenError
    }
}

impl From<EmitError> for CodegenError {
    fn from(_: EmitError) -> Self {
        CodegenError
    }
}

impl From<EmitLoadLiteralError> for CodegenError {
    fn from(_: EmitLoadLiteralError) -> Self {
        CodegenError
    }
}

impl From<InstrCountError> for CodegenError {
    fn from(_: InstrCountError) -> Self {
        CodegenError
    }
}

impl From<FunctionCapacityError> for CodegenError {
    fn from(_: FunctionCapacityError) -> Self {
        CodegenError
    }
}

#[derive(Debug, Error)]
#[error("failed to parse input")]
pub struct ParseFailure {
    pub(crate) mode: FailureMode,
    #[source]
    pub(crate) cause: ParseCause,
}

impl ParseFailure {
    pub fn with_mode(self, mode: FailureMode) -> Self {
        ParseFailure { mode, ..self }
    }
}

impl BitOrAssign for ParseFailure {
    fn bitor_assign(&mut self, rhs: Self) {
        if self.mode <= rhs.mode {
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
pub(crate) enum ParseCause {
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
    FunctionArgs(#[from] FnArgsParExprFailure),
    #[error("failed to parse expression")]
    Expr(#[from] ExprFailure),
    #[error("failed to parse expression list")]
    ExprList(#[from] ExprListError),
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
    #[error("failed to parse return statement")]
    Return(#[from] ReturnFailure),
}

impl From<Never> for ParseCause {
    fn from(value: Never) -> Self {
        match value {}
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

impl HaveFailureMode for Never {
    fn mode(&self) -> FailureMode {
        match *self {}
    }
}

pub trait WithMode {
    fn with_mode(self, mode: FailureMode) -> Self;
}

impl WithMode for Never {
    fn with_mode(self, mode: FailureMode) -> Self {
        match self {}
    }
}

impl WithMode for ParseFailure {
    fn with_mode(self, mode: FailureMode) -> Self {
        ParseFailure { mode, ..self }
    }
}

#[derive(Debug, Error)]
pub enum FailFast {
    #[error(transparent)]
    Lex(#[from] LexError),
    #[error(transparent)]
    Codegen(CodegenError),
}

impl<T> From<T> for FailFast
where
    T: Into<CodegenError>,
{
    fn from(value: T) -> Self {
        FailFast::Codegen(value.into())
    }
}

#[derive(Debug)]
pub(crate) struct Complete;

impl HaveFailureMode for Complete {
    fn mode(&self) -> FailureMode {
        FailureMode::Success
    }
}

pub(crate) type ParseFailureOrComplete = CompleteOr<ParseFailure>;

pub(crate) enum CompleteOr<T> {
    Complete(Complete),
    Other(T),
}

impl<T> From<Complete> for CompleteOr<T> {
    fn from(value: Complete) -> Self {
        CompleteOr::Complete(value)
    }
}

impl From<ParseFailure> for ParseFailureOrComplete {
    fn from(value: ParseFailure) -> Self {
        ParseFailureOrComplete::Other(value)
    }
}

pub trait Combine<Other> {
    type Output;

    fn combine(self, other: Other) -> Self::Output;
}

impl<T> Combine<T> for Never {
    type Output = Never;

    fn combine(self, _: T) -> Self::Output {
        self
    }
}

impl Combine<Never> for ParseFailure {
    type Output = Never;

    fn combine(self, other: Never) -> Self::Output {
        other
    }
}

impl Combine<ParseFailure> for ParseFailure {
    type Output = Self;

    fn combine(self, other: ParseFailure) -> Self::Output {
        self | other
    }
}

impl Combine<Complete> for ParseFailure {
    type Output = Self;

    fn combine(self, _: Complete) -> Self::Output {
        self
    }
}

impl Combine<ParseFailureOrComplete> for ParseFailure {
    type Output = Self;

    fn combine(self, other: ParseFailureOrComplete) -> Self::Output {
        match other {
            ParseFailureOrComplete::Complete(_) => self,
            ParseFailureOrComplete::Other(this) => self.combine(this),
        }
    }
}

impl<T> Combine<T> for Complete {
    type Output = T;

    fn combine(self, other: T) -> Self::Output {
        other
    }
}

impl<T> Combine<Never> for CompleteOr<T> {
    type Output = Never;

    fn combine(self, other: Never) -> Self::Output {
        other
    }
}

impl Combine<ParseFailure> for ParseFailureOrComplete {
    type Output = ParseFailure;

    fn combine(self, other: ParseFailure) -> Self::Output {
        other.combine(self)
    }
}

impl Combine<Complete> for ParseFailureOrComplete {
    type Output = Self;

    fn combine(self, _: Complete) -> Self::Output {
        self
    }
}

impl Combine<ParseFailureOrComplete> for ParseFailureOrComplete {
    type Output = Self;

    fn combine(self, other: ParseFailureOrComplete) -> Self::Output {
        use CompleteOr::{Complete as EComplete, Other};

        match (self, other) {
            (Other(lhs), Other(rhs)) => Other(lhs.combine(rhs)),
            (Other(failure), _) | (_, Other(failure)) => Other(failure),
            (EComplete(_), EComplete(_)) => EComplete(Complete),
        }
    }
}
