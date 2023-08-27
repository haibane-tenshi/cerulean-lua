//! Complete Complete
//!
//! type Success = Complete
//! type Failure = FailureA | FailureB
//!
//! Complete Branch
//!
//! type Success = SuccessB
//! type Failure = FailureA | FailureB
//!
//! Branch Complete
//!
//! type Success = Complete
//! type Branch = FailureA | (SuccessA -> FailureB)
//!
//! Branch Branch
//!
//! type Success = SuccessA -> SuccessB
//! type Failure = FailureA | (SuccessA -> FailureB)
//!
//!
//! (BranchA BranchB) BranchC
//!
//! type Success = (SuccessA -> SuccessB) -> SuccessC
//! type Failure = (FailureA | (SuccessA -> FailureB)) | ((SuccessA -> SuccessB) -> FailureC)

use std::ops::{BitOr, BitOrAssign};

use logos::Span;
use thiserror::Error;

use super::expr::expr::ExprFailure;
use super::expr::function::FunctionFailure;
use super::expr::table::TableFailure;
use super::expr::variadic::VariadicExprError;
use super::expr::ExprListError;
use super::func_def::FuncBodyFailure;
use super::prefix_expr::{
    FieldFailure, FnArgsFailure, FnCallFailure, IndexFailure, TabCallFailure, VariableFailure,
};
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

pub use std::convert::Infallible as Never;

// use prelude::*;

#[derive(Debug, Error)]
#[error("failed to tokenize input")]
pub enum LexError {
    Token(Span),
    Str(Span),
    Number(Span),
}

impl LexError {
    pub fn into_diagnostic(self) -> codespan_reporting::diagnostic::Diagnostic<()> {
        use codespan_reporting::diagnostic::{Diagnostic, Label};

        match self {
            LexError::Token(span) => {
                let labels = vec![Label::primary((), span)];

                Diagnostic::error()
                    .with_labels(labels)
                    .with_message("encountered unrecognized token")
            }
            LexError::Number(span) => {
                let labels = vec![Label::primary((), span)];

                Diagnostic::error()
                    .with_labels(labels)
                    .with_message("number is written in unrecognized format")
            }
            LexError::Str(span) => {
                let labels = vec![Label::primary((), span)];

                Diagnostic::error()
                    .with_labels(labels)
                    .with_message("string contains unknown escape sequence")
            }
        }
    }
}

#[derive(Debug, Error)]
#[error("codegen error")]
pub enum CodegenError {
    #[error("malformed variadic expression")]
    VariadicExpr(#[from] VariadicExprError),

    #[error("failed to resolve variable name")]
    VariableFailure(#[from] VariableFailure),
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
    T: Into<ParseCause>,
{
    fn from(value: T) -> Self {
        let mode = FailureMode::Mismatch;
        let cause = value.into();

        ParseFailure { mode, cause }
    }
}

#[derive(Debug, Error)]
pub(crate) enum ParseCause {
    #[error("expected expression")]
    ExpectedExpr(Span),
    #[error("failed to parse table constructor")]
    TableExpr(#[from] TableFailure),
    #[error("failed to parse function expression")]
    FunctionExpr(#[from] FunctionFailure),
    #[error("failed to parse function definition")]
    FunctionBodyExpr(#[from] FuncBodyFailure),
    #[error("failed to parse expression")]
    Expr(#[from] ExprFailure),
    #[error("failed to parse expression list")]
    ExprList(#[from] ExprListError),
    #[error("failed to access table field")]
    TabField(#[from] FieldFailure),
    #[error("failed to index into table")]
    TabIndex(#[from] IndexFailure),
    #[error("failed to call table method")]
    TabCall(#[from] TabCallFailure),
    #[error("expected function call")]
    FunctionCall(#[from] FnCallFailure),
    #[error("failed to parse function call arguments")]
    FunctionArgs(#[from] FnArgsFailure),
    #[error("expected statement")]
    ExpectedStmt(Span),
    #[error("failed to parse local assignment")]
    LocalAssignment(#[from] LocalAssignmentFailure),
    #[error("failed to parse local function declaration")]
    LocalFunction(#[from] LocalFunctionFailure),
    #[error("failed to parse variable assignment")]
    Assignment(#[from] AssignmentFailure),
    #[error("failed to parse if-then statement")]
    IfThen(#[from] IfThenFailure),
    #[error("failed to parse generic-for statement")]
    GenericFor(#[from] GenericForFailure),
    #[error("failed to parse numerical-for statement")]
    NumericalFor(#[from] NumericalForFailure),
    #[error("failed to parse repeat-until statement")]
    RepeatUntil(#[from] RepeatUntilFailure),
    #[error("failed to parse while-do statement")]
    WhileDo(#[from] WhileDoFailure),
    #[error("failed to parse do-end block")]
    DoEnd(#[from] DoEndFailure),
    #[error("failed to parse return statement")]
    Return(#[from] ReturnFailure),
}

impl ParseCause {
    pub(crate) fn into_diagnostic(self) -> codespan_reporting::diagnostic::Diagnostic<()> {
        use codespan_reporting::diagnostic::Label;
        type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

        match self {
            ParseCause::ExpectedExpr(span) => {
                let labels = vec![Label::primary((), span)];

                Diagnostic::error()
                    .with_labels(labels)
                    .with_message("expected expression")
            }
            ParseCause::TableExpr(err) => {
                use super::expr::table::{BracketFailure, NameFailure};
                use TableFailure::*;

                let (msg, span) = match err {
                    CurlyL(err) => ("expected opening curly brace `{`", err.span),
                    BracketSetter(BracketFailure::BracketL(err)) => (
                        "failed to parse table setter, expected opnenig square bracket `[`",
                        err.span,
                    ),
                    BracketSetter(BracketFailure::BracketR(err)) => (
                        "failed to parse table setter, expected closing square bracket `]`",
                        err.span,
                    ),
                    BracketSetter(BracketFailure::EqualsSign(err)) => (
                        "failed to parse table setter, expected equals sign `=`",
                        err.span,
                    ),
                    NameSetter(NameFailure::Ident(err)) => (
                        "failed to parse table setter, expected identifier",
                        err.span,
                    ),
                    NameSetter(NameFailure::EqualsSign(err)) => (
                        "failed to parse table setter, expected equals sign `=`",
                        err.span,
                    ),
                    Sep(_err) => unreachable!(),
                    CurlyR(err) => ("expected closing curly brace `}`", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::FunctionExpr(err) => {
                let (msg, span) = match err {
                    FunctionFailure::Function(err) => ("expected `function` keyword", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::FunctionBodyExpr(err) => {
                let (msg, span) = match err {
                    FuncBodyFailure::ParL(err) => ("missing openening parenthesis", err.span),
                    FuncBodyFailure::ParR(err) => ("missing closing parenthesis", err.span),
                    FuncBodyFailure::ArgListIdent(err) => {
                        ("function parameters should be identifiers", err.span)
                    }
                    FuncBodyFailure::ArgListComma(err) => ("expected comma", err.span),
                    FuncBodyFailure::ArgListVariadic(err) => {
                        ("expected variadic argument", err.span)
                    }
                    FuncBodyFailure::End(err) => ("expected `end` keyword", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::Expr(err) => {
                let (msg, span) = match err {
                    ExprFailure::Prefix(err) => ("expected prefix op", err.span),
                    ExprFailure::Infix(err) => ("expected infix op", err.span),
                    ExprFailure::ParL(err) => ("expected opening parenthesis", err.span),
                    ExprFailure::ParR(err) => ("expected closing parenthesis", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::ExprList(err) => {
                let (msg, span) = match err {
                    ExprListError::Comma(err) => ("expected comma", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::TabField(err) => {
                let (msg, span) = match err {
                    FieldFailure::Dot(err) => ("expected dot", err.span),
                    FieldFailure::Ident(err) => {
                        ("only valid identifier can be used for indexing", err.span)
                    }
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::TabIndex(err) => {
                let (msg, span) = match err {
                    IndexFailure::BracketL(err) => ("expected opnening bracket", err.span),
                    IndexFailure::BracketR(err) => ("expected closing bracket", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::TabCall(err) => {
                let (msg, span) = match err {
                    TabCallFailure::Colon(err) => ("expected colon", err.span),
                    TabCallFailure::Ident(err) => ("expected identifier", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::FunctionCall(err) => {
                let labels = vec![Label::primary((), err.span)];

                Diagnostic::error()
                    .with_labels(labels)
                    .with_message("expected function call")
            }
            ParseCause::FunctionArgs(err) => {
                let (msg, span) = match err {
                    FnArgsFailure::ParL(err) => ("expected opnening parenthesis", err.span),
                    FnArgsFailure::ParR(err) => ("expected closing parenthesis", err.span),
                    FnArgsFailure::String(err) => ("expected string parameter", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::ExpectedStmt(span) => {
                let labels = vec![Label::primary((), span)];

                Diagnostic::error()
                    .with_labels(labels)
                    .with_message("expected statement")
            }
            ParseCause::LocalAssignment(err) => {
                let (msg, span) = match err {
                    LocalAssignmentFailure::Local(err) => ("expected `local` keyword", err.span),
                    LocalAssignmentFailure::Ident(err) => ("expceted identifier", err.span),
                    LocalAssignmentFailure::EqualsSign(err) => ("expected equals sign", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::LocalFunction(err) => {
                let (msg, span) = match err {
                    LocalFunctionFailure::Local(err) => ("expected `local` keyword", err.span),
                    LocalFunctionFailure::Function(err) => {
                        ("expected `function` keyword", err.span)
                    }
                    LocalFunctionFailure::Ident(err) => ("expected identifier", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::Assignment(err) => {
                let (msg, span) = match err {
                    AssignmentFailure::Place(err) => ("expected place", err.span),
                    AssignmentFailure::Comma(err) => ("expected comma", err.span),
                    AssignmentFailure::EqualsSign(err) => ("expected equals sign", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::IfThen(err) => {
                let (msg, span) = match err {
                    IfThenFailure::If(err) => ("expcted `if` keyword", err.span),
                    IfThenFailure::Then(err) => ("expected `then` keyword", err.span),
                    IfThenFailure::ElseIf(err) => ("expceted `elseif` keyword", err.span),
                    IfThenFailure::Else(err) => ("expected `else` keyword", err.span),
                    IfThenFailure::End(err) => ("expcted `end` keyword", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::GenericFor(err) => {
                let (msg, span) = match err {
                    GenericForFailure::For(err) => ("expected `for` keyword", err.span),
                    GenericForFailure::Ident(err) => ("expected identifier", err.span),
                    GenericForFailure::In(err) => ("expected `in` keyword", err.span),
                    GenericForFailure::Do(err) => ("expected `do` keyword", err.span),
                    GenericForFailure::End(err) => ("expected `end` keyword", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::NumericalFor(err) => {
                let (msg, span) = match err {
                    NumericalForFailure::For(err) => ("expected `for` keyword", err.span),
                    NumericalForFailure::Ident(err) => ("expected identifier", err.span),
                    NumericalForFailure::EqualsSign(err) => ("expected equals sign", err.span),
                    NumericalForFailure::Comma(err) => ("expected comma", err.span),
                    NumericalForFailure::Do(err) => ("expected `do` keyword", err.span),
                    NumericalForFailure::End(err) => ("expected `end` keyword", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::RepeatUntil(err) => {
                let (msg, span) = match err {
                    RepeatUntilFailure::Repeat(err) => ("expected `repeat` keyword", err.span),
                    RepeatUntilFailure::Until(err) => ("expected `until` keyword", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::WhileDo(err) => {
                let (msg, span) = match err {
                    WhileDoFailure::While(err) => ("expected `while` keyword", err.span),
                    WhileDoFailure::Do(err) => ("expected `do` keyword", err.span),
                    WhileDoFailure::End(err) => ("expected `end` keyword", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::DoEnd(err) => {
                let (msg, span) = match err {
                    DoEndFailure::Do(err) => ("expected `do` keyword", err.span),
                    DoEndFailure::End(err) => ("expected `end` keyword", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
            ParseCause::Return(err) => {
                let (msg, span) = match err {
                    ReturnFailure::Return(err) => ("expected `return` keyword", err.span),
                };

                let labels = vec![Label::primary((), span)];

                Diagnostic::error().with_message(msg).with_labels(labels)
            }
        }
    }
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

pub trait WithMode {
    fn with_mode(self, mode: FailureMode) -> Self;
}

impl WithMode for Never {
    fn with_mode(self, _: FailureMode) -> Self {
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

pub(crate) type ParseFailureOrComplete = CompleteOr<ParseFailure>;

#[derive(Debug)]
pub(crate) enum CompleteOr<T> {
    Complete(Complete),
    Other(T),
}

impl<T> CompleteOr<T> {
    pub(crate) fn map<U>(self, f: impl FnOnce(T) -> U) -> CompleteOr<U> {
        match self {
            CompleteOr::Complete(t) => CompleteOr::Complete(t),
            CompleteOr::Other(t) => CompleteOr::Other(f(t)),
        }
    }
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

pub trait Arrow<Other> {
    type Output;

    fn arrow(self, other: Other) -> Self::Output;
}

impl<T> Arrow<Complete> for T {
    type Output = Complete;

    fn arrow(self, other: Complete) -> Self::Output {
        other
    }
}

impl<T> Arrow<Never> for T {
    type Output = Never;

    fn arrow(self, other: Never) -> Self::Output {
        other
    }
}

impl Arrow<ParseFailure> for Complete {
    type Output = ParseFailure;

    fn arrow(self, other: ParseFailure) -> Self::Output {
        other
    }
}

impl<T> Arrow<CompleteOr<T>> for Complete {
    type Output = CompleteOr<T>;

    fn arrow(self, other: CompleteOr<T>) -> Self::Output {
        other
    }
}

impl Arrow<ParseFailure> for ParseFailure {
    type Output = Self;

    fn arrow(self, other: ParseFailure) -> Self::Output {
        self | other
    }
}

impl Arrow<CompleteOr<ParseFailure>> for ParseFailure {
    type Output = CompleteOr<ParseFailure>;

    fn arrow(self, other: CompleteOr<ParseFailure>) -> Self::Output {
        match other {
            CompleteOr::Complete(t) => CompleteOr::Complete(t),
            CompleteOr::Other(rhs) => CompleteOr::Other(self.arrow(rhs)),
        }
    }
}

impl Arrow<ParseFailure> for CompleteOr<ParseFailure> {
    type Output = ParseFailure;

    fn arrow(self, other: ParseFailure) -> Self::Output {
        match self {
            CompleteOr::Complete(_) => other,
            CompleteOr::Other(this) => this.arrow(other),
        }
    }
}

impl Arrow<CompleteOr<ParseFailure>> for CompleteOr<ParseFailure> {
    type Output = Self;

    fn arrow(self, other: CompleteOr<ParseFailure>) -> Self::Output {
        match (self, other) {
            (CompleteOr::Other(lhs), CompleteOr::Other(rhs)) => CompleteOr::Other(lhs.arrow(rhs)),
            (CompleteOr::Other(r), _) | (_, CompleteOr::Other(r)) => CompleteOr::Other(r),
            (CompleteOr::Complete(_), CompleteOr::Complete(_)) => CompleteOr::Complete(Complete),
        }
    }
}
