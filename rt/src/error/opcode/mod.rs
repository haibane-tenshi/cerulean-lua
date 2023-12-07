mod bin_op;
mod missing_args;
mod table;
mod una_op;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use repr::debug_info::opcode;
use repr::opcode::OpCode;
use std::ops::Range;

pub use bin_op::Cause as BinOpCause;
pub use missing_args::MissingArgsError;
pub use table::RuntimeCause as TabCause;
pub use una_op::Cause as UnaOpCause;

#[derive(Debug)]
pub struct Error {
    pub opcode: OpCode,
    pub debug_info: Option<opcode::DebugInfo>,
    pub cause: Cause,
}

#[derive(Debug)]
pub enum Cause {
    CatchAll,
    MissingArgs(MissingArgsError),
    UnaOp(UnaOpCause),
    BinOp(BinOpCause),
    TabGet(TabCause),
    TabSet(TabCause),
}

impl Error {
    pub fn into_diagnostic<FileId>(self, file_id: FileId) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use Cause::*;

        let Error {
            opcode,
            debug_info,
            cause,
        } = self;

        let malformed_diagnostic = || Diagnostic::error().with_message("malformed diagnostic");

        match cause {
            CatchAll => Diagnostic::error().with_message("diagnostic not implemented yet"),
            MissingArgs(err) => {
                let debug_info = debug_info.as_ref().map(TotalSpan::total_span);
                err.into_diagnostic(file_id, opcode, debug_info)
            }
            UnaOp(cause) => {
                if let OpCode::UnaOp(op) = opcode {
                    let debug_info = debug_info.and_then(Extract::extract);
                    cause.into_diagnostic(file_id, debug_info, op)
                } else {
                    malformed_diagnostic()
                }
            }
            BinOp(cause) => {
                if let OpCode::BinOp(op) = opcode {
                    let debug_info = debug_info.and_then(Extract::extract);
                    cause.into_diagnostic(file_id, debug_info, op)
                } else {
                    malformed_diagnostic()
                }
            }
            TabGet(cause) => {
                let debug_info = debug_info
                    .and_then(Extract::<opcode::TabGet>::extract)
                    .map(Into::into);
                cause.into_diagnostic(file_id, debug_info)
            }
            TabSet(cause) => {
                let debug_info = debug_info
                    .and_then(Extract::<opcode::TabSet>::extract)
                    .map(Into::into);
                cause.into_diagnostic(file_id, debug_info)
            }
        }
    }
}

impl From<MissingArgsError> for Cause {
    fn from(value: MissingArgsError) -> Self {
        Cause::MissingArgs(value)
    }
}

impl From<BinOpCause> for Cause {
    fn from(value: BinOpCause) -> Self {
        Cause::BinOp(value)
    }
}

impl From<UnaOpCause> for Cause {
    fn from(value: UnaOpCause) -> Self {
        Cause::UnaOp(value)
    }
}

trait TotalSpan {
    fn total_span(&self) -> Range<usize>;
}

impl TotalSpan for opcode::DebugInfo {
    fn total_span(&self) -> Range<usize> {
        use opcode::DebugInfo::*;

        match self {
            Generic(span) => span.clone(),
            LoadConst(t) => t.total_span(),
            UnaOp(t) => t.total_span(),
            BinOp(t) => t.total_span(),
            TabGet(t) => t.total_span(),
            TabSet(t) => t.total_span(),
        }
    }
}

impl TotalSpan for opcode::LoadConst {
    fn total_span(&self) -> Range<usize> {
        self.literal.clone()
    }
}

impl TotalSpan for opcode::UnaOp {
    fn total_span(&self) -> Range<usize> {
        self.op.start..self.arg.end
    }
}

impl TotalSpan for opcode::BinOp {
    fn total_span(&self) -> Range<usize> {
        self.lhs.start..self.rhs.end
    }
}

impl TotalSpan for opcode::TabGet {
    fn total_span(&self) -> Range<usize> {
        use opcode::TabGet::*;

        match self {
            GlobalEnv { ident } => ident.clone(),
            Local {
                table,
                index: _,
                indexing,
            } => table.start..indexing.end,
        }
    }
}

impl TotalSpan for opcode::TabSet {
    fn total_span(&self) -> Range<usize> {
        use opcode::TabSet::*;

        match self {
            Local {
                table,
                index: _,
                indexing: _,
                eq_sign,
            } => table.start..eq_sign.end,
            GlobalEnv { ident, eq_sign } => {
                let end = eq_sign.as_ref().map(|span| span.end).unwrap_or(ident.end);

                ident.start..end
            }
            Constructor { table: _, flavor } => flavor.total_span(),
        }
    }
}

impl TotalSpan for opcode::TabConstructor {
    fn total_span(&self) -> Range<usize> {
        use opcode::TabConstructor::*;

        match self {
            Index {
                index: _,
                indexing,
                eq_sign,
            } => indexing.start..eq_sign.end,
            Field { ident, eq_sign } => ident.start..eq_sign.end,
            Value { value } => value.clone(),
        }
    }
}

pub(crate) trait Extract<T> {
    fn extract(self) -> Option<T>;
}

impl Extract<opcode::UnaOp> for opcode::DebugInfo {
    fn extract(self) -> Option<opcode::UnaOp> {
        match self {
            opcode::DebugInfo::UnaOp(t) => Some(t),
            _ => None,
        }
    }
}

impl Extract<opcode::BinOp> for opcode::DebugInfo {
    fn extract(self) -> Option<opcode::BinOp> {
        match self {
            opcode::DebugInfo::BinOp(t) => Some(t),
            _ => None,
        }
    }
}

impl Extract<opcode::TabGet> for opcode::DebugInfo {
    fn extract(self) -> Option<opcode::TabGet> {
        match self {
            opcode::DebugInfo::TabGet(t) => Some(t),
            _ => None,
        }
    }
}

impl Extract<opcode::TabSet> for opcode::DebugInfo {
    fn extract(self) -> Option<opcode::TabSet> {
        match self {
            opcode::DebugInfo::TabSet(t) => Some(t),
            _ => None,
        }
    }
}

trait ExtraDiagnostic<FileId> {
    fn with_label(&mut self, iter: impl IntoIterator<Item = Label<FileId>>);
    fn with_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
    fn with_help(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
}

impl<FileId> ExtraDiagnostic<FileId> for Diagnostic<FileId> {
    fn with_label(&mut self, iter: impl IntoIterator<Item = Label<FileId>>) {
        self.labels.extend(iter)
    }

    fn with_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>) {
        self.notes
            .extend(iter.into_iter().map(|s| format!("note: {}", s.as_ref())));
    }

    fn with_help(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>) {
        self.notes
            .extend(iter.into_iter().map(|s| format!("help: {}", s.as_ref())));
    }
}

fn compiler_bug<'a>(iter: impl IntoIterator<Item = &'a str>) -> impl Iterator<Item = &'a str> {
    std::iter::once("this is not bug in your Lua code, error is caused by malformed bytecode")
        .chain(iter.into_iter().map(AsRef::as_ref))
        .chain(std::iter::once(
            "this most likely resulted from bug in compiler and should be reported",
        ))
}
