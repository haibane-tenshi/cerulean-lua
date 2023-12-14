mod bin_op;
mod missing_args;
mod missing_const_id;
mod missing_recipe;
mod missing_upvalue_slot;
mod table;
mod una_op;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use repr::debug_info::opcode;
use repr::opcode::OpCode;
use std::ops::Range;

pub use bin_op::Cause as BinOpCause;
pub use missing_args::MissingArgsError;
pub use missing_const_id::MissingConstId;
pub use missing_recipe::MissingRecipe;
pub use missing_upvalue_slot::MissingUpvalue;
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
    MissingConstId(MissingConstId),
    MissingRecipe(MissingRecipe),
    MissingUpvalue(MissingUpvalue),
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
            MissingConstId(err) => err.into_diagnostic(file_id, opcode, debug_info),
            MissingRecipe(err) => err.into_diagnostic(file_id, opcode, debug_info),
            MissingUpvalue(err) => err.into_diagnostic(file_id, opcode, debug_info),
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

impl From<MissingUpvalue> for Cause {
    fn from(value: MissingUpvalue) -> Self {
        Cause::MissingUpvalue(value)
    }
}

impl From<MissingConstId> for Cause {
    fn from(value: MissingConstId) -> Self {
        Cause::MissingConstId(value)
    }
}

impl From<MissingRecipe> for Cause {
    fn from(value: MissingRecipe) -> Self {
        Cause::MissingRecipe(value)
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
            Invoke(t) => t.total_span(),
            Return(t) => t.total_span(),
            LoadStack(t) => t.total_span(),
            LoadUpvalue(t) => t.total_span(),
            LoadConst(t) => t.total_span(),
            MakeClosure(t) => t.total_span(),
            StoreStack(t) => t.total_span(),
            StoreUpvalue(t) => t.total_span(),
            UnaOp(t) => t.total_span(),
            BinOp(t) => t.total_span(),
            TabGet(t) => t.total_span(),
            TabSet(t) => t.total_span(),
        }
    }
}

impl TotalSpan for opcode::Invoke {
    fn total_span(&self) -> Range<usize> {
        use opcode::Invoke::*;

        match self {
            Call { callable, args } => callable.start..args.end,
            ForLoop { for_token: _, iter } => iter.clone(),
        }
    }
}

impl TotalSpan for opcode::Return {
    fn total_span(&self) -> Range<usize> {
        let opcode::Return { return_token } = self;

        return_token.clone()
    }
}

impl TotalSpan for opcode::LoadStack {
    fn total_span(&self) -> Range<usize> {
        use opcode::LoadStack::*;

        match self {
            Local(span) => span.clone(),
            Global(span) => span.clone(),
            Expr(span) => span.clone(),
        }
    }
}

impl TotalSpan for opcode::LoadUpvalue {
    fn total_span(&self) -> Range<usize> {
        use opcode::LoadUpvalue::*;

        match self {
            Upvalue(span) => span.clone(),
            Global(span) => span.clone(),
        }
    }
}

impl TotalSpan for opcode::LoadConst {
    fn total_span(&self) -> Range<usize> {
        let opcode::LoadConst { literal } = self;

        literal.clone()
    }
}

impl TotalSpan for opcode::MakeClosure {
    fn total_span(&self) -> Range<usize> {
        let opcode::MakeClosure {
            function_token: _,
            total_span,
        } = self;

        total_span.clone()
    }
}

impl TotalSpan for opcode::StoreStack {
    fn total_span(&self) -> Range<usize> {
        let opcode::StoreStack { ident, eq_sign: _ } = self;

        ident.clone()
    }
}

impl TotalSpan for opcode::StoreUpvalue {
    fn total_span(&self) -> Range<usize> {
        let opcode::StoreUpvalue { ident, eq_sign: _ } = self;

        ident.clone()
    }
}

impl TotalSpan for opcode::UnaOp {
    fn total_span(&self) -> Range<usize> {
        let opcode::UnaOp { op, arg } = self;

        op.start..arg.end
    }
}

impl TotalSpan for opcode::BinOp {
    fn total_span(&self) -> Range<usize> {
        let opcode::BinOp { lhs, op: _, rhs } = self;

        lhs.start..rhs.end
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
    fn with_compiler_bug_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
    fn with_runtime_bug_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
    fn no_debug_info(&mut self);
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

    fn with_compiler_bug_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>) {
        self.with_note(["this is not bug in your Lua code, error is caused by malformed bytecode"]);
        self.with_note(iter);
        self.with_note(["this most likely resulted from bug in compiler and should be reported"]);
    }

    fn with_runtime_bug_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>) {
        self.with_note([
            "this is not bug in your Lua code, error is caused by imporperly executed bytecode",
        ]);
        self.with_note(iter);
        self.with_note(["this most likely resulted from bug in runtime and should be reported"]);
    }

    fn no_debug_info(&mut self) {
        self.with_note([
                "no debug info is available, it is possible debug info was stripped",
                "it is also possible that erroneous bytecode was handcrafted\nplease check with where you got it",
            ]);
    }
}
