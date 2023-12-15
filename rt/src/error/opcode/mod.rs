mod bin_op;
mod invoke;
mod ip_out_of_bounds;
mod missing_args;
mod missing_const_id;
mod missing_recipe;
mod missing_stack_slot;
mod missing_upvalue_slot;
mod panic;
mod table;
mod una_op;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use repr::debug_info::opcode;
use repr::opcode::OpCode;
use std::ops::Range;

pub use bin_op::Cause as BinOpCause;
pub use invoke::Invoke;
pub use ip_out_of_bounds::IpOutOfBounds;
pub use missing_args::MissingArgsError;
pub use missing_const_id::MissingConstId;
pub use missing_recipe::MissingRecipe;
pub use missing_stack_slot::MissingStackSlot;
pub use missing_upvalue_slot::MissingUpvalue;
pub use panic::Panic;
pub use table::RuntimeCause as TabCause;
pub use una_op::Cause as UnaOpCause;

#[derive(Debug, Clone)]
pub struct Error {
    pub opcode: OpCode,
    pub debug_info: Option<opcode::DebugInfo>,
    pub cause: Cause,
}

#[derive(Debug, Clone, Copy)]
pub enum Cause {
    Panic(Panic),
    IpOutOfBounds(IpOutOfBounds),
    MissingArgs(MissingArgsError),
    MissingConstId(MissingConstId),
    MissingRecipe(MissingRecipe),
    MissingStackSlot(MissingStackSlot),
    MissingUpvalue(MissingUpvalue),
    Invoke(Invoke),
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
        use table::TabDebugInfo;
        use Cause::*;

        let Error {
            opcode,
            debug_info,
            cause,
        } = self;

        let malformed_diagnostic = || Diagnostic::error().with_message("malformed diagnostic");

        match cause {
            Panic(err) => err.into_diagnostic(file_id, opcode, debug_info),
            IpOutOfBounds(err) => err.into_diagnostic(file_id, opcode, debug_info),
            MissingArgs(err) => {
                let debug_info = debug_info.as_ref().map(TotalSpan::total_span);
                err.into_diagnostic(file_id, opcode, debug_info)
            }
            MissingConstId(err) => err.into_diagnostic(file_id, opcode, debug_info),
            MissingRecipe(err) => err.into_diagnostic(file_id, opcode, debug_info),
            MissingStackSlot(err) => err.into_diagnostic(file_id, opcode, debug_info),
            MissingUpvalue(err) => err.into_diagnostic(file_id, opcode, debug_info),
            Invoke(err) => err.into_diagnostic(file_id, opcode, debug_info),
            UnaOp(cause) => {
                if let OpCode::UnaOp(op) = opcode {
                    cause.into_diagnostic(file_id, debug_info, op)
                } else {
                    malformed_diagnostic()
                }
            }
            BinOp(cause) => {
                if let OpCode::BinOp(op) = opcode {
                    cause.into_diagnostic(file_id, debug_info, op)
                } else {
                    malformed_diagnostic()
                }
            }
            TabGet(cause) => {
                cause.into_diagnostic(file_id, debug_info.and_then(TabDebugInfo::with_tab_get))
            }
            TabSet(cause) => {
                cause.into_diagnostic(file_id, debug_info.and_then(TabDebugInfo::with_tab_set))
            }
        }
    }
}

impl From<Panic> for Cause {
    fn from(value: Panic) -> Self {
        Cause::Panic(value)
    }
}

impl From<MissingStackSlot> for Cause {
    fn from(value: MissingStackSlot) -> Self {
        Cause::MissingStackSlot(value)
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

impl From<IpOutOfBounds> for Cause {
    fn from(value: IpOutOfBounds) -> Self {
        Cause::IpOutOfBounds(value)
    }
}

impl From<Invoke> for Cause {
    fn from(value: Invoke) -> Self {
        Cause::Invoke(value)
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
            Literal(span) => span.clone(),
            FnCall { callable, args } => callable.start..args.end,
            FnExpr { total_span, .. } => total_span.clone(),
            FnDecl { total_span, .. } => total_span.clone(),
            Return { return_ } => return_.clone(),
            Break { break_ } => break_.clone(),
            IfElse { if_ } => if_.clone(),
            NumericForLoop { for_, .. } => for_.clone(),
            GenericForLoop { for_, .. } => for_.clone(),
            WhileLoop { while_ } => while_.clone(),
            RepeatLoop { repeat, .. } => repeat.clone(),
            Label(span) => span.clone(),
            Goto { goto } => goto.clone(),
            LoadVarargs(span) => span.clone(),
            LoadPlace { ident, .. } => ident.clone(),
            StorePlace { ident, expr, .. } => ident.start..expr.end,
            UnaOp { op, arg } => op.start..arg.end,
            BinOp { lhs, rhs, .. } => lhs.start..rhs.end,
            LoadTable {
                table, indexing, ..
            } => table.start..indexing.end,
            StoreTable { table, expr, .. } => table.start..expr.end,
            CreateTable { table } => table.clone(),
            ConstructTable { entry, .. } => entry.total_span(),
        }
    }
}

impl TotalSpan for opcode::TabConstructor {
    fn total_span(&self) -> Range<usize> {
        use opcode::TabConstructor::*;

        match self {
            Index { indexing, .. } => indexing.clone(),
            Field { ident, .. } => ident.clone(),
            Value { value } => value.clone(),
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
