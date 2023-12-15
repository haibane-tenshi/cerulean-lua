use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::opcode::DebugInfo;
use repr::index::StackSlot;
use repr::opcode::OpCode;

#[derive(Debug, Clone, Copy)]
pub struct MissingStackSlot(pub StackSlot);

impl MissingStackSlot {
    pub(super) fn into_diagnostic<FileId>(
        self,
        file_id: FileId,
        opcode: OpCode,
        debug_info: Option<DebugInfo>,
    ) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use super::{ExtraDiagnostic, TotalSpan};
        use codespan_reporting::diagnostic::Label;
        use repr::debug_info::opcode::Place::*;
        use DebugInfo::*;

        let MissingStackSlot(stack_slot) = self;
        let have_debug_info = debug_info.is_some();

        let name = opcode.name();
        let mut diag = Diagnostic::bug().with_message(format!(
            "bytecode instruction {name} attempted to access non-existent stack slot with id={stack_slot}",
        ));

        if let Some(debug_info) = debug_info {
            match (opcode, debug_info) {
                (
                    OpCode::LoadStack(_),
                    LoadPlace {
                        place: Temporary,
                        ident,
                    },
                ) => diag.with_label([Label::primary(file_id, ident)
                    .with_message("while trying to load this variable")]),
                (
                    OpCode::LoadStack(_),
                    LoadPlace {
                        place: Global,
                        ident,
                    },
                ) => diag.with_label([Label::primary(file_id, ident)
                    .with_message("while trying to lookup this global variable")]),
                (OpCode::LoadStack(_), StorePlace { ident, eq_sign, .. }) => diag.with_label([
                    Label::primary(file_id.clone(), eq_sign)
                        .with_message("while trying to perform this assignment"),
                    Label::secondary(file_id.clone(), ident)
                        .with_message("targeting this variable"),
                ]),
                (
                    OpCode::LoadStack(_),
                    StoreTable {
                        table,
                        indexing,
                        eq_sign,
                        ..
                    },
                ) => diag.with_label([
                    Label::primary(file_id.clone(), eq_sign)
                        .with_message("while trying to perform this assignment"),
                    Label::secondary(file_id.clone(), table).with_message("targeting this table"),
                    Label::secondary(file_id, indexing).with_message("indexing happens here"),
                ]),
                (
                    OpCode::LoadStack(_) | OpCode::StoreStack(_),
                    GenericForLoop { for_, .. } | NumericForLoop { for_, .. },
                ) => diag.with_label([Label::secondary(file_id, for_)
                    .with_message("as part of desugaring this `for` loop")]),
                (
                    OpCode::StoreStack(_),
                    LoadPlace {
                        place: Temporary,
                        ident,
                    },
                ) => diag.with_label([Label::primary(file_id, ident)
                    .with_message("while trying to write to this variable")]),
                (OpCode::Invoke(_), FnCall { callable, args }) => diag.with_label([
                    Label::primary(file_id.clone(), callable)
                        .with_message("while trying to call this function"),
                    Label::secondary(file_id, args).with_message("invocation happens here"),
                ]),
                (OpCode::Invoke(_), GenericForLoop { for_, iter }) => diag.with_label([
                    Label::primary(file_id.clone(), iter)
                        .with_message("while trying to call this function"),
                    Label::secondary(file_id, for_)
                        .with_message("as part of desugaring this `for` loop"),
                ]),
                (OpCode::Return(_), Return { return_ }) => diag
                    .with_label([Label::primary(file_id, return_)
                        .with_message("while trying to return from function")]),
                (OpCode::MakeClosure(_), FnExpr { total_span, .. } | FnDecl { total_span, .. }) => {
                    diag.with_label([Label::secondary(file_id, total_span)
                        .with_message("while trying to capture upvalues for this function")])
                }
                (_, debug_info) => {
                    diag.with_label([Label::secondary(file_id, debug_info.total_span())
                        .with_message("triggered by this")])
                }
            }
        }

        match opcode {
            OpCode::LoadStack(id) if id == stack_slot => diag.with_compiler_bug_note([
                "the opcode loads temporary value from specific slot on stack",
            ]),
            OpCode::StoreStack(id) if id == stack_slot => diag.with_compiler_bug_note([
                "the opcode writes value to temporary at specific slot on stack",
            ]),
            OpCode::Invoke(id) if id == stack_slot => diag.with_compiler_bug_note([
                "the opcode invokes function in target stack slot",
            ]),
            OpCode::Return(id) if id == stack_slot => diag.with_compiler_bug_note([
                "the opcode returns values from function starting from targeted slot",
            ]),
            OpCode::MakeClosure(_) => diag.with_compiler_bug_note([
                "the opcode constructs closure for the function following specified recipe",
                "construction failed because recipe erroneously referred to non-existent stack slot",
            ]),
            OpCode::LoadStack(id) | OpCode::StoreStack(id) | OpCode::Invoke(id) => diag.with_runtime_bug_note([
                format!("the opcode should access temporary in slot with id={id}, but it attempted to access slot with id={stack_slot}"),
                "likely, this diagnostic is malformed".to_string(),
            ]),
            OpCode::Return(id) => diag.with_runtime_bug_note([
                format!("the opcode should target temporary in slot with id={id}, but it attempted to target slot with id={stack_slot}"),
            ]),
            _ => diag.with_runtime_bug_note([
                format!("instruction {name} doesn't access stack"),
                "likely, this diagnostic is malformed".to_string(),
            ])
        };

        if !have_debug_info {
            diag.no_debug_info();
        }

        diag
    }
}
