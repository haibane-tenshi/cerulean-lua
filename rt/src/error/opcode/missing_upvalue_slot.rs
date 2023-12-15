use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::opcode::DebugInfo;
use repr::index::UpvalueSlot;
use repr::opcode::OpCode;

#[derive(Debug, Clone, Copy)]
pub struct MissingUpvalue(pub UpvalueSlot);

impl MissingUpvalue {
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

        let MissingUpvalue(upvalue_slot) = self;
        let have_debug_info = debug_info.is_some();

        let name = opcode.name();
        let mut diag = Diagnostic::bug().with_message(format!(
            "bytecode instruction {name} attempted to access non-existent upvalue slot with id={upvalue_slot}",
        ));

        if let Some(debug_info) = debug_info {
            match (opcode, debug_info) {
                (
                    OpCode::StoreUpvalue(_),
                    StorePlace {
                        place: Upvalue,
                        ident,
                        eq_sign,
                        ..
                    },
                ) => {
                    diag.with_label([
                        Label::primary(file_id.clone(), ident)
                            .with_message("variable is not local to current function"),
                        Label::secondary(file_id, eq_sign)
                            .with_message("triggered by this assignment"),
                    ]);
                }
                (OpCode::LoadUpvalue(_), LoadPlace { place: _, ident }) => {
                    diag.with_label([Label::primary(file_id, ident)
                        .with_message("variable is not local to curent function")]);
                }
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
            OpCode::LoadUpvalue(slot) if slot == upvalue_slot => diag.with_compiler_bug_note([
                "the opcode loads upvalue associated with current function",
                "number of upvalue slots is determined by function signature",
            ]),
            OpCode::LoadUpvalue(id) => diag.with_runtime_bug_note([
                format!("the opcode should load upvalue with id={id}, but it attempted to load upvalue with id={upvalue_slot}"),
                "likely, this diagnostic is malformed".to_string(),
            ]),
            OpCode::StoreUpvalue(slot) if slot == upvalue_slot => diag.with_compiler_bug_note([
                "the opcode writes to upvalue associated with current function",
                "number of upvalue slots is determined by function signature",
            ]),
            OpCode::StoreUpvalue(id) => diag.with_runtime_bug_note([
                format!("the opcode should write to upvalue with id={id}, but it attempted to write to upvalue with id={upvalue_slot}"),
                "likely, this diagnostic is malformed".to_string(),
            ]),
            OpCode::MakeClosure(_) => diag.with_compiler_bug_note([
                "the opcode constructs closure for the function following specified recipe",
                "construction failed because recipe erroneously referred to non-existent stack slot",
            ]),
            _ => diag.with_runtime_bug_note([
                format!("instruction {name} doesn't access upvalues"),
                "likely, this diagnostic is malformed".to_string(),
            ])
        };

        if !have_debug_info {
            diag.no_debug_info();
        }

        diag
    }
}
