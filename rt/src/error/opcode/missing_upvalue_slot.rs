use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::OpCodeDebugInfo;
use repr::index::UpvalueSlot;
use repr::opcode::OpCode;

#[derive(Debug, Clone)]
pub struct MissingUpvalue(pub UpvalueSlot);

impl MissingUpvalue {
    pub(super) fn into_diagnostic<FileId>(
        self,
        file_id: FileId,
        opcode: OpCode,
        debug_info: Option<OpCodeDebugInfo>,
    ) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use super::ExtraDiagnostic;
        use codespan_reporting::diagnostic::Label;

        let MissingUpvalue(upvalue_slot) = self;
        let have_debug_info = debug_info.is_some();

        let name = opcode.name();
        let mut diag = Diagnostic::bug().with_message(format!(
            "bytecode instruction {name} attempted to access non-existent upvalue slot with id={upvalue_slot}",
        ));

        if let Some(debug_info) = debug_info {
            use repr::debug_info::opcode::LoadUpvalue;

            match (opcode, debug_info) {
                (OpCode::StoreUpvalue(_), OpCodeDebugInfo::StoreUpvalue(info)) => {
                    diag.with_label([
                        Label::primary(file_id.clone(), info.ident)
                            .with_message("variable is not local to current function"),
                        Label::secondary(file_id, info.eq_sign)
                            .with_message("triggered by this assignment"),
                    ]);
                }
                (
                    OpCode::LoadUpvalue(_),
                    OpCodeDebugInfo::LoadUpvalue(LoadUpvalue::Upvalue(span)),
                ) => {
                    diag.with_label([Label::primary(file_id, span)
                        .with_message("variable is not local to curent function")]);
                }
                (
                    OpCode::LoadUpvalue(_),
                    OpCodeDebugInfo::LoadUpvalue(LoadUpvalue::Global(span)),
                ) => {
                    diag.with_label([Label::secondary(file_id, span)
                        .with_message("variable is not local to curent function")]);
                }
                _ => (),
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
