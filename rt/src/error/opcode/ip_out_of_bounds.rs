use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::OpCodeDebugInfo;
use repr::index::InstrId;
use repr::opcode::OpCode;

#[derive(Debug, Clone)]
pub struct IpOutOfBounds(pub InstrId);

impl IpOutOfBounds {
    pub(super) fn into_diagnostic<FileId>(
        self,
        file_id: FileId,
        opcode: OpCode,
        debug_info: Option<OpCodeDebugInfo>,
    ) -> Diagnostic<FileId> {
        use super::{ExtraDiagnostic, TotalSpan};
        use codespan_reporting::diagnostic::Label;

        let IpOutOfBounds(instr_id) = self;
        let have_debug_info = debug_info.is_some();

        let name = opcode.name();
        let mut diag = Diagnostic::bug().with_message(format!(
            "bytecode instruction {name} attempted move instruction pointer out of bounds",
        ));

        if let Some(debug_info) = debug_info {
            match (opcode, debug_info) {
                (OpCode::LoadConstant(_), OpCodeDebugInfo::Literal(span)) => diag
                    .with_label([Label::primary(file_id, span)
                        .with_message("constructed out of this literal")]),
                (_, debug_info) => {
                    diag.with_label([Label::secondary(file_id, debug_info.total_span())
                        .with_message("triggered by this")])
                }
            }
        }

        match opcode {
            OpCode::Jump {offset} | OpCode::JumpIf { offset, .. } => diag.with_compiler_bug_note([
                format!("the opcode increments ip by {offset}, but resulting pointer ({}) points past the last instruction for current function", instr_id + offset),
            ]),
            OpCode::Loop { offset } => diag.with_compiler_bug_note([
                format!("the opcode decrements ip by {offset}, but resulting pointer would be negative"),
            ]),
            _ => diag.with_runtime_bug_note([
                format!("instruction {name} doesn't access instruction pointer"),
                "likely, this diagnostic is malformed".to_string(),
            ])
        };

        if !have_debug_info {
            diag.no_debug_info();
        }

        diag
    }
}
