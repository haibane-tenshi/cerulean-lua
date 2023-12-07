use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::OpCodeDebugInfo;
use repr::index::ConstId;
use repr::opcode::OpCode;

#[derive(Debug, Clone)]
pub struct MissingConstId(pub ConstId);

impl MissingConstId {
    pub(super) fn into_diagnostic<FileId>(
        self,
        file_id: FileId,
        opcode: OpCode,
        debug_info: Option<OpCodeDebugInfo>,
    ) -> Diagnostic<FileId> {
        use super::ExtraDiagnostic;
        use codespan_reporting::diagnostic::Label;

        let MissingConstId(const_id) = self;
        let have_debug_info = debug_info.is_some();

        let name = opcode.name();
        let mut diag = Diagnostic::bug().with_message(format!(
            "bytecode instruction {name} attempted to load non-existent constant with id={const_id}",
        ));

        if let Some(debug_info) = debug_info {
            match (opcode, debug_info) {
                (OpCode::LoadConstant(_), OpCodeDebugInfo::LoadConst(info)) => diag
                    .with_label([Label::primary(file_id, info.literal)
                        .with_message("constructed out of this literal")]),
                (OpCode::LoadConstant(_), OpCodeDebugInfo::Generic(span)) => diag.with_label([
                    Label::secondary(file_id, span).with_message("triggered by this"),
                ]),
                _ => (),
            }
        }

        match opcode {
            OpCode::LoadConstant(id) if id == const_id => diag.with_compiler_bug_note([
                "the opcode loads constant from constant table of the chunk current function is defined in",
            ]),
            OpCode::LoadConstant(id) => diag.with_runtime_bug_note([
                format!("the opcode should load constant with id={id}, but it attempted to load constant with id={const_id}"),
                "likely, this diagnostic is malformed".to_string(),
            ]),
            _ => diag.with_runtime_bug_note([
                format!("instruction {name} doesn't access constants"),
                "likely, this diagnostic is malformed".to_string(),
            ])
        };

        if !have_debug_info {
            diag.no_debug_info();
        }

        diag
    }
}
