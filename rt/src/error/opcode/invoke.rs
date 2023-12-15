use crate::value::Type;
use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::opcode::DebugInfo;
use repr::opcode::OpCode;

#[derive(Debug, Clone, Copy)]
pub struct Invoke(pub Type);

impl Invoke {
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
        use DebugInfo::*;
        use Type::*;

        let have_debug_info = debug_info.is_some();
        let Invoke(type_) = self;

        let name = opcode.name();
        let mut diag = Diagnostic::error().with_message(format!(
            "value of type {type_} cannot be invoked as function"
        ));

        if let Some(debug_info) = debug_info {
            match (opcode, debug_info) {
                (OpCode::Invoke(_), FnCall { callable, args }) => {
                    diag.with_label([
                        Label::primary(file_id.clone(), callable)
                            .with_message("this value is not callable"),
                        Label::secondary(file_id, args).with_message("invocation happens here"),
                    ]);
                }
                (_, debug_info) => {
                    diag.with_label([Label::secondary(file_id, debug_info.total_span())
                        .with_message("triggered by this")])
                }
            }
        }

        match (opcode, type_) {
            (OpCode::Invoke(_), Table) => diag.with_help([
                "by default tables cannot be invoked,\nhowever defining `__call` metamethod will allow you to do it"
            ]),
            (OpCode::Invoke(_), _) => (),
            _ => diag.with_runtime_bug_note([
                format!("instruction {name} cannot invoke functions"),
                "likely, this diagnostic is malformed".to_string(),
            ]),
        };

        if !have_debug_info {
            diag.no_debug_info();
        }

        diag
    }
}
