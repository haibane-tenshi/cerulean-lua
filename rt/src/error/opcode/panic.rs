use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::opcode::DebugInfo;
use repr::opcode::OpCode;

#[derive(Debug, Clone)]
pub struct Panic;

impl Panic {
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

        let have_debug_info = debug_info.is_some();

        let name = opcode.name();
        let mut diag = match &debug_info {
            Some(NumericForLoop { .. }) => {
                Diagnostic::error().with_message("attempted to use step of 0 in numeric for loop")
            }
            _ => Diagnostic::error().with_message("function panicked"),
        };

        match (opcode, &debug_info) {
            (OpCode::Panic, Some(NumericForLoop { step: Some(_), .. })) => {
                diag.with_note(["Lua does not permit iteration with step 0 in numeric `for` loop"]);
                diag.with_help([
                    "if this is intentional consider using `while true do <loop body> end` construct instead"
                ]);
            }
            (OpCode::Panic, Some(NumericForLoop { step: None, .. })) => diag
                .with_compiler_bug_note([
                    "the default increment for numeric `for` loops is 1, but something went wrong",
                ]),
            (OpCode::Panic, _) => (),
            _ => diag.with_runtime_bug_note([
                format!("instruction {name} cannot directly invoke panic"),
                "likely, this diagnostic is malformed".to_string(),
            ]),
        };

        if let Some(debug_info) = debug_info {
            match (opcode, debug_info) {
                (OpCode::Panic, NumericForLoop { for_, step, .. }) => {
                    if let Some(step) = step {
                        diag.with_label([Label::primary(file_id.clone(), step)
                            .with_message("this evaluated to 0")])
                    }

                    diag.with_label([
                        Label::secondary(file_id, for_).with_message("as part of this for loop")
                    ]);
                }
                (_, debug_info) => {
                    diag.with_label([Label::secondary(file_id, debug_info.total_span())
                        .with_message("triggered by this")])
                }
            }
        }

        if !have_debug_info {
            diag.no_debug_info();
        }

        diag
    }
}
