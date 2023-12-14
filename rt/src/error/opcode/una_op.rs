use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::opcode::DebugInfo;
use repr::opcode::UnaOp;

use crate::value::Type;

#[derive(Debug)]
pub struct Cause {
    pub arg: Type,
}

impl Cause {
    pub(super) fn into_diagnostic<FileId>(
        self,
        file_id: FileId,
        info: Option<DebugInfo>,
        op: UnaOp,
    ) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use super::{ExtraDiagnostic, TotalSpan};
        use codespan_reporting::diagnostic::Label;

        let Cause { arg } = self;

        let mut diag = Diagnostic::error().with_message(format!(
            "operation `{op}` is not defined for value of type `{arg}`"
        ));

        if let Some(debug_info) = info {
            match debug_info {
                DebugInfo::UnaOp {
                    op: op_span,
                    arg: arg_span,
                } => diag.with_label([
                    Label::primary(file_id.clone(), op_span).with_message("op happens here"),
                    Label::secondary(file_id, arg_span)
                        .with_message(format!("this has type `{arg}`")),
                ]),
                debug_info => diag.with_label([Label::secondary(file_id, debug_info.total_span())
                    .with_message("triggered by this")]),
            }
        }

        if let Type::Table = arg {
            diag.with_help([
                format!("by default `{op}` cannot be applied to tables,\nhowever it is possible to define it via <?> metamethod"),
            ])
        }

        diag
    }
}
