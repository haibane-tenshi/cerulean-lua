use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::opcode as debug_info;
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
        info: Option<debug_info::UnaOp>,
        op: UnaOp,
    ) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use super::ExtraDiagnostic;
        use codespan_reporting::diagnostic::Label;

        let Cause { arg } = self;

        let mut diag = Diagnostic::error().with_message(format!(
            "operation `{op}` is not defined for value of type `{arg}`"
        ));

        if let Some(info) = info {
            diag.with_label([
                Label::primary(file_id.clone(), info.op).with_message("op happens here"),
                Label::secondary(file_id, info.arg).with_message(format!("this has type `{arg}`")),
            ]);
        }

        if let Type::Table = arg {
            diag.with_help([
                format!("by default `{op}` cannot be applied to tables,\nhowever it is possible to define it via <?> metamethod"),
            ])
        }

        diag
    }
}
