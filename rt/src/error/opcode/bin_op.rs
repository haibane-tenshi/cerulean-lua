use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::opcode as debug_info;
use repr::opcode::BinOp;

use crate::value::Type;

#[derive(Debug, Clone, Copy)]
pub struct Cause {
    pub lhs: Type,
    pub rhs: Type,
}

impl Cause {
    pub(super) fn into_diagnostic<FileId>(
        self,
        file_id: FileId,
        info: Option<debug_info::BinOp>,
        op: BinOp,
    ) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use super::ExtraDiagnostic;
        use codespan_reporting::diagnostic::Label;
        use repr::opcode::{
            AriBinOp::Add,
            BitBinOp::{And, Or, Xor},
        };
        use Type::*;

        let Cause { lhs, rhs } = self;

        let diag = Diagnostic::error();

        let mut diag = match (lhs, rhs) {
            (Table, _) | (_, Table) => {
                diag.with_message(format!("table doesn't define `{op}` operation"))
            }
            _ => diag.with_message(format!(
                "operation `{op}` is not defined between operands of types `{lhs}` and `{rhs}`"
            )),
        };

        if let Some(info) = info {
            diag.with_label([
                Label::primary(file_id.clone(), info.op).with_message("op happens here"),
                Label::secondary(file_id.clone(), info.lhs)
                    .with_message(format!("this has type `{lhs}`")),
                Label::secondary(file_id, info.rhs).with_message(format!("this has type `{rhs}`")),
            ]);
        }

        diag
    }
}
