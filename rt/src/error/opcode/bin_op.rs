use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::opcode::DebugInfo;
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
        info: Option<DebugInfo>,
        op: BinOp,
    ) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use super::{ExtraDiagnostic, TotalSpan};
        use codespan_reporting::diagnostic::Label;
        use repr::opcode::{
            AriBinOp::{Add, Pow},
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

        if let Some(debug_info) = info {
            match debug_info {
                DebugInfo::BinOp {
                    lhs: lhs_span,
                    op: op_span,
                    rhs: rhs_span,
                } => diag.with_label([
                    Label::primary(file_id.clone(), op_span).with_message("op happens here"),
                    Label::secondary(file_id.clone(), lhs_span)
                        .with_message(format!("this has type `{lhs}`")),
                    Label::secondary(file_id, rhs_span)
                        .with_message(format!("this has type `{rhs}`")),
                ]),
                debug_info => diag.with_label([Label::secondary(file_id, debug_info.total_span())
                    .with_message("triggered by this")]),
            }
        }

        // Well-formedness of ops when applied between values of the same type
        //
        //         | nil   | bool  | int   | float | string | fn    | table
        // ari     |       |       | +     | +     |        |       | ?
        // bit     |       |       | +     |       |        |       | ?
        // rel eq  | +     | +     | +     | +     | +      | +     | +
        // rel cmp |       |       | +     | +     | +      |       | ?
        // str     |       |       |       |       | +      |       | ?
        //
        match (lhs, op, rhs) {
            // Tables out first.
            // It can always provide metamethods to override the op.
            (Table, _, _) | (_, _, Table) => diag.with_help([
                format!("by default `{op}` cannot be applied to tables,\nhowever defining <?> metamethod will allow you to do it"),
            ]),
            // Special help in case someone tries to do logical ops on bools.
            (Bool, BinOp::Bit(And | Or | Xor) | BinOp::Ari(Pow), Bool) => {
                diag.with_help([
                    "it appears you are trying to perform logical op on booleans\nLua does not define bitwise operations for this purpose"
                ]);
                match op {
                    BinOp::Bit(And) => diag.with_help([
                        "use `and` operator instead"
                    ]),
                    BinOp::Bit(Or) => diag.with_help([
                        "use `or` operator instead"
                    ]),
                    BinOp::Bit(Xor) | BinOp::Ari(Pow) => diag.with_help([
                        "there is no logical XOR, but you can write it as combination of `and`, `or` and `not` operators"
                    ]),
                    _ => (),
                };
                diag.with_help([
                    "alternatively, you can convert arguments to integers first\nremember to convert the result back!\nLua treats all integer values (including 0) as `true`",
                ]);
                if let BinOp::Ari(Pow) = op{
                    diag.with_note([
                        "Lua defines `^` as exponentiation operator and `~` as bitwise XOR\nboth are well defined for integers, mixing the two will lead to silently wrong results",
                    ]);
                }
            },
            // Nil, bool and functions never have ops.
            // No extra help required.
            (Nil | Bool | Function, _, _) | (_, _, Nil | Bool | Function) => (),
            // From this point only ints, floats and strings are left.
            (String, BinOp::Ari(Add), String) => {
                diag.with_help([
                    "it appears you are trying to concatenate two strings\nLua does not use addition for string concatenation\nuse special concatenation operator `..` instead",
                ]);
                diag.with_note([
                    "currently runtime doesn't perform automatic coercion from strings to numbers",
                ]);
            },
            (String, BinOp::Ari(_), _) | (_, BinOp::Ari(_), String) => diag.with_note([
                "currently runtime doesn't perform automatic coercion from strings to numbers in arithmetic ops",
            ]),
            (Int | Float, BinOp::Ari(_) | BinOp::Bit(_) | BinOp::Rel(_), Int | Float) => diag.with_note([
                "currently runtime doesn't perform automatic coercion between numeric types",
            ]),
            (String, BinOp::Str(_), Int | Float) | (Int | Float, BinOp::Str(_), String) => diag.with_note([
                "currently runtime doesn't perform automatic coercion from numbers to strings"
            ]),
            _ => (),
        }

        diag
    }
}
