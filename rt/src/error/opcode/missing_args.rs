use codespan_reporting::diagnostic::Diagnostic;
use repr::opcode::OpCode;
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct MissingArgsError {
    pub expected_args: usize,
    pub stack_len: usize,
}

impl MissingArgsError {
    pub(super) fn into_diagnostic<FileId>(
        self,
        file_id: FileId,
        opcode: OpCode,
        debug_info: Option<Range<usize>>,
    ) -> Diagnostic<FileId> {
        use super::ExtraDiagnostic;
        use codespan_reporting::diagnostic::Label;

        let MissingArgsError {
            expected_args,
            stack_len,
        } = self;

        let name = opcode.name();
        let have_debug_info = debug_info.is_some();
        let mut diag = Diagnostic::bug()
            .with_message(format!("bytecode instruction {name} expects {expected_args} argument(s) but only {stack_len} were provided"));

        if let Some(span) = debug_info {
            diag.with_label([Label::secondary(file_id, span)
                .with_message("error occurred here, but its cause in different place")]);
        }

        diag.with_compiler_bug_note([
            "the opcode gets its arguments from stack but it seems there is not enough values to take",
            "the error implies that too few values were put on stack however it could have happenned long before this point",
        ]);

        if !have_debug_info {
            diag.no_debug_info()
        }

        diag
    }
}
