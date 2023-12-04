mod tab_get;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use repr::debug_info::opcode;
use std::ops::Range;

pub use tab_get::{Cause as TabGetCause, RuntimeCause as TabGetRuntimeCause};

#[derive(Debug)]
pub enum Error {
    TabGet {
        debug_info: Option<opcode::TabGet>,
        cause: TabGetCause,
    },
}

impl Error {
    pub fn into_diagnostic<FileId>(self, file_id: FileId) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use Error::*;

        match self {
            TabGet { debug_info, cause } => cause.into_diagnostic(file_id, debug_info),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MissingArgsError {
    pub stack_len: usize,
}

impl MissingArgsError {
    fn into_diagnostic<FileId>(
        self,
        opcode_name: &str,
        expected_args: usize,
        debug_info: Option<(FileId, Range<usize>)>,
    ) -> Diagnostic<FileId> {
        let have_debug_info = debug_info.is_some();
        let message = format!("bytecode instruction {opcode_name} expects {expected_args} argument(s) but only {} were provided", self.stack_len);

        let labels = debug_info
            .map(|(file_id, span)| {
                vec![Label::secondary(file_id, span)
                    .with_message("error occurred here, but its cause in different place")]
            })
            .unwrap_or_default();

        let mut notes = Vec::new();

        notes.extend([
            "this is not bug in your Lua code, error is caused by malformed bytecode".to_string(),
            "the opcode gets its arguments from stack but it seems there is not enough values to take".to_string(),
            "the error implies that too few values were put on stack however it could have happenned long before this point".to_string(),
            "this most likely resulted from bug in compiler and should be reported".to_string(),
        ]);

        if have_debug_info {
            notes.extend([
                "no debug info is available".to_string(),
                "it is possible debug info was stripped\nit is also possible that erroneous bytecode was handcrafted\nplease check with where you got it".to_string(),
            ]);
        }

        Diagnostic::bug()
            .with_message(message)
            .with_labels(labels)
            .with_notes(notes)
    }
}

// #[derive(Debug)]
// pub enum TabSetCause {
//     NoTable,
//     NoTableAndIndex,
//     NoTableAndIndexAndValue,
//     TableTypeMismatch(Type),
//     InvalidKey(InvalidTableKeyError),
// }

trait TotalSpan {
    fn total_span(&self) -> Range<usize>;
}

impl TotalSpan for opcode::TabGet {
    fn total_span(&self) -> Range<usize> {
        use opcode::TabGet::*;

        match self {
            GlobalEnv { ident } => ident.clone(),
            Local {
                table,
                index: _,
                indexing,
            } => table.start..indexing.end,
        }
    }
}
