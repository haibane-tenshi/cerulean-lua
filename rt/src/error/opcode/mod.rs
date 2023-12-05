mod table;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use repr::debug_info::opcode;
use std::ops::Range;

pub use table::{Cause as TabCause, RuntimeCause as TabRuntimeCause};

#[derive(Debug)]
pub enum Error {
    TabGet {
        debug_info: Option<opcode::TabGet>,
        cause: TabCause,
    },
    TabSet {
        debug_info: Option<opcode::TabSet>,
        cause: TabCause,
    },
}

impl Error {
    pub fn into_diagnostic<FileId>(self, file_id: FileId) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use Error::*;

        match self {
            TabGet { debug_info, cause } => cause.into_diagnostic_tab_get(file_id, debug_info),
            TabSet { debug_info, cause } => cause.into_diagnostic_tab_set(file_id, debug_info),
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
        let mut diag = Diagnostic::bug()
            .with_message(format!("bytecode instruction {opcode_name} expects {expected_args} argument(s) but only {} were provided", self.stack_len));

        if let Some((file_id, span)) = debug_info {
            diag.with_label([Label::secondary(file_id, span)
                .with_message("error occurred here, but its cause in different place")]);
        }

        diag.with_note(compiler_bug([
            "the opcode gets its arguments from stack but it seems there is not enough values to take",
            "the error implies that too few values were put on stack however it could have happenned long before this point",
        ]));

        if !have_debug_info {
            diag.with_note([
                "no debug info is available",
                "it is possible debug info was stripped\nit is also possible that erroneous bytecode was handcrafted\nplease check with where you got it",
            ]);
        }

        diag
    }
}

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

impl TotalSpan for opcode::TabSet {
    fn total_span(&self) -> Range<usize> {
        todo!()
    }
}

impl TotalSpan for opcode::TabConstructor {
    fn total_span(&self) -> Range<usize> {
        todo!()
    }
}

trait ExtraDiagnostic<FileId> {
    fn with_label(&mut self, iter: impl IntoIterator<Item = Label<FileId>>);
    fn with_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
    fn with_help(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>);
}

impl<FileId> ExtraDiagnostic<FileId> for Diagnostic<FileId> {
    fn with_label(&mut self, iter: impl IntoIterator<Item = Label<FileId>>) {
        self.labels.extend(iter)
    }

    fn with_note(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>) {
        self.notes
            .extend(iter.into_iter().map(|s| format!("note: {}", s.as_ref())));
    }

    fn with_help(&mut self, iter: impl IntoIterator<Item = impl AsRef<str>>) {
        self.notes
            .extend(iter.into_iter().map(|s| format!("help: {}", s.as_ref())));
    }
}

fn compiler_bug<'a>(iter: impl IntoIterator<Item = &'a str>) -> impl Iterator<Item = &'a str> {
    std::iter::once("this is not bug in your Lua code, error is caused by malformed bytecode")
        .chain(iter.into_iter().map(AsRef::as_ref))
        .chain(std::iter::once(
            "this most likely resulted from bug in compiler and should be reported",
        ))
}
