use codespan_reporting::diagnostic::{Diagnostic, Label};
use repr::debug_info::opcode::TabGet;

use super::{ExtraDiagnostic, MissingArgsError, TotalSpan};
use crate::value::table::InvalidTableKeyError;
use crate::value::Type;

#[derive(Debug, Clone, Copy)]
pub enum Cause {
    Compile(MissingArgsError),
    Runtime(RuntimeCause),
}

impl Cause {
    pub(super) fn into_diagnostic<FileId>(
        self,
        file_id: FileId,
        debug_info: Option<TabGet>,
    ) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        match self {
            Cause::Compile(err) => {
                let debug_info = debug_info.map(|info| (file_id, info.total_span()));
                err.into_diagnostic("TabGet", 2, debug_info)
            }
            Cause::Runtime(err) => err.into_diagnostic(file_id, debug_info),
        }
    }
}

impl From<MissingArgsError> for Cause {
    fn from(value: MissingArgsError) -> Self {
        Cause::Compile(value)
    }
}

impl From<RuntimeCause> for Cause {
    fn from(value: RuntimeCause) -> Self {
        Cause::Runtime(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RuntimeCause {
    TableTypeMismatch(Type),
    InvalidKey(InvalidTableKeyError),
}

impl RuntimeCause {
    #[allow(clippy::single_match)]
    fn into_diagnostic<FileId>(
        self,
        file_id: FileId,
        debug_info: Option<TabGet>,
    ) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use RuntimeCause::*;
        use TabGet::*;

        let diag = Diagnostic::error();

        // Message
        let mut diag = match (self, &debug_info) {
            (TableTypeMismatch(ty), Some(GlobalEnv { .. })) => diag.with_message(format!(
                "expected `_ENV` to be table but it has type `{}`",
                ty.to_lua_name()
            )),
            (TableTypeMismatch(_), _) => diag.with_message("only tables can be indexed"),
            (InvalidKey(_), _) => diag.with_message("invalid table key"),
        };

        // Labels
        match (self, debug_info.clone()) {
            (
                TableTypeMismatch(ty),
                Some(Local {
                    table,
                    index: _,
                    indexing,
                }),
            ) => {
                diag.with_label([
                    Label::primary(file_id.clone(), table).with_message(format!(
                        "expected table, but this has type `{}`",
                        ty.to_lua_name()
                    )),
                    Label::secondary(file_id.clone(), indexing)
                        .with_message("table indexing happens here"),
                ]);
            }
            (TableTypeMismatch(_), Some(GlobalEnv { ident })) => {
                diag.with_label([Label::secondary(file_id.clone(), ident)
                    .with_message("identifier does not refer to local variable")]);
            }
            (
                InvalidKey(err),
                Some(Local {
                    table: _,
                    index,
                    indexing,
                }),
            ) => {
                diag.with_label([
                    Label::primary(file_id.clone(), index)
                        .with_message(format!("index has value `{}`", err.value_str())),
                    Label::secondary(file_id.clone(), indexing)
                        .with_message("table indexing happens here"),
                ]);
            }
            (InvalidKey(err), Some(GlobalEnv { ident })) => {
                diag.with_label([Label::primary(file_id.clone(), ident)
                    .with_message(format!("index has value `{}`", err.value_str()))]);
            }
            (_, None) => (),
        }

        // General notes
        match &debug_info {
            Some(GlobalEnv { .. }) => {
                diag.with_note([
                    "Lua implicitly accesses `_ENV` table to look up non-local variables",
                ]);
            }
            _ => (),
        }

        match self {
            TableTypeMismatch(_) => (),
            InvalidKey(err) => match err {
                InvalidTableKeyError::Nan => diag.with_note([
                    "Lua does not permit indexing tables using float NaN (not a number)",
                    "Lua follows floating point standard which mandates that no two NaN values are equal even if their bitwise representation is identical\ntherefore, values under NaN keys would be impossible to look up",
                ]),
                InvalidTableKeyError::Nil => diag.with_note(["Lua does not permit indexing tables using `nil`"]),
            }
        };

        // Special notes
        match (self, &debug_info) {
            (TableTypeMismatch(_), Some(GlobalEnv { .. })) => {
                diag.with_help([
                    "perhaps you forgot to initialize global env when creating runtime?",
                    "perhaps you assigned to `_ENV` somewhere?\nconsider that `_ENV` is a regular variable, so it is possible to assign to or shadow it",
                    "perhaps script received misconfigured global env?\nby default scripts recieve runtime's global env on call, but it is possible to override this behavior\nsee documentation for Lua std `load` function",
                ]);
            }
            (_, None) => {
                diag.with_note(["no debug info is available"]);
            }
            _ => (),
        }

        diag
    }
}
