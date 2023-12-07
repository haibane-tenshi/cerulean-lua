use codespan_reporting::diagnostic::{Diagnostic, Label};
use repr::debug_info::opcode::{TabConstructor, TabGet, TabSet};
use std::ops::Range;

use super::{ExtraDiagnostic, TotalSpan};
use crate::value::table::InvalidTableKeyError;
use crate::value::Type;

#[derive(Debug, Clone, Copy)]
pub enum RuntimeCause {
    TableTypeMismatch(Type),
    InvalidKey(InvalidTableKeyError),
}

impl RuntimeCause {
    #[allow(clippy::single_match)]
    pub(super) fn into_diagnostic<FileId>(
        self,
        file_id: FileId,
        debug_info: Option<TabDebugInfo>,
    ) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use repr::debug_info::opcode::TabConstructor::*;
        use RuntimeCause::*;
        use TabDebugInfo::*;

        let diag = Diagnostic::error();

        // Message
        let mut diag = match (self, &debug_info) {
            (TableTypeMismatch(ty), Some(GlobalEnv { .. })) => diag.with_message(format!(
                "expected `_ENV` to be table but it has type `{}`",
                ty.to_lua_name()
            )),
            (TableTypeMismatch(_), Some(Constructor { .. })) => {
                diag.with_message("table constructor failed to create a table")
            }
            (TableTypeMismatch(_), _) => diag.with_message("only tables can be indexed"),
            (InvalidKey(_), _) => diag.with_message("invalid table key"),
        };

        // Labels
        match (self, debug_info.clone()) {
            (
                TableTypeMismatch(ty),
                Some(Local {
                    table, indexing, ..
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
            (TableTypeMismatch(_), Some(GlobalEnv { ident, .. })) => {
                diag.with_label([Label::secondary(file_id.clone(), ident)
                    .with_message("identifier does not refer to local variable")]);
            }
            (TableTypeMismatch(_), Some(Constructor { flavor, .. })) => diag
                .with_label([Label::secondary(file_id.clone(), flavor.total_span())
                    .with_message("table indexing happens here")]),
            (
                InvalidKey(err),
                Some(
                    Local {
                        index, indexing, ..
                    }
                    | Constructor {
                        flavor:
                            Index {
                                index, indexing, ..
                            },
                        ..
                    },
                ),
            ) => {
                diag.with_label([
                    Label::primary(file_id.clone(), index)
                        .with_message(format!("index has value `{}`", err.value_str())),
                    Label::secondary(file_id.clone(), indexing)
                        .with_message("table indexing happens here"),
                ]);
            }
            (
                InvalidKey(err),
                Some(
                    GlobalEnv { ident, .. }
                    | Constructor {
                        flavor: Field { ident, .. },
                        ..
                    },
                ),
            ) => {
                diag.with_label(
                    [Label::primary(file_id.clone(), ident).with_message(format!(
                        "identifier resolved to value `{}`",
                        err.value_str()
                    ))],
                );
            }
            (
                InvalidKey(err),
                Some(Constructor {
                    flavor: Value { value },
                    ..
                }),
            ) => {
                diag.with_label([Label::primary(file_id.clone(), value)
                    .with_message(format!("value is assigned index of `{}`", err.value_str()))]);
            }
            (_, None) => (),
        }

        match debug_info.clone() {
            Some(
                Local {
                    eq_sign: Some(eq_sign),
                    ..
                }
                | GlobalEnv {
                    eq_sign: Some(eq_sign),
                    ..
                },
            ) => diag.with_label([
                Label::secondary(file_id, eq_sign).with_message("triggered by this assignment")
            ]),
            Some(Constructor { table, .. }) => {
                diag.with_label([
                    Label::secondary(file_id, table).with_message("in table constructor")
                ]);
            }
            _ => (),
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
            (TableTypeMismatch(_), Some(Constructor{..})) => {
                diag.with_compiler_bug_note([
                    "table constructors first make an empty table then fill it with key-value pairs as reqested",
                    "the error indicates that the table under construction was not provided here for some reason",
                ]);
            }
            (InvalidKey(_), Some(Constructor { flavor: Field{..}, .. } | GlobalEnv {..})) => {
                diag.with_compiler_bug_note([
                    "table should be indexed using this identifier as string but that didn't happen for some reason"
                ])
            }
            (InvalidKey(_), Some(Constructor{flavor: Value{..}, ..})) => {
                diag.with_compiler_bug_note([
                    "keyless value should be provided with integer index but that didn't happen for some reason"
                ])
            }
            (_, None) => {
                diag.with_note(["no debug info is available"]);
            }
            _ => (),
        }

        diag
    }
}

#[derive(Debug, Clone)]
pub(super) enum TabDebugInfo {
    Local {
        table: Range<usize>,
        index: Range<usize>,
        indexing: Range<usize>,
        eq_sign: Option<Range<usize>>,
    },
    GlobalEnv {
        ident: Range<usize>,
        eq_sign: Option<Range<usize>>,
    },
    Constructor {
        table: Range<usize>,
        flavor: TabConstructor,
    },
}

impl From<TabSet> for TabDebugInfo {
    fn from(value: TabSet) -> Self {
        match value {
            TabSet::Local {
                table,
                index,
                indexing,
                eq_sign,
            } => TabDebugInfo::Local {
                table,
                index,
                indexing,
                eq_sign: Some(eq_sign),
            },
            TabSet::GlobalEnv { ident, eq_sign } => TabDebugInfo::GlobalEnv { ident, eq_sign },
            TabSet::Constructor { table, flavor } => TabDebugInfo::Constructor { table, flavor },
        }
    }
}

impl From<TabGet> for TabDebugInfo {
    fn from(value: TabGet) -> Self {
        match value {
            TabGet::Local {
                table,
                index,
                indexing,
            } => TabDebugInfo::Local {
                table,
                index,
                indexing,
                eq_sign: None,
            },
            TabGet::GlobalEnv { ident } => TabDebugInfo::GlobalEnv {
                ident,
                eq_sign: None,
            },
        }
    }
}
