use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::opcode;

use crate::value::table::InvalidTableKeyError;
use crate::value::Type;

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
        use codespan_reporting::diagnostic::Label;
        use Error::*;

        match self {
            TabGet { debug_info, cause } => {
                use repr::debug_info::opcode::TabGet::*;
                use TabGetCause::*;

                match cause {
                    NoTable => {
                        let msg = "no table value on the stack";

                        let (labels, notes) = match debug_info {
                            Some(Local {
                                table,
                                index: _,
                                indexing,
                            }) => {
                                let labels = vec![
                                    Label::primary(file_id.clone(), table)
                                        .with_message("presumed table value"),
                                    Label::secondary(file_id.clone(), indexing)
                                        .with_message("indexing happens here"),
                                ];

                                (labels, Vec::new())
                            }
                            Some(GlobalEnv { ident }) => {
                                let labels = vec![Label::secondary(file_id.clone(), ident)
                                    .with_message("non-local identifier here")];

                                let notes = vec!["Lua implicitly accesses `_ENV` table to look up non-local variables".to_string()];

                                (labels, notes)
                            }
                            None => {
                                let notes = vec!["no debug info is available".to_string()];

                                (Vec::new(), notes)
                            }
                        };

                        Diagnostic::bug()
                            .with_message(msg)
                            .with_labels(labels)
                            .with_notes(notes)
                    }
                    NoTableAndIndex => {
                        let msg = "no table and index value on the stack";

                        let (labels, notes) = match debug_info {
                            Some(Local {
                                table,
                                index,
                                indexing,
                            }) => {
                                let labels = vec![
                                    Label::primary(file_id.clone(), table)
                                        .with_message("presumed table value"),
                                    Label::primary(file_id.clone(), index)
                                        .with_message("presumed index value"),
                                    Label::secondary(file_id.clone(), indexing)
                                        .with_message("indexing happens here"),
                                ];

                                (labels, Vec::new())
                            }
                            Some(GlobalEnv { ident }) => {
                                let labels = vec![Label::secondary(file_id.clone(), ident)
                                    .with_message("presumed non-local identifier here")];

                                let notes = vec!["Lua implicitly accesses `_ENV` table to look up non-local variables".to_string()];

                                (labels, notes)
                            }
                            None => {
                                let notes = vec!["no debug info is available".to_string()];

                                (Vec::new(), notes)
                            }
                        };

                        Diagnostic::bug()
                            .with_message(msg)
                            .with_labels(labels)
                            .with_notes(notes)
                    }
                    TableTypeMismatch(ty) => {
                        let msg = "value is not table";

                        let (labels, notes) = match debug_info {
                            Some(Local {
                                table,
                                index: _,
                                indexing,
                            }) => {
                                let labels = vec![
                                    Label::primary(file_id.clone(), table).with_message(format!(
                                        "expected table, found {}",
                                        ty.to_lua_name()
                                    )),
                                    Label::secondary(file_id.clone(), indexing)
                                        .with_message("indexing happens here"),
                                ];

                                (labels, Vec::new())
                            }
                            Some(GlobalEnv { ident }) => {
                                let labels = vec![Label::primary(file_id.clone(), ident)
                                    .with_message("non-local identifier here")];

                                let notes = vec![
                                    format!("expected `_ENV` to be table, but it has type {}", ty.to_lua_name()),
                                    "Lua implicitly accesses `_ENV` table to look up non-local variables".to_string(),
                                    "maybe you forgot to initialize global env when creating runtime?".to_string(),
                                    "maybe you assigned to `_ENV` somewhere?\nconsider that `_ENV` is a regular variable, so it is possible to assign to or shadow it".to_string(),
                                    "maybe script received misconfigured global env?\nby default scripts recieve runtime's global env on call, but it is possible to override this behavior\nsee documentation to Lua `load` function".to_string(),
                                ];

                                (labels, notes)
                            }
                            None => {
                                let notes = vec!["no debug info is available".to_string()];

                                (Vec::new(), notes)
                            }
                        };

                        Diagnostic::error()
                            .with_message(msg)
                            .with_labels(labels)
                            .with_notes(notes)
                    }
                    InvalidKey(err) => {
                        let msg = "invalid table key";

                        let value = match &err {
                            InvalidTableKeyError::Nan => "NaN",
                            InvalidTableKeyError::Nil => "nil",
                        };

                        let mut notes = match err {
                            InvalidTableKeyError::Nan => vec![
                                "Lua does not permit indexing tables using float NaN (not a number)".to_string(),
                                "Lua follows floating point standard which mandates that no two NaN values are equal even if their bitwise representation is identical\ntherefore, values under NaN keys would be impossible to look up".to_string(),
                            ],
                            InvalidTableKeyError::Nil => vec!["Lua does not permit indexing tables using `nil`".to_string()]
                        };

                        let labels = match debug_info {
                            Some(Local {
                                table: _,
                                index,
                                indexing,
                            }) => {
                                vec![
                                    Label::primary(file_id.clone(), index)
                                        .with_message(format!("index has value `{value}`")),
                                    Label::secondary(file_id.clone(), indexing)
                                        .with_message("indexing happens here"),
                                ]
                            }
                            Some(GlobalEnv { ident }) => {
                                let labels = vec![Label::primary(file_id.clone(), ident)
                                    .with_message("index value")];

                                notes.push("Lua implicitly accesses `_ENV` table to look up non-local variables".to_string());

                                labels
                            }
                            None => {
                                notes.push("no debug info is available".to_string());

                                Vec::new()
                            }
                        };

                        Diagnostic::error()
                            .with_message(msg)
                            .with_labels(labels)
                            .with_notes(notes)
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum TabGetCause {
    NoTable,
    NoTableAndIndex,
    TableTypeMismatch(Type),
    InvalidKey(InvalidTableKeyError),
}
