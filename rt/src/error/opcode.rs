use codespan_reporting::diagnostic::{Diagnostic, Label};
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
        use Error::*;

        match self {
            TabGet { debug_info, cause } => cause.into_diagnostic(debug_info, file_id),
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

impl TabGetCause {
    fn into_diagnostic<FileId>(
        self,
        debug_info: Option<opcode::TabGet>,
        file_id: FileId,
    ) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use opcode::TabGet::*;
        use TabGetCause::*;

        let mut labels = Vec::new();
        let mut notes = Vec::new();

        let msg = match self {
            NoTable => {
                match debug_info {
                    Some(Local {
                        table,
                        index: _,
                        indexing,
                    }) => {
                        labels.extend([
                            Label::primary(file_id.clone(), table)
                                .with_message("presumed table value"),
                            Label::secondary(file_id.clone(), indexing)
                                .with_message("indexing happens here"),
                        ]);
                    }
                    Some(GlobalEnv { ident }) => {
                        labels.push(
                            Label::secondary(file_id.clone(), ident)
                                .with_message("identifier does not refer to local variable"),
                        );

                        notes.push(
                            "Lua implicitly accesses `_ENV` table to look up non-local variables"
                                .to_string(),
                        );
                    }
                    None => {
                        notes.push("no debug info is available".to_string());
                    }
                };

                "no table value on the stack"
            }
            NoTableAndIndex => {
                match debug_info {
                    Some(Local {
                        table,
                        index,
                        indexing,
                    }) => {
                        labels.extend([
                            Label::primary(file_id.clone(), table)
                                .with_message("presumed table value"),
                            Label::primary(file_id.clone(), index)
                                .with_message("presumed index value"),
                            Label::secondary(file_id.clone(), indexing)
                                .with_message("indexing happens here"),
                        ]);
                    }
                    Some(GlobalEnv { ident }) => {
                        labels.extend([Label::secondary(file_id.clone(), ident)
                            .with_message("identifier does not refer to local variable")]);

                        notes.push(
                            "Lua implicitly accesses `_ENV` table to look up non-local variables"
                                .to_string(),
                        );
                    }
                    None => {
                        notes.push("no debug info is available".to_string());
                    }
                };

                "no table and index value on the stack"
            }
            TableTypeMismatch(ty) => {
                match debug_info {
                    Some(Local {
                        table,
                        index: _,
                        indexing,
                    }) => labels.extend([
                        Label::primary(file_id.clone(), table)
                            .with_message(format!("expected table, found {}", ty.to_lua_name())),
                        Label::secondary(file_id.clone(), indexing)
                            .with_message("indexing happens here"),
                    ]),
                    Some(GlobalEnv { ident }) => {
                        labels.push(
                            Label::secondary(file_id.clone(), ident)
                                .with_message("identifier does not refer to local variable"),
                        );

                        notes.extend([
                            format!("expected `_ENV` to be table, but it has type {}", ty.to_lua_name()),
                            "Lua implicitly accesses `_ENV` table to look up non-local variables".to_string(),
                            "maybe you forgot to initialize global env when creating runtime?".to_string(),
                            "maybe you assigned to `_ENV` somewhere?\nconsider that `_ENV` is a regular variable, so it is possible to assign to or shadow it".to_string(),
                            "maybe script received misconfigured global env?\nby default scripts recieve runtime's global env on call, but it is possible to override this behavior\nsee documentation to Lua `load` function".to_string(),
                        ]);
                    }
                    None => {
                        notes.push("no debug info is available".to_string());
                    }
                };

                "expected table"
            }
            InvalidKey(err) => {
                let value = match &err {
                    InvalidTableKeyError::Nan => "NaN",
                    InvalidTableKeyError::Nil => "nil",
                };

                match err {
                    InvalidTableKeyError::Nan => notes.extend([
                        "Lua does not permit indexing tables using float NaN (not a number)".to_string(),
                        "Lua follows floating point standard which mandates that no two NaN values are equal even if their bitwise representation is identical\ntherefore, values under NaN keys would be impossible to look up".to_string(),
                    ]),
                    InvalidTableKeyError::Nil => notes.push("Lua does not permit indexing tables using `nil`".to_string()),
                };

                match debug_info {
                    Some(Local {
                        table: _,
                        index,
                        indexing,
                    }) => {
                        labels.extend([
                            Label::primary(file_id.clone(), index)
                                .with_message(format!("index has value `{value}`")),
                            Label::secondary(file_id.clone(), indexing)
                                .with_message("table indexing happens here"),
                        ]);
                    }
                    Some(GlobalEnv { ident }) => {
                        labels.push(
                            Label::primary(file_id.clone(), ident).with_message("index value"),
                        );

                        notes.push(
                            "Lua implicitly accesses `_ENV` table to look up non-local variables"
                                .to_string(),
                        );
                    }
                    None => {
                        notes.push("no debug info is available".to_string());
                    }
                };

                "invalid table key"
            }
        };

        Diagnostic::error()
            .with_message(msg)
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
