use codespan_reporting::diagnostic::Diagnostic;
use std::fmt::{Debug, Display};

use crate::value::{TypeProvider, Value};

pub struct ValueError<Types: TypeProvider>(pub Value<Types>);

impl<Types> ValueError<Types>
where
    Types: TypeProvider<String = String>,
    Value<Types>: Display,
{
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        use super::ExtraDiagnostic;
        use Value::*;

        let ValueError(value) = self;

        let diag = Diagnostic::error();
        let mut diag = match &value {
            String(s) => diag.with_message(s),
            value => diag.with_message(format!("panicked with value: {value}")),
        };

        if let Table(_) = value {
            diag.with_note([
                "runtime does not evaluate any additional code (both Lua or Rust) while generating diagnostic\nthis applies even when table provides method allowing to be converted to string",
                "you need to manually format error value to string if you want the message to contain more relevant data",
            ]);
        }

        diag
    }
}

impl<Types> From<Value<Types>> for ValueError<Types>
where
    Types: TypeProvider,
{
    fn from(value: Value<Types>) -> Self {
        ValueError(value)
    }
}

impl<Types> Debug for ValueError<Types>
where
    Types: TypeProvider,
    Value<Types>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Value").field(&self.0).finish()
    }
}
