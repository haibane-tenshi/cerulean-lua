use codespan_reporting::diagnostic::Diagnostic;
use std::fmt::Debug;

use crate::value::Value;

#[derive(Clone)]
pub struct ValueError<C>(pub Value<C>);

impl<C> ValueError<C> {
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

impl<C> From<Value<C>> for ValueError<C> {
    fn from(value: Value<C>) -> Self {
        ValueError(value)
    }
}

impl<C> Debug for ValueError<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Value").field(&self.0).finish()
    }
}
