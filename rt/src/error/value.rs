use codespan_reporting::diagnostic::Diagnostic;
use std::fmt::{Debug, Display};

use crate::value::{RootValue, TypeProvider as Types, Value};

pub struct ValueError<Gc: Types>(pub RootValue<Gc>);

impl<Gc> ValueError<Gc>
where
    Gc: Types,
    Gc::String: AsRef<[u8]>,
    RootValue<Gc>: Display,
{
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        use super::ExtraDiagnostic;
        use Value::*;

        let ValueError(value) = self;

        let mut diag = Diagnostic::error();
        let valid_utf8 = match &value {
            String(s) => {
                if let Ok(s) = std::str::from_utf8(s.as_ref().as_ref()) {
                    diag.message = s.into();
                    true
                } else {
                    false
                }
            }
            _ => false,
        };

        if !valid_utf8 {
            diag = diag.with_message(format!("panicked with value: {value}"));
        }

        if let Table(_) = value {
            diag.with_note([
                "runtime does not evaluate any additional code (both Lua or Rust) while generating diagnostic\nthis applies even when table provides method allowing to be converted to string",
                "you need to manually format error value to string if you want the message to contain more relevant data",
            ]);
        }

        diag
    }
}

impl<Gc> From<RootValue<Gc>> for ValueError<Gc>
where
    Gc: Types,
{
    fn from(value: RootValue<Gc>) -> Self {
        ValueError(value)
    }
}

impl<Gc> Debug for ValueError<Gc>
where
    Gc: Types,
    RootValue<Gc>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Value").field(&self.0).finish()
    }
}
