use codespan_reporting::diagnostic::Diagnostic;
use std::fmt::{Debug, Display};

use crate::gc::Heap;
use crate::value::{StrongValue, Types};

pub struct ValueError<Ty>(pub StrongValue<Ty>)
where
    Ty: Types;

impl<Ty> ValueError<Ty>
where
    Ty: Types,
{
    pub(crate) fn into_diagnostic<FileId>(self, heap: &Heap<Ty>) -> Diagnostic<FileId> {
        use super::ExtraDiagnostic;
        use crate::gc::LuaPtr;
        use crate::value::Value::*;

        let ValueError(value) = self;

        let mut diag = Diagnostic::error();
        let valid_utf8 = match &value {
            String(LuaPtr(s)) => {
                use crate::value::string::IntoEncoding;

                if let Some(value) = heap.get_root(s).to_str() {
                    diag.message = value.into_owned();
                    true
                } else {
                    false
                }
            }
            _ => false,
        };

        if !valid_utf8 {
            diag = diag.with_message(format!("panicked with value: {}", value.fmt_with(heap)));
        }

        if let Table(_) = value {
            diag.with_note([
                "runtime does not evaluate any additional code (both Lua or Rust) while generating diagnostic; this applies even when table provides method allowing to be converted to string",
                "you need to manually format error value to string if you want the message to contain more relevant data",
            ]);
        }

        diag
    }
}

impl<Ty> From<StrongValue<Ty>> for ValueError<Ty>
where
    Ty: Types,
{
    fn from(value: StrongValue<Ty>) -> Self {
        ValueError(value)
    }
}

impl<Ty> Clone for ValueError<Ty>
where
    Ty: Types,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Ty> Debug for ValueError<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ValueError").field(&self.0).finish()
    }
}

impl<Ty> Display for ValueError<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "panicked with value: {:#}", self.0.fmt_stringless())
    }
}
