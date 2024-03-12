use codespan_reporting::diagnostic::Diagnostic;
use std::fmt::Debug;

use gc::Heap;

use crate::gc::DisplayWith;
use crate::value::{CoreTypes, StrongValue};

pub struct ValueError<Value>(pub Value);

impl<Ty, Conv> ValueError<StrongValue<Ty, Conv>>
where
    Ty: CoreTypes,
    Ty::String: AsRef<[u8]>,
    StrongValue<Ty, Conv>: DisplayWith<Heap>,
{
    pub(crate) fn into_diagnostic<FileId>(self, heap: &Heap) -> Diagnostic<FileId> {
        use super::ExtraDiagnostic;
        use crate::gc::LuaPtr;
        use crate::value::Value::*;

        let ValueError(value) = self;

        let mut diag = Diagnostic::error();
        let valid_utf8 = match &value {
            String(LuaPtr(s)) => {
                if let Ok(s) = std::str::from_utf8(heap.get_root(s).as_ref().as_ref()) {
                    diag.message = s.into();
                    true
                } else {
                    false
                }
            }
            _ => false,
        };

        if !valid_utf8 {
            diag = diag.with_message(format!("panicked with value: {}", value.display(heap)));
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

impl<Value> From<Value> for ValueError<Value> {
    fn from(value: Value) -> Self {
        ValueError(value)
    }
}

impl<Value> Debug for ValueError<Value>
where
    Value: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Value").field(&self.0).finish()
    }
}
