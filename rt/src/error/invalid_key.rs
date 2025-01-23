use std::fmt::Display;

use super::Message;

#[derive(Debug, Clone, Copy)]
pub enum InvalidKeyError {
    Nil,
    Nan,
}

impl InvalidKeyError {
    pub(crate) fn value_str(self) -> &'static str {
        match self {
            InvalidKeyError::Nan => "NaN",
            InvalidKeyError::Nil => "nil",
        }
    }

    pub(crate) fn into_diagnostic<FileId>(self) -> Message<FileId> {
        Message::error()
            .with_message(self.to_string())
            .with_notes(vec![
                "Lua does not permit table indexing using nil or NaN".to_string()
            ])
    }
}

impl Display for InvalidKeyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} cannot be used to index tables", self.value_str())
    }
}
