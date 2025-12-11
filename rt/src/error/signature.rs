use std::error::Error;
use std::fmt::Display;

use codespan_reporting::diagnostic::Diagnostic;

use crate::ffi::arg_parser::ParseError;

#[derive(Debug, Clone)]
pub enum SignatureError {
    TooFewArgs { found: usize },
    TooManyArgs { found: usize, expected: usize },
    ConversionFailure { index: usize, msg: String },
}

impl SignatureError {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        Diagnostic::error().with_message(self.to_string())
    }
}

impl Display for SignatureError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SignatureError::TooFewArgs { found } => {
                write!(
                    f,
                    "function received {found} args, but not all required arguments are provided"
                )
            }
            SignatureError::TooManyArgs { found, expected } => {
                write!(
                    f,
                    "function received {found} args, but it expects at most {expected} arguments"
                )
            }
            SignatureError::ConversionFailure { index, msg } => {
                write!(f, "failed to convert arg[{index}]: {msg}")
            }
        }
    }
}

impl Error for SignatureError {}

impl<E> From<ParseError<E>> for SignatureError
where
    E: Display,
{
    fn from(value: ParseError<E>) -> Self {
        match value {
            ParseError::TooFewArgs { found } => SignatureError::TooFewArgs { found },
            ParseError::TooManyArgs { found, expected } => {
                SignatureError::TooManyArgs { found, expected }
            }
            ParseError::ConversionFailure { index, err } => SignatureError::ConversionFailure {
                index,
                msg: err.to_string(),
            },
        }
    }
}
