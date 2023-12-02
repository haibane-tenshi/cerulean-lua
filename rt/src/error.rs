use std::error::Error;
use std::fmt::Display;

#[derive(Debug)]
pub enum RuntimeError {
    CatchAll,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "runtime error")
    }
}

impl Error for RuntimeError {}
