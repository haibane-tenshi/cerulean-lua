pub mod chunk_cache;
pub mod ffi;
pub mod runtime;
pub mod value;

use std::error::Error;
use std::fmt::Display;

#[derive(Debug)]
pub struct RuntimeError;

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "runtime error")
    }
}

impl Error for RuntimeError {}
