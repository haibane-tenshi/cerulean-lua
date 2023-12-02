use std::ops::Range;

#[derive(Debug, Clone)]
pub enum OpCodeDebugInfo {
    Generic(Range<usize>),
}
