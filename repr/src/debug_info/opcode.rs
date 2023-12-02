use std::ops::Range;

#[derive(Debug, Clone)]
pub enum DebugInfo {
    Generic(Range<usize>),
    TabSet(TabSet),
}

#[derive(Debug, Clone)]
pub struct TabSet {
    /// Span highlighting table value.
    pub table: Range<usize>,

    /// Span highlighting index value.
    pub index: Range<usize>,

    /// Span highlighting indexing portion of expression.
    ///
    /// Should point at
    /// * `[index]` in `table[index]`
    /// * `.index` in `table.index`
    /// * `:index` in `table:index(...)`
    pub indexing: Range<usize>,
}
