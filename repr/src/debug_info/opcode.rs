use std::ops::Range;

#[derive(Debug, Clone)]
pub enum DebugInfo {
    Generic(Range<usize>),
    TabGet(TabSet),
}

/// Debug info for [`OpCode::TabSet`](crate::opcode::OpCode::TabSet).
#[derive(Debug, Clone)]
pub struct TabSet {
    /// Span highlighting table value.
    pub table: TableRange,

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

#[derive(Debug, Clone)]
pub enum TableRange {
    GlobalEnv,
    Local(Range<usize>),
}

impl From<TabSet> for DebugInfo {
    fn from(value: TabSet) -> Self {
        DebugInfo::TabGet(value)
    }
}
