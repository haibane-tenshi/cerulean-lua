use std::ops::Range;

#[derive(Debug, Clone)]
pub enum DebugInfo {
    Generic(Range<usize>),
    TabGet(TabGet),
}

/// Debug info for [`OpCode::TabSet`](crate::opcode::OpCode::TabSet).
#[derive(Debug, Clone)]
pub enum TabGet {
    Local {
        /// Span highlighting table value.
        table: Range<usize>,

        /// Span highlighting index value.
        index: Range<usize>,

        /// Span highlighting indexing portion of expression.
        ///
        /// Should point at
        /// * `[index]` in `table[index]`
        /// * `.index` in `table.index`
        /// * `:index` in `table:index(...)`
        indexing: Range<usize>,
    },
    GlobalEnv {
        ident: Range<usize>,
    },
}

impl From<TabGet> for DebugInfo {
    fn from(value: TabGet) -> Self {
        DebugInfo::TabGet(value)
    }
}
