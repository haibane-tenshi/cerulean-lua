use std::ops::Range;

#[derive(Debug, Clone)]
pub enum DebugInfo {
    Generic(Range<usize>),
    UnaOp(UnaOp),
    BinOp(BinOp),
    TabGet(TabGet),
    TabSet(TabSet),
}

/// Debug info for [`OpCode::UnaOp`](crate::opcode::OpCode::UnaOp).
#[derive(Debug, Clone)]
pub struct UnaOp {
    pub op: Range<usize>,
    pub arg: Range<usize>,
}

/// Debug info for [`OpCode::BinOp`](crate::opcode::OpCode::BinOp).
#[derive(Debug, Clone)]
pub struct BinOp {
    pub lhs: Range<usize>,
    pub op: Range<usize>,
    pub rhs: Range<usize>,
}

/// Debug info for [`OpCode::TabGet`](crate::opcode::OpCode::TabGet).
#[derive(Debug, Clone)]
pub enum TabGet {
    Local {
        table: Range<usize>,
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
        /// Span highlighting identifier causing global env lookup.
        ident: Range<usize>,
    },
}

/// Debug info for [`OpCode::TabSet`](crate::opcode::OpCode::TabSet).
#[derive(Debug, Clone)]
pub enum TabSet {
    GlobalEnv {
        ident: Range<usize>,
        eq_sign: Option<Range<usize>>,
    },
    Local {
        table: Range<usize>,
        index: Range<usize>,

        /// Span highlighting indexing portion of expression.
        ///
        /// Should point at
        /// * `[index]` in `table[index]`
        /// * `.index` in `table.index`
        /// * `:index` in `table:index(...)`
        indexing: Range<usize>,
        eq_sign: Range<usize>,
    },
    Constructor {
        /// Span covering entirety of table constructor.
        table: Range<usize>,
        flavor: TabConstructor,
    },
}

/// Description of entries in table constructor.
#[derive(Debug, Clone)]
pub enum TabConstructor {
    /// Corresponds to explicitly indexed entry.
    ///
    /// ```lua
    /// {
    ///     [index] = ...
    /// }
    /// ```
    Index {
        index: Range<usize>,

        /// Span highlighting indexing portion of expression.
        ///
        /// Should point at `[index]` in
        /// ```lua
        /// {
        ///     [index] = ...
        /// }
        /// ```
        indexing: Range<usize>,
        eq_sign: Range<usize>,
    },
    /// Corresponds to ident-based entry.
    ///
    /// ```lua
    /// {
    ///     ident = ...
    /// }
    /// ```
    Field {
        ident: Range<usize>,
        eq_sign: Range<usize>,
    },

    /// Corresponds to value entry.
    ///
    /// ```lua
    /// {
    ///     value
    /// }
    /// ```
    Value { value: Range<usize> },
}

impl From<TabGet> for DebugInfo {
    fn from(value: TabGet) -> Self {
        DebugInfo::TabGet(value)
    }
}

impl From<TabSet> for DebugInfo {
    fn from(value: TabSet) -> Self {
        DebugInfo::TabSet(value)
    }
}

impl From<UnaOp> for DebugInfo {
    fn from(value: UnaOp) -> Self {
        DebugInfo::UnaOp(value)
    }
}

impl From<BinOp> for DebugInfo {
    fn from(value: BinOp) -> Self {
        DebugInfo::BinOp(value)
    }
}
