use std::ops::Range;

#[derive(Debug, Clone)]
pub enum DebugInfo {
    /// Opcode doesn't have direct syntax associated with it
    /// but is required for correct operation of a language construct.
    ///
    /// The primary use here is to link back to the related position in code
    /// without implying any specific use for the spanned fragment.
    ///
    /// More specific information should be conveyed through other variants.
    Generic(Range<usize>),
    Invoke(Invoke),
    Return(Return),
    LoadUpvalue(LoadUpvalue),
    LoadConst(LoadConst),
    MakeClosure(MakeClosure),
    StoreUpvalue(StoreUpvalue),
    UnaOp(UnaOp),
    BinOp(BinOp),
    TabGet(TabGet),
    TabSet(TabSet),
}

/// Debug info for [`OpCode::Invoke`](crate::opcode::OpCode::Invoke).
#[derive(Debug, Clone)]
pub enum Invoke {
    Call {
        callable: Range<usize>,
        args: Range<usize>,
    },

    /// Emitted as part of generic `for` loop desugaring.
    ForLoop {
        for_token: Range<usize>,

        /// Span pointing to expression that emitted iterator function.
        iter: Range<usize>,
    },
}

/// Debug info for [`OpCode::Return`](crate::opcode::OpCode::Return).
#[derive(Debug, Clone)]
pub struct Return {
    pub return_token: Range<usize>,
}

/// Debug info for [`OpCode::StoreUpvalue`](crate::opcode::OpCode::StoreUpvalue).
#[derive(Debug, Clone)]
pub enum LoadUpvalue {
    /// Span pointing to identifier resolved to be an upvalue.
    Upvalue(Range<usize>),

    /// Instruction attempts to load `_ENV` variable from an upvalue slot.
    ///
    /// Span should point to identifier resolved to be global variable
    /// which triggered lookup of `_ENV` variable.
    Global(Range<usize>),
}

/// Debug info for [`OpCode::StoreUpvalue`](crate::opcode::OpCode::StoreUpvalue).
#[derive(Debug, Clone)]
pub struct StoreUpvalue {
    pub ident: Range<usize>,
    pub eq_sign: Range<usize>,
}

/// Debug info for [`OpCode::LoadConstant`](crate::opcode::OpCode::LoadConstant).
#[derive(Debug, Clone)]
pub struct LoadConst {
    /// Constant was created out of a literal value.
    pub literal: Range<usize>,
}

/// Debug info for [`OpCode::MakeClosure`](crate::opcode::OpCode::MakeClosure).
#[derive(Debug, Clone)]
pub struct MakeClosure {
    /// `function` token which triggered function creation.
    pub function_token: Range<usize>,

    pub total_span: Range<usize>,
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

impl From<Invoke> for DebugInfo {
    fn from(value: Invoke) -> Self {
        DebugInfo::Invoke(value)
    }
}

impl From<Return> for DebugInfo {
    fn from(value: Return) -> Self {
        DebugInfo::Return(value)
    }
}

impl From<LoadUpvalue> for DebugInfo {
    fn from(value: LoadUpvalue) -> Self {
        DebugInfo::LoadUpvalue(value)
    }
}

impl From<LoadConst> for DebugInfo {
    fn from(value: LoadConst) -> Self {
        DebugInfo::LoadConst(value)
    }
}

impl From<MakeClosure> for DebugInfo {
    fn from(value: MakeClosure) -> Self {
        DebugInfo::MakeClosure(value)
    }
}

impl From<StoreUpvalue> for DebugInfo {
    fn from(value: StoreUpvalue) -> Self {
        DebugInfo::StoreUpvalue(value)
    }
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
