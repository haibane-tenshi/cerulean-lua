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
    Literal(Range<usize>),
    FnCall {
        callable: Range<usize>,
        args: Range<usize>,
    },
    FnExpr {
        function: Range<usize>,
        total_span: Range<usize>,
    },
    FnDecl {
        local: Option<Range<usize>>,
        function: Range<usize>,
        name: Range<usize>,
        total_span: Range<usize>,
    },
    Return {
        return_: Range<usize>,
    },
    Break {
        break_: Range<usize>,
    },
    IfElse {
        if_: Range<usize>,
    },
    NumericForLoop {
        for_: Range<usize>,
        ident: Range<usize>,
        step: Option<Range<usize>>,
    },
    GenericForLoop {
        for_: Range<usize>,
        iter: Range<usize>,
    },
    WhileLoop {
        while_: Range<usize>,
    },
    RepeatLoop {
        repeat: Range<usize>,
        until: Range<usize>,
    },
    Label(Range<usize>),
    Goto {
        goto: Range<usize>,
    },
    LoadVarargs(Range<usize>),
    LoadPlace {
        place: Place,
        ident: Range<usize>,
    },
    StorePlace {
        place: Place,
        ident: Range<usize>,
        eq_sign: Range<usize>,
        expr: Range<usize>,
    },
    UnaOp {
        op: Range<usize>,
        arg: Range<usize>,
    },
    BinOp {
        lhs: Range<usize>,
        op: Range<usize>,
        rhs: Range<usize>,
    },
    LoadTable {
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
    StoreTable {
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
        expr: Range<usize>,
    },
    CreateTable {
        table: Range<usize>,
    },
    ConstructTable {
        table: Range<usize>,
        entry: TabConstructor,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum Place {
    Temporary,
    Upvalue,
    Global,
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
