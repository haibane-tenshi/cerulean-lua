use logos::Logos;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Logos)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'s> {
    #[token("nil")]
    Nil,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("not")]
    Not,

    #[token("and")]
    And,

    #[token("or")]
    Or,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("elseif")]
    ElseIf,

    #[token("for")]
    For,

    #[token("in")]
    In,

    #[token("while")]
    While,

    #[token("then")]
    Then,

    #[token("repeat")]
    Repeat,

    #[token("until")]
    Until,

    #[token("break")]
    Break,

    #[token("do")]
    Do,

    #[token("end")]
    End,

    #[token("local")]
    Local,

    #[token("function")]
    Function,

    #[token("return")]
    Return,

    #[token("goto")]
    Goto,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Asterisk,

    #[token("/")]
    Slash,

    #[token("%")]
    Percent,

    #[token("^")]
    Caret,

    #[token("#")]
    Hash,

    #[token("&")]
    Ampersand,

    #[token("~")]
    Tilde,

    #[token("|")]
    Pipe,

    #[token("<<")]
    LeftShift,

    #[token(">>")]
    RightShift,

    #[token("//")]
    DoubleSlash,

    #[token("==")]
    Equal,

    #[token("~=")]
    NotEqual,

    #[token("<=")]
    LessThanOrEqual,

    #[token(">=")]
    GreaterThanOrEqual,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

    #[token("=")]
    Assign,

    #[token("(")]
    ParL,

    #[token(")")]
    ParR,

    #[token("{")]
    CurlyL,

    #[token("}")]
    CurlyR,

    #[token("[")]
    BracketL,

    #[token("]")]
    BracketR,

    #[token("::")]
    DoubleColon,

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("..")]
    DoubleDot,

    #[token("...")]
    TripleDot,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'s str),

    #[regex(r#""(([\\].)|.)*""#)]
    #[regex(r#"'(([\\].)|.)*'"#)]
    ShortLiteralString(&'s str),

    #[regex("[0-9]+(.[0-9]+)?([eE][+-]?[0-9]+)?")]
    #[regex("0[xX][0-9a-fA-F]+([.][0-9a-fA-F]+)?(([eE]|[pP])[+-]?[0-9a-fA-F]+)?")]
    Numeral(&'s str),

    #[regex("--.*\n?")]
    Comment(&'s str),
}
