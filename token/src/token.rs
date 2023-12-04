#![allow(dead_code)]

use std::fmt;

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    //
    // Literal
    //
    CharLiteral(i8),
    // UCharLiteral(u8),
    // ShortLiteral(i16),
    // UShortLiteral(u16),
    IntLiteral(u128),
    UIntLiteral(u32),
    LongLiteral(i64),
    ULongLiteral(u64),
    LongLongLiteral(i128),
    ULongLongLiteral(u128),
    StringLiteral(String),
    // FloatLiteral(f32),
    DoubleLiteral(f64),

    //
    // Symbol
    //
    Symbol(String),

    //
    // Operators
    //
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Inc,  // ++
    Dec,  // --

    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,

    And,
    Or,
    Not,

    BitAnd,
    BitAndAssign,
    BitOr,
    BitOrAssign,
    BitXor,
    BitXorAssign,
    ShiftLeft,
    ShiftLeftAssign,
    ShiftRight,
    ShiftRightAssign,

    Tilda,
    Question,  // ?:

    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,

    Colon,         // ':'
    SemiColon,     // ';'
    Dot,           // '.'
    TripleDot,    // '...'
    Comma,         // ','
    ParenLeft,     // '('
    ParenRight,    // ')'
    BraceLeft,     // '{'
    BraceRight,    // '}'
    BracketLeft,   // '['
    BracketRight,  // ']'

    MemberSelection,  // '->'

    //
    // Keywords
    //
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    _Alignas,
    _Alignof,
    _Atomic,
    _Bool,
    _Complex,
    _Generic,
    _Imaginary,
    _Noreturn,
    _Static_assert,
    _Thread_local,

    // CUST
    Trait,
    Impl,
    _Self,
    _self,
    Dyn,
    Let,
    Match,
}

impl Token {
    pub fn is_type(&self) -> bool {
        match self {
            Token::Char | Token::Double | Token::Float | Token::Int | Token::Long | Token::Short | Token::Void => true,
            // Token::Symbol(_name) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::_Bool  => write!(f, "_Bool"),
            Token::Char   => write!(f, "char"),
            Token::Double => write!(f, "double"),
            Token::Float  => write!(f, "float"),
            Token::Int    => write!(f, "int"),
            Token::Long   => write!(f, "long"),
            Token::Short  => write!(f, "short"),
            Token::Void   => write!(f, "void"),
            Token::Symbol(name) => write!(f, "{}", name),
            _ => Err(fmt::Error),
        }
    }
}