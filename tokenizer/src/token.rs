#![allow(dead_code)]

use std::fmt;

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    //
    // end of input
    //
    EndOfInput,
    //
    // Literal
    //
    CharLiteral(u32),
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
    WColon,        // '::'
    SemiColon,     // ';'
    Dot,           // '.'
    TripleDot,     // '...'
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
    // Let,
    // Match,
}

impl Token {
    pub fn is_type(&self) -> bool {
        match self {
            Token::Char | Token::Double | Token::Float | Token::Int | Token::Long | Token::Short | Token::Void => true,
            // Token::Symbol(_name) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_eof(&self) -> bool {
        match self {
            Token::EndOfInput => true,
            _ => false,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::EndOfInput => write!(f, "<end of input>"),
            //
            // Literal
            //
            Token::CharLiteral(ch) => write!(f, "char literal '{}'", ch),
            Token::IntLiteral(i) => write!(f, "int literal '{}'", i),
            Token::UIntLiteral(i) => write!(f, "unsigned int literal '{}'", i),
            Token::LongLiteral(i) => write!(f, "long literal '{}'", i),
            Token::ULongLiteral(i) => write!(f, "unsigned long literal '{}'", i),
            Token::LongLongLiteral(i) => write!(f, "long long literal '{}'", i),
            Token::ULongLongLiteral(i) => write!(f, "unsigned long long literal '{}'", i),
            Token::StringLiteral(s) => write!(f, "string literal \"{}\"", s),
            Token::DoubleLiteral(d) => write!(f, "double literal '{}'", d),
            //
            // Symbol
            //
            Token::Symbol(s) => write!(f, "symbol '{}'", s),
            //
            // Operators
            //
            Token::Add => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Mod => write!(f, "%"),
        
            Token::Inc => write!(f, "++"),
            Token::Dec => write!(f, "--"),
        
            Token::Assign => write!(f, "="),
            Token::AddAssign => write!(f, "+="),
            Token::SubAssign => write!(f, "-="),
            Token::MulAssign => write!(f, "*="),
            Token::DivAssign => write!(f, "/="),
            Token::ModAssign => write!(f, "%="),
        
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::Not => write!(f, "!"),
        
            Token::BitAnd => write!(f, "&"),
            Token::BitAndAssign => write!(f, "&="),
            Token::BitOr => write!(f, "|"),
            Token::BitOrAssign => write!(f, "|="),
            Token::BitXor => write!(f, "^"),
            Token::BitXorAssign => write!(f, "^="),
            Token::ShiftLeft => write!(f, "<<"),
            Token::ShiftLeftAssign => write!(f, "<<="),
            Token::ShiftRight => write!(f, ">>"),
            Token::ShiftRightAssign => write!(f, ">>="),
        
            Token::Tilda => write!(f, "~"),
            Token::Question => write!(f, "?"),
        
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
        
            Token::Colon => write!(f, ":"),         // ':'
            Token::WColon => write!(f, "::"),       // '::'
            Token::SemiColon => write!(f, ";"),     // ';'
            Token::Dot => write!(f, "."),           // '.'
            Token::TripleDot => write!(f, "..."),   // '...'
            Token::Comma => write!(f, ","),         // ','
            Token::ParenLeft => write!(f, "("),     // '('
            Token::ParenRight => write!(f, ")"),    // ')'
            Token::BraceLeft => write!(f, "{{"),    // '{'
            Token::BraceRight => write!(f, "}}"),   // '}'
            Token::BracketLeft => write!(f, "["),   // '['
            Token::BracketRight => write!(f, "]"),  // ']'
        
            Token::MemberSelection => write!(f, "->"),  // '->'
        
            //
            // Keywords
            //
            Token::Auto => write!(f, "auto"),
            Token::Break => write!(f, "break"),
            Token::Case => write!(f, "case"),
            Token::Char => write!(f, "char"),
            Token::Const => write!(f, "const"),
            Token::Continue => write!(f, "continue"),
            Token::Default => write!(f, "default"),
            Token::Do => write!(f, "do"),
            Token::Double => write!(f, "double"),
            Token::Else => write!(f, "else"),
            Token::Enum => write!(f, "enum"),
            Token::Extern => write!(f, "extern"),
            Token::Float => write!(f, "float"),
            Token::For => write!(f, "for"),
            Token::Goto => write!(f, "goto"),
            Token::If => write!(f, "if"),
            Token::Inline => write!(f, "inline"),
            Token::Int => write!(f, "int"),
            Token::Long => write!(f, "long"),
            Token::Register => write!(f, "register"),
            Token::Restrict => write!(f, "restrict"),
            Token::Return => write!(f, "return"),
            Token::Short => write!(f, "short"),
            Token::Signed => write!(f, "signed"),
            Token::Sizeof => write!(f, "sizeof"),
            Token::Static => write!(f, "static"),
            Token::Struct => write!(f, "struct"),
            Token::Switch => write!(f, "switch"),
            Token::Typedef => write!(f, "typedef"),
            Token::Union => write!(f, "union"),
            Token::Unsigned => write!(f, "unsigned"),
            Token::Void => write!(f, "void"),
            Token::Volatile => write!(f, "volatile"),
            Token::While => write!(f, "while"),
            Token::_Alignas => write!(f, "_alignas"),
            Token::_Alignof => write!(f, "_alignof"),
            Token::_Atomic => write!(f, "_atomic"),
            Token::_Bool => write!(f, "_bool"),
            Token::_Complex => write!(f, "_complex"),
            Token::_Generic => write!(f, "_generic"),
            Token::_Imaginary => write!(f, "_imaginary"),
            Token::_Noreturn => write!(f, "_noreturn"),
            Token::_Static_assert => write!(f, "_static_assert"),
            Token::_Thread_local => write!(f, "_thread_local"),
        
            // CUST
            Token::Trait => write!(f, "trait"),
            Token::Impl => write!(f, "impl"),
            Token::_Self => write!(f, "Self"),
            Token::_self => write!(f, "self"),
            Token::Dyn => write!(f, "dyn"),
            // Token::Let => write!(f, "let"),
            // Token::Match => write!(f, "match"),
        }
    }
}