use once_cell::sync::Lazy;
use std::collections::HashMap;
use super::Position;
use super::TokenizerError;

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

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Token::_Bool  => String::from("_Bool"),
            Token::Char   => String::from("char"),
            Token::Double => String::from("double"),
            Token::Float  => String::from("float"),
            Token::Int    => String::from("int"),
            Token::Long   => String::from("long"),
            Token::Short  => String::from("short"),
            Token::Void   => String::from("void"),
            Token::Symbol(name) => name.to_string(),
            _ => String::from("no such a type"),
        }
    }
}

/*
auto
break
case
char
const
continue
default
do
double
else
enum
extern
float
for
goto
if
inline
int
long
register
restrict
return
short
signed
sizeof
static
struct
switch
typedef
union
unsigned
void
volatile
while
_Alignas
_Alignof
_Atomic
_Bool
_Complex
_Generic
_Imaginary
_Noreturn
_Static_assert
_Thread_local

trait
impl
self
*/

#[allow(non_upper_case_globals)]
static Keywords: Lazy<HashMap<String, Token>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert(String::from("auto"), Token::Auto);
    m.insert(String::from("break"), Token::Break);
    m.insert(String::from("case"), Token::Case);
    m.insert(String::from("char"), Token::Char);
    m.insert(String::from("const"), Token::Const);
    m.insert(String::from("continue"), Token::Continue);
    m.insert(String::from("default"), Token::Default);
    m.insert(String::from("do"), Token::Do);
    m.insert(String::from("double"), Token::Double);
    m.insert(String::from("else"), Token::Else);
    m.insert(String::from("enum"), Token::Enum);
    m.insert(String::from("extern"), Token::Extern);
    m.insert(String::from("float"), Token::Float);
    m.insert(String::from("for"), Token::For);
    m.insert(String::from("goto"), Token::Goto);
    m.insert(String::from("if"), Token::If);
    m.insert(String::from("inline"), Token::Inline);
    m.insert(String::from("int"), Token::Int);
    m.insert(String::from("long"), Token::Long);
    m.insert(String::from("register"), Token::Register);
    m.insert(String::from("restrict"), Token::Restrict);
    m.insert(String::from("return"), Token::Return);
    m.insert(String::from("short"), Token::Short);
    m.insert(String::from("signed"), Token::Signed);
    m.insert(String::from("sizeof"), Token::Sizeof);
    m.insert(String::from("static"), Token::Static);
    m.insert(String::from("struct"), Token::Struct);
    m.insert(String::from("switch"), Token::Switch);
    m.insert(String::from("typedef"), Token::Typedef);
    m.insert(String::from("union"), Token::Union);
    m.insert(String::from("unsigned"), Token::Unsigned);
    m.insert(String::from("void"), Token::Void);
    m.insert(String::from("volatile"), Token::Volatile);
    m.insert(String::from("while"), Token::While);
    m.insert(String::from("_Alignas"), Token::_Alignas);
    m.insert(String::from("_Alignof"), Token::_Alignof);
    m.insert(String::from("_Atomic"), Token::_Atomic);
    m.insert(String::from("_Bool"), Token::_Bool);
    m.insert(String::from("_Complex"), Token::_Complex);
    m.insert(String::from("_Generic"), Token::_Generic);
    m.insert(String::from("_Imaginary"), Token::_Imaginary);
    m.insert(String::from("_Noreturn"), Token::_Noreturn);
    m.insert(String::from("_Static_assert"), Token::_Static_assert);
    m.insert(String::from("_Thread_local"), Token::_Thread_local);
    m.insert(String::from("trait"), Token::Trait);
    m.insert(String::from("impl"), Token::Impl);
    m.insert(String::from("self"), Token::_self);
    m.insert(String::from("Self"), Token::_Self);
    m.insert(String::from("dyn"), Token::Dyn);
    m.insert(String::from("let"), Token::Let);
    m.insert(String::from("match"), Token::Match);

    m
});


struct TokenizerContext<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    pos: Position,
}

impl<'a> TokenizerContext<'a> {
    pub fn new(input: &str) -> TokenizerContext {
        TokenizerContext {
            chars: input.chars().peekable(),
            pos: Position::new(),
        }
    }
}

#[derive(Debug)]
pub struct Tokenizer;

impl Tokenizer {
    pub fn new() -> Self {
        Tokenizer {}
    }

    pub fn tokenize(&self, input: &str) -> Result<Vec<(Token, Position)>, TokenizerError> {
        // let mut chars = input.chars().peekable();
        // let mut pos = Position::new();
        let mut ctx = TokenizerContext::new(input);
        let mut v = Vec::new();

        loop {
            let result = self.next_token(&mut ctx);
            match result {
                Ok(opt_tok_pos) => if let Some(tok_pos) = opt_tok_pos {
                        v.push(tok_pos);
                    }else{
                        break;
                    },
                Err(err) => return Err(err),
            }
        }

        Ok(v)
    }

    fn next_token(&self, ctx: &mut TokenizerContext) -> Result<Option<(Token, Position)>, TokenizerError> {
        self.skip_whitespace(ctx);

        let start_pos = ctx.pos.clone();

        if let Some(ch) = ctx.chars.peek() {
            match ch {
                c if c.is_digit(10) => self.tokenize_digit(ctx),
                '\'' => self.tokenize_char(ctx),
                '"' => self.tokenize_string(ctx),
                '+' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        match c {
                            '=' => {
                                self.next_char(ctx);
                                Ok(Some((Token::AddAssign, start_pos)))
                            },
                            '+' => {
                                self.next_char(ctx);
                                Ok(Some((Token::Inc, start_pos)))
                            },
                            _ => Ok(Some((Token::Add, start_pos)))
                        }

                    }else{
                        Ok(Some((Token::Add, start_pos)))
                    }
                },
                '-' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        match c {
                            '=' => {
                                self.next_char(ctx);
                                Ok(Some((Token::SubAssign, start_pos)))
                            },
                            '-' => {
                                self.next_char(ctx);
                                Ok(Some((Token::Dec, start_pos)))
                            },
                            '>' => {
                                self.next_char(ctx);
                                Ok(Some((Token::MemberSelection, start_pos)))
                            },
                            _ => Ok(Some((Token::Sub, start_pos)))
                        }

                    }else{
                        Ok(Some((Token::Sub, start_pos)))
                    }
                },
                '*' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == '=' {
                            self.next_char(ctx);
                            Ok(Some((Token::MulAssign, start_pos)))
                        }else{
                            Ok(Some((Token::Mul, start_pos)))
                        }
                    }else{
                        Ok(Some((Token::Mul, start_pos)))
                    }
                },
                '/' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == '=' {
                            self.next_char(ctx);
                            Ok(Some((Token::DivAssign, start_pos)))
                        }else{
                            Ok(Some((Token::Div, start_pos)))
                        }
                    }else{
                        Ok(Some((Token::Div, start_pos)))
                    }
                },
                '%' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == '=' {
                            self.next_char(ctx);
                            Ok(Some((Token::ModAssign, start_pos)))
                        }else{
                            Ok(Some((Token::Mod, start_pos)))
                        }
                    }else{
                        Ok(Some((Token::Mod, start_pos)))
                    }
                },
                '<' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        match c {
                            '<' => {
                                self.next_char(ctx);
                                Ok(Some((Token::ShiftLeft, start_pos)))
                            },
                            '=' => {
                                self.next_char(ctx);
                                Ok(Some((Token::LessEqual, start_pos)))
                            },
                            _ => Ok(Some((Token::Less, start_pos)))
                        }
                    }else{
                        Ok(Some((Token::Less, start_pos)))
                    }
                },
                '>' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        match c {
                            '>' => {
                                self.next_char(ctx);
                                Ok(Some((Token::ShiftRight, start_pos)))
                            },
                            '=' => {
                                self.next_char(ctx);
                                Ok(Some((Token::GreaterEqual, start_pos)))
                            },
                            _ => Ok(Some((Token::Greater, start_pos)))
                        }
                    }else{
                        Ok(Some((Token::Greater, start_pos)))
                    }
                },
                '=' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == '=' {
                            self.next_char(ctx);
                            Ok(Some((Token::Equal, start_pos)))
                        }else{
                            Ok(Some((Token::Assign, start_pos)))
                        }
                    }else{
                        Ok(Some((Token::Assign, start_pos)))
                    }
                },
                '!' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == '=' {
                            self.next_char(ctx);
                            Ok(Some((Token::NotEqual, start_pos)))
                        }else{
                            Ok(Some((Token::Not, start_pos)))
                        }
                    }else{
                        Ok(Some((Token::Not, start_pos)))
                    }
                },
                '&' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        match c {
                            '&' => {
                                self.next_char(ctx);
                                Ok(Some((Token::And, start_pos)))
                            },
                            '=' => {
                                self.next_char(ctx);
                                Ok(Some((Token::BitAndAssign, start_pos)))
                            },
                            _ => Ok(Some((Token::BitAnd, start_pos)))
                        }
                    }else{
                        Ok(Some((Token::BitAnd, start_pos)))
                    }
                },
                '|' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        match c {
                            '|' => {
                                self.next_char(ctx);
                                Ok(Some((Token::Or, start_pos)))
                            },
                            '=' => {
                                self.next_char(ctx);
                                Ok(Some((Token::BitOrAssign, start_pos)))
                            },
                            _ => Ok(Some((Token::BitOr, start_pos)))
                        }
                    }else{
                        Ok(Some((Token::BitAnd, start_pos)))
                    }
                },
                '^' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == '=' {
                            self.next_char(ctx);
                            Ok(Some((Token::BitXorAssign, start_pos)))
                        }else{
                            Ok(Some((Token::BitXor, start_pos)))
                        }
                    }else{
                        Ok(Some((Token::BitXor, start_pos)))
                    }
                },
                '~' => {
                    self.next_char(ctx);
                    Ok(Some((Token::Tilda, start_pos)))
                },
                '?' => {
                    self.next_char(ctx);
                    Ok(Some((Token::Question, start_pos)))
                },
                // Colon,         // ':'
                ':' => {
                    self.next_char(ctx);
                    Ok(Some((Token::Colon, start_pos)))
                },
                // SemiColon,     // ';'
                ';' => {
                    self.next_char(ctx);
                    Ok(Some((Token::SemiColon, start_pos)))
                },
                // Dot,           // '.'
                '.' => {
                    self.next_char(ctx);

                    if let Some(ch2) = ctx.chars.peek() {
                        if *ch2 == '.' {
                            self.next_char(ctx);  // skip '.'
                            let ch3 = self.next_char(ctx).ok_or(TokenizerError::illegal_end_of_input(Some(start_pos.clone())))?;
                            if ch3 == '.' {
                                Ok(Some((Token::TripleDot, start_pos)))
                            }else{
                                Err(TokenizerError::syntax_error(Some(start_pos)))
                            }

                        }else{
                            Ok(Some((Token::Dot, start_pos)))
                        }

                    }else{
                        Ok(Some((Token::Dot, start_pos)))
                    }
                },
                ',' => {
                    self.next_char(ctx);
                    Ok(Some((Token::Comma, start_pos)))
                },
                // ParenLeft,     // '('
                '(' => {
                    self.next_char(ctx);
                    Ok(Some((Token::ParenLeft, start_pos)))
                },
                // ParenRight,    // ')'
                ')' => {
                    self.next_char(ctx);
                    Ok(Some((Token::ParenRight, start_pos)))
                },
                // BraceLeft,     // '{'
                '{' => {
                    self.next_char(ctx);
                    Ok(Some((Token::BraceLeft, start_pos)))
                },
                // BraceRight,    // '}'
                '}' => {
                    self.next_char(ctx);
                    Ok(Some((Token::BraceRight, start_pos)))
                },
                // BracketLeft,   // '['
                '[' => {
                    self.next_char(ctx);
                    Ok(Some((Token::BracketLeft, start_pos)))
                },
                // BracketRight,  // ']'
                ']' => {
                    self.next_char(ctx);
                    Ok(Some((Token::BracketRight, start_pos)))
                },
                _ => {
                    let mut s = String::new();
                    while let Some(c) = self.peek_char(ctx) {
                        if c.is_whitespace() || c.is_control() ||
                           c == '\n' ||
                           c == '.' ||
                           c == ',' ||
                           c == ':' ||
                           c == ';' ||
                           c == '+' ||
                           c == '-' ||
                           c == '*' ||
                           c == '/' ||
                           c == '%' ||
                           c == '=' ||
                           c == '~' ||
                           c == '^' ||
                           c == '@' ||
                           c == '\'' ||
                           c == '`' ||
                           c == '"' ||
                           c == '!' ||
                           c == '?' ||
                           c == '/' ||
                           c == '#' ||
                           c == '$' ||
                           c == '&' ||
                           c == '|' ||
                           c == '<' ||
                           c == '>' ||
                           c == '(' ||
                           c == ')' ||
                           c == '{' ||
                           c == '}' ||
                           c == '[' ||
                           c == ']'
                        {
                            break;
                        }

                        self.next_char(ctx);
                        s.push(c);
                    }

                    if let Some(tok) = Keywords.get(&s) {
                        Ok(Some((tok.clone(), start_pos)))
                    }else{
                        Ok(Some((Token::Symbol(s), start_pos)))
                    }
                },
            }

        }else{ // end of input
            Ok(None)
        }
    }

    #[inline]
    fn peek_char(&self, ctx: &mut TokenizerContext) -> Option<char> {
        if let Some(c) = ctx.chars.peek() {
            Some(*c)
        }else{
            None
        }
    }

    fn next_char(&self, ctx: &mut TokenizerContext) -> Option<char> {
        let ch = ctx.chars.next();

        if let Some(c) = ch {
            if c == '\n' {
                ctx.pos.line += 1;
                ctx.pos.column = 0;
            }else{
                ctx.pos.column += 1;
            }
        }

        ch
    }

    fn skip_whitespace(&self, ctx: &mut TokenizerContext) {
        while let Some(c) = ctx.chars.peek() {
            if !(c.is_whitespace() || *c == '\n') {
                break;
            }
            self.next_char(ctx);
        }
    }

    fn tokenize_digit(&self, ctx: &mut TokenizerContext) -> Result<Option<(Token, Position)>, TokenizerError> {
        if let Some(c) = ctx.chars.peek() {
            if ! c.is_digit(10) {
                return Err(TokenizerError::IllegalCharWhileParsingDigit(Some(ctx.pos.clone())));
            }
        }

        let start_pos = ctx.pos.clone();
        let ch = ctx.chars.peek().ok_or(TokenizerError::illegal_end_of_input(Some(ctx.pos.clone())))?;

        match ch {
            '0' => {
                self.next_char(ctx);  // skip '0'

                let ch2 = if let Some(c) = ctx.chars.peek() {
                    c
                }else{
                    return Ok(Some((Token::IntLiteral(0), start_pos)));
                };

                match ch2 {
                    'b' => {  // Binary Number
                        self.next_char(ctx);  // skip 'b'
                        return self.tokenize_digit_sub(ctx, 2, start_pos);
                    },
                    'q' => {  // Quaternary
                        self.next_char(ctx);  // skip 'q'
                        return self.tokenize_digit_sub(ctx, 4, start_pos);
                    },
                    'x' => {  // Hexadecimal
                        self.next_char(ctx);  // skip 'x'
                        return self.tokenize_digit_sub(ctx, 16, start_pos);
                    },
                    // '.' => {  // float, double or member access
                    //     self.next_char(ctx);  // skip '.'
                    //     let ch3 = if let Some(c) = ctx.chars.peek() {
                    //         c
                    //     }else{
                    //         return Ok(Some((Token::DoubleLiteral(0 as f64), start_pos)));
                    //     };
                    //     if ch3.is_digit(10) {
                    //         return self.tokenize_float(ctx, 0, start_pos);
                    //     }else{
                    //         unimplemented!()
                    //     }
                    // },
                    _ => {    // Octal
                        if ch2.is_digit(8) {
                            return self.tokenize_digit_sub(ctx, 8, start_pos);
                        }else{
                            return Ok(Some((Token::IntLiteral(0), start_pos)))
                        }
                    },
                }
            },
            _ => (),  // do nothing
        }

        let mut i: u128 = 0;
        
        loop {
            let c = if let Some(ch) = self.peek_char(ctx) {
                ch
            }else{
                break;
            };

            if c.is_digit(10) {
                i = (i * 10) + (c as u128 - '0' as u128);
            }else if c == '.' {
                self.next_char(ctx);  // skip '.'

                let ch3 = if let Some(c) = self.peek_char(ctx) {
                    c
                }else{
                    return Ok(Some((Token::DoubleLiteral(i as f64), start_pos)));
                };
                if ch3.is_digit(10) {
                    return self.tokenize_float(ctx, i, start_pos);
                }else if ch3 == 'e' || ch3 == 'E' {
                    return self.tokenize_float_exponent(ctx, i as f64, start_pos);
                }else{
                    return Ok(Some((Token::DoubleLiteral(i as f64), start_pos)));
                }
            }else if c == 'e' || c == 'E' {
                self.next_char(ctx);  // skip 'e' or 'E'
                return self.tokenize_float_exponent(ctx, i as f64, start_pos);
            }else{
                match c {
                    'u' | 'U' => {
                        self.next_char(ctx); // skip 'u' or 'U'
                        if let Some(c4) = self.peek_char(ctx) {
                            if c4 == 'l' || c4 == 'L' {
                                self.next_char(ctx);  // skip 'l' or 'L'
                                return Ok(Some((Token::ULongLiteral(i as u64), start_pos)));
                            }else{
                                return Ok(Some((Token::UIntLiteral(i as u32), start_pos)));
                            }
                        }else{
                            return  Ok(Some((Token::UIntLiteral(i as u32), start_pos)));
                        }
                    },
                    'l' | 'L' => {
                        self.next_char(ctx);  // skip 'l' or 'L'
                        return Ok(Some((Token::LongLiteral(i as i64), start_pos)));
                    },
                    _ => {},  // do nothing
                }
                break;
            }

            self.next_char(ctx);
        }

        Ok(Some((Token::IntLiteral(i), start_pos)))
    }

    fn tokenize_digit_sub(&self, ctx: &mut TokenizerContext, base: u32, start_pos: Position) -> Result<Option<(Token, Position)>, TokenizerError> {
        let mut i: u128 = 0;

        loop {
            let opt_c = ctx.chars.peek();
            let c = if let Some(ch) = opt_c {
                ch
            }else{
                break;
            };

            if c.is_digit(base) {
                i = (i * base as u128) + c.to_digit(base).unwrap() as u128;
            }else if *c == '.' {
                self.next_char(ctx);  // skip '.'
                return self.tokenize_float(ctx, i, start_pos);
            }else if *c == '_' {
                // do nothig. just skip '_'
            }else{
                break;
            }

            self.next_char(ctx);
        }

        Ok(Some((Token::IntLiteral(i), start_pos)))
    }

    fn tokenize_float(&self, ctx: &mut TokenizerContext, integer: u128, start_pos: Position) -> Result<Option<(Token, Position)>, TokenizerError> {
        let mut f: f64 = 0f64;
        let mut ratio: f64 = 0.1;

        loop {
            let opt_c = ctx.chars.peek();
            if let Some(c) = opt_c {
                if c.is_digit(10) {
                    f = f + (c.to_digit(10).unwrap() as f64) * ratio;
                }else if *c == 'e' || *c == 'E' {
                    self.next_char(ctx);  // skip '.'
                    f = (integer as f64) + f;
                    return self.tokenize_float_exponent(ctx, f, start_pos);
                }else{
                    break;
                }
            }else{
                break;
            }

            ratio = ratio * 0.1;
            self.next_char(ctx);
        }

        f = (integer as f64) + f;
        Ok(Some((Token::DoubleLiteral(f), start_pos)))
    }

    fn tokenize_float_exponent(&self, ctx: &mut TokenizerContext, f: f64, start_pos: Position) -> Result<Option<(Token, Position)>, TokenizerError> {
        let ch = self.peek_char(ctx).ok_or(TokenizerError::syntax_error(Some(start_pos.clone())))?;
        let is_minus: bool;

        match ch {
            '+' => {
                self.next_char(ctx);  // skip '+'
                is_minus = false;
            },
            '-' => {
                self.next_char(ctx);  // skip '-'
                is_minus = true
            },
            _ => {
                is_minus = false;
            }
        }

        let mut i: u32 = 0;
        loop {
            let opt_c = self.peek_char(ctx);
            if let Some(c) = opt_c {
                if c.is_digit(10) {
                    i = (i * 10) + (c.to_digit(10).unwrap());
                }else{
                    break;
                }
            }else{
                break;
            }

            self.next_char(ctx);
        }

        let power;
        if is_minus {
            power = 0.1_f64.powf(i as f64);
        }else{
            power = 10_u32.pow(i) as f64;
        }

        let result = f * power;
        Ok(Some((Token::DoubleLiteral(result), start_pos)))
    }

    fn tokenize_char(&self, ctx: &mut TokenizerContext) -> Result<Option<(Token, Position)>, TokenizerError> {
        if let Some(c) = ctx.chars.peek() {
            if *c != '\'' {
                return Err(TokenizerError::IllegalCharWhileParsingChar(Some(ctx.pos.clone())));
            }

            let start_pos = ctx.pos.clone();
            self.next_char(ctx); // skip '\''

            if let Some(result_ch) = self.next_char(ctx) {
                if result_ch == '\\' {
                    if let Some(result) = self.next_char(ctx) {
                        if let Some(c) = self.next_char(ctx) {
                            if c == '\'' {
                                Ok(Some((Token::CharLiteral(result as i8), start_pos)))
                            }else{
                                Err(TokenizerError::IllegalCharWhileParsingCharAfter(Some(ctx.pos.clone()), c))
                            }
                        }else{
                            Err(TokenizerError::IllegalEndOfInputWhileParsingChar(Some(ctx.pos.clone())))
                        }                        

                    }else{
                        Err(TokenizerError::IllegalEndOfInputWhileParsingChar(Some(ctx.pos.clone())))
                    }

                }else{
                    if let Some(c) = self.next_char(ctx) {
                        if c == '\'' {
                            Ok(Some((Token::CharLiteral(result_ch as i8), start_pos)))
                        }else{
                            Err(TokenizerError::IllegalCharWhileParsingCharAfter(Some(ctx.pos.clone()), c))
                        }
                    }else{
                        Err(TokenizerError::IllegalEndOfInputWhileParsingChar(Some(ctx.pos.clone())))
                    }
                }
                
            }else{
                Err(TokenizerError::IllegalEndOfInputWhileParsingChar(Some(ctx.pos.clone())))
            }


        }else{
            Err(TokenizerError::IllegalEndOfInputWhileParsingChar(Some(ctx.pos.clone())))
        }
    }

    fn tokenize_string(&self, ctx: &mut TokenizerContext) -> Result<Option<(Token, Position)>, TokenizerError> {
        if let Some(c) = ctx.chars.peek() {
            if *c != '"' {
                return Err(TokenizerError::IllegalCharWhileParsingString(Some(ctx.pos.clone())));
            }

            let start_pos = ctx.pos.clone();
            self.next_char(ctx); // skip '"'

            let mut s: String = String::new();

            loop {
                let opt_c = self.next_char(ctx);
                if let Some(c) = opt_c {
                    match c {
                        '"' => {
                            break;
                        },
                        '\\' => {
                            if let Some(c) = self.next_char(ctx) {
                                s.push(c);
                            }else{
                                return Err(TokenizerError::IllegalEndOfInputWhileParsingString(Some(ctx.pos.clone())));
                            }
                        },
                        _ => s.push(c),
                    }
                }else{
                    return Err(TokenizerError::IllegalEndOfInputWhileParsingString(Some(ctx.pos.clone())));
                }
            }
    
            Ok(Some((Token::StringLiteral(s), start_pos)))

        }else{
            Err(TokenizerError::IllegalEndOfInputWhileParsingString(Some(ctx.pos.clone())))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_digit() {
        let tokenizer = Tokenizer::new();
        let result = tokenizer.tokenize("123 0");
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::IntLiteral(123));
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::IntLiteral(0));
                assert_eq!(*pos, Position {line: 1, column: 4});
            },
            Err(_err)  => panic!("can't tokenize digit"),
        }
    }

    #[test]
    fn tokenize_char() {
        let tokenizer = Tokenizer::new();
        let src = "'a' '錆' '\\\\'";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::CharLiteral('a' as i8));
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::CharLiteral('錆' as i8));
                assert_eq!(*pos, Position {line: 1, column: 4});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::CharLiteral('\\' as i8));
                assert_eq!(*pos, Position {line: 1, column: 8});
            },
            Err(_err)  => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_float() {
        let tokenizer = Tokenizer::new();
        let result = tokenizer.tokenize("3.14 12345e2 12345e-3 123.45e3 123.45e-1");
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 5);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::DoubleLiteral(3.14));
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::DoubleLiteral(1234500_f64));
                assert_eq!(*pos, Position {line: 1, column: 5});

                let (tok, pos) = &v[2];
                if let Token::DoubleLiteral(num) = tok {
                    assert!(12.3449 < *num && *num < 12.3451);
                    assert_eq!(*pos, Position {line: 1, column: 13});
                }else{
                    panic!("tokenize failed")
                }

                let (tok, pos) = &v[3];
                if let Token::DoubleLiteral(num) = tok {
                    assert!(123449_f64 < *num && *num < 123451_f64);
                    assert_eq!(*pos, Position {line: 1, column: 22});
                }else{
                    panic!("tokenize failed")
                }

                let (tok, pos) = &v[4];
                if let Token::DoubleLiteral(num) = tok {
                    assert!(12.3449 < *num && *num < 12.3451);
                    assert_eq!(*pos, Position {line: 1, column: 31});
                }else{
                    panic!("tokenize failed")
                }
            },
            Err(_err)  => panic!("can't tokenize float"),
        }
    }

    #[test]
    fn tokenize_string() {
        let tokenizer = Tokenizer::new();
        let src = "\"Hello, world!\" \"こんにちは、世界。\" \"\\\"文字列\\\"\"";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::StringLiteral("Hello, world!".to_string()));
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::StringLiteral("こんにちは、世界。".to_string()));
                assert_eq!(*pos, Position {line: 1, column: 16});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::StringLiteral("\"文字列\"".to_string()));
                assert_eq!(*pos, Position {line: 1, column: 28});
            },
            Err(_err) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_add() {
        let tokenizer = Tokenizer::new();
        let src = "+ += ++";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Add);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::AddAssign);
                assert_eq!(*pos, Position {line: 1, column: 2});
 
                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::Inc);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_sub() {
        let tokenizer = Tokenizer::new();
        let src = "- -= --";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Sub);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::SubAssign);
                assert_eq!(*pos, Position {line: 1, column: 2});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::Dec);
                assert_eq!(*pos, Position {line: 1, column: 5});
             },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_mul() {
        let tokenizer = Tokenizer::new();
        let src = "* *=";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Mul);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::MulAssign);
                assert_eq!(*pos, Position {line: 1, column: 2});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_div() {
        let tokenizer = Tokenizer::new();
        let src = "/ /=";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Div);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::DivAssign);
                assert_eq!(*pos, Position {line: 1, column: 2});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_mod() {
        let tokenizer = Tokenizer::new();
        let src = "% %=";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Mod);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::ModAssign);
                assert_eq!(*pos, Position {line: 1, column: 2});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_less() {
        let tokenizer = Tokenizer::new();
        let src = "< <= <<";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Less);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::LessEqual);
                assert_eq!(*pos, Position {line: 1, column: 2});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::ShiftLeft);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_greater() {
        let tokenizer = Tokenizer::new();
        let src = "> >= >>";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Greater);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::GreaterEqual);
                assert_eq!(*pos, Position {line: 1, column: 2});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::ShiftRight);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_assign() {
        let tokenizer = Tokenizer::new();
        let src = "= ==";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Assign);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::Equal);
                assert_eq!(*pos, Position {line: 1, column: 2});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_not() {
        let tokenizer = Tokenizer::new();
        let src = "! !=";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Not);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::NotEqual);
                assert_eq!(*pos, Position {line: 1, column: 2});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_and() {
        let tokenizer = Tokenizer::new();
        let src = "& &= &&";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::BitAnd);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::BitAndAssign);
                assert_eq!(*pos, Position {line: 1, column: 2});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::And);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_or() {
        let tokenizer = Tokenizer::new();
        let src = "| |= ||";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::BitOr);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::BitOrAssign);
                assert_eq!(*pos, Position {line: 1, column: 2});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::Or);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_xor() {
        let tokenizer = Tokenizer::new();
        let src = "^ ^=";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::BitXor);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::BitXorAssign);
                assert_eq!(*pos, Position {line: 1, column: 2});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_tilda() {
        let tokenizer = Tokenizer::new();
        let src = "~";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Tilda);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_question() {
        let tokenizer = Tokenizer::new();
        let src = "?";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Question);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_others() {
        let tokenizer = Tokenizer::new();
        let src = ": ; . -> () {} []";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 10);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Colon);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::SemiColon);
                assert_eq!(*pos, Position {line: 1, column: 2});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::Dot);
                assert_eq!(*pos, Position {line: 1, column: 4});

                let (tok, pos) = &v[3];
                assert_eq!(*tok, Token::MemberSelection);
                assert_eq!(*pos, Position {line: 1, column: 6});

                let (tok, pos) = &v[4];
                assert_eq!(*tok, Token::ParenLeft);
                assert_eq!(*pos, Position {line: 1, column: 9});

                let (tok, pos) = &v[5];
                assert_eq!(*tok, Token::ParenRight);
                assert_eq!(*pos, Position {line: 1, column: 10});

                let (tok, pos) = &v[6];
                assert_eq!(*tok, Token::BraceLeft);
                assert_eq!(*pos, Position {line: 1, column: 12});

                let (tok, pos) = &v[7];
                assert_eq!(*tok, Token::BraceRight);
                assert_eq!(*pos, Position {line: 1, column: 13});

                let (tok, pos) = &v[8];
                assert_eq!(*tok, Token::BracketLeft);
                assert_eq!(*pos, Position {line: 1, column: 15});

                let (tok, pos) = &v[9];
                assert_eq!(*tok, Token::BracketRight);
                assert_eq!(*pos, Position {line: 1, column: 16});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_keywords() {
        let tokenizer = Tokenizer::new();
        let src = "int x = 0;";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 5);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Int);
                assert_eq!(*pos, Position {line: 1, column: 0});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::Symbol(String::from("x")));
                assert_eq!(*pos, Position {line: 1, column: 4});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::Assign);
                assert_eq!(*pos, Position {line: 1, column: 6});

                let (tok, pos) = &v[3];
                assert_eq!(*tok, Token::IntLiteral(0));
                assert_eq!(*pos, Position {line: 1, column: 8});

                let (tok, pos) = &v[4];
                assert_eq!(*tok, Token::SemiColon);
                assert_eq!(*pos, Position {line: 1, column: 9});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }

        let src = "auto";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Auto);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "break";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Break);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "case";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Case);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "char";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Char);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "const";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Const);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "continue";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Continue);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "default";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Default);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "do";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Do);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "double";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Double);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "else";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Else);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "enum";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Enum);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "extern";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Extern);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "float";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Float);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "for";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::For);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "goto";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Goto);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "if";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::If);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "inline";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Inline);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "int";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Int);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "long";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Long);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "register";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Register);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "restrict";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Restrict);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "return";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Return);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "short";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Short);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "signed";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Signed);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "sizeof";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Sizeof);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "static";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Static);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "struct";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Struct);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "switch";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Switch);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "typedef";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Typedef);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "union";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Union);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "unsigned";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Unsigned);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "void";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Void);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "volatile";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Volatile);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "while";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::While);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Alignas";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Alignas);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Alignof";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Alignof);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Atomic";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Atomic);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Bool";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Bool);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Complex";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Complex);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Generic";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Generic);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Imaginary";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Imaginary);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Noreturn";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Noreturn);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Static_assert";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Static_assert);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Thread_local";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Thread_local);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "trait";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Trait);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "impl";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Impl);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "self";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_self);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "dyn";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Dyn);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "let";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Let);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "match";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Match);
                assert_eq!(*pos, Position {line: 1, column: 0});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }
}