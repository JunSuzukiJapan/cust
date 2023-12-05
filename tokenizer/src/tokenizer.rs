#![allow(dead_code)]

use once_cell::sync::Lazy;
use std::collections::HashMap;
use super::TokenizerError;
use super::Location;
use super::TokenType;
use super::Token;

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
static Keywords: Lazy<HashMap<String, TokenType>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert(String::from("auto"), TokenType::Auto);
    m.insert(String::from("break"), TokenType::Break);
    m.insert(String::from("case"), TokenType::Case);
    m.insert(String::from("char"), TokenType::Char);
    m.insert(String::from("const"), TokenType::Const);
    m.insert(String::from("continue"), TokenType::Continue);
    m.insert(String::from("default"), TokenType::Default);
    m.insert(String::from("do"), TokenType::Do);
    m.insert(String::from("double"), TokenType::Double);
    m.insert(String::from("else"), TokenType::Else);
    m.insert(String::from("enum"), TokenType::Enum);
    m.insert(String::from("extern"), TokenType::Extern);
    m.insert(String::from("float"), TokenType::Float);
    m.insert(String::from("for"), TokenType::For);
    m.insert(String::from("goto"), TokenType::Goto);
    m.insert(String::from("if"), TokenType::If);
    m.insert(String::from("inline"), TokenType::Inline);
    m.insert(String::from("int"), TokenType::Int);
    m.insert(String::from("long"), TokenType::Long);
    m.insert(String::from("register"), TokenType::Register);
    m.insert(String::from("restrict"), TokenType::Restrict);
    m.insert(String::from("return"), TokenType::Return);
    m.insert(String::from("short"), TokenType::Short);
    m.insert(String::from("signed"), TokenType::Signed);
    m.insert(String::from("sizeof"), TokenType::Sizeof);
    m.insert(String::from("static"), TokenType::Static);
    m.insert(String::from("struct"), TokenType::Struct);
    m.insert(String::from("switch"), TokenType::Switch);
    m.insert(String::from("typedef"), TokenType::Typedef);
    m.insert(String::from("union"), TokenType::Union);
    m.insert(String::from("unsigned"), TokenType::Unsigned);
    m.insert(String::from("void"), TokenType::Void);
    m.insert(String::from("volatile"), TokenType::Volatile);
    m.insert(String::from("while"), TokenType::While);
    m.insert(String::from("_Alignas"), TokenType::_Alignas);
    m.insert(String::from("_Alignof"), TokenType::_Alignof);
    m.insert(String::from("_Atomic"), TokenType::_Atomic);
    m.insert(String::from("_Bool"), TokenType::_Bool);
    m.insert(String::from("_Complex"), TokenType::_Complex);
    m.insert(String::from("_Generic"), TokenType::_Generic);
    m.insert(String::from("_Imaginary"), TokenType::_Imaginary);
    m.insert(String::from("_Noreturn"), TokenType::_Noreturn);
    m.insert(String::from("_Static_assert"), TokenType::_Static_assert);
    m.insert(String::from("_Thread_local"), TokenType::_Thread_local);
    m.insert(String::from("trait"), TokenType::Trait);
    m.insert(String::from("impl"), TokenType::Impl);
    m.insert(String::from("self"), TokenType::_self);
    m.insert(String::from("Self"), TokenType::_Self);
    m.insert(String::from("dyn"), TokenType::Dyn);
    m.insert(String::from("let"), TokenType::Let);
    m.insert(String::from("match"), TokenType::Match);

    m
});

struct TokenizerContext<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    pos: Location,
}

impl<'a> TokenizerContext<'a> {
    pub fn new(input: &str) -> TokenizerContext {
        TokenizerContext {
            chars: input.chars().peekable(),
            pos: Location::new(),
        }
    }
}

#[derive(Debug)]
pub struct Tokenizer;

impl Tokenizer {
    pub fn new() -> Self {
        Tokenizer {}
    }

    // pub fn tokenize(&self, input: &str) -> Result<Vec<(TokenType, Location)>, TokenizerError> {
    pub fn tokenize(&self, input: &str) -> Result<Vec<Token>, TokenizerError> {
        // let mut chars = input.chars().peekable();
        // let mut pos = Location::new();
        let mut ctx = TokenizerContext::new(input);
        let mut v = Vec::new();

        loop {
            let result = self.next_token(&mut ctx);
            match result {
                Ok(opt_tok_pos) => if let Some((typ, loc)) = opt_tok_pos {
                        let tok = Token::new(typ, loc);
                        v.push(tok);
                    }else{
                        break;
                    },
                Err(err) => return Err(err),
            }
        }

        Ok(v)
    }

    fn next_token(&self, ctx: &mut TokenizerContext) -> Result<Option<(TokenType, Location)>, TokenizerError> {
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
                                Ok(Some((TokenType::AddAssign, start_pos)))
                            },
                            '+' => {
                                self.next_char(ctx);
                                Ok(Some((TokenType::Inc, start_pos)))
                            },
                            _ => Ok(Some((TokenType::Add, start_pos)))
                        }

                    }else{
                        Ok(Some((TokenType::Add, start_pos)))
                    }
                },
                '-' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        match c {
                            '=' => {
                                self.next_char(ctx);
                                Ok(Some((TokenType::SubAssign, start_pos)))
                            },
                            '-' => {
                                self.next_char(ctx);
                                Ok(Some((TokenType::Dec, start_pos)))
                            },
                            '>' => {
                                self.next_char(ctx);
                                Ok(Some((TokenType::MemberSelection, start_pos)))
                            },
                            _ => Ok(Some((TokenType::Sub, start_pos)))
                        }

                    }else{
                        Ok(Some((TokenType::Sub, start_pos)))
                    }
                },
                '*' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == '=' {
                            self.next_char(ctx);
                            Ok(Some((TokenType::MulAssign, start_pos)))
                        }else{
                            Ok(Some((TokenType::Mul, start_pos)))
                        }
                    }else{
                        Ok(Some((TokenType::Mul, start_pos)))
                    }
                },
                '/' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == '=' {
                            self.next_char(ctx);
                            Ok(Some((TokenType::DivAssign, start_pos)))
                        }else{
                            Ok(Some((TokenType::Div, start_pos)))
                        }
                    }else{
                        Ok(Some((TokenType::Div, start_pos)))
                    }
                },
                '%' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == '=' {
                            self.next_char(ctx);
                            Ok(Some((TokenType::ModAssign, start_pos)))
                        }else{
                            Ok(Some((TokenType::Mod, start_pos)))
                        }
                    }else{
                        Ok(Some((TokenType::Mod, start_pos)))
                    }
                },
                '<' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        match c {
                            '<' => {
                                self.next_char(ctx);
                                Ok(Some((TokenType::ShiftLeft, start_pos)))
                            },
                            '=' => {
                                self.next_char(ctx);
                                Ok(Some((TokenType::LessEqual, start_pos)))
                            },
                            _ => Ok(Some((TokenType::Less, start_pos)))
                        }
                    }else{
                        Ok(Some((TokenType::Less, start_pos)))
                    }
                },
                '>' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        match c {
                            '>' => {
                                self.next_char(ctx);
                                Ok(Some((TokenType::ShiftRight, start_pos)))
                            },
                            '=' => {
                                self.next_char(ctx);
                                Ok(Some((TokenType::GreaterEqual, start_pos)))
                            },
                            _ => Ok(Some((TokenType::Greater, start_pos)))
                        }
                    }else{
                        Ok(Some((TokenType::Greater, start_pos)))
                    }
                },
                '=' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == '=' {
                            self.next_char(ctx);
                            Ok(Some((TokenType::Equal, start_pos)))
                        }else{
                            Ok(Some((TokenType::Assign, start_pos)))
                        }
                    }else{
                        Ok(Some((TokenType::Assign, start_pos)))
                    }
                },
                '!' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == '=' {
                            self.next_char(ctx);
                            Ok(Some((TokenType::NotEqual, start_pos)))
                        }else{
                            Ok(Some((TokenType::Not, start_pos)))
                        }
                    }else{
                        Ok(Some((TokenType::Not, start_pos)))
                    }
                },
                '&' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        match c {
                            '&' => {
                                self.next_char(ctx);
                                Ok(Some((TokenType::And, start_pos)))
                            },
                            '=' => {
                                self.next_char(ctx);
                                Ok(Some((TokenType::BitAndAssign, start_pos)))
                            },
                            _ => Ok(Some((TokenType::BitAnd, start_pos)))
                        }
                    }else{
                        Ok(Some((TokenType::BitAnd, start_pos)))
                    }
                },
                '|' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        match c {
                            '|' => {
                                self.next_char(ctx);
                                Ok(Some((TokenType::Or, start_pos)))
                            },
                            '=' => {
                                self.next_char(ctx);
                                Ok(Some((TokenType::BitOrAssign, start_pos)))
                            },
                            _ => Ok(Some((TokenType::BitOr, start_pos)))
                        }
                    }else{
                        Ok(Some((TokenType::BitAnd, start_pos)))
                    }
                },
                '^' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == '=' {
                            self.next_char(ctx);
                            Ok(Some((TokenType::BitXorAssign, start_pos)))
                        }else{
                            Ok(Some((TokenType::BitXor, start_pos)))
                        }
                    }else{
                        Ok(Some((TokenType::BitXor, start_pos)))
                    }
                },
                '~' => {
                    self.next_char(ctx);
                    Ok(Some((TokenType::Tilda, start_pos)))
                },
                '?' => {
                    self.next_char(ctx);
                    Ok(Some((TokenType::Question, start_pos)))
                },
                // Colon,         // ':'
                ':' => {
                    self.next_char(ctx);
                    Ok(Some((TokenType::Colon, start_pos)))
                },
                // SemiColon,     // ';'
                ';' => {
                    self.next_char(ctx);
                    Ok(Some((TokenType::SemiColon, start_pos)))
                },
                // Dot,           // '.'
                '.' => {
                    self.next_char(ctx);

                    if let Some(ch2) = ctx.chars.peek() {
                        if *ch2 == '.' {
                            self.next_char(ctx);  // skip '.'
                            let ch3 = self.next_char(ctx).ok_or(TokenizerError::IllegalEndOfInput(Some(start_pos.clone())))?;
                            if ch3 == '.' {
                                Ok(Some((TokenType::TripleDot, start_pos)))
                            }else{
                                Err(TokenizerError::SyntaxError(Some(start_pos), file!(), line!(), column!()))
                            }

                        }else{
                            Ok(Some((TokenType::Dot, start_pos)))
                        }

                    }else{
                        Ok(Some((TokenType::Dot, start_pos)))
                    }
                },
                ',' => {
                    self.next_char(ctx);
                    Ok(Some((TokenType::Comma, start_pos)))
                },
                // ParenLeft,     // '('
                '(' => {
                    self.next_char(ctx);
                    Ok(Some((TokenType::ParenLeft, start_pos)))
                },
                // ParenRight,    // ')'
                ')' => {
                    self.next_char(ctx);
                    Ok(Some((TokenType::ParenRight, start_pos)))
                },
                // BraceLeft,     // '{'
                '{' => {
                    self.next_char(ctx);
                    Ok(Some((TokenType::BraceLeft, start_pos)))
                },
                // BraceRight,    // '}'
                '}' => {
                    self.next_char(ctx);
                    Ok(Some((TokenType::BraceRight, start_pos)))
                },
                // BracketLeft,   // '['
                '[' => {
                    self.next_char(ctx);
                    Ok(Some((TokenType::BracketLeft, start_pos)))
                },
                // BracketRight,  // ']'
                ']' => {
                    self.next_char(ctx);
                    Ok(Some((TokenType::BracketRight, start_pos)))
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
                        Ok(Some((TokenType::Symbol(s), start_pos)))
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

    fn tokenize_digit(&self, ctx: &mut TokenizerContext) -> Result<Option<(TokenType, Location)>, TokenizerError> {
        if let Some(c) = ctx.chars.peek() {
            if ! c.is_digit(10) {
                return Err(TokenizerError::IllegalCharWhileParsingDigit(Some(ctx.pos.clone())));
            }
        }

        let start_pos = ctx.pos.clone();
        let ch = ctx.chars.peek().ok_or(TokenizerError::IllegalEndOfInput(Some(ctx.pos.clone())))?;

        match ch {
            '0' => {
                self.next_char(ctx);  // skip '0'

                let ch2 = if let Some(c) = ctx.chars.peek() {
                    c
                }else{
                    return Ok(Some((TokenType::IntLiteral(0), start_pos)));
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
                            return Ok(Some((TokenType::IntLiteral(0), start_pos)))
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
                    return Ok(Some((TokenType::DoubleLiteral(i as f64), start_pos)));
                };
                if ch3.is_digit(10) {
                    return self.tokenize_float(ctx, i, start_pos);
                }else if ch3 == 'e' || ch3 == 'E' {
                    return self.tokenize_float_exponent(ctx, i as f64, start_pos);
                }else{
                    return Ok(Some((TokenType::DoubleLiteral(i as f64), start_pos)));
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
                                return Ok(Some((TokenType::ULongLiteral(i as u64), start_pos)));
                            }else{
                                return Ok(Some((TokenType::UIntLiteral(i as u32), start_pos)));
                            }
                        }else{
                            return  Ok(Some((TokenType::UIntLiteral(i as u32), start_pos)));
                        }
                    },
                    'l' | 'L' => {
                        self.next_char(ctx);  // skip 'l' or 'L'
                        return Ok(Some((TokenType::LongLiteral(i as i64), start_pos)));
                    },
                    _ => {},  // do nothing
                }
                break;
            }

            self.next_char(ctx);
        }

        Ok(Some((TokenType::IntLiteral(i), start_pos)))
    }

    fn tokenize_digit_sub(&self, ctx: &mut TokenizerContext, base: u32, start_pos: Location) -> Result<Option<(TokenType, Location)>, TokenizerError> {
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

        Ok(Some((TokenType::IntLiteral(i), start_pos)))
    }

    fn tokenize_float(&self, ctx: &mut TokenizerContext, integer: u128, start_pos: Location) -> Result<Option<(TokenType, Location)>, TokenizerError> {
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
        Ok(Some((TokenType::DoubleLiteral(f), start_pos)))
    }

    fn tokenize_float_exponent(&self, ctx: &mut TokenizerContext, f: f64, start_pos: Location) -> Result<Option<(TokenType, Location)>, TokenizerError> {
        let ch = self.peek_char(ctx).ok_or(TokenizerError::SyntaxError(Some(start_pos.clone()), file!(), line!(), column!()))?;
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
        Ok(Some((TokenType::DoubleLiteral(result), start_pos)))
    }

    fn tokenize_char(&self, ctx: &mut TokenizerContext) -> Result<Option<(TokenType, Location)>, TokenizerError> {
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
                                Ok(Some((TokenType::CharLiteral(result as i8), start_pos)))
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
                            Ok(Some((TokenType::CharLiteral(result_ch as i8), start_pos)))
                        }else{
                            Err(TokenizerError::IllegalEndOfInputWhileParsingCharAfter(Some(ctx.pos.clone()), c))
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

    fn tokenize_string(&self, ctx: &mut TokenizerContext) -> Result<Option<(TokenType, Location)>, TokenizerError> {
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
    
            Ok(Some((TokenType::StringLiteral(s), start_pos)))

        }else{
            Err(TokenizerError::IllegalCharWhileParsingString(Some(ctx.pos.clone())))
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::IntLiteral(123));
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::IntLiteral(0));
                assert_eq!(*tok.get_location(), Location {line: 1, column: 5});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::CharLiteral('a' as i8));
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::CharLiteral('錆' as i8));
                assert_eq!(*tok.get_location(), Location {line: 1, column: 5});

                let tok = &v[2];
                assert_eq!(*tok.get_type(), TokenType::CharLiteral('\\' as i8));
                assert_eq!(*tok.get_location(), Location {line: 1, column: 9});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::DoubleLiteral(3.14));
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::DoubleLiteral(1234500_f64));
                assert_eq!(*tok.get_location(), Location {line: 1, column: 6});

                let tok = &v[2];
                if let TokenType::DoubleLiteral(num) = tok.get_type() {
                    assert!(12.3449 < *num && *num < 12.3451);
                    assert_eq!(*tok.get_location(), Location {line: 1, column: 14});
                }else{
                    panic!("tokenize failed")
                }

                let tok = &v[3];
                if let TokenType::DoubleLiteral(num) = tok.get_type() {
                    assert!(123449_f64 < *num && *num < 123451_f64);
                    assert_eq!(*tok.get_location(), Location {line: 1, column: 23});
                }else{
                    panic!("tokenize failed")
                }

                let tok = &v[4];
                if let TokenType::DoubleLiteral(num) = tok.get_type() {
                    assert!(12.3449 < *num && *num < 12.3451);
                    assert_eq!(*tok.get_location(), Location {line: 1, column: 32});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::StringLiteral("Hello, world!".to_string()));
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::StringLiteral("こんにちは、世界。".to_string()));
                assert_eq!(*tok.get_location(), Location {line: 1, column: 17});

                let tok = &v[2];
                assert_eq!(*tok.get_type(), TokenType::StringLiteral("\"文字列\"".to_string()));
                assert_eq!(*tok.get_location(), Location {line: 1, column: 29});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Add);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::AddAssign);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});
 
                let tok = &v[2];
                assert_eq!(*tok.get_type(), TokenType::Inc);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 6});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Sub);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::SubAssign);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});

                let tok = &v[2];
                assert_eq!(*tok.get_type(), TokenType::Dec);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 6});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Mul);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::MulAssign);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Div);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::DivAssign);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Mod);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::ModAssign);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Less);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::LessEqual);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});

                let tok = &v[2];
                assert_eq!(*tok.get_type(), TokenType::ShiftLeft);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 6});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Greater);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::GreaterEqual);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});

                let tok = &v[2];
                assert_eq!(*tok.get_type(), TokenType::ShiftRight);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 6});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Assign);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::Equal);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Not);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::NotEqual);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::BitAnd);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::BitAndAssign);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});

                let tok = &v[2];
                assert_eq!(*tok.get_type(), TokenType::And);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 6});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::BitOr);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::BitOrAssign);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});

                let tok = &v[2];
                assert_eq!(*tok.get_type(), TokenType::Or);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 6});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::BitXor);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::BitXorAssign);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Tilda);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Question);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Colon);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::SemiColon);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 3});

                let tok = &v[2];
                assert_eq!(*tok.get_type(), TokenType::Dot);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 5});

                let tok = &v[3];
                assert_eq!(*tok.get_type(), TokenType::MemberSelection);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 7});

                let tok = &v[4];
                assert_eq!(*tok.get_type(), TokenType::ParenLeft);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 10});

                let tok = &v[5];
                assert_eq!(*tok.get_type(), TokenType::ParenRight);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 11});

                let tok = &v[6];
                assert_eq!(*tok.get_type(), TokenType::BraceLeft);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 13});

                let tok = &v[7];
                assert_eq!(*tok.get_type(), TokenType::BraceRight);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 14});

                let tok = &v[8];
                assert_eq!(*tok.get_type(), TokenType::BracketLeft);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 16});

                let tok = &v[9];
                assert_eq!(*tok.get_type(), TokenType::BracketRight);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 17});
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

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Int);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});

                let tok = &v[1];
                assert_eq!(*tok.get_type(), TokenType::Symbol(String::from("x")));
                assert_eq!(*tok.get_location(), Location {line: 1, column: 5});

                let tok = &v[2];
                assert_eq!(*tok.get_type(), TokenType::Assign);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 7});

                let tok = &v[3];
                assert_eq!(*tok.get_type(), TokenType::IntLiteral(0));
                assert_eq!(*tok.get_location(), Location {line: 1, column: 9});

                let tok = &v[4];
                assert_eq!(*tok.get_type(), TokenType::SemiColon);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 10});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }

        let src = "auto";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Auto);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "break";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Break);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "case";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Case);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "char";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Char);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "const";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Const);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "continue";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Continue);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "default";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Default);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "do";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Do);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "double";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Double);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "else";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Else);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "enum";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Enum);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "extern";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Extern);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "float";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Float);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "for";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::For);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "goto";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Goto);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "if";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::If);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "inline";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Inline);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "int";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Int);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "long";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Long);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "register";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Register);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "restrict";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Restrict);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "return";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Return);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "short";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Short);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "signed";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Signed);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "sizeof";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Sizeof);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "static";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Static);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "struct";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Struct);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "switch";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Switch);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "typedef";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Typedef);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "union";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Union);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "unsigned";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Unsigned);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "void";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Void);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "volatile";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Volatile);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "while";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::While);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Alignas";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::_Alignas);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Alignof";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::_Alignof);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Atomic";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::_Atomic);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Bool";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::_Bool);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Complex";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::_Complex);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Generic";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::_Generic);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Imaginary";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::_Imaginary);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Noreturn";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::_Noreturn);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Static_assert";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::_Static_assert);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Thread_local";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::_Thread_local);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "trait";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Trait);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "impl";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Impl);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "self";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::_self);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "dyn";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Dyn);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "let";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Let);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "match";
        let result = tokenizer.tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 1);

                let tok = &v[0];
                assert_eq!(*tok.get_type(), TokenType::Match);
                assert_eq!(*tok.get_location(), Location {line: 1, column: 1});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }
}