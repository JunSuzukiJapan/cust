use once_cell::sync::Lazy;
use std::collections::HashMap;
use super::{Position, Token};
use super::TokenizerError;

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
    // m.insert(String::from("let"), Token::Let);
    // m.insert(String::from("match"), Token::Match);

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
            pos: Position::default(),
        }
    }
}

#[derive(Debug)]
pub struct Tokenizer;

impl Tokenizer {
    fn new() -> Self {
        Tokenizer {}
    }

    pub fn tokenize(input: &str) -> Result<Vec<(Token, Position)>, TokenizerError> {
        let mut ctx = TokenizerContext::new(input);
        let mut v = Vec::new();
        let tokenizer = Tokenizer::new();

        loop {
            let result = tokenizer.next_token(&mut ctx);
            match result {
                Ok(opt_tok_pos) => {
                    if let Some(tok_pos) = opt_tok_pos {
                        v.push(tok_pos);
                    }else{
                        break;
                    }
                },
                Err(err) => return Err(err),
            }
        }

        // push end-of-input Token
        v.push((Token::EndOfInput, ctx.pos));

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
                                if let Some(c2) = self.peek_char(ctx){
                                    match c2 {
                                        '=' => {
                                            self.next_char(ctx);
                                            Ok(Some((Token::ShiftLeftAssign, start_pos)))
                                        },
                                        _ => Ok(Some((Token::ShiftLeft, start_pos)))
                                    }

                                }else{
                                    Ok(Some((Token::ShiftLeft, start_pos)))
                                }
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
                                if let Some(c2) = self.peek_char(ctx){
                                    match c2 {
                                        '=' => {
                                            self.next_char(ctx);
                                            Ok(Some((Token::ShiftRightAssign, start_pos)))
                                        },
                                        _ => Ok(Some((Token::ShiftRight, start_pos)))
                                    }

                                }else{
                                    Ok(Some((Token::ShiftRight, start_pos)))
                                }
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
                        }else if c == '>' {
                            self.next_char(ctx);
                            Ok(Some((Token::WhenMatch, start_pos)))
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
                // Colon,         // ':' or '::'
                ':' => {
                    self.next_char(ctx);
                    if let Some(c) = self.peek_char(ctx) {
                        if c == ':' {
                            self.next_char(ctx);
                            Ok(Some((Token::WColon, start_pos)))
                        }else{
                            Ok(Some((Token::Colon, start_pos)))
                        }
                    }else{
                        Ok(Some((Token::Colon, start_pos)))
                    }
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

                            let ch3 = self.next_char(ctx).ok_or(TokenizerError::illegal_end_of_input(start_pos.clone()))?;
                            match ch3 {
                                '.' => Ok(Some((Token::TripleDot, start_pos))),
                                '=' => Ok(Some((Token::RangeEqual, start_pos))),
                                _ => Err(TokenizerError::syntax_error(start_pos)),
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
                '@' => {
                    self.next_char(ctx);
                    Ok(Some((Token::At, start_pos)))
                },
                '$' => {
                    self.next_char(ctx);  // skip '$'

                    if let Some(ch2) = ctx.chars.peek() {
                        match ch2 {
                            '(' => {
                                self.next_char(ctx);  // skip '('
                                Ok(Some((Token::TupleStart, start_pos)))
                            },
                            '<' => {
                                self.next_char(ctx);  // skip '<'
                                Ok(Some((Token::TupleTypeStart, start_pos)))
                            },
                            // '"' => {
                            //     self.next_char(ctx);  // skip '"'
                            //     Ok(Some((Token::UnicodeStringStart, start_pos)))
                            // },
                            // '\'' => {
                            //     self.next_char(ctx);  // skip '\''
                            //     Ok(Some((Token::UnicodeCharStart, start_pos)))
                            // },
                            _ => {
                                Ok(Some((Token::Dollar, start_pos)))
                            },
                        }

                    }else{
                        Ok(Some((Token::Dollar, start_pos)))
                    }
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
                return Err(TokenizerError::IllegalCharWhileParsingDigit(ctx.pos.clone()));
            }
        }

        let start_pos = ctx.pos.clone();
        let ch = ctx.chars.peek().ok_or(TokenizerError::illegal_end_of_input(ctx.pos.clone()))?;

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

        let mut i: i128 = 0;
        
        loop {
            let c = if let Some(ch) = self.peek_char(ctx) {
                ch
            }else{
                break;
            };

            if c.is_digit(10) {
                i = (i * 10) + (c as i128 - '0' as i128);
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
                                return Ok(Some((Token::UIntLiteral(i as u128), start_pos)));
                            }
                        }else{
                            return  Ok(Some((Token::UIntLiteral(i as u128), start_pos)));
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
        let mut i: i128 = 0;

        loop {
            let opt_c = ctx.chars.peek();
            let c = if let Some(ch) = opt_c {
                ch
            }else{
                break;
            };

            if c.is_digit(base) {
                i = (i * base as i128) + c.to_digit(base).unwrap() as i128;
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

    fn tokenize_float(&self, ctx: &mut TokenizerContext, integer: i128, start_pos: Position) -> Result<Option<(Token, Position)>, TokenizerError> {
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
        let ch = self.peek_char(ctx).ok_or(TokenizerError::syntax_error(start_pos.clone()))?;
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
                return Err(TokenizerError::IllegalCharWhileParsingChar(ctx.pos.clone()));
            }

            let start_pos = ctx.pos.clone();
            self.next_char(ctx); // skip '\''

            if let Some(result_ch) = self.next_char(ctx) {
                if result_ch == '\\' {
                    if let Some(result) = self.next_char(ctx) {
                        if let Some(c) = self.next_char(ctx) {
                            if c == '\'' {
                                Ok(Some((Token::CharLiteral(result), start_pos)))
                            }else{
                                Err(TokenizerError::IllegalCharWhileParsingCharAfter(ctx.pos.clone(), c))
                            }
                        }else{
                            Err(TokenizerError::IllegalEndOfInputWhileParsingChar(ctx.pos.clone()))
                        }                        

                    }else{
                        Err(TokenizerError::IllegalEndOfInputWhileParsingChar(ctx.pos.clone()))
                    }

                }else{
                    if let Some(c) = self.next_char(ctx) {
                        if c == '\'' {
                            Ok(Some((Token::CharLiteral(result_ch), start_pos)))
                        }else{
                            Err(TokenizerError::IllegalCharWhileParsingCharAfter(ctx.pos.clone(), c))
                        }
                    }else{
                        Err(TokenizerError::IllegalEndOfInputWhileParsingChar(ctx.pos.clone()))
                    }
                }
                
            }else{
                Err(TokenizerError::IllegalEndOfInputWhileParsingChar(ctx.pos.clone()))
            }


        }else{
            Err(TokenizerError::IllegalEndOfInputWhileParsingChar(ctx.pos.clone()))
        }
    }

    fn tokenize_string(&self, ctx: &mut TokenizerContext) -> Result<Option<(Token, Position)>, TokenizerError> {
        if let Some(c) = ctx.chars.peek() {
            if *c != '"' {
                return Err(TokenizerError::IllegalCharWhileParsingString(ctx.pos.clone()));
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
                                match c {
                                    // 'a' => s.push('\a'),
                                    // 'b' => s.push('\b'),
                                    // 'f' => s.push('\f'),
                                    'n' => s.push('\n'),
                                    'r' => s.push('\r'),
                                    't' => s.push('\t'),
                                    // 'v' => s.push('\v'),
                                    '0' => s.push('\0'),
                                    'o' => self.read_oct(&mut s, ctx)?,
                                    'x' => self.read_hex(&mut s, ctx)?,
                                    _ => s.push(c),
                                }
                            }else{
                                return Err(TokenizerError::IllegalEndOfInputWhileParsingString(ctx.pos.clone()));
                            }
                        },
                        _ => s.push(c),
                    }
                }else{
                    return Err(TokenizerError::IllegalEndOfInputWhileParsingString(ctx.pos.clone()));
                }
            }
    
            Ok(Some((Token::StringLiteral(s), start_pos)))

        }else{
            Err(TokenizerError::IllegalEndOfInputWhileParsingString(ctx.pos.clone()))
        }
    }

    fn read_oct(&self, s: &mut String, ctx: &mut TokenizerContext) -> Result<(), TokenizerError> {
        let mut oct = 0;

        if let Some(c) = self.next_char(ctx) {
            match c {
                '0' ..= '7' => {
                    oct = (c as u8 - '0' as u8) * 8;
                },
                _ => return Err(TokenizerError::NotOct(c, ctx.pos.clone()))
            }
        }else{
            return Err(TokenizerError::IllegalEndOfInputWhileParsingString(ctx.pos.clone()));
        }
        if let Some(c) = self.next_char(ctx) {
            match c {
                '0' ..= '7' => {
                    oct += c as u8 - '0' as u8;
                },
                _ => return Err(TokenizerError::NotOct(c, ctx.pos.clone()))
            }
        }else{
            return Err(TokenizerError::IllegalEndOfInputWhileParsingString(ctx.pos.clone()));
        }

        s.push(oct as char);

        Ok(())
    }

    fn read_hex(&self, s: &mut String, ctx: &mut TokenizerContext) -> Result<(), TokenizerError> {
        let mut hex: u8 = 0;

        if let Some(c) = self.next_char(ctx) {
            match c {
                '0' ..= '9' => {
                    hex = (c as u8 - '0' as u8) * 16;
                },
                'a' ..= 'f' => {
                    hex = (c as u8 - 'a' as u8 + 10) * 16;
                },
                'A' ..= 'F' => {
                    hex = (c as u8 - 'A' as u8 + 10) * 16;
                },
                _ => return Err(TokenizerError::NotHex(c, ctx.pos.clone()))
            }
        }else{
            return Err(TokenizerError::IllegalEndOfInputWhileParsingString(ctx.pos.clone()));
        }
        if let Some(c) = self.next_char(ctx) {
            match c {
                '0' ..= '9' => {
                    hex += c as u8 - '0' as u8;
                },
                'a' ..= 'f' => {
                    hex += c as u8 - 'a' as u8 + 10;
                },
                'A' ..= 'F' => {
                    hex += c as u8 - 'A' as u8 + 10;
                },
                _ => return Err(TokenizerError::NotHex(c, ctx.pos.clone()))
            }
        }else{
            return Err(TokenizerError::IllegalEndOfInputWhileParsingString(ctx.pos.clone()));
        }

        s.push(hex as char);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_digit() {
        let result = Tokenizer::tokenize("123 0");
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::IntLiteral(123));
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::IntLiteral(0));
                assert_eq!(*pos, Position {line: 1, column: 5});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 6});
            },
            Err(_err)  => panic!("can't tokenize digit"),
        }
    }

    #[test]
    fn tokenize_char() {
        let src = "'a' '錆' '\\\\'";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 4);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::CharLiteral('a'));
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::CharLiteral('錆'));
                assert_eq!(*pos, Position {line: 1, column: 5});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::CharLiteral('\\'));
                assert_eq!(*pos, Position {line: 1, column: 9});

                let (tok, pos) = &v[3];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 13});
            },
            Err(_err)  => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_float() {
        let result = Tokenizer::tokenize("3.14 12345e2 12345e-3 123.45e3 123.45e-1");
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 6);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::DoubleLiteral(3.14));
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::DoubleLiteral(1234500_f64));
                assert_eq!(*pos, Position {line: 1, column: 6});

                let (tok, pos) = &v[2];
                if let Token::DoubleLiteral(num) = tok {
                    assert!(12.3449 < *num && *num < 12.3451);
                    assert_eq!(*pos, Position {line: 1, column: 14});
                }else{
                    panic!("tokenize failed")
                }

                let (tok, pos) = &v[3];
                if let Token::DoubleLiteral(num) = tok {
                    assert!(123449_f64 < *num && *num < 123451_f64);
                    assert_eq!(*pos, Position {line: 1, column: 23});
                }else{
                    panic!("tokenize failed")
                }

                let (tok, pos) = &v[4];
                if let Token::DoubleLiteral(num) = tok {
                    assert!(12.3449 < *num && *num < 12.3451);
                    assert_eq!(*pos, Position {line: 1, column: 32});
                }else{
                    panic!("tokenize failed")
                }

                let (tok, pos) = &v[5];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 41});
            },
            Err(_err)  => panic!("can't tokenize float"),
        }
    }

    #[test]
    fn tokenize_string() {
        let src = "\"Hello, world!\" \"こんにちは、世界。\" \"\\\"文字列\\\"\"";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 4);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::StringLiteral("Hello, world!".to_string()));
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::StringLiteral("こんにちは、世界。".to_string()));
                assert_eq!(*pos, Position {line: 1, column: 17});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::StringLiteral("\"文字列\"".to_string()));
                assert_eq!(*pos, Position {line: 1, column: 29});

                let (tok, pos) = &v[3];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 38});
            },
            Err(_err) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_add() {
        let src = "+ += ++";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 4);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Add);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::AddAssign);
                assert_eq!(*pos, Position {line: 1, column: 3});
 
                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::Inc);
                assert_eq!(*pos, Position {line: 1, column: 6});

                let (tok, pos) = &v[3];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 8});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_sub() {
        let src = "- -= --";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 4);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Sub);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::SubAssign);
                assert_eq!(*pos, Position {line: 1, column: 3});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::Dec);
                assert_eq!(*pos, Position {line: 1, column: 6});

                let (tok, pos) = &v[3];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 8});
             },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_mul() {
        let src = "* *=";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Mul);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::MulAssign);
                assert_eq!(*pos, Position {line: 1, column: 3});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_div() {
        let src = "/ /=";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Div);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::DivAssign);
                assert_eq!(*pos, Position {line: 1, column: 3});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_mod() {
        let src = "% %=";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Mod);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::ModAssign);
                assert_eq!(*pos, Position {line: 1, column: 3});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_less() {
        let src = "< <= <<";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 4);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Less);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::LessEqual);
                assert_eq!(*pos, Position {line: 1, column: 3});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::ShiftLeft);
                assert_eq!(*pos, Position {line: 1, column: 6});

                let (tok, pos) = &v[3];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 8});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_greater() {
        let src = "> >= >>";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 4);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Greater);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::GreaterEqual);
                assert_eq!(*pos, Position {line: 1, column: 3});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::ShiftRight);
                assert_eq!(*pos, Position {line: 1, column: 6});

                let (tok, pos) = &v[3];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 8});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_assign() {
        let src = "= ==";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Assign);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::Equal);
                assert_eq!(*pos, Position {line: 1, column: 3});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_not() {
        let src = "! !=";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Not);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::NotEqual);
                assert_eq!(*pos, Position {line: 1, column: 3});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_and() {
        let src = "& &= &&";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 4);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::BitAnd);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::BitAndAssign);
                assert_eq!(*pos, Position {line: 1, column: 3});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::And);
                assert_eq!(*pos, Position {line: 1, column: 6});

                let (tok, pos) = &v[3];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 8});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_or() {
        let src = "| |= ||";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 4);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::BitOr);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::BitOrAssign);
                assert_eq!(*pos, Position {line: 1, column: 3});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::Or);
                assert_eq!(*pos, Position {line: 1, column: 6});

                let (tok, pos) = &v[3];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 8});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_xor() {
        let src = "^ ^=";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 3);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::BitXor);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::BitXorAssign);
                assert_eq!(*pos, Position {line: 1, column: 3});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_tilda() {
        let src = "~";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Tilda);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 2});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_operator_question() {
        let src = "?";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Question);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 2});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_at() {
        let result = Tokenizer::tokenize("@");
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::At);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 2});
            },
            Err(_err)  => panic!("can't tokenize '@'"),
        }
    }

    #[test]
    fn tokenize_operator_others() {
        let src = ": ; . -> () {} [] :: ... ..=";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 14);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Colon);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::SemiColon);
                assert_eq!(*pos, Position {line: 1, column: 3});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::Dot);
                assert_eq!(*pos, Position {line: 1, column: 5});

                let (tok, pos) = &v[3];
                assert_eq!(*tok, Token::MemberSelection);
                assert_eq!(*pos, Position {line: 1, column: 7});

                let (tok, pos) = &v[4];
                assert_eq!(*tok, Token::ParenLeft);
                assert_eq!(*pos, Position {line: 1, column: 10});

                let (tok, pos) = &v[5];
                assert_eq!(*tok, Token::ParenRight);
                assert_eq!(*pos, Position {line: 1, column: 11});

                let (tok, pos) = &v[6];
                assert_eq!(*tok, Token::BraceLeft);
                assert_eq!(*pos, Position {line: 1, column: 13});

                let (tok, pos) = &v[7];
                assert_eq!(*tok, Token::BraceRight);
                assert_eq!(*pos, Position {line: 1, column: 14});

                let (tok, pos) = &v[8];
                assert_eq!(*tok, Token::BracketLeft);
                assert_eq!(*pos, Position {line: 1, column: 16});

                let (tok, pos) = &v[9];
                assert_eq!(*tok, Token::BracketRight);
                assert_eq!(*pos, Position {line: 1, column: 17});

                let (tok, pos) = &v[10];
                assert_eq!(*tok, Token::WColon);
                assert_eq!(*pos, Position {line: 1, column: 19});

                let (tok, pos) = &v[11];
                assert_eq!(*tok, Token::TripleDot);
                assert_eq!(*pos, Position {line: 1, column: 22});

                let (tok, pos) = &v[12];
                assert_eq!(*tok, Token::RangeEqual);
                assert_eq!(*pos, Position {line: 1, column: 26});

                let (tok, pos) = &v[13];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 29});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
    }

    #[test]
    fn tokenize_keywords() {
        let src = "int x = 0;";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 6);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Int);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::Symbol(String::from("x")));
                assert_eq!(*pos, Position {line: 1, column: 5});

                let (tok, pos) = &v[2];
                assert_eq!(*tok, Token::Assign);
                assert_eq!(*pos, Position {line: 1, column: 7});

                let (tok, pos) = &v[3];
                assert_eq!(*tok, Token::IntLiteral(0));
                assert_eq!(*pos, Position {line: 1, column: 9});

                let (tok, pos) = &v[4];
                assert_eq!(*tok, Token::SemiColon);
                assert_eq!(*pos, Position {line: 1, column: 10});

                let (tok, pos) = &v[5];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 11});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }

        let src = "auto";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Auto);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "break";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Break);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 6});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "case";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Case);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "char";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Char);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "const";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Const);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 6});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "continue";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Continue);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 9});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "default";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Default);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 8});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "do";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Do);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 3});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "double";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Double);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 7});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "else";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Else);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "enum";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Enum);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "extern";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Extern);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 7});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "float";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Float);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 6});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "for";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::For);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 4});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "goto";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Goto);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "if";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::If);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 3});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "inline";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Inline);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 7});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "int";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Int);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 4});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "long";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Long);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "register";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Register);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 9});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "restrict";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Restrict);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 9});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "return";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Return);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 7});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "short";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Short);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 6});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "signed";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Signed);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 7});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "sizeof";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Sizeof);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 7});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "static";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Static);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 7});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "struct";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Struct);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 7});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "switch";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Switch);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 7});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "typedef";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Typedef);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 8});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "union";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Union);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 6});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "unsigned";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Unsigned);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 9});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "void";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Void);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "volatile";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Volatile);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 9});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "while";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::While);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 6});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Alignas";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Alignas);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 9});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Alignof";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Alignof);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 9});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Atomic";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Atomic);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 8});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Bool";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Bool);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 6});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Complex";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Complex);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 9});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Generic";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Generic);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 9});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Imaginary";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Imaginary);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 11});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Noreturn";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Noreturn);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 10});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Static_assert";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Static_assert);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 15});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "_Thread_local";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_Thread_local);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 14});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "trait";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Trait);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 6});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "impl";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Impl);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "self";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::_self);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 5});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        let src = "dyn";
        let result = Tokenizer::tokenize(src);
        match result {
            Ok(v) => {
                assert_eq!(v.len(), 2);

                let (tok, pos) = &v[0];
                assert_eq!(*tok, Token::Dyn);
                assert_eq!(*pos, Position {line: 1, column: 1});

                let (tok, pos) = &v[1];
                assert_eq!(*tok, Token::EndOfInput);
                assert_eq!(*pos, Position {line: 1, column: 4});
            },
            Err(_) => panic!("can't tokenize {}", src),
        }
 
        // let src = "let";
        // let result = Tokenizer::tokenize(src);
        // match result {
        //     Ok(v) => {
        //         assert_eq!(v.len(), 2);

        //         let (tok, pos) = &v[0];
        //         assert_eq!(*tok, Token::Let);
        //         assert_eq!(*pos, Position {line: 1, column: 1});

        //         let (tok, pos) = &v[1];
        //         assert_eq!(*tok, Token::EndOfInput);
        //         assert_eq!(*pos, Position {line: 1, column: 4});
        //     },
        //     Err(_) => panic!("can't tokenize {}", src),
        // }
 
        // let src = "match";
        // let result = Tokenizer::tokenize(src);
        // match result {
        //     Ok(v) => {
        //         assert_eq!(v.len(), 2);

        //         let (tok, pos) = &v[0];
        //         assert_eq!(*tok, Token::Match);
        //         assert_eq!(*pos, Position {line: 1, column: 1});

        //         let (tok, pos) = &v[1];
        //         assert_eq!(*tok, Token::EndOfInput);
        //         assert_eq!(*pos, Position {line: 1, column: 6});
        //     },
        //     Err(_) => panic!("can't tokenize {}", src),
        // }
    }
}