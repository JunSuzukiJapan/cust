//! Constant-expression evaluator for `#if` / `#elif`.
//!
//! `#if` arithmetic is `intmax_t`/`uintmax_t` (64-bit, like clang): every
//! value is 64-bit signed or unsigned, and the usual conversions only pick
//! the signedness. Signed overflow wraps (two's complement) with an
//! "integer overflow" warning; division by zero is an error, as are
//! out-of-range shifts except that an out-of-range count warns and yields
//! 0 (or -1 for a negative value shifted right). `&&`, `||` and `?:`
//! evaluate only the taken side. Bare identifiers (after macro expansion)
//! are 0; `true`/`false` are 0 too (C17 has no predefined boolean macros).
//!
//! Warnings (overflow) go through a caller-supplied callback; the driver
//! renders them to stderr. Only `+`/`-`/`*` overflow and out-of-range
//! shifts warn — bit-loss inside `<<` and negation wrap silently.

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalError {
    pub message: String,
}

impl EvalError {
    fn new(message: impl Into<String>) -> Self {
        EvalError {
            message: message.into(),
        }
    }
}

/// An `#if` integer: two's-complement bits plus the signedness and width
/// the usual arithmetic conversions need. `bits` is always masked to width.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct PpInt {
    bits: u64,
    signed: bool,
    width: u32, // 32 or 64
}

impl PpInt {
    fn mask(width: u32) -> u64 {
        if width >= 64 {
            u64::MAX
        } else {
            (1u64 << width) - 1
        }
    }

    fn new(raw: u64, signed: bool, width: u32) -> Self {
        PpInt {
            bits: raw & Self::mask(width),
            signed,
            width,
        }
    }

    /// Comparison/logical result: `int` 0 or 1.
    fn int1(b: bool) -> Self {
        PpInt::new(b as u64, true, 64)
    }

    /// Value as i128 (sign-extended when signed).
    fn as_i128(self) -> i128 {
        if self.signed {
            (((self.bits << (64 - self.width)) as i64) >> (64 - self.width)) as i128
        } else {
            self.bits as i128
        }
    }

    fn as_u128(self) -> u128 {
        self.bits as u128
    }

    fn is_nonzero(self) -> bool {
        self.bits != 0
    }
}

/// Usual arithmetic conversions: the common (signedness, width).
fn common_type(a: PpInt, b: PpInt) -> (bool, u32) {
    if a.signed == b.signed {
        (a.signed, a.width.max(b.width))
    } else {
        let (s, u) = if a.signed { (a, b) } else { (b, a) };
        if u.width >= s.width {
            (false, u.width)
        } else {
            // The signed operand is wider, so it represents every value
            // of the unsigned one (only 32/64-bit ranks exist here).
            (true, s.width)
        }
    }
}

fn binary(op: Op, a: PpInt, b: PpInt, warn: &dyn Fn(&str)) -> Result<PpInt, EvalError> {
    let (signed, width) = common_type(a, b);
    let order = |x: PpInt, y: PpInt| {
        if signed {
            x.as_i128().cmp(&y.as_i128())
        } else {
            x.as_u128().cmp(&y.as_u128())
        }
    };
    let arithmetic = |f: fn(u128, u128) -> u128, g: fn(i128, i128) -> i128| {
        let (v, overflow) = if signed {
            let v = g(a.as_i128(), b.as_i128());
            (
                v as u64,
                v < i64::MIN as i128 || v > i64::MAX as i128,
            )
        } else {
            let v = f(a.as_u128(), b.as_u128());
            (v as u64, v > u64::MAX as u128)
        };
        if overflow {
            warn("integer overflow in preprocessor expression");
        }
        PpInt::new(v, signed, width)
    };
    match op {
        Op::Add => Ok(arithmetic(u128::wrapping_add, i128::wrapping_add)),
        Op::Sub => Ok(arithmetic(u128::wrapping_sub, i128::wrapping_sub)),
        Op::Mul => Ok(arithmetic(u128::wrapping_mul, i128::wrapping_mul)),
        Op::Div | Op::Mod => {
            if !b.is_nonzero() {
                let what = if op == Op::Div { "division" } else { "remainder" };
                return Err(EvalError::new(format!(
                    "{what} by zero in preprocessor expression"
                )));
            }
            // wrapping_* also covers INT_MIN / -1 (wraps, like the rest).
            Ok(match op {
                Op::Div => arithmetic(u128::wrapping_div, i128::wrapping_div),
                _ => arithmetic(u128::wrapping_rem, i128::wrapping_rem),
            })
        }
        Op::BitAnd => Ok(PpInt::new(a.bits & b.bits, signed, width)),
        Op::BitOr => Ok(PpInt::new(a.bits | b.bits, signed, width)),
        Op::BitXor => Ok(PpInt::new(a.bits ^ b.bits, signed, width)),
        Op::Shl | Op::Shr => shift(op, a, b, warn),
        Op::Eq => Ok(PpInt::int1(order(a, b) == std::cmp::Ordering::Equal)),
        Op::Ne => Ok(PpInt::int1(order(a, b) != std::cmp::Ordering::Equal)),
        Op::Lt => Ok(PpInt::int1(order(a, b) == std::cmp::Ordering::Less)),
        Op::Gt => Ok(PpInt::int1(order(a, b) == std::cmp::Ordering::Greater)),
        Op::Le => Ok(PpInt::int1(order(a, b) != std::cmp::Ordering::Greater)),
        Op::Ge => Ok(PpInt::int1(order(a, b) != std::cmp::Ordering::Less)),
        _ => unreachable!("logical ops and ?: short-circuit in eval"),
    }
}

/// Shifts keep the left operand's signedness. A count outside
/// `0 <= count < 64` warns and saturates: `<<` yields 0, `>>` yields -1
/// for a negative value and 0 otherwise (clang's `#if` behavior).
fn shift(op: Op, a: PpInt, count: PpInt, warn: &dyn Fn(&str)) -> Result<PpInt, EvalError> {
    let c = count.as_i128();
    if c < 0 || c >= 64 {
        warn("integer overflow in preprocessor expression");
        let v = match op {
            Op::Shl => 0,
            _ if a.signed && (a.bits as i64) < 0 => u64::MAX,
            _ => 0,
        };
        return Ok(PpInt::new(v, a.signed, a.width));
    }
    let c = c as u32;
    match op {
        Op::Shl => Ok(PpInt::new(a.bits << c, a.signed, a.width)),
        _ => {
            let v = if a.signed {
                (a.as_i128() >> c) as u64
            } else {
                a.bits >> c
            };
            Ok(PpInt::new(v, a.signed, a.width))
        }
    }
}

fn unary(op: Op, a: PpInt) -> PpInt {
    match op {
        Op::Not => PpInt::int1(!a.is_nonzero()),
        Op::BitNot => PpInt::new(!a.bits, a.signed, a.width),
        Op::Sub => PpInt::new(0u64.wrapping_sub(a.bits), a.signed, a.width),
        _ => a, // unary plus
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Or,
    And,
    BitOr,
    BitXor,
    BitAnd,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Not,
    BitNot,
    Question,
    Colon,
}

impl Op {
    fn text(self) -> &'static str {
        match self {
            Op::Or => "||",
            Op::And => "&&",
            Op::BitOr => "|",
            Op::BitXor => "^",
            Op::BitAnd => "&",
            Op::Eq => "==",
            Op::Ne => "!=",
            Op::Lt => "<",
            Op::Gt => ">",
            Op::Le => "<=",
            Op::Ge => ">=",
            Op::Shl => "<<",
            Op::Shr => ">>",
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Mod => "%",
            Op::Not => "!",
            Op::BitNot => "~",
            Op::Question => "?",
            Op::Colon => ":",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Etoken {
    /// A typed integer. `spelling` is the source text, kept so the
    /// expand-and-relex roundtrip sees the same literal (a rendered value
    /// could re-lex with a different type: `0xFFFFFFFF` vs `4294967295`).
    Number { spelling: String, value: PpInt },
    Ident(String),
    /// A string literal, raw with quotes; the parser rejects it.
    Str(String),
    Op(Op),
    LParen,
    RParen,
}

impl Etoken {
    fn render(&self) -> String {
        match self {
            Etoken::Number { spelling, .. } => spelling.clone(),
            Etoken::Ident(name) => name.clone(),
            Etoken::Str(raw) => raw.clone(),
            Etoken::Op(op) => op.text().to_string(),
            Etoken::LParen => "(".to_string(),
            Etoken::RParen => ")".to_string(),
        }
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_' || c == '$'
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '$'
}

/// Length of a character-literal prefix (`L`, `u`, `U`, `u8`) at `cs[i]`
/// when a quote immediately follows it, else None.
fn char_prefix_len(cs: &[char], i: usize) -> Option<usize> {
    let at = |j: usize| cs.get(j).copied();
    if at(i) == Some('u') && at(i + 1) == Some('8') && at(i + 2) == Some('\'') {
        Some(2)
    } else if matches!(at(i), Some('L' | 'u' | 'U')) && at(i + 1) == Some('\'') {
        Some(1)
    } else {
        None
    }
}

fn lex(expr: &str) -> Result<Vec<Etoken>, EvalError> {
    let cs: Vec<char> = expr.chars().collect();
    let mut tokens = Vec::new();
    let mut i = 0;
    while i < cs.len() {
        let c = cs[i];
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if c.is_ascii_digit() || (c == '.' && cs.get(i + 1).is_some_and(|d| d.is_ascii_digit())) {
            let start = i;
            while i < cs.len() && (cs[i].is_ascii_alphanumeric() || cs[i] == '_' || cs[i] == '.') {
                i += 1;
            }
            let text: String = cs[start..i].iter().collect();
            let value = classify_number(&text)?;
            tokens.push(Etoken::Number { spelling: text, value });
            continue;
        }
        if is_ident_start(c) {
            if let Some(n) = char_prefix_len(&cs, i) {
                let (value, next) = lex_char(&cs, i + n)?;
                tokens.push(Etoken::Number {
                    spelling: cs[i..next].iter().collect(),
                    value,
                });
                i = next;
                continue;
            }
            let start = i;
            while i < cs.len() && is_ident_char(cs[i]) {
                i += 1;
            }
            tokens.push(Etoken::Ident(cs[start..i].iter().collect()));
            continue;
        }
        if c == '\'' {
            let (value, next) = lex_char(&cs, i)?;
            tokens.push(Etoken::Number {
                spelling: cs[i..next].iter().collect(),
                value,
            });
            i = next;
            continue;
        }
        if c == '"' {
            let start = i;
            i += 1;
            let mut closed = false;
            while i < cs.len() {
                if cs[i] == '\n' {
                    break;
                }
                if cs[i] == '\\' {
                    i += 2;
                    continue;
                }
                i += 1;
                if cs[i - 1] == '"' {
                    closed = true;
                    break;
                }
            }
            if !closed {
                return Err(EvalError::new("missing terminating \" character"));
            }
            tokens.push(Etoken::Str(cs[start..i].iter().collect()));
            continue;
        }
        // Operators, longest match first among the valid ones.
        let two = if i + 1 < cs.len() {
            Some((c, cs[i + 1]))
        } else {
            None
        };
        let op = match two {
            Some(('|', '|')) => Some(Op::Or),
            Some(('&', '&')) => Some(Op::And),
            Some(('=', '=')) => Some(Op::Eq),
            Some(('!', '=')) => Some(Op::Ne),
            Some(('<', '<')) => Some(Op::Shl),
            Some(('>', '>')) => Some(Op::Shr),
            Some(('<', '=')) => Some(Op::Le),
            Some(('>', '=')) => Some(Op::Ge),
            _ => None,
        };
        if let Some(op) = op {
            tokens.push(Etoken::Op(op));
            i += 2;
            continue;
        }
        match c {
            '|' => tokens.push(Etoken::Op(Op::BitOr)),
            '^' => tokens.push(Etoken::Op(Op::BitXor)),
            '&' => tokens.push(Etoken::Op(Op::BitAnd)),
            '<' => tokens.push(Etoken::Op(Op::Lt)),
            '>' => tokens.push(Etoken::Op(Op::Gt)),
            '+' => tokens.push(Etoken::Op(Op::Add)),
            '-' => tokens.push(Etoken::Op(Op::Sub)),
            '*' => tokens.push(Etoken::Op(Op::Mul)),
            '/' => tokens.push(Etoken::Op(Op::Div)),
            '%' => tokens.push(Etoken::Op(Op::Mod)),
            '!' => tokens.push(Etoken::Op(Op::Not)),
            '~' => tokens.push(Etoken::Op(Op::BitNot)),
            '?' => tokens.push(Etoken::Op(Op::Question)),
            ':' => tokens.push(Etoken::Op(Op::Colon)),
            '(' => tokens.push(Etoken::LParen),
            ')' => tokens.push(Etoken::RParen),
            _ => {
                return Err(EvalError::new(format!("invalid character {c:?} in expression")));
            }
        }
        i += 1;
    }
    Ok(tokens)
}

/// Types an integer pp-number. Binary literals are accepted (clang accepts
/// them in C17 mode as an extension); anything else that is not a plain
/// integer constant — floats, bad digits, bad suffixes — is an error.
fn classify_number(text: &str) -> Result<PpInt, EvalError> {
    let invalid = || EvalError::new(format!("invalid number `{text}` in expression"));
    let bs = text.as_bytes();
    let (base, digits): (u32, &[u8]) = if let Some(rest) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
        (16, rest.as_bytes())
    } else if let Some(rest) = text.strip_prefix("0b").or_else(|| text.strip_prefix("0B")) {
        (2, rest.as_bytes())
    } else if bs.first() == Some(&b'0') {
        (8, bs)
    } else {
        (10, bs)
    };
    // Split digits from suffix. Note `0x` itself carries no digits.
    let skip = bs.len() - digits.len();
    let digit_len = digits
        .iter()
        .take_while(|d| (**d as char).is_digit(base))
        .count();
    if base != 8 && digit_len == 0 {
        return Err(invalid());
    }
    let (digits, suffix) = (
        &digits[..digit_len],
        &text[skip + digit_len..],
    );
    let suffix_ok = matches!(
        suffix.to_ascii_lowercase().as_str(),
        "" | "u" | "l" | "ll" | "ul" | "ull" | "lu" | "llu"
    );
    if !suffix_ok {
        return Err(invalid());
    }
    // `0` followed by `8`/`9` (or anything non-octal) falls out here as a
    // bad suffix, which is still the correct verdict: an error.
    let too_large = || {
        EvalError::new("integer literal is too large to be represented in any integer type")
    };
    let mut value: u128 = 0;
    for d in digits {
        let v = (*d as char).to_digit(base).unwrap() as u128;
        value = value
            .checked_mul(base as u128)
            .and_then(|acc| acc.checked_add(v))
            .ok_or_else(too_large)?;
    }
    if value > u64::MAX as u128 {
        return Err(too_large());
    }
    let has_u = suffix.to_ascii_lowercase().contains('u');
    // `#if` values are intmax_t/uintmax_t: the `L`/`LL` suffixes only
    // validate, they never narrow. Decimal without `U` must fit the signed
    // range; anything else takes the first fitting 64-bit signedness.
    let cands: &[(bool, u32)] = match (base == 10, has_u) {
        (true, false) => &[(true, 64)],
        (true, true) => &[(false, 64)],
        (false, false) => &[(true, 64), (false, 64)],
        (false, true) => &[(false, 64)],
    };
    for (signed, width) in cands {
        let max = if *signed {
            PpInt::mask(*width) >> 1
        } else {
            PpInt::mask(*width)
        };
        if value <= max as u128 {
            return Ok(PpInt::new(value as u64, *signed, *width));
        }
    }
    Err(EvalError::new(
        "integer literal is too large to be represented in any integer type",
    ))
}

/// Reads a character constant at `cs[i]` (the opening quote) and returns
/// its value with the index past the closing quote. Accumulates bytes
/// big-endian GCC-style (`'ab' == 0x6162`); each escape contributes its
/// low 8 bits; the total keeps its low 32 bits as `int`.
fn lex_char(cs: &[char], i: usize) -> Result<(PpInt, usize), EvalError> {
    debug_assert_eq!(cs[i], '\'');
    let mut i = i + 1;
    let mut value: u64 = 0;
    let mut units = 0u32;
    loop {
        if i >= cs.len() || cs[i] == '\n' {
            return Err(EvalError::new("missing terminating ' character"));
        }
        if cs[i] == '\'' {
            i += 1;
            break;
        }
        let byte: u64 = if cs[i] == '\\' {
            i += 1;
            if i >= cs.len() {
                return Err(EvalError::new("missing terminating ' character"));
            }
            escape_value(cs, &mut i)?
        } else {
            // One scalar may be several bytes; each byte is a unit.
            let mut buf = [0u8; 4];
            let s = cs[i].encode_utf8(&mut buf);
            let mut b = 0u64;
            for (k, byte) in s.bytes().enumerate() {
                if k > 0 {
                    value = (value << 8) | b;
                    units += 1;
                    b = 0;
                }
                b = (b << 8) | byte as u64;
            }
            i += 1;
            // Fall through to the shared accumulate step with the last byte.
            value = (value << 8) | b;
            units += 1;
            continue;
        };
        value = (value << 8) | byte;
        units += 1;
    }
    if units == 0 {
        return Err(EvalError::new("empty character constant"));
    }
    let narrow = (value & 0xFFFF_FFFF) as u32 as i32 as i64 as u64;
    Ok((PpInt::new(narrow, true, 64), i))
}

/// Reads one escape sequence at `cs[*i]` (the char after the backslash),
/// advances past it, and returns its low 8 bits.
fn escape_value(cs: &[char], i: &mut usize) -> Result<u64, EvalError> {
    let c = cs[*i];
    let simple = match c {
        'a' => Some(7),
        'b' => Some(8),
        'f' => Some(12),
        'n' => Some(10),
        'r' => Some(13),
        't' => Some(9),
        'v' => Some(11),
        '\\' => Some(b'\\' as u64),
        '\'' => Some(b'\'' as u64),
        '"' => Some(b'"' as u64),
        '?' => Some(b'?' as u64),
        _ => None,
    };
    if let Some(v) = simple {
        *i += 1;
        return Ok(v);
    }
    if c == 'x' || c == 'X' {
        *i += 1;
        let start = *i;
        let mut v: u64 = 0;
        while *i < cs.len() {
            if let Some(d) = cs[*i].to_digit(16) {
                v = (v << 4) | d as u64;
                *i += 1;
            } else {
                break;
            }
        }
        if *i == start {
            return Err(EvalError::new("expected hex digits after \\x"));
        }
        return Ok(v & 0xFF);
    }
    if ('0'..='7').contains(&c) {
        let mut v: u64 = 0;
        for _ in 0..3 {
            if *i < cs.len() {
                if let Some(d) = cs[*i].to_digit(8) {
                    v = (v << 3) | d as u64;
                    *i += 1;
                    continue;
                }
            }
            break;
        }
        return Ok(v & 0xFF);
    }
    Err(EvalError::new(format!("unknown escape sequence `\\{c}`")))
}

/// Evaluates an `#if` controlling expression.
///
/// 1. `defined` forms are recognized on the unexpanded tokens and replaced
///    by 1/0 (the operand is exempt from expansion).
/// 2. The remainder is macro-expanded.
/// 3. A `defined` token produced by expansion is an error (undefined
///    behavior per the standard); every other bare identifier is 0.
/// 4. The whole expression is parsed first, so parse errors surface even in
///    unevaluated branches; then it is evaluated with short-circuiting
///    `&&`, `||` and `?:`.
pub fn eval_if_expression(
    expr: &str,
    is_defined: &dyn Fn(&str) -> bool,
    expand: &dyn Fn(&str) -> Result<String, EvalError>,
    warn: &dyn Fn(&str),
) -> Result<bool, EvalError> {
    let tokens = lex(expr)?;
    let tokens = apply_defined(&tokens, is_defined)?;
    let rendered = tokens.iter().map(Etoken::render).collect::<Vec<_>>().join(" ");
    let tokens = lex(&expand(&rendered)?)?;
    let tokens = fold_identifiers(&tokens)?;
    let ast = Parser::new(&tokens).parse()?;
    Ok(ast.eval(warn)?.is_nonzero())
}

/// Replaces `defined IDENT` / `defined ( IDENT )` with 1/0.
fn apply_defined(
    tokens: &[Etoken],
    is_defined: &dyn Fn(&str) -> bool,
) -> Result<Vec<Etoken>, EvalError> {
    let missing = || EvalError::new("macro name missing");
    let must_be_ident = || EvalError::new("macro name must be an identifier");
    let mut out = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        let is_op = matches!(&tokens[i], Etoken::Ident(n) if n == "defined");
        if !is_op {
            out.push(tokens[i].clone());
            i += 1;
            continue;
        }
        let (name, next) = match tokens.get(i + 1) {
            None => return Err(missing()),
            Some(Etoken::Ident(n)) => (n.as_str(), i + 2),
            Some(Etoken::LParen) => match tokens.get(i + 2) {
                None | Some(Etoken::RParen) => return Err(missing()),
                Some(Etoken::Ident(n)) => {
                    if !matches!(tokens.get(i + 3), Some(Etoken::RParen)) {
                        return Err(EvalError::new("missing ')' after 'defined'"));
                    }
                    (n.as_str(), i + 4)
                }
                Some(_) => return Err(must_be_ident()),
            },
            Some(_) => return Err(must_be_ident()),
        };
        let v = if is_defined(name) { 1 } else { 0 };
        out.push(Etoken::Number {
            spelling: v.to_string(),
            value: PpInt::new(v, true, 64),
        });
        i = next;
    }
    Ok(out)
}

/// Rejects a `defined` that survived expansion, and folds every other bare
/// identifier to 0.
fn fold_identifiers(tokens: &[Etoken]) -> Result<Vec<Etoken>, EvalError> {
    tokens
        .iter()
        .map(|t| match t {
            Etoken::Ident(n) if n == "defined" => {
                Err(EvalError::new("`defined` generated by macro expansion"))
            }
            Etoken::Ident(_) => Ok(Etoken::Number {
                spelling: "0".to_string(),
                value: PpInt::new(0, true, 64),
            }),
            other => Ok(other.clone()),
        })
        .collect()
}

#[derive(Debug)]
enum Expr {
    Num(PpInt),
    Unary(Op, Box<Expr>),
    Binary(Op, Box<Expr>, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl Expr {
    fn eval(&self, warn: &dyn Fn(&str)) -> Result<PpInt, EvalError> {
        match self {
            Expr::Num(v) => Ok(*v),
            Expr::Unary(op, e) => Ok(unary(*op, e.eval(warn)?)),
            Expr::Binary(Op::And, l, r) => Ok(PpInt::int1(
                l.eval(warn)?.is_nonzero() && r.eval(warn)?.is_nonzero(),
            )),
            Expr::Binary(Op::Or, l, r) => Ok(PpInt::int1(
                l.eval(warn)?.is_nonzero() || r.eval(warn)?.is_nonzero(),
            )),
            Expr::Binary(op, l, r) => binary(*op, l.eval(warn)?, r.eval(warn)?, warn),
            Expr::Ternary(c, a, b) => {
                if c.eval(warn)?.is_nonzero() {
                    a.eval(warn)
                } else {
                    b.eval(warn)
                }
            }
        }
    }
}

struct Parser<'a> {
    tokens: &'a [Etoken],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Etoken]) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn parse(&mut self) -> Result<Expr, EvalError> {
        if self.tokens.is_empty() {
            return Err(EvalError::new("expected value in expression"));
        }
        let expr = self.parse_ternary()?;
        if self.pos != self.tokens.len() {
            return Err(EvalError::new(
                "token is not a valid binary operator in a preprocessor subexpression",
            ));
        }
        Ok(expr)
    }

    fn peek_op(&self) -> Option<Op> {
        match self.tokens.get(self.pos) {
            Some(Etoken::Op(op)) => Some(*op),
            _ => None,
        }
    }

    fn eat(&mut self, op: Op) -> bool {
        if self.peek_op() == Some(op) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn parse_ternary(&mut self) -> Result<Expr, EvalError> {
        let cond = self.parse_or()?;
        if self.eat(Op::Question) {
            let yes = self.parse_ternary()?;
            if !self.eat(Op::Colon) {
                return Err(EvalError::new("expected ':' in expression"));
            }
            let no = self.parse_ternary()?;
            Ok(Expr::Ternary(Box::new(cond), Box::new(yes), Box::new(no)))
        } else {
            Ok(cond)
        }
    }

    fn parse_or(&mut self) -> Result<Expr, EvalError> {
        self.parse_left_assoc(&[Op::Or], Self::parse_and)
    }

    fn parse_and(&mut self) -> Result<Expr, EvalError> {
        self.parse_left_assoc(&[Op::And], Self::parse_bitor)
    }

    fn parse_bitor(&mut self) -> Result<Expr, EvalError> {
        self.parse_left_assoc(&[Op::BitOr], Self::parse_bitxor)
    }

    fn parse_bitxor(&mut self) -> Result<Expr, EvalError> {
        self.parse_left_assoc(&[Op::BitXor], Self::parse_bitand)
    }

    fn parse_bitand(&mut self) -> Result<Expr, EvalError> {
        self.parse_left_assoc(&[Op::BitAnd], Self::parse_equal)
    }

    fn parse_equal(&mut self) -> Result<Expr, EvalError> {
        self.parse_left_assoc(&[Op::Eq, Op::Ne], Self::parse_cmp)
    }

    fn parse_cmp(&mut self) -> Result<Expr, EvalError> {
        self.parse_left_assoc(&[Op::Lt, Op::Gt, Op::Le, Op::Ge], Self::parse_shift)
    }

    fn parse_shift(&mut self) -> Result<Expr, EvalError> {
        self.parse_left_assoc(&[Op::Shl, Op::Shr], Self::parse_add)
    }

    fn parse_add(&mut self) -> Result<Expr, EvalError> {
        self.parse_left_assoc(&[Op::Add, Op::Sub], Self::parse_mul)
    }

    fn parse_mul(&mut self) -> Result<Expr, EvalError> {
        self.parse_left_assoc(&[Op::Mul, Op::Div, Op::Mod], Self::parse_unary)
    }

    fn parse_left_assoc(
        &mut self,
        ops: &[Op],
        mut next: impl FnMut(&mut Self) -> Result<Expr, EvalError>,
    ) -> Result<Expr, EvalError> {
        let mut left = next(self)?;
        while let Some(op) = self.peek_op() {
            if !ops.contains(&op) {
                break;
            }
            self.pos += 1;
            let right = next(self)?;
            left = Expr::Binary(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, EvalError> {
        match self.peek_op() {
            Some(op @ (Op::Add | Op::Sub | Op::Not | Op::BitNot)) => {
                self.pos += 1;
                Ok(Expr::Unary(op, Box::new(self.parse_unary()?)))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, EvalError> {
        match self.tokens.get(self.pos) {
            Some(Etoken::Number { value, .. }) => {
                self.pos += 1;
                Ok(Expr::Num(*value))
            }
            Some(Etoken::LParen) => {
                self.pos += 1;
                let expr = self.parse_ternary()?;
                match self.tokens.get(self.pos) {
                    Some(Etoken::RParen) => {
                        self.pos += 1;
                        Ok(expr)
                    }
                    _ => Err(EvalError::new("expected ')' in preprocessor expression")),
                }
            }
            Some(Etoken::Str(_)) => Err(EvalError::new("string literal in expression")),
            _ => Err(EvalError::new("expected value in expression")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    fn eval(expr: &str) -> Result<bool, EvalError> {
        eval_with(&[], expr, |s| Ok(s.to_string()))
    }

    fn eval_with(
        defined: &[&str],
        expr: &str,
        expand: impl Fn(&str) -> Result<String, EvalError>,
    ) -> Result<bool, EvalError> {
        let set: HashSet<&str> = defined.iter().copied().collect();
        eval_if_expression(expr, &|n| set.contains(n), &expand, &|_| {})
    }

    fn eval_warnings(expr: &str) -> (Result<bool, EvalError>, Vec<String>) {
        let warnings = std::cell::RefCell::new(Vec::new());
        let r = eval_if_expression(
            expr,
            &|_| false,
            &|s| Ok(s.to_string()),
            &|w| warnings.borrow_mut().push(w.to_string()),
        );
        (r, warnings.into_inner())
    }

    fn is_err_containing(r: Result<bool, EvalError>, part: &str) {
        match r {
            Ok(v) => panic!("expected error containing {part:?}, got Ok({v})"),
            Err(e) => assert!(e.message.contains(part), "error {:?} lacks {part:?}", e.message),
        }
    }

    #[test]
    fn arithmetic_and_precedence() {
        assert_eq!(eval("1 + 2 * 3"), Ok(true));
        assert_eq!(eval("(1 + 2) * 3 == 9"), Ok(true));
        assert_eq!(eval("10 / 3 == 3"), Ok(true));
        assert_eq!(eval("-7 / 2 == -3"), Ok(true));
        assert_eq!(eval("-7 % 2 == -1"), Ok(true));
        assert_eq!(eval("7 % -2 == 1"), Ok(true));
        assert_eq!(eval("0"), Ok(false));
        assert_eq!(eval("!0"), Ok(true));
        assert_eq!(eval("~-1 == 0"), Ok(true));
        assert_eq!(eval("--5 == 5"), Ok(true));
        assert_eq!(eval("(1 << 20) == 1048576"), Ok(true));
        assert_eq!(eval("(256 >> 4) == 16"), Ok(true));
        assert_eq!(eval("(-8 >> 1) == -4"), Ok(true));
        assert_eq!(eval("(1 | 2) == 3 && (3 & 1) == 1 && (3 ^ 1) == 2"), Ok(true));
    }

    #[test]
    fn usual_conversions_match_clang() {
        // Everything is intmax_t/uintmax_t: -1 converts to a huge unsigned
        // against an unsigned operand, but stays -1 against signed ones.
        assert_eq!(eval("-1 < 0U"), Ok(false));
        assert_eq!(eval("-1 > 0U"), Ok(true));
        assert_eq!(eval("-1 < 0xFFFFFFFF"), Ok(true));
        assert_eq!(eval("-1 < 0xFFFFFFFFU"), Ok(false));
        assert_eq!(eval("0xFFFFFFFF > 0"), Ok(true));
        assert_eq!(eval("0xFFFFFFFF == 4294967295U"), Ok(true));
        assert_eq!(eval("0xFFFFFFFF == 4294967295"), Ok(true));
        assert_eq!(eval("4294967295 > 0"), Ok(true));
        assert_eq!(eval("4294967295 < 0U"), Ok(false));
        // No narrowing to int: INT_MAX + 1 is exactly 2**31.
        assert_eq!(eval("(2147483647 + 1) == 2147483648"), Ok(true));
        assert_eq!(eval("1L + 1 == 2"), Ok(true));
        assert_eq!(eval("1UL - 2 > 0U"), Ok(true)); // wraps unsigned
        // A hex constant past int64 is unsigned.
        assert_eq!(eval("-1 < 0x8000000000000000"), Ok(false));
        assert_eq!(eval("0x8000000000000000 > 0"), Ok(true));
    }

    #[test]
    fn shifts_and_ternary() {
        assert_eq!(eval("1 ? 10 : 20"), Ok(true));
        assert_eq!(eval("0 ? 10 : 0"), Ok(false));
        // Only the taken branch evaluates: no division by zero.
        assert_eq!(eval("0 ? 1/0 : 1"), Ok(true));
        assert_eq!(eval("1 ? 1 : 1/0"), Ok(true));
        // Right-associative nesting.
        assert_eq!(eval("0 ? 1 : 1 ? 7 : 8"), Ok(true));
    }

    #[test]
    fn out_of_range_shifts_warn_and_saturate() {
        // `<<` saturates to 0, `>>` to -1 for negatives, else 0.
        assert_eq!(eval_warnings("1 << 64"), (Ok(false), vec![overflow()]));
        assert_eq!(eval_warnings("1 << 100"), (Ok(false), vec![overflow()]));
        assert_eq!(eval_warnings("1 << -1"), (Ok(false), vec![overflow()]));
        assert_eq!(eval_warnings("1 >> 100"), (Ok(false), vec![overflow()]));
        assert_eq!(eval_warnings("(-1 >> 100) == -1"), (Ok(true), vec![overflow()]));
        assert_eq!(eval_warnings("(-1 >> -1) == -1"), (Ok(true), vec![overflow()]));
        // In-range shifts compute full 64-bit: no narrowing to int.
        assert_eq!(eval("(1 << 40) == 1099511627776"), Ok(true));
        assert_eq!(eval("(1 << 32) != 0"), Ok(true));
        assert_eq!(eval("(1L << 40) == 1099511627776"), Ok(true));
        // No warning when nothing overflows.
        assert_eq!(eval_warnings("1 << 2 == 4"), (Ok(true), Vec::new()));
    }

    fn overflow() -> String {
        "integer overflow in preprocessor expression".to_string()
    }

    #[test]
    fn arithmetic_overflow_warns_and_wraps() {
        assert_eq!(
            eval_warnings("9223372036854775807 + 1 == 0 - 9223372036854775807 - 1"),
            (Ok(true), vec![overflow()])
        );
        assert_eq!(
            eval_warnings("0 - 9223372036854775807 - 1 - 1 == 9223372036854775807"),
            (Ok(true), vec![overflow()])
        );
        assert_eq!(
            eval_warnings("0xFFFFFFFFFFFFFFFF + 1 == 0"),
            (Ok(true), vec![overflow()])
        );
        is_err_containing(eval("1 % 0"), "remainder by zero");
    }

    #[test]
    fn logical_short_circuit() {
        assert_eq!(eval("0 && (1/0)"), Ok(false));
        assert_eq!(eval("1 || (1/0)"), Ok(true));
        assert_eq!(eval("1 && 2 || 0"), Ok(true));
    }

    #[test]
    fn parse_errors_surface_even_in_untaken_branches() {
        is_err_containing(eval("0 && !!!"), "expected value");
        is_err_containing(eval("0 ? !!! : 1"), "expected value");
        is_err_containing(eval("1 ? 1 : !!!"), "expected value");
        is_err_containing(eval(""), "expected value");
        is_err_containing(eval("1 2"), "not a valid binary operator");
        is_err_containing(eval("(1"), "expected ')'");
        is_err_containing(eval("1 ? 2"), "expected ':'");
        is_err_containing(eval("\"s\""), "string literal");
        is_err_containing(eval("sizeof(int)"), "not a valid binary operator");
        is_err_containing(eval("1.5"), "invalid number");
        is_err_containing(eval("1/0"), "division by zero");
    }

    #[test]
    fn numbers_bases_and_suffixes() {
        assert_eq!(eval("0xFF == 255 && 010 == 8 && 0b101 == 5"), Ok(true));
        assert_eq!(eval("0XaBc == 2748"), Ok(true));
        assert_eq!(eval("10l == 10 && 10ul == 10u && 10llu == 10LU"), Ok(true));
        assert_eq!(eval("0 == 0x0 && 0 == 00"), Ok(true));
        is_err_containing(eval("09"), "invalid number");
        is_err_containing(eval("0x"), "invalid number");
        is_err_containing(eval("123abc"), "invalid number");
        is_err_containing(eval("1lll"), "invalid number");
        is_err_containing(eval("0xFFFFFFFFFFFFFFFFF"), "too large");
    }

    #[test]
    fn char_constants() {
        assert_eq!(eval("'a' == 97"), Ok(true));
        assert_eq!(eval("'\\n' == 10 && '\\x41' == 65 && '\\101' == 65"), Ok(true));
        assert_eq!(eval("'ab' == 0x6162"), Ok(true));
        assert_eq!(eval("L'a' == 97"), Ok(true));
        is_err_containing(eval("''"), "empty character");
        is_err_containing(eval("'ab"), "missing terminating");
        is_err_containing(eval("'\\q'"), "unknown escape");
        is_err_containing(eval("'\\x'"), "hex digits");
    }

    #[test]
    fn defined_operator() {
        assert_eq!(eval_with(&["FOO"], "defined(FOO)", |s| Ok(s.to_string())), Ok(true));
        assert_eq!(eval_with(&["FOO"], "defined FOO", |s| Ok(s.to_string())), Ok(true));
        assert_eq!(
            eval_with(&["FOO"], "defined ( FOO )", |s| Ok(s.to_string())),
            Ok(true)
        );
        assert_eq!(eval_with(&[], "defined(NOPE)", |s| Ok(s.to_string())), Ok(false));
        // The operand is exempt from expansion: rewriting FOO to BAR must
        // not affect the already-substituted result.
        assert_eq!(
            eval_with(&["FOO"], "defined(FOO)", |s| Ok(s.replace("FOO", "BAR"))),
            Ok(true)
        );
        // ...but so is a bare `defined` at the end: an error, like clang's.
        is_err_containing(eval("defined"), "macro name missing");
        is_err_containing(eval("defined("), "macro name missing");
        is_err_containing(eval("defined((X))"), "must be an identifier");
        is_err_containing(eval("defined 42"), "must be an identifier");
        is_err_containing(eval("defined(X Y)"), "missing ')' after 'defined'");
    }

    #[test]
    fn identifiers_fold_to_zero_after_expansion() {
        assert_eq!(eval("UNDEFINED_THING"), Ok(false));
        assert_eq!(eval("!UNDEFINED_THING"), Ok(true));
        assert_eq!(eval("true"), Ok(false)); // C17: no boolean macros
        // Expansion runs before folding.
        assert_eq!(
            eval_with(&[], "A + B", |s| Ok(s.replace('A', "40").replace('B', "2"))),
            Ok(true)
        );
    }

    #[test]
    fn defined_from_expansion_is_an_error() {
        is_err_containing(
            eval_with(&[], "D(X)", |_| Ok("defined(X)".to_string())),
            "generated by macro expansion",
        );
    }
}
