//! Macro definitions and token-based expansion with rescan.
//!
//! Arguments are prescanned (except `#`/`##` operands, which substitute
//! raw), `#` stringizes with whitespace normalization, `##` pastes pairwise
//! left-associatively with re-lexing, and rescanning is guarded by hidden
//! sets (a macro never expands inside its own replacement scan, while the
//! following source tokens stay unaffected). Newlines inside substituted
//! arguments vanish unless dropping them would merge tokens (then a
//! space); only stringize turns them into spaces unconditionally.
//! Structural newlines pass through.

use std::collections::{HashMap, HashSet, VecDeque};

use crate::pp_token::{pp_lex, PpToken};

/// Function-like parameter list shape.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Variadic {
    No,
    /// C99 `...`: `__VA_ARGS__` is bound.
    C99,
    /// GNU `name...`: usable by name; `__VA_ARGS__` is NOT bound.
    Named(String),
}

/// A macro definition with a tokenized replacement list.
#[derive(Debug, Clone)]
pub struct Macro {
    /// None = object-like; Some (possibly empty) = fixed function-like params.
    pub params: Option<Vec<String>>,
    pub variadic: Variadic,
    /// Lexed replacement list (leading/trailing whitespace trimmed).
    pub body: Vec<PpToken>,
}

pub type MacroMap = HashMap<String, Macro>;

/// A hidden set: macros already expanded "above" this token, which must
/// not expand it again. This is what makes rescan terminate (a macro never
/// expands inside its own replacement scan) while leaving the following
/// source tokens unaffected. Shared immutably; unioning allocates.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hs(std::rc::Rc<HashSet<String>>);

impl Hs {
    pub fn empty() -> Self {
        Hs(std::rc::Rc::new(HashSet::new()))
    }

    pub fn contains(&self, name: &str) -> bool {
        self.0.contains(name)
    }

    /// Returns this set plus `extra` plus `name`.
    fn extended(&self, extra: &Hs, name: &str) -> Hs {
        let mut set = HashSet::with_capacity(self.0.len() + extra.0.len() + 1);
        set.extend(self.0.iter().cloned());
        set.extend(extra.0.iter().cloned());
        set.insert(name.to_string());
        Hs(std::rc::Rc::new(set))
    }
}

/// An expansion token: a pp-token with its 1-based physical origin line and
/// its hidden set. Body tokens take the invocation line on instantiation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpTok {
    pub tok: PpToken,
    pub line: u32,
    pub hidden: Hs,
}

impl SpTok {
    fn new(tok: PpToken, line: u32) -> Self {
        SpTok {
            tok,
            line,
            hidden: Hs::empty(),
        }
    }

    /// Re-stamps a token with the union of its set, the invocation context
    /// and the macro being replaced.
    fn stamped(mut self, ctx: &Hs, name: &str) -> Self {
        self.hidden = self.hidden.extended(ctx, name);
        self
    }
}

/// Placemarker pp-token: the residue of pasting nothing with nothing.
/// Renders as nothing and vanishes.
fn placemarker(line: u32) -> SpTok {
    SpTok::new(PpToken::Ws(String::new()), line)
}

fn is_placemarker(t: &SpTok) -> bool {
    matches!(&t.tok, PpToken::Ws(s) if s.is_empty())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpandError {
    pub line: u32,
    pub message: String,
}

impl ExpandError {
    fn new(line: u32, message: impl Into<String>) -> Self {
        ExpandError {
            line,
            message: message.into(),
        }
    }
}

/// Fully expands a token stream. Spliced replacement tokens carry the
/// replaced name in their hidden sets, so rescan terminates while the
/// following source tokens stay unaffected.
pub fn expand_stream(tokens: Vec<SpTok>, macros: &MacroMap) -> Result<Vec<SpTok>, ExpandError> {
    let mut out: Vec<SpTok> = Vec::new();
    let mut rest: VecDeque<SpTok> = tokens.into();
    while let Some(t) = rest.pop_front() {
        let name = match &t.tok {
            PpToken::Ident(name) => name.clone(),
            _ => {
                out.push(t);
                continue;
            }
        };
        let Some(m) = macros.get(&name) else {
            out.push(t);
            continue;
        };
        if t.hidden.contains(&name) {
            out.push(t);
            continue;
        }
        if m.params.is_none() {
            let body: Vec<SpTok> = m
                .body
                .iter()
                .map(|b| SpTok::new(b.clone(), t.line).stamped(&t.hidden, &name))
                .collect();
            for tok in body.into_iter().rev() {
                rest.push_front(tok);
            }
            continue;
        }
        // Function-like: needs `(` after whitespace (possibly across lines).
        let mut j = 0;
        while rest.get(j).is_some_and(|x| x.tok.is_ws_or_newline()) {
            j += 1;
        }
        if !rest.get(j).is_some_and(|x| matches!(x.tok, PpToken::LParen)) {
            out.push(t);
            continue;
        }
        for _ in 0..=j {
            rest.pop_front();
        }
        let args = collect_args(&mut rest, t.line)?;
        let bound = bind_args(m, &args, t.line)?;
        // Prescan each fixed argument and the joined remainder with their
        // own hidden sets only: `F(F(1))` with `F(x)=x` yields `1`.
        let mut cooked = Vec::with_capacity(bound.fixed.len());
        for arg in &bound.fixed {
            cooked.push(expand_stream(arg.clone(), macros)?);
        }
        let va_raw = join_varargs(&bound.varargs, t.line);
        let va_cooked = expand_stream(va_raw.clone(), macros)?;
        let subst = Subst {
            params: m.params.as_ref().expect("function-like"),
            variadic: &m.variadic,
            raw: &bound.fixed,
            cooked: &cooked,
            va_raw: &va_raw,
            va_cooked: &va_cooked,
            varargs_present: !bound.varargs.is_empty(),
        };
        let replaced = substitute(&m.body, &subst, t.line, &name, &t.hidden)?;
        for tok in replaced.into_iter().rev() {
            rest.push_front(tok);
        }
    }
    Ok(out)
}

/// Collects the parenthesized argument list after the consumed `(`.
/// Leading/trailing whitespace of each argument is discarded; interior
/// newlines are kept (stringize collapses them, plain substitution drops
/// them at splice time).
fn collect_args(rest: &mut VecDeque<SpTok>, line: u32) -> Result<Vec<Vec<SpTok>>, ExpandError> {
    let mut args = Vec::new();
    let mut current = Vec::new();
    let mut depth = 0u32;
    loop {
        let Some(t) = rest.pop_front() else {
            return Err(ExpandError::new(line, "unterminated function-like macro invocation"));
        };
        match &t.tok {
            PpToken::LParen => {
                depth += 1;
                current.push(t);
            }
            PpToken::RParen => {
                if depth == 0 {
                    args.push(trim_arg(current));
                    return Ok(args);
                }
                depth -= 1;
                current.push(t);
            }
            PpToken::Comma if depth == 0 => {
                args.push(trim_arg(current));
                current = Vec::new();
            }
            _ => current.push(t),
        }
    }
}

fn trim_arg(mut arg: Vec<SpTok>) -> Vec<SpTok> {
    while arg.first().is_some_and(|t| t.tok.is_ws_or_newline()) {
        arg.remove(0);
    }
    while arg.last().is_some_and(|t| t.tok.is_ws_or_newline()) {
        arg.pop();
    }
    arg
}

/// Bound invocation arguments: fixed ones plus the variadic remainder.
/// `varargs` is empty when no variadic arguments were given at all, which
/// the GNU `, ## __VA_ARGS__` elision distinguishes from present-but-empty.
struct BoundArgs {
    fixed: Vec<Vec<SpTok>>,
    varargs: Vec<Vec<SpTok>>,
}

fn bind_args(m: &Macro, args: &[Vec<SpTok>], line: u32) -> Result<BoundArgs, ExpandError> {
    let params = m.params.as_ref().expect("function-like");
    // A lone empty argument with no fixed parameters means zero arguments:
    // `F()` for `F()`, `F(...)` or `F(args...)`.
    let args: &[Vec<SpTok>] = if args.len() == 1 && args[0].is_empty() && params.is_empty() {
        &[]
    } else {
        args
    };
    if args.len() < params.len() {
        return Err(ExpandError::new(
            line,
            "too few arguments provided to function-like macro invocation",
        ));
    }
    if matches!(m.variadic, Variadic::No) && args.len() > params.len() {
        return Err(ExpandError::new(
            line,
            "too many arguments provided to function-like macro invocation",
        ));
    }
    Ok(BoundArgs {
        fixed: args[..params.len()].to_vec(),
        varargs: args[params.len()..].to_vec(),
    })
}

/// Joins variadic arguments with `, ` separators (which take the
/// invocation line). The original separators are consumed by splitting.
fn join_varargs(varargs: &[Vec<SpTok>], line: u32) -> Vec<SpTok> {
    let mut out = Vec::new();
    for (i, arg) in varargs.iter().enumerate() {
        if i > 0 {
            out.push(SpTok::new(PpToken::Comma, line));
            out.push(SpTok::new(PpToken::Ws(" ".to_string()), line));
        }
        out.extend(arg.iter().cloned());
    }
    out
}

/// What a parameter name refers to during substitution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParamRef {
    /// A fixed parameter by index.
    Fixed(usize),
    /// The joined variadic remainder (C99 `__VA_ARGS__` or a GNU name).
    Varargs,
}

/// Everything substitution needs for one invocation.
struct Subst<'a> {
    params: &'a [String],
    variadic: &'a Variadic,
    raw: &'a [Vec<SpTok>],
    cooked: &'a [Vec<SpTok>],
    va_raw: &'a [SpTok],
    va_cooked: &'a [SpTok],
    varargs_present: bool,
}

impl Subst<'_> {
    fn lookup(&self, name: &str) -> Option<ParamRef> {
        if let Some(i) = self.params.iter().position(|p| p == name) {
            return Some(ParamRef::Fixed(i));
        }
        match self.variadic {
            Variadic::C99 if name == "__VA_ARGS__" => Some(ParamRef::Varargs),
            Variadic::Named(n) if n == name => Some(ParamRef::Varargs),
            _ => None,
        }
    }
}

/// Substitutes parameters into a replacement list, processing `#` and `##`
/// along the way. `#` operands and `##`-adjacent parameters substitute raw;
/// everything else substitutes prescanned. `line` is the invocation line,
/// which pasted/stringized results take as their origin. Every result token
/// is stamped with the invocation context plus the replaced name, so the
/// rescan hides the macro being replaced (including inside substituted
/// arguments) yet terminates.
fn substitute(
    body: &[PpToken],
    subst: &Subst,
    line: u32,
    name: &str,
    ctx: &Hs,
) -> Result<Vec<SpTok>, ExpandError> {
    let mut mid: Vec<SpTok> = Vec::new();
    let mut i = 0;
    // Skips whitespace/newlines from `i`, returning the first significant index.
    let skip_ws = |i: usize| {
        let mut j = i;
        while j < body.len() && body[j].is_ws_or_newline() {
            j += 1;
        }
        j
    };
    while i < body.len() {
        match &body[i] {
            PpToken::Hash => {
                let j = skip_ws(i + 1);
                let name = match body.get(j) {
                    Some(PpToken::Ident(n)) => n,
                    _ => {
                        return Err(ExpandError::new(
                            line,
                            "'#' is not followed by a macro parameter",
                        ));
                    }
                };
                let pref = subst.lookup(name).ok_or_else(|| {
                    ExpandError::new(line, "'#' is not followed by a macro parameter")
                })?;
                mid.push(SpTok::new(PpToken::Str(stringize(subst, pref)), line));
                i = j + 1;
            }
            PpToken::Paste => {
                // Left operand: last significant token so far (None doubles
                // as the placemarker side).
                while mid.last().is_some_and(|t| t.tok.is_ws_or_newline()) {
                    mid.pop();
                }
                let left = mid.pop();
                // Right operand: next significant body token.
                let k = skip_ws(i + 1);
                enum Right {
                    Tokens(Vec<SpTok>),
                    /// From `__VA_ARGS__` or a GNU name; `empty` means no
                    /// variadic arguments were given at all (distinct from
                    /// present-but-empty, which keeps the comma).
                    Varargs { tokens: Vec<SpTok>, empty: bool },
                }
                let (right, rest_start) = match body.get(k) {
                    Some(PpToken::Ident(n)) => match subst.lookup(n) {
                        Some(pref @ ParamRef::Fixed(_)) => {
                            (Right::Tokens(raw_tokens(subst, pref)), k + 1)
                        }
                        Some(ParamRef::Varargs) => (
                            Right::Varargs {
                                tokens: subst.va_raw.to_vec(),
                                empty: !subst.varargs_present,
                            },
                            k + 1,
                        ),
                        None => (Right::Tokens(vec![SpTok::new(body[k].clone(), line)]), k + 1),
                    },
                    Some(t) if !matches!(t, PpToken::Hash | PpToken::Paste) => {
                        (Right::Tokens(vec![SpTok::new(t.clone(), line)]), k + 1)
                    }
                    _ => {
                        return Err(ExpandError::new(line, "invalid operands to '##'"));
                    }
                };
                // GNU `, ## __VA_ARGS__`: with no variadic arguments at all
                // the comma goes too; otherwise the `##` vanishes, the comma
                // stays, and a single space separates it from a non-empty
                // substitution. (A non-comma left side pastes normally.)
                if let Right::Varargs { tokens, empty } = &right {
                    if left.as_ref().is_some_and(|t| matches!(t.tok, PpToken::Comma)) {
                        if *empty {
                            i = rest_start;
                            continue;
                        }
                        mid.push(left.expect("comma"));
                        if tokens.iter().any(|t| !t.tok.is_ws_or_newline()) {
                            mid.push(SpTok::new(PpToken::Ws(" ".to_string()), line));
                        }
                        mid.extend(tokens.iter().cloned());
                        i = rest_start;
                        continue;
                    }
                }
                let right = match right {
                    Right::Varargs { tokens, .. } => tokens,
                    Right::Tokens(t) => t,
                };
                let (left_tok, right_tok) = split_paste_sides(left, &right);
                match (left_tok, right_tok) {
                    (None, None) => mid.push(placemarker(line)),
                    (Some(t), None) | (None, Some(t)) => mid.push(t),
                    (Some(l), Some(r)) => mid.push(paste_tokens(&l, &r, line)?),
                }
                mid.extend(right_rest(&right));
                i = rest_start;
            }
            PpToken::Ident(n) => match subst.lookup(n) {
                Some(pref) => {
                    // `##`-adjacent parameters (paste follows) substitute raw.
                    let raw = body.get(skip_ws(i + 1)).is_some_and(|t| matches!(t, PpToken::Paste));
                    mid.extend(cooked_or_raw(subst, pref, raw));
                    i += 1;
                }
                None => {
                    mid.push(SpTok::new(body[i].clone(), line));
                    i += 1;
                }
            },
            _ => {
                mid.push(SpTok::new(body[i].clone(), line));
                i += 1;
            }
        }
    }
    let mid = resolve_newlines(mid);
    Ok(mid.into_iter().map(|t| t.stamped(ctx, name)).collect())
}

/// Resolves newlines inside a substituted replacement list: each newline
/// between significant tokens becomes a space iff dropping it would merge
/// them, else it vanishes. (Structural newlines never reach here.)
fn resolve_newlines(mid: Vec<SpTok>) -> Vec<SpTok> {
    let sig: Vec<usize> = mid
        .iter()
        .enumerate()
        .filter(|(_, t)| !t.tok.is_ws_or_newline())
        .map(|(i, _)| i)
        .collect();
    let mut space_at = vec![false; mid.len()];
    for (i, t) in mid.iter().enumerate() {
        if !matches!(t.tok, PpToken::Newline) {
            continue;
        }
        let l = sig.iter().rev().find(|&&j| j < i);
        let r = sig.iter().find(|&&j| j > i);
        if let (Some(&li), Some(&ri)) = (l, r) {
            if needs_space(&mid[li], &mid[ri]) {
                space_at[i] = true;
            }
        }
    }
    mid.into_iter()
        .enumerate()
        .filter_map(|(i, t)| {
            if matches!(t.tok, PpToken::Newline) {
                if space_at[i] {
                    Some(SpTok {
                        tok: PpToken::Ws(" ".to_string()),
                        line: t.line,
                        hidden: t.hidden,
                    })
                } else {
                    None
                }
            } else {
                Some(t)
            }
        })
        .collect()
}

/// Whether whitespace between two adjacent tokens is load-bearing: re-lex
/// the concatenation and compare. A space is always safe; dropping is
/// allowed only when the re-lex reproduces both tokens.
fn needs_space(l: &SpTok, r: &SpTok) -> bool {
    let text = format!("{}{}", l.tok.render(), r.tok.render());
    let rel = pp_lex(&text);
    rel.len() != 2 || rel[0].render() != l.tok.render() || rel[1].render() != r.tok.render()
}

fn raw_tokens(subst: &Subst, pref: ParamRef) -> Vec<SpTok> {
    match pref {
        ParamRef::Fixed(i) => subst.raw[i].clone(),
        ParamRef::Varargs => subst.va_raw.to_vec(),
    }
}

fn cooked_or_raw(subst: &Subst, pref: ParamRef, raw: bool) -> Vec<SpTok> {
    match (pref, raw) {
        (ParamRef::Fixed(i), true) => subst.raw[i].clone(),
        (ParamRef::Fixed(i), false) => subst.cooked[i].clone(),
        (ParamRef::Varargs, true) => subst.va_raw.to_vec(),
        (ParamRef::Varargs, false) => subst.va_cooked.to_vec(),
    }
}

/// Splits paste operands into (left token, right first token); the right
/// side's remaining tokens follow the paste result untouched. Placemarkers
/// count as missing sides.
fn split_paste_sides(left: Option<SpTok>, right: &[SpTok]) -> (Option<SpTok>, Option<SpTok>) {
    let l = left.filter(|t| !is_placemarker(t));
    let r = right.first().filter(|t| !is_placemarker(t)).cloned();
    (l, r)
}

/// Everything after the right side's first token.
fn right_rest(right: &[SpTok]) -> Vec<SpTok> {
    if right.is_empty() {
        Vec::new()
    } else {
        right[1..].to_vec()
    }
}

/// Concatenates two adjacent tokens and re-lexes the result, which must be
/// exactly one preprocessing token.
fn paste_tokens(l: &SpTok, r: &SpTok, line: u32) -> Result<SpTok, ExpandError> {
    let text = format!("{}{}", l.tok.render(), r.tok.render());
    let toks = pp_lex(&text);
    if toks.len() != 1 {
        return Err(ExpandError::new(
            line,
            format!("pasting formed '{text}', an invalid preprocessing token"),
        ));
    }
    Ok(SpTok::new(toks.into_iter().next().expect("one"), line))
}

/// Renders raw argument tokens as a string literal: whitespace runs become
/// single spaces, `"` and `\` gain backslashes.
fn stringize(subst: &Subst, pref: ParamRef) -> String {
    let raw = match pref {
        ParamRef::Fixed(i) => &subst.raw[i],
        ParamRef::Varargs => subst.va_raw,
    };
    let mut s = String::from("\"");
    let mut pending_space = false;
    let mut first = true;
    for t in raw {
        if t.tok.is_ws_or_newline() {
            pending_space = true;
            continue;
        }
        if pending_space && !first {
            s.push(' ');
        }
        pending_space = false;
        first = false;
        for c in t.tok.render().chars() {
            if c == '"' || c == '\\' {
                s.push('\\');
            }
            s.push(c);
        }
    }
    s.push('"');
    s
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pp_token::pp_lex;

    fn obj(body: &str) -> Macro {
        Macro {
            params: None,
            variadic: Variadic::No,
            body: pp_lex(body),
        }
    }

    fn fun(params: &[&str], body: &str) -> Macro {
        Macro {
            params: Some(params.iter().map(|s| s.to_string()).collect()),
            variadic: Variadic::No,
            body: pp_lex(body),
        }
    }

    fn vfun(params: &[&str], body: &str) -> Macro {
        Macro {
            params: Some(params.iter().map(|s| s.to_string()).collect()),
            variadic: Variadic::C99,
            body: pp_lex(body),
        }
    }

    fn expand_src(src: &str, macros: &MacroMap) -> Vec<PpToken> {
        expand_lines(&[src], macros)
    }

    fn expand_lines(lines: &[&str], macros: &MacroMap) -> Vec<PpToken> {
        let mut toks = Vec::new();
        for (k, l) in lines.iter().enumerate() {
            if k > 0 {
                toks.push(SpTok::new(PpToken::Newline, k as u32 + 1));
            }
            toks.extend(pp_lex(l).into_iter().map(|t| SpTok::new(t, k as u32 + 1)));
        }
        expand_stream(toks, macros)
            .unwrap()
            .into_iter()
            .map(|t| t.tok)
            .collect()
    }

    fn render(toks: &[PpToken]) -> String {
        toks.iter().map(|t| t.render()).collect()
    }

    fn map(defs: &[(&str, Macro)]) -> MacroMap {
        defs.iter()
            .map(|(n, m)| (n.to_string(), m.clone()))
            .collect()
    }

    #[test]
    fn self_and_mutual_reference_terminate() {
        let macros = map(&[("SELF", obj("SELF")), ("M1", obj("M2")), ("M2", obj("M1"))]);
        assert_eq!(render(&expand_src("SELF", &macros)), "SELF");
        assert_eq!(render(&expand_src("M1", &macros)), "M1");
    }

    #[test]
    fn rescan_chains_but_spares_following_tokens() {
        let macros = map(&[
            ("A", obj("B")),
            ("B", obj("1")),
            ("SELF", obj("SELF")),
            ("X", obj("1")),
        ]);
        assert_eq!(render(&expand_src("A", &macros)), "1");
        // A blocked self-reference must not hide later source tokens.
        assert_eq!(render(&expand_src("SELF X", &macros)), "SELF 1");
    }

    #[test]
    fn function_recursion_prescan_and_non_call() {
        let macros = map(&[("F", fun(&["x"], "F(x)")), ("G", fun(&["x"], "x"))]);
        assert_eq!(render(&expand_src("F(1)", &macros)), "F(1)");
        assert_eq!(render(&expand_src("G(G(1))", &macros)), "1");
        assert_eq!(render(&expand_src("G + 1", &macros)), "G + 1");
    }

    #[test]
    fn stringize_uses_raw_spelling_and_collapses_newlines() {
        let macros = map(&[("S", fun(&["x"], "#x"))]);
        assert_eq!(render(&expand_src("S(a + b)", &macros)), "\"a + b\"");
        assert_eq!(render(&expand_lines(&["S(a", "b)"], &macros)), "\"a b\"");
    }

    #[test]
    fn paste_relexes_and_empty_side_yields_other() {
        let macros = map(&[("CAT", fun(&["a", "b"], "a ## b"))]);
        assert_eq!(
            expand_src("CAT(<, =)", &macros),
            vec![PpToken::Punct("<=".to_string())]
        );
        assert_eq!(render(&expand_src("CAT(, q)", &macros)), "q");
    }

    #[test]
    fn substituted_newlines_drop_or_space_by_merge_hazard() {
        let macros = map(&[("F", fun(&["x"], "[x]")), ("G", fun(&["x"], "x"))]);
        assert_eq!(render(&expand_lines(&["F(a", "+", "b)"], &macros)), "[a+b]");
        assert_eq!(render(&expand_lines(&["G(p", "q)"], &macros)), "p q");
    }

    #[test]
    fn variadic_join_and_empty() {
        let macros = map(&[
            ("V", vfun(&[], "f(__VA_ARGS__)")),
            (
                "N",
                Macro {
                    params: Some(Vec::new()),
                    variadic: Variadic::Named("args".to_string()),
                    body: pp_lex("args"),
                },
            ),
        ]);
        assert_eq!(render(&expand_src("V(1, 2)", &macros)), "f(1, 2)");
        assert_eq!(render(&expand_src("V()", &macros)), "f()");
        assert_eq!(render(&expand_src("N(7, 8)", &macros)), "7, 8");
    }

    #[test]
    fn gnu_comma_swallow_and_vanish() {
        let macros = map(&[("L", vfun(&["fmt"], "printf(fmt, ##__VA_ARGS__)"))]);
        assert_eq!(render(&expand_src("L(\"x\", 1)", &macros)), "printf(\"x\", 1)");
        assert_eq!(render(&expand_src("L(\"x\")", &macros)), "printf(\"x\")");
        assert_eq!(render(&expand_src("L(\"x\",)", &macros)), "printf(\"x\",)");
    }
}
