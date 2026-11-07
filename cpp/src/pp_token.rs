//! Preprocessing tokens and a total lexer for macro bodies and code lines.
//!
//! The lexer never fails: unterminated literals run to end of line (a line
//! never contains a newline). It exists for expansion, so identifiers,
//! `#`/`##`, parens/commas, whole strings/chars, and exact spellings are
//! what matter. Digraph spellings (`<%`, `%:`, ...) lex as ordinary
//! punctuators without digraph equivalence (rendering stays faithful).

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PpToken {
    Ident(String),
    /// A pp-number, raw spelling (never interpreted here).
    Number(String),
    /// String literal with quotes, raw.
    Str(String),
    /// Character literal with quotes, raw.
    Chr(String),
    Hash,
    Paste,
    LParen,
    RParen,
    Comma,
    /// Any other punctuator, maximal munch, exact spelling.
    Punct(String),
    /// A whitespace run (never contains a newline), exact spelling.
    Ws(String),
    /// Line boundary inside a code run. Inserted by the run joiner, never
    /// produced by the lexer.
    Newline,
}

impl PpToken {
    pub fn render(&self) -> &str {
        match self {
            PpToken::Ident(s)
            | PpToken::Number(s)
            | PpToken::Str(s)
            | PpToken::Chr(s)
            | PpToken::Punct(s)
            | PpToken::Ws(s) => s,
            PpToken::Hash => "#",
            PpToken::Paste => "##",
            PpToken::LParen => "(",
            PpToken::RParen => ")",
            PpToken::Comma => ",",
            PpToken::Newline => "\n",
        }
    }

    pub fn is_ws_or_newline(&self) -> bool {
        matches!(self, PpToken::Ws(_) | PpToken::Newline)
    }
}

/// Lexes one physical line into pp-tokens. Total: never fails.
pub fn pp_lex(line: &str) -> Vec<PpToken> {
    let cs: Vec<char> = line.chars().collect();
    let mut out = Vec::new();
    let mut i = 0;
    while i < cs.len() {
        let c = cs[i];
        if matches!(c, ' ' | '\t' | '\x0B' | '\x0C' | '\r') {
            let start = i;
            while i < cs.len() && matches!(cs[i], ' ' | '\t' | '\x0B' | '\x0C' | '\r') {
                i += 1;
            }
            out.push(PpToken::Ws(cs[start..i].iter().collect()));
            continue;
        }
        if c == '#' {
            if cs.get(i + 1) == Some(&'#') {
                out.push(PpToken::Paste);
                i += 2;
            } else {
                out.push(PpToken::Hash);
                i += 1;
            }
            continue;
        }
        match c {
            '(' => {
                out.push(PpToken::LParen);
                i += 1;
                continue;
            }
            ')' => {
                out.push(PpToken::RParen);
                i += 1;
                continue;
            }
            ',' => {
                out.push(PpToken::Comma);
                i += 1;
                continue;
            }
            _ => {}
        }
        if c.is_ascii_alphabetic() || c == '_' || c == '$' {
            let start = i;
            while i < cs.len() && (cs[i].is_ascii_alphanumeric() || cs[i] == '_' || cs[i] == '$') {
                i += 1;
            }
            out.push(PpToken::Ident(cs[start..i].iter().collect()));
            continue;
        }
        if c.is_ascii_digit() || (c == '.' && cs.get(i + 1).is_some_and(|d| d.is_ascii_digit())) {
            let start = i;
            while i < cs.len() && (cs[i].is_ascii_alphanumeric() || cs[i] == '_' || cs[i] == '.') {
                i += 1;
            }
            out.push(PpToken::Number(cs[start..i].iter().collect()));
            continue;
        }
        if c == '\'' || c == '"' {
            let start = i;
            let quote = c;
            i += 1;
            while i < cs.len() {
                if cs[i] == '\\' {
                    i += 2;
                    continue;
                }
                if cs[i] == quote {
                    i += 1;
                    break;
                }
                i += 1;
            }
            let text: String = cs[start..i.min(cs.len())].iter().collect();
            if quote == '"' {
                out.push(PpToken::Str(text));
            } else {
                out.push(PpToken::Chr(text));
            }
            continue;
        }
        // Punctuators, longest match first.
        let rest: String = cs[i..].iter().collect();
        let punct = ["%:%:", "...", "<<=", ">>="]
            .iter()
            .find(|p| rest.starts_with(*p))
            .copied()
            .or_else(|| {
                [
                    "&&", "||", "==", "!=", "<=", ">=", "<<", ">>", "++", "--", "->", "+=", "-=",
                    "*=", "/=", "%=", "&=", "|=", "^=", "<%", "%>", "<:", ":>", "%:",
                ]
                .iter()
                .find(|p| rest.starts_with(*p))
                .copied()
            })
            .unwrap_or_else(|| &rest[..c.len_utf8()]);
        out.push(PpToken::Punct(punct.to_string()));
        i += punct.chars().count();
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn toks(line: &str) -> Vec<PpToken> {
        pp_lex(line)
    }

    #[test]
    fn lexes_identifiers_numbers_and_spacing() {
        assert_eq!(
            toks("int  x1 = 42;"),
            vec![
                PpToken::Ident("int".into()),
                PpToken::Ws("  ".into()),
                PpToken::Ident("x1".into()),
                PpToken::Ws(" ".into()),
                PpToken::Punct("=".into()),
                PpToken::Ws(" ".into()),
                PpToken::Number("42".into()),
                PpToken::Punct(";".into()),
            ]
        );
    }

    #[test]
    fn lexes_hash_paste_parens_comma() {
        assert_eq!(
            toks("#x ## (a, b)"),
            vec![
                PpToken::Hash,
                PpToken::Ident("x".into()),
                PpToken::Ws(" ".into()),
                PpToken::Paste,
                PpToken::Ws(" ".into()),
                PpToken::LParen,
                PpToken::Ident("a".into()),
                PpToken::Comma,
                PpToken::Ws(" ".into()),
                PpToken::Ident("b".into()),
                PpToken::RParen,
            ]
        );
    }

    #[test]
    fn lexes_multi_char_punctuators_maximally() {
        assert_eq!(
            toks("a <<= b ... c >>="),
            vec![
                PpToken::Ident("a".into()),
                PpToken::Ws(" ".into()),
                PpToken::Punct("<<=".into()),
                PpToken::Ws(" ".into()),
                PpToken::Ident("b".into()),
                PpToken::Ws(" ".into()),
                PpToken::Punct("...".into()),
                PpToken::Ws(" ".into()),
                PpToken::Ident("c".into()),
                PpToken::Ws(" ".into()),
                PpToken::Punct(">>=".into()),
            ]
        );
        assert_eq!(toks("a++ + ++b")[1], PpToken::Punct("++".into()));
        assert_eq!(toks("a->b")[1], PpToken::Punct("->".into()));
    }

    #[test]
    fn lexes_strings_and_chars_whole_with_escapes() {
        assert_eq!(
            toks("s(\"a,b(c)\", 'x')"),
            vec![
                PpToken::Ident("s".into()),
                PpToken::LParen,
                PpToken::Str("\"a,b(c)\"".into()),
                PpToken::Comma,
                PpToken::Ws(" ".into()),
                PpToken::Chr("'x'".into()),
                PpToken::RParen,
            ]
        );
        // Unterminated literals run to end of line without failing.
        assert_eq!(toks("\"abc"), vec![PpToken::Str("\"abc".into())]);
        assert_eq!(toks("'ab"), vec![PpToken::Chr("'ab".into())]);
        assert_eq!(
            toks("\"a\\\"b\""),
            vec![PpToken::Str("\"a\\\"b\"".into())]
        );
    }

    #[test]
    fn rendering_is_faithful() {
        for line in [
            "int x = f(a, \"s)\") + 0x10; // kept?",
            "#define F(x) x ## __VA_ARGS__",
            "a\tb  c",
        ] {
            let back: String = toks(line).iter().map(PpToken::render).collect();
            assert_eq!(back, line);
        }
    }
}
