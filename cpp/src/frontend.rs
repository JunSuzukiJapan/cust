//! Front end: line splicing, comment stripping, directive splitting.
//!
//! Translation phases 2-3. Block comments spanning lines are handled on the
//! whole text at once; newlines are preserved so line numbers survive.

use std::fmt;

/// One logical line: spliced, comment-free text with its physical span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LogicalLine {
    pub text: String,
    /// 1-based physical line where this logical line starts.
    #[allow(dead_code)] // consumed by diagnostics once they land
    pub start_line: u32,
    /// 1-based physical line where this logical line ends.
    pub end_line: u32,
}

/// A comment-stripping failure. Columns count characters, not bytes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommentError {
    pub line: u32,
    pub column: u32,
    pub message: String,
}

impl fmt::Display for CommentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.column, self.message)
    }
}

/// Runs phases 2-3 and splits the result into logical lines.
pub fn logical_lines(content: &str) -> Result<Vec<LogicalLine>, CommentError> {
    let (spliced, spans) = splice(content);
    if spans.is_empty() {
        return Ok(Vec::new());
    }
    let stripped = strip_comments(&spliced)?;
    let mut out = Vec::with_capacity(spans.len());
    for (i, text) in stripped.split('\n').enumerate() {
        if i == spans.len() {
            debug_assert!(text.is_empty(), "spliced text must end with a newline");
            break;
        }
        out.push(LogicalLine {
            text: text.to_string(),
            start_line: spans[i].0,
            end_line: spans[i].1,
        });
    }
    Ok(out)
}

/// Splits a directive line into (name, rest): `#`, skipping whitespace, an
/// identifier, then the untouched remainder. Returns None for non-directive
/// lines. `#` alone yields an empty name (null directive), as does `#`
/// followed by no identifier (e.g. `# 123`).
pub fn split_directive(line: &str) -> Option<(String, String)> {
    let rest = line.trim_start().strip_prefix('#')?.trim_start();
    let mut chars = rest.chars();
    let valid_start = matches!(chars.next(), Some(c) if c.is_ascii_alphabetic() || c == '_');
    if !valid_start {
        return Some((String::new(), rest.to_string()));
    }
    let end = rest
        .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
        .unwrap_or(rest.len());
    Some((rest[..end].to_string(), rest[end..].to_string()))
}

/// Joins backslash-newline pairs (phase 2). Returns the spliced text plus,
/// per spliced line, its (start, end) physical lines (1-based). `\r\n`
/// counts as one newline. A trailing backslash with no following line is
/// dropped (the standard leaves unterminated files undefined).
fn splice(content: &str) -> (String, Vec<(u32, u32)>) {
    if content.is_empty() {
        return (String::new(), Vec::new());
    }
    let mut pieces: Vec<&str> = content.split('\n').collect();
    if pieces.len() > 1 && pieces.last() == Some(&"") {
        pieces.pop();
    }
    let mut spliced = String::new();
    let mut spans = Vec::new();
    let mut buf = String::new();
    let mut start = 1u32;
    let mut cont = false;
    for (idx, raw) in pieces.iter().enumerate() {
        let phys = (idx + 1) as u32;
        let line = raw.strip_suffix('\r').unwrap_or(raw);
        if !cont {
            start = phys;
        }
        if let Some(without) = line.strip_suffix('\\') {
            buf.push_str(without);
            cont = true;
        } else {
            buf.push_str(line);
            spliced.push_str(&buf);
            spliced.push('\n');
            spans.push((start, phys));
            buf.clear();
            cont = false;
        }
    }
    if cont {
        spliced.push_str(&buf);
        spliced.push('\n');
        spans.push((start, pieces.len() as u32));
    }
    (spliced, spans)
}

/// Strips comments (phase 3). String and character literals (with escapes)
/// protect comment markers. Each block comment becomes one space; newlines
/// inside it are preserved so line numbers survive. Line comments vanish to
/// the newline. Unterminated block comments are an error, reported at the
/// comment's start; an unterminated string is passed through (diagnosing it
/// is the compiler's job).
fn strip_comments(text: &str) -> Result<String, CommentError> {
    #[derive(PartialEq)]
    enum State {
        Normal,
        Line,
        Block,
        Str,
        Chr,
        StrEsc,
        ChrEsc,
    }
    let mut out = String::new();
    let mut state = State::Normal;
    let mut line = 1u32;
    let mut col = 1u32;
    let mut comment_line = 1u32;
    let mut comment_col = 1u32;
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        let cur_line = line;
        let cur_col = col;
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
        match state {
            State::Normal => {
                if c == '/' && chars.peek() == Some(&'/') {
                    chars.next();
                    col += 1;
                    state = State::Line;
                } else if c == '/' && chars.peek() == Some(&'*') {
                    chars.next();
                    col += 1;
                    comment_line = cur_line;
                    comment_col = cur_col;
                    out.push(' ');
                    state = State::Block;
                } else {
                    if c == '"' {
                        state = State::Str;
                    } else if c == '\'' {
                        state = State::Chr;
                    }
                    out.push(c);
                }
            }
            State::Line => {
                if c == '\n' {
                    out.push(c);
                    state = State::Normal;
                }
            }
            State::Block => {
                if c == '*' && chars.peek() == Some(&'/') {
                    chars.next();
                    col += 1;
                    state = State::Normal;
                } else if c == '\n' {
                    out.push(c);
                }
            }
            State::Str => {
                out.push(c);
                if c == '\\' {
                    state = State::StrEsc;
                } else if c == '"' {
                    state = State::Normal;
                }
            }
            State::Chr => {
                out.push(c);
                if c == '\\' {
                    state = State::ChrEsc;
                } else if c == '\'' {
                    state = State::Normal;
                }
            }
            State::StrEsc => {
                out.push(c);
                state = State::Str;
            }
            State::ChrEsc => {
                out.push(c);
                state = State::Chr;
            }
        }
    }
    if state == State::Block {
        return Err(CommentError {
            line: comment_line,
            column: comment_col,
            message: "unterminated block comment".to_string(),
        });
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn splice_joins_continuations_and_tracks_spans() {
        let (text, spans) = splice("a \\\n b \\\n c\nd\n");
        assert_eq!(text, "a  b  c\nd\n");
        assert_eq!(spans, vec![(1, 3), (4, 4)]);
    }

    #[test]
    fn splice_empty_input_has_no_lines() {
        let (text, spans) = splice("");
        assert_eq!(text, "");
        assert!(spans.is_empty());
    }

    #[test]
    fn splice_tolerates_crlf_and_missing_final_newline() {
        let (text, spans) = splice("a \\\r\n b\r\nc");
        assert_eq!(text, "a  b\nc\n");
        assert_eq!(spans, vec![(1, 2), (3, 3)]);
    }

    #[test]
    fn splice_backslash_only_line_does_not_reset_start() {
        // A lone `\` contributes nothing; the span must still start at line 1.
        let (text, spans) = splice("\\\nabc\n");
        assert_eq!(text, "abc\n");
        assert_eq!(spans, vec![(1, 2)]);
    }

    #[test]
    fn strip_line_comment() {
        assert_eq!(strip_comments("int x = 1; // comment\n"), Ok("int x = 1; \n".to_string()));
    }

    #[test]
    fn strip_block_comment_becomes_one_space() {
        assert_eq!(
            strip_comments("int x = /* c */ 1;\n"),
            Ok("int x =   1;\n".to_string())
        );
    }

    #[test]
    fn strip_block_comment_spanning_lines_keeps_newlines() {
        assert_eq!(
            strip_comments("a /* x\ny */ b\n"),
            Ok("a  \n b\n".to_string())
        );
    }

    #[test]
    fn strip_ignores_markers_in_strings_and_chars() {
        assert_eq!(
            strip_comments("char *s = \"/* //\";\nchar c = '/';\n"),
            Ok("char *s = \"/* //\";\nchar c = '/';\n".to_string())
        );
    }

    #[test]
    fn strip_handles_escapes_in_strings_and_chars() {
        assert_eq!(
            strip_comments("char *s = \"a\\\"/*\";\nchar c = '\\'';\n"),
            Ok("char *s = \"a\\\"/*\";\nchar c = '\\'';\n".to_string())
        );
    }

    #[test]
    fn strip_unterminated_block_is_an_error_at_its_start() {
        assert_eq!(
            strip_comments("int x;\n/* oops\n"),
            Err(CommentError {
                line: 2,
                column: 1,
                message: "unterminated block comment".to_string(),
            })
        );
    }

    #[test]
    fn strip_unterminated_string_passes_through() {
        assert_eq!(
            strip_comments("\"abc // x\n"),
            Ok("\"abc // x\n".to_string())
        );
    }

    #[test]
    fn split_directive_forms() {
        assert_eq!(
            split_directive("#define FOO 1"),
            Some(("define".to_string(), " FOO 1".to_string()))
        );
        assert_eq!(
            split_directive("  #  define\tFOO"),
            Some(("define".to_string(), "\tFOO".to_string()))
        );
        assert_eq!(split_directive("#"), Some((String::new(), String::new())));
        assert_eq!(split_directive("   #  "), Some((String::new(), String::new())));
        assert_eq!(
            split_directive("# 123 \"x\""),
            Some((String::new(), "123 \"x\"".to_string()))
        );
        assert_eq!(split_directive("int x;"), None);
        assert_eq!(split_directive(""), None);
    }

    #[test]
    fn logical_lines_combine_splice_and_strip() {
        // Splicing runs first: `a \` + `// c` joins, then the comment eats all.
        let lines = logical_lines("a \\\n// c\nb /* x\ny */ c\nd\n").unwrap();
        // A spanning block comment keeps its newline: lines stay split.
        assert_eq!(lines.len(), 4);
        assert_eq!(lines[0].text, "a ");
        assert_eq!((lines[0].start_line, lines[0].end_line), (1, 2));
        assert_eq!(lines[1].text, "b  ");
        assert_eq!((lines[1].start_line, lines[1].end_line), (3, 3));
        assert_eq!(lines[2].text, " c");
        assert_eq!((lines[2].start_line, lines[2].end_line), (4, 4));
        assert_eq!(lines[3].text, "d");
        assert_eq!((lines[3].start_line, lines[3].end_line), (5, 5));
    }
}
