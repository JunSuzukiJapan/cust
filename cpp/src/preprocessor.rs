use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use crate::clock;
use crate::cond::{CondStack, ElifAction};
use crate::eval::eval_if_expression;
use crate::frontend::{logical_lines, split_directive, LogicalLine};
use crate::macros::{expand_stream, Hs, Macro, MacroMap, SpTok, Variadic};
use crate::pp_token::{pp_lex, PpToken};
use crate::{PreprocessError, PreprocessOptions};

/// Maximum `#include` nesting, matching the conventional limit.
const MAX_INCLUDE_DEPTH: usize = 200;

/// A gap of this many consumed lines (plus a taken-`#elif` bonus, see
/// below) resyncs with a marker instead of blank lines, like clang.
const MARKER_GAP_THRESHOLD: u32 = 9;

/// Builtin macros. Static ones live in the macro map; dynamic ones are
/// rewritten after expansion. Redefining or undefining any of them warns
/// (like clang) but takes effect.
const STATIC_BUILTINS: &[(&str, &str)] = &[
    ("__STDC__", "1"),
    ("__STDC_VERSION__", "201710L"),
    ("__STDC_HOSTED__", "1"),
];
const DYNAMIC_BUILTINS: &[&str] = &[
    "__LINE__",
    "__FILE__",
    "__DATE__",
    "__TIME__",
    "__TIMESTAMP__",
    "__COUNTER__",
    "__INCLUDE_LEVEL__",
];

fn is_builtin(name: &str) -> bool {
    STATIC_BUILTINS.iter().any(|(n, _)| *n == name) || DYNAMIC_BUILTINS.contains(&name)
}

/// Whether `s` is a well-formed macro/parameter name. `$` is accepted
/// (a ubiquitous extension).
fn is_ident(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' || c == '$' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$')
}

/// A driver failure with its source position.
#[derive(Debug, Clone, PartialEq, Eq)]
struct DriverError {
    path: PathBuf,
    line: u32,
    column: u32,
    message: String,
}

impl DriverError {
    fn finish(self) -> PreprocessError {
        PreprocessError::new(&self.path, self.line, self.column, self.message)
    }
}

/// Main function that preprocesses the given C source code.
/// - Supports macro definition/expansion, conditional compilation, #include, comment removal, and backslash logical lines.
/// - Outputs #line directives with correct file name and line number when processing #include.
///
/// # Arguments
/// * `content` - Input source code string
/// * `path`    - Path to the input file (used for #include and #line tracking)
/// * `options` - Search paths, predefined/suppressed macros
/// # Returns
/// * Preprocessed source code string
pub fn preprocess_file(
    content: &str,
    path: &Path,
    options: &PreprocessOptions,
) -> Result<String, PreprocessError> {
    let (date, time, start_ctime) = clock::translation_time();
    let mut driver = Driver {
        files: Vec::new(),
        macros: MacroMap::new(),
        disabled_builtin: HashSet::new(),
        cond_stack: CondStack::new(),
        output: String::new(),
        marker_next: None,
        pending: Vec::new(),
        include_paths: options.include_paths.clone(),
        once: HashSet::new(),
        counter: Cell::new(0),
        date,
        time,
        start_ctime,
        gap_taken_elif: false,
        guard_map: HashMap::new(),
        sync_lines: options.emit_line_markers,
        emitted_anything: false,
    };
    driver.seed(options, path).map_err(|e| e.finish())?;
    driver.push_main(content, path).map_err(|e| e.finish())?;
    driver.run().map_err(|e| e.finish())?;
    Ok(driver.output)
}

/// One open source file. The driver processes the stack top; `#include`
/// pushes, end-of-file pops.
struct FileCtx {
    path: PathBuf,
    /// Path spelling used in markers and `__FILE__`.
    display: String,
    lines: Vec<LogicalLine>,
    pos: usize,
    /// Conditional depth at entry: groups opened beyond it must close
    /// before this file ends.
    cond_depth: usize,
    /// User `#line` mappings, in directive order.
    line_adjust: Vec<LineAdjust>,
    /// `__TIMESTAMP__` for this file (already quoted).
    timestamp: String,
    is_main: bool,
    /// Physical line in the parent to resume at, if included.
    resume_phys: Option<u32>,
    /// Controlling macro, if the file matches the include-guard pattern.
    guard: Option<String>,
}

/// A `#line N ["file"]` on physical line `phys_start - 1`: from
/// `phys_start` on, physical line P reads as logical
/// `logical_start + (P - phys_start)` in `file` (or the current file).
struct LineAdjust {
    phys_start: u32,
    logical_start: u32,
    file: Option<String>,
}

struct Driver {
    files: Vec<FileCtx>,
    macros: MacroMap,
    /// Builtins suppressed by `#undef`/`-U`: the post-expansion rewrite
    /// leaves them alone.
    disabled_builtin: HashSet<String>,
    cond_stack: CondStack,
    output: String,
    /// The (logical line, file) the reader expects next; `None` forces a
    /// marker before the next code line.
    marker_next: Option<(u32, String)>,
    /// Accumulated code run: contiguous lines expanded as one token stream
    /// so invocations may span lines. Directives flush it.
    pending: Vec<LogicalLine>,
    include_paths: Vec<PathBuf>,
    /// Canonical paths guarded by `#pragma once`.
    once: HashSet<PathBuf>,
    /// Next `__COUNTER__` value.
    counter: Cell<u32>,
    /// Translation-start `__DATE__`/`__TIME__` (already quoted).
    date: String,
    time: String,
    /// Fallback `__TIMESTAMP__` when a file has no readable mtime.
    start_ctime: String,
    /// Whether the current gap (lines since the last emission) holds a
    /// skipped `#elif` of a taken branch: clang counts such a gap as one
    /// line longer when deciding marker-vs-blanks (reverse-engineered).
    gap_taken_elif: bool,
    /// Controlling macro per canonical header path, for include-guard
    /// skipping.
    guard_map: HashMap<PathBuf, String>,
    /// Whether to emit `#line` markers and gap blanks (`-P` disables
    /// both: dense output).
    sync_lines: bool,
    /// Whether any line was emitted (for `-P`'s leading-blank quirk).
    emitted_anything: bool,
}

impl Driver {
    /// Seeds predefined macros, then `-D` definitions, then `-U`
    /// suppressions (suppression wins across the two lists).
    fn seed(&mut self, options: &PreprocessOptions, path: &Path) -> Result<(), DriverError> {
        for (name, value) in STATIC_BUILTINS {
            self.macros.insert(
                name.to_string(),
                Macro {
                    params: None,
                    variadic: Variadic::No,
                    body: pp_lex(value),
                },
            );
        }
        for (name, value) in &options.defines {
            if !is_ident(name) {
                return Err(DriverError {
                    path: path.to_path_buf(),
                    line: 1,
                    column: 1,
                    message: format!("invalid macro name '{name}'"),
                });
            }
            let body = pp_lex(value.as_deref().unwrap_or("1"));
            self.disabled_builtin.remove(name);
            self.macros.insert(
                name.clone(),
                Macro {
                    params: None,
                    variadic: Variadic::No,
                    body,
                },
            );
        }
        for name in &options.undefs {
            if !is_ident(name) {
                return Err(DriverError {
                    path: path.to_path_buf(),
                    line: 1,
                    column: 1,
                    message: format!("invalid macro name '{name}'"),
                });
            }
            self.macros.remove(name);
            if is_builtin(name) {
                self.disabled_builtin.insert(name.clone());
            }
        }
        Ok(())
    }

    fn push_main(&mut self, content: &str, path: &Path) -> Result<(), DriverError> {
        let lines = logical_lines(content).map_err(|e| DriverError {
            path: path.to_path_buf(),
            line: e.line,
            column: e.column,
            message: e.message,
        })?;
        let timestamp =
            clock::file_timestamp(path).unwrap_or_else(|| self.start_ctime.clone());
        let display = path.display().to_string();
        let guard = detect_guard(&lines);
        self.files.push(FileCtx {
            path: path.to_path_buf(),
            display: display.clone(),
            lines,
            pos: 0,
            cond_depth: 0,
            line_adjust: Vec::new(),
            timestamp,
            is_main: true,
            resume_phys: None,
            guard,
        });
        // The reader starts synced to line 1; consumed lines below become
        // gap-fill blanks (or a resync marker past the threshold).
        if self.sync_lines {
            self.output.push_str(&format!("#line 1 {}\n", quot(&display)));
        }
        self.marker_next = Some((1, display));
        Ok(())
    }

    fn run(&mut self) -> Result<(), DriverError> {
        while !self.files.is_empty() {
            let next = self
                .files
                .last()
                .and_then(|f| f.lines.get(f.pos).cloned());
            match next {
                Some(logical) => {
                    self.files.last_mut().expect("current file").pos += 1;
                    self.process_line(&logical)?;
                }
                None => {
                    self.flush_run()?;
                    self.pop_file()?;
                }
            }
        }
        Ok(())
    }

    /// Pops the finished file: a conditional group it left open is an
    /// error at the opener, and an include holds its resume marker for
    /// the next emission.
    fn pop_file(&mut self) -> Result<(), DriverError> {
        let file = self.files.pop().expect("file to pop");
        if self.cond_stack.depth() > file.cond_depth {
            let opener = self
                .cond_stack
                .opener_line(file.cond_depth)
                .unwrap_or(1);
            let column = directive_name_col(file.line_text(opener));
            return Err(DriverError {
                path: file.path,
                line: opener,
                column,
                message: "unterminated conditional directive".to_string(),
            });
        }
        if let Some(guard) = &file.guard {
            let key = fs::canonicalize(&file.path).unwrap_or_else(|_| file.path.clone());
            self.guard_map.insert(key, guard.clone());
        }
        // The resume marker breaks the parent's gap even when nothing
        // follows (clang emits it eagerly, including at end of input).
        self.gap_taken_elif = false;
        if let Some(resume) = file.resume_phys {
            let (logical, display) = self.map_position(resume);
            if self.sync_lines {
                self.output.push_str(&format!("#line {logical} {}\n", quot(&display)));
            }
            self.marker_next = Some((logical, display));
        }
        Ok(())
    }

    /// Syncs the reader ahead of emitting a line with physical origin
    /// `phys` in the current file: the gap since the last emission becomes
    /// blanks, or a resync marker past the threshold (file changes and
    /// backward jumps always mark). Returns the mapped position the line
    /// prints at. Without line sync (`-P`), output is dense except for
    /// one quirk both clang and gcc share: a single blank precedes the
    /// first emission when it sits at mapped line 2.
    fn sync_before(&mut self, phys: u32) -> (u32, String) {
        let (logical, display) = self.map_position(phys);
        if !self.sync_lines {
            if !self.emitted_anything && logical == 2 {
                self.output.push('\n');
            }
            self.emitted_anything = true;
            self.marker_next = Some((logical, display.clone()));
            return (logical, display);
        }
        let gap = match &self.marker_next {
            Some((reader, file)) if *file == display && *reader <= logical => {
                logical - reader
            }
            _ => u32::MAX,
        };
        let effective = gap.saturating_add(u32::from(self.gap_taken_elif));
        if effective < MARKER_GAP_THRESHOLD {
            for _ in 0..gap {
                self.output.push('\n');
            }
        } else {
            self.output.push_str(&format!("#line {logical} {}\n", quot(&display)));
        }
        self.gap_taken_elif = false;
        self.marker_next = Some((logical, display.clone()));
        (logical, display)
    }

    /// Maps a physical line of the current file to (logical line, file).
    fn map_position(&self, phys: u32) -> (u32, String) {
        let file = self.current();
        let mut logical = phys;
        let mut display = file.display.clone();
        for adjust in file.line_adjust.iter().rev() {
            if adjust.phys_start <= phys {
                logical = adjust
                    .logical_start
                    .saturating_add(phys - adjust.phys_start);
                if let Some(f) = &adjust.file {
                    display = f.clone();
                }
                break;
            }
        }
        (logical, display)
    }

    fn current(&self) -> &FileCtx {
        self.files.last().expect("current file")
    }

    fn err(&self, line: u32, column: u32, message: impl Into<String>) -> DriverError {
        DriverError {
            path: self.current().path.clone(),
            line,
            column,
            message: message.into(),
        }
    }

    /// A non-fatal diagnostic on stderr; preprocessing continues.
    fn warn(&self, line: u32, column: u32, message: &str) {
        eprintln!(
            "{}:{line}:{column}: warning: {message}",
            self.current().path.display()
        );
    }

    fn active(&self) -> bool {
        self.cond_stack.active()
    }

    /// Whether `name` counts as defined (a macro, or an enabled builtin).
    fn is_defined(&self, name: &str) -> bool {
        self.macros.contains_key(name)
            || (is_builtin(name) && !self.disabled_builtin.contains(name))
    }

    fn process_line(&mut self, logical: &LogicalLine) -> Result<(), DriverError> {
        if let Some((name, rest)) = split_directive(&logical.text) {
            self.flush_run()?;
            self.directive(&name, &rest, logical)
        } else if self.active() {
            self.pending.push(logical.clone());
            Ok(())
        } else {
            Ok(())
        }
    }

    /// Expands the accumulated code run and emits it with `#line` resync:
    /// a marker precedes every code line whose mapped origin is not the
    /// expected next line.
    fn flush_run(&mut self) -> Result<(), DriverError> {
        if self.pending.is_empty() {
            return Ok(());
        }
        let mut stream = Vec::new();
        for (k, logical) in self.pending.iter().enumerate() {
            if k > 0 {
                stream.push(SpTok {
                    tok: PpToken::Newline,
                    line: logical.start_line,
                    hidden: Hs::empty(),
                });
            }
            for tok in pp_lex(&logical.text) {
                stream.push(SpTok {
                    tok,
                    line: logical.start_line,
                    hidden: Hs::empty(),
                });
            }
        }
        self.pending.clear();
        let mut expanded = expand_stream(stream, &self.macros)
            .map_err(|e| self.err(e.line, 1, e.message))?;
        self.rewrite_builtins(&mut expanded);
        let mut current: Vec<&SpTok> = Vec::new();
        let mut lines: Vec<Vec<&SpTok>> = Vec::new();
        for tok in &expanded {
            if matches!(tok.tok, PpToken::Newline) {
                lines.push(std::mem::take(&mut current));
            } else {
                current.push(tok);
            }
        }
        lines.push(current);
        for toks in &lines {
            self.emit_code_line(toks)?;
        }
        Ok(())
    }

    /// Rewrites dynamic builtins (`__LINE__` etc.) after expansion, so both
    /// source tokens and macro-introduced ones read the use site. Runs on
    /// code runs and on expanded directive operands alike.
    fn rewrite_builtins(&self, toks: &mut [SpTok]) {
        for t in toks.iter_mut() {
            let PpToken::Ident(name) = &t.tok else {
                continue;
            };
            if self.disabled_builtin.contains(name) {
                continue;
            }
            let replacement = match name.as_str() {
                "__LINE__" => PpToken::Number(self.map_position(t.line).0.to_string()),
                "__FILE__" => PpToken::Str(quot(&self.map_position(t.line).1)),
                "__DATE__" => PpToken::Str(self.date.clone()),
                "__TIME__" => PpToken::Str(self.time.clone()),
                "__TIMESTAMP__" => PpToken::Str(self.current().timestamp.clone()),
                "__COUNTER__" => {
                    let n = self.counter.get();
                    self.counter.set(n + 1);
                    PpToken::Number(n.to_string())
                }
                "__INCLUDE_LEVEL__" => PpToken::Number((self.files.len() - 1).to_string()),
                _ => continue,
            };
            t.tok = replacement;
        }
    }

    fn emit_code_line(&mut self, toks: &[&SpTok]) -> Result<(), DriverError> {
        let segments = self.split_pragma_ops(toks)?;
        if segments.is_empty() {
            // No tokens to print: silent, like clang (the gap continues).
            return Ok(());
        }
        for segment in &segments {
            match segment {
                Segment::Code { toks, pad } => {
                    let origin = toks.first().map(|t| t.line).unwrap_or(1);
                    let (logical, display) = self.sync_before(origin);
                    for _ in 0..*pad {
                        self.output.push(' ');
                    }
                    for t in toks {
                        self.output.push_str(t.tok.render());
                    }
                    self.output.push('\n');
                    self.marker_next = Some((logical.saturating_add(1), display));
                }
                Segment::Pragma { text, line, column } => {
                    self.pragma_text(text, *line, *column)?;
                }
            }
        }
        Ok(())
    }

    /// Splits `_Pragma` operators out of a code line. Each becomes its own
    /// `#pragma` line and splits the code around it; the code after it keeps
    /// its original column (like clang). Parts without significant tokens
    /// are suppressed. Empty means the line passes through untouched.
    fn split_pragma_ops<'a>(
        &self,
        toks: &[&'a SpTok],
    ) -> Result<Vec<Segment<'a>>, DriverError> {
        if !toks
            .iter()
            .any(|t| matches!(&t.tok, PpToken::Ident(n) if n == "_Pragma"))
        {
            if toks.iter().any(|t| !t.tok.is_ws_or_newline()) {
                return Ok(vec![Segment::Code { toks: toks.to_vec(), pad: 0 }]);
            }
            return Ok(Vec::new());
        }
        let mut segments = Vec::new();
        let mut code: Vec<&SpTok> = Vec::new();
        let mut pad = 0usize;
        let mut col = 0usize;
        let mut i = 0;
        while i < toks.len() {
            if let Some(op) = self.match_pragma_op(toks, i, col)? {
                if code.iter().any(|t| !t.tok.is_ws_or_newline()) {
                    segments.push(Segment::Code {
                        toks: std::mem::take(&mut code),
                        pad,
                    });
                }
                col += op.width;
                pad = col;
                segments.push(Segment::Pragma {
                    text: op.text,
                    line: op.line,
                    column: op.column,
                });
                i = op.next;
            } else {
                col += toks[i].tok.render().chars().count();
                code.push(toks[i]);
                i += 1;
            }
        }
        if code.iter().any(|t| !t.tok.is_ws_or_newline()) {
            segments.push(Segment::Code { toks: code, pad });
        }
        Ok(segments)
    }

    /// Matches `_Pragma ( "text" )` at `toks[i]` (`col` is the rendered
    /// width before it, for diagnostics). A bare `_Pragma` not followed by
    /// `(` is an ordinary identifier.
    fn match_pragma_op(
        &self,
        toks: &[&SpTok],
        i: usize,
        col: usize,
    ) -> Result<Option<PragmaOp>, DriverError> {
        if !matches!(&toks[i].tok, PpToken::Ident(n) if n == "_Pragma") {
            return Ok(None);
        }
        let err_col = (col + 1) as u32;
        let mut j = i + 1;
        while j < toks.len() && matches!(toks[j].tok, PpToken::Ws(_)) {
            j += 1;
        }
        if j >= toks.len() || !matches!(toks[j].tok, PpToken::LParen) {
            return Ok(None);
        }
        j += 1;
        while j < toks.len() && matches!(toks[j].tok, PpToken::Ws(_)) {
            j += 1;
        }
        let literal = match toks.get(j) {
            Some(t) => match &t.tok {
                PpToken::Str(s) => s.clone(),
                _ => {
                    return Err(self.err(
                        toks[i].line,
                        err_col,
                        "_Pragma takes a parenthesized string literal",
                    ));
                }
            },
            None => {
                return Err(self.err(
                    toks[i].line,
                    err_col,
                    "_Pragma takes a parenthesized string literal",
                ));
            }
        };
        j += 1;
        while j < toks.len() && matches!(toks[j].tok, PpToken::Ws(_)) {
            j += 1;
        }
        if j >= toks.len() || !matches!(toks[j].tok, PpToken::RParen) {
            return Err(self.err(
                toks[i].line,
                err_col,
                "_Pragma takes a parenthesized string literal",
            ));
        }
        let width: usize = toks[i..=j].iter().map(|t| t.tok.render().chars().count()).sum();
        Ok(Some(PragmaOp {
            text: destringize(&literal),
            next: j + 1,
            width,
            line: toks[i].line,
            column: err_col,
        }))
    }

    fn eval_condition(&self, rest: &str, line: u32, column: u32) -> Result<bool, DriverError> {
        let path = self.current().path.clone();
        eval_if_expression(
            rest,
            &|name| self.is_defined(name),
            &|text| {
                self.expand_text(text, line)
                    .map_err(|e| crate::eval::EvalError { message: e.message })
            },
            &|message| {
                eprintln!("{}:{line}:{column}: warning: {message}", path.display());
            },
        )
        .map_err(|e| self.err(line, column, e.message))
    }

    /// Expands directive-operand text with the token engine, then rewrites
    /// dynamic builtins against the operand's line.
    fn expand_text(&self, text: &str, line: u32) -> Result<String, DriverError> {
        let toks: Vec<SpTok> = pp_lex(text)
            .into_iter()
            .map(|tok| SpTok {
                tok,
                line,
                hidden: Hs::empty(),
            })
            .collect();
        let mut expanded = expand_stream(toks, &self.macros)
            .map_err(|e| self.err(e.line, 1, e.message))?;
        self.rewrite_builtins(&mut expanded);
        Ok(expanded.iter().map(|t| t.tok.render()).collect())
    }

    fn directive(&mut self, name: &str, rest: &str, logical: &LogicalLine) -> Result<(), DriverError> {
        let line = logical.start_line;
        let (_hash_col, name_col, rest_col) = directive_cols(&logical.text);
        match name {
            "undef" => {
                if self.active() {
                    let operand = Self::directive_ident(rest, line, rest_col, self)?;
                    Self::check_extra_tokens(rest, operand.len(), "undef", line, rest_col, self);
                    if operand == "defined" {
                        return Err(self.err(line, rest_col, "'defined' cannot be used as a macro name"));
                    }
                    if is_builtin(operand) {
                        self.warn(line, rest_col, "undefining builtin macro");
                        self.disabled_builtin.insert(operand.to_string());
                    }
                    self.macros.remove(operand);
                }
                Ok(())
            }
            "ifdef" => {
                if self.active() {
                    let operand = Self::directive_ident(rest, line, rest_col, self)?;
                    Self::check_extra_tokens(rest, operand.len(), "ifdef", line, rest_col, self);
                    let cond = self.is_defined(operand);
                    self.cond_stack.open(cond, line);
                } else {
                    self.cond_stack.open(false, line);
                }
                Ok(())
            }
            "ifndef" => {
                if self.active() {
                    let operand = Self::directive_ident(rest, line, rest_col, self)?;
                    Self::check_extra_tokens(rest, operand.len(), "ifndef", line, rest_col, self);
                    let cond = !self.is_defined(operand);
                    self.cond_stack.open(cond, line);
                } else {
                    self.cond_stack.open(false, line);
                }
                Ok(())
            }
            "if" => {
                let cond = if self.active() {
                    self.eval_condition(rest, line, rest_col)?
                } else {
                    false
                };
                self.cond_stack.open(cond, line);
                Ok(())
            }
            "elif" => {
                // Extra tokens are part of the expression (a parse error if
                // junk); the structural check runs even in skipped groups.
                // A skipped `#elif` of a taken branch lengthens the gap.
                let enclosing = self.cond_stack.enclosing_active();
                match self.cond_stack.elif_begin() {
                    Ok(ElifAction::Evaluate) => {
                        let cond = self.eval_condition(rest, line, rest_col)?;
                        self.cond_stack.elif_end(cond);
                        Ok(())
                    }
                    Ok(ElifAction::Skip) => {
                        if enclosing {
                            self.gap_taken_elif = true;
                        }
                        Ok(())
                    }
                    Err(e) => Err(self.err(line, name_col, e.message)),
                }
            }
            "else" => {
                Self::check_extra_tokens(rest, 0, "else", line, rest_col, self);
                self.cond_stack
                    .else_()
                    .map_err(|e| self.err(line, name_col, e.message))
            }
            "endif" => {
                Self::check_extra_tokens(rest, 0, "endif", line, rest_col, self);
                self.cond_stack
                    .close()
                    .map_err(|e| self.err(line, name_col, e.message))
            }
            "define" => {
                if self.active() {
                    self.define(rest, line, rest_col)?;
                }
                Ok(())
            }
            "include" => {
                if self.active() {
                    self.include(rest, rest_col, logical)?;
                }
                Ok(())
            }
            "line" => {
                if self.active() {
                    self.do_line(rest, rest_col, logical, false)?;
                }
                Ok(())
            }
            "error" => {
                if self.active() {
                    let message = render_tokens(rest);
                    return Err(self.err(line, name_col, message));
                }
                Ok(())
            }
            "warning" => {
                if self.active() {
                    let message = render_tokens(rest);
                    self.warn(line, name_col, &message);
                }
                Ok(())
            }
            "pragma" => {
                if self.active() {
                    self.pragma_directive(rest, logical)?;
                }
                Ok(())
            }
            "" if rest.trim().is_empty() => {
                // Null directive: dropped.
                Ok(())
            }
            "" => {
                // Bare `#` with operands is a line directive; with no name
                // the operand starts where the name would.
                if self.active() {
                    self.do_line(rest, name_col, logical, true)?;
                }
                Ok(())
            }
            _ => {
                if self.active() {
                    return Err(self.err(line, name_col, "invalid preprocessing directive"));
                }
                Ok(())
            }
        }
    }

    /// The single identifier operand of `#ifdef` / `#ifndef` / `#undef`.
    fn directive_ident<'a>(
        rest: &'a str,
        line: u32,
        rest_col: u32,
        driver: &Driver,
    ) -> Result<&'a str, DriverError> {
        match rest.split_whitespace().next() {
            None => Err(driver.err(line, rest_col, "macro name missing")),
            Some(name) if is_ident(name) => Ok(name),
            Some(_) => Err(driver.err(line, rest_col, "macro name must be an identifier")),
        }
    }

    /// Warns about tokens after the operand of single-operand directives
    /// (`operand_len` chars consumed) or bare directives (`0`).
    fn check_extra_tokens(
        rest: &str,
        operand_len: usize,
        directive: &str,
        line: u32,
        rest_col: u32,
        driver: &Driver,
    ) {
        let leading = rest.len() - rest.trim_start().len();
        let trimmed = &rest[leading..];
        let after_operand = trimmed.get(operand_len..).unwrap_or("");
        let middle = after_operand.len() - after_operand.trim_start().len();
        if !after_operand[middle..].is_empty() {
            let column = rest_col + trimmed[..operand_len + middle].chars().count() as u32;
            driver.warn(
                line,
                column,
                &format!("extra tokens at end of #{directive} directive"),
            );
        }
    }

    fn define(&mut self, rest: &str, line: u32, rest_col: u32) -> Result<(), DriverError> {
        let rest = rest.trim();
        let name_len = rest
            .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == '$'))
            .unwrap_or(rest.len());
        let (name, after) = (&rest[..name_len], &rest[name_len..]);
        if name.is_empty() {
            if rest.is_empty() {
                return Err(self.err(line, rest_col, "macro name missing"));
            } else {
                return Err(self.err(line, rest_col, "macro name must be an identifier"));
            }
        }
        if !is_ident(name) {
            return Err(self.err(line, rest_col, "macro name must be an identifier"));
        }
        if name == "defined" {
            return Err(self.err(line, rest_col, "'defined' cannot be used as a macro name"));
        }
        if is_builtin(name) {
            self.warn(line, rest_col, "redefining builtin macro");
            self.disabled_builtin.remove(name);
        }
        // Only `(` immediately after the name makes it function-like;
        // `#define FOO (x)` is object-like with body `(x)`.
        if !after.starts_with('(') {
            let body = pp_lex(after.trim());
            Self::validate_body(&body, None, line, rest_col, self)?;
            self.macros.insert(
                name.to_string(),
                Macro {
                    params: None,
                    variadic: Variadic::No,
                    body,
                },
            );
            return Ok(());
        }
        let close = after
            .find(')')
            .ok_or_else(|| self.err(line, rest_col, "expected ')' in macro parameter list"))?;
        let args_str = &after[1..close];
        let body = pp_lex(after[close + 1..].trim());
        let (params, variadic) = Self::parse_params(args_str, line, rest_col, self)?;
        Self::validate_body(&body, Some((&params, &variadic)), line, rest_col, self)?;
        self.macros.insert(
            name.to_string(),
            Macro {
                params: Some(params),
                variadic,
                body,
            },
        );
        Ok(())
    }

    /// Parses a function-like parameter list (without the parens). Accepts
    /// C99 `...` and GNU `name...`, which must come last.
    fn parse_params(
        args_str: &str,
        line: u32,
        rest_col: u32,
        driver: &Driver,
    ) -> Result<(Vec<String>, Variadic), DriverError> {
        if args_str.trim().is_empty() {
            return Ok((Vec::new(), Variadic::No));
        }
        let mut params = Vec::new();
        let mut variadic = Variadic::No;
        let parts: Vec<&str> = args_str.split(',').collect();
        for (i, part) in parts.iter().enumerate() {
            let part = part.trim();
            let last = i + 1 == parts.len();
            if part == "..." {
                if !last {
                    return Err(driver.err(line, rest_col, "`...` must be the last macro parameter"));
                }
                variadic = Variadic::C99;
                continue;
            }
            if let Some(name) = part.strip_suffix("...") {
                if !last {
                    return Err(driver.err(line, rest_col, "`...` must be the last macro parameter"));
                }
                if !is_ident(name) {
                    return Err(driver.err(line, rest_col, "macro parameter must be an identifier"));
                }
                if params.contains(&name.to_string()) {
                    return Err(driver.err(line, rest_col, "duplicate macro parameter"));
                }
                variadic = Variadic::Named(name.to_string());
                continue;
            }
            if !is_ident(part) {
                return Err(driver.err(line, rest_col, "macro parameter must be an identifier"));
            }
            if params.contains(&part.to_string()) {
                return Err(driver.err(line, rest_col, "duplicate macro parameter"));
            }
            params.push(part.to_string());
        }
        Ok((params, variadic))
    }

    /// Rejects misplaced `#`/`##` at definition time (clang diagnoses these
    /// without any use).
    fn validate_body(
        body: &[PpToken],
        params: Option<(&[String], &Variadic)>,
        line: u32,
        rest_col: u32,
        driver: &Driver,
    ) -> Result<(), DriverError> {
        // Columns point at the operand; the offending token's own column
        // is not tracked.
        let significant: Vec<&PpToken> =
            body.iter().filter(|t| !t.is_ws_or_newline()).collect();
        if significant
            .first()
            .is_some_and(|t| matches!(t, PpToken::Paste))
        {
            return Err(driver.err(line, rest_col, "'##' cannot appear at start of macro expansion"));
        }
        if significant
            .last()
            .is_some_and(|t| matches!(t, PpToken::Paste))
        {
            return Err(driver.err(line, rest_col, "'##' cannot appear at end of macro expansion"));
        }
        let mut i = 0;
        while i < significant.len() {
            if matches!(significant[i], PpToken::Hash) {
                let operand_ok = match significant.get(i + 1) {
                    Some(PpToken::Ident(n)) => match &params {
                        Some((ps, v)) => {
                            ps.contains(n) || (n == "__VA_ARGS__" && matches!(v, Variadic::C99))
                        }
                        None => false,
                    },
                    _ => false,
                };
                if !operand_ok {
                    return Err(driver.err(line, rest_col, "'#' is not followed by a macro parameter"));
                }
            }
            i += 1;
        }
        Ok(())
    }

    fn include(
        &mut self,
        rest: &str,
        rest_col: u32,
        logical: &LogicalLine,
    ) -> Result<(), DriverError> {
        let line = logical.start_line;
        let expanded = self.expand_text(rest, line)?;
        let trimmed = expanded.trim();
        let (name, is_quote, trailing) = match parse_header_operand(trimmed) {
            Ok(parsed) => parsed,
            Err(HeaderParseError::ExpectedFilename) => {
                return Err(self.err(line, rest_col, "expected \"FILENAME\" or <FILENAME>"));
            }
            Err(HeaderParseError::UnterminatedString) => {
                return Err(self.err(line, rest_col, "missing terminating '\"' character"));
            }
        };
        if !trailing.trim().is_empty() {
            let prefix = trimmed.len() - trailing.len();
            let column = rest_col + trimmed[..prefix].chars().count() as u32;
            self.warn(line, column, "extra tokens at end of #include directive");
        }
        let mut candidates = Vec::new();
        if is_quote {
            if let Some(parent) = self.current().path.parent() {
                candidates.push(parent.join(&name));
            }
        }
        candidates.extend(self.include_paths.iter().map(|p| p.join(&name)));
        let mut found = None;
        for candidate in &candidates {
            let key = fs::canonicalize(candidate).unwrap_or_else(|_| candidate.clone());
            if self.once.contains(&key) {
                return Ok(());
            }
            if self
                .guard_map
                .get(&key)
                .is_some_and(|guard| self.is_defined(guard))
            {
                return Ok(());
            }
            if let Ok(content) = fs::read_to_string(candidate) {
                found = Some((candidate.clone(), content));
                break;
            }
        }
        let Some((inc_path, inc_content)) = found else {
            return Err(self.err(line, rest_col, format!("'{name}' file not found")));
        };
        if self.files.len() >= MAX_INCLUDE_DEPTH {
            return Err(self.err(line, rest_col, "#include nested too deeply"));
        }
        let lines = logical_lines(&inc_content).map_err(|e| DriverError {
            path: inc_path.clone(),
            line: e.line,
            column: e.column,
            message: e.message,
        })?;
        let guard = detect_guard(&lines);
        if guard.as_ref().is_some_and(|g| self.is_defined(g)) {
            let key = fs::canonicalize(&inc_path).unwrap_or_else(|_| inc_path.clone());
            self.guard_map.insert(key, guard.clone().expect("guard"));
            return Ok(());
        }
        let display = inc_path.display().to_string();
        if self.sync_lines {
            self.output.push_str(&format!("#line 1 {}\n", quot(&display)));
        }
        self.marker_next = Some((1, display.clone()));
        self.gap_taken_elif = false;
        let timestamp =
            clock::file_timestamp(&inc_path).unwrap_or_else(|| self.start_ctime.clone());
        self.files.push(FileCtx {
            path: inc_path,
            display,
            lines,
            pos: 0,
            cond_depth: self.cond_stack.depth(),
            line_adjust: Vec::new(),
            timestamp,
            is_main: false,
            resume_phys: Some(logical.end_line + 1),
            guard,
        });
        Ok(())
    }

    /// Handles `#line` and bare-`#` operands: macro-expand, then expect a
    /// u32 line number with an optional filename. A bare `#` whose operand
    /// is not a number is not a directive at all.
    fn do_line(
        &mut self,
        rest: &str,
        rest_col: u32,
        logical: &LogicalLine,
        bare: bool,
    ) -> Result<(), DriverError> {
        let line = logical.start_line;
        let expanded = self.expand_text(rest, line)?;
        let trimmed = expanded.trim();
        let toks = pp_lex(trimmed);
        let sig: Vec<&PpToken> = toks.iter().filter(|t| !t.is_ws_or_newline()).collect();
        let number = match sig.first() {
            Some(PpToken::Number(n)) if n.chars().all(|c| c.is_ascii_digit()) => {
                n.parse::<u32>().ok()
            }
            _ => None,
        };
        let Some(number) = number else {
            if bare {
                return Err(self.err(line, rest_col, "invalid preprocessing directive"));
            }
            return Err(
                self.err(line, rest_col, "#line directive requires a positive integer argument")
            );
        };
        let mut file = None;
        if let Some(second) = sig.get(1) {
            let PpToken::Str(s) = second else {
                return Err(self.err(
                    line,
                    self.token_column(&toks, 1, rest_col, trimmed),
                    "invalid filename for #line directive",
                ));
            };
            if !s.ends_with('"') || s.len() < 2 {
                return Err(self.err(
                    line,
                    self.token_column(&toks, 1, rest_col, trimmed),
                    "missing terminating '\"' character",
                ));
            }
            file = Some(unescape_filename(&s[1..s.len() - 1]));
        }
        if sig.len() > 2 {
            self.warn(
                line,
                self.token_column(&toks, 2, rest_col, trimmed),
                "extra tokens at end of #line directive",
            );
        }
        // Without a filename the current mapped file carries over (which a
        // previous `#line` may have overridden), so resolve it now.
        let file = file.or_else(|| Some(self.map_position(logical.end_line).1));
        self.current_mut().line_adjust.push(LineAdjust {
            phys_start: logical.end_line + 1,
            logical_start: number,
            file,
        });
        Ok(())
    }

    /// Column of the `n`th significant token of a re-lexed operand string.
    fn token_column(&self, toks: &[PpToken], n: usize, rest_col: u32, trimmed: &str) -> u32 {
        let mut seen = 0;
        let mut width = 0usize;
        for tok in toks {
            if !tok.is_ws_or_newline() {
                if seen == n {
                    break;
                }
                seen += 1;
            }
            width += tok.render().chars().count();
        }
        // Re-lexing is exact for direct operands; macro expansion can shift
        // it, so clamp into the operand.
        let max = trimmed.chars().count() as u32;
        rest_col + width.min(max as usize) as u32
    }

    fn current_mut(&mut self) -> &mut FileCtx {
        self.files.last_mut().expect("current file")
    }

    fn pragma_directive(&mut self, rest: &str, logical: &LogicalLine) -> Result<(), DriverError> {
        let line = logical.start_line;
        let (_, _, rest_col) = directive_cols(&logical.text);
        if rest.split_whitespace().next() == Some("once") {
            return self.pragma_once(line, rest_col);
        }
        if pragma_consumed(rest) {
            return Ok(());
        }
        // Any other pragma passes through verbatim (comments already
        // stripped), syncing like a code line at its first physical line.
        let start = logical.start_line;
        let text = logical.text.clone();
        let (mapped, display) = self.sync_before(start);
        self.output.push_str(&text);
        self.output.push('\n');
        self.marker_next = Some((mapped.saturating_add(1), display));
        Ok(())
    }

    /// Handles one `_Pragma("...")` string as pragma text.
    fn pragma_text(&mut self, text: &str, line: u32, column: u32) -> Result<(), DriverError> {
        if text.split_whitespace().next() == Some("once") {
            return self.pragma_once(line, column);
        }
        if pragma_consumed(text) {
            return Ok(());
        }
        let (logical, display) = self.sync_before(line);
        self.output.push_str("#pragma ");
        self.output.push_str(text.trim());
        self.output.push('\n');
        self.marker_next = Some((logical.saturating_add(1), display));
        Ok(())
    }

    fn pragma_once(&mut self, line: u32, column: u32) -> Result<(), DriverError> {
        if self.current().is_main {
            self.warn(line, column, "#pragma once in main file");
            return Ok(());
        }
        let key = fs::canonicalize(&self.current().path)
            .unwrap_or_else(|_| self.current().path.clone());
        self.once.insert(key);
        Ok(())
    }
}

/// A `_Pragma`-free code run or a pragma line inside one output line.
enum Segment<'a> {
    Code { toks: Vec<&'a SpTok>, pad: usize },
    Pragma { text: String, line: u32, column: u32 },
}

struct PragmaOp {
    text: String,
    /// Token index after the operator.
    next: usize,
    /// Rendered width of the operator, for column-keeping splits.
    width: usize,
    line: u32,
    column: u32,
}

impl FileCtx {
    fn line_text(&self, phys: u32) -> &str {
        self.lines
            .get(phys.saturating_sub(1) as usize)
            .map(|l| l.text.as_str())
            .unwrap_or("")
    }
}

/// 1-based columns (chars) of the `#`, the directive name and the first
/// operand char in a directive line. For a bare `#`, name and operand
/// coincide; past the end, the column is one past the last char.
fn directive_cols(text: &str) -> (u32, u32, u32) {
    let hash_idx = text.find('#').unwrap_or(0);
    let hash = text[..hash_idx].chars().count() as u32 + 1;
    let tail = text.get(hash_idx + 1..).unwrap_or("");
    let mut it = tail.chars().peekable();
    let mut col = hash + 1;
    while it.peek().is_some_and(|c| c.is_whitespace()) {
        it.next();
        col += 1;
    }
    let name = col;
    while it
        .peek()
        .is_some_and(|c| c.is_ascii_alphanumeric() || *c == '_')
    {
        it.next();
        col += 1;
    }
    while it.peek().is_some_and(|c| c.is_whitespace()) {
        it.next();
        col += 1;
    }
    (hash, name, col)
}

/// 1-based column of the directive name in a directive line (1 if none).
fn directive_name_col(text: &str) -> u32 {
    directive_cols(text).1
}

/// Quotes a filename for markers and `__FILE__`.
fn quot(name: &str) -> String {
    let mut out = String::with_capacity(name.len() + 2);
    out.push('"');
    for c in name.chars() {
        if c == '"' || c == '\\' {
            out.push('\\');
        }
        out.push(c);
    }
    out.push('"');
    out
}

/// Removes the quotes of a `_Pragma` string and undoes `\"`/`\\`.
fn destringize(literal: &str) -> String {
    let inner = literal.strip_prefix('"').unwrap_or(literal);
    let inner = inner.strip_suffix('"').unwrap_or(inner);
    let mut out = String::with_capacity(inner.len());
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some(q @ ('"' | '\\')) => out.push(q),
                Some(other) => {
                    out.push('\\');
                    out.push(other);
                }
                None => out.push('\\'),
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// Unescapes `\"`/`\\` in a `#line`/`#include` filename.
fn unescape_filename(inner: &str) -> String {
    let mut out = String::with_capacity(inner.len());
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some(q @ ('"' | '\\')) => out.push(q),
                Some(other) => {
                    out.push('\\');
                    out.push(other);
                }
                None => out.push('\\'),
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// Whether a `#pragma`/`_Pragma` body is consumed rather than passed
/// through. Besides `once` (handled separately), clang drops only the
/// purely organizational `mark` and `region`/`endregion` from `-E`
/// output; everything else (even `STDC`, `GCC ...` and `clang ...`)
/// passes through verbatim so downstream stages honor it. (One corner
/// differs: clang diagnoses an unknown `-W...` name in `GCC diagnostic`
/// and drops that pragma, while we pass it silently.)
fn pragma_consumed(rest: &str) -> bool {
    matches!(
        rest.split_whitespace().next(),
        Some("mark" | "region" | "endregion")
    )
}

/// Renders directive message text (`#error`/`#warning`): significant tokens
/// joined with single spaces, not macro-expanded.
fn render_tokens(rest: &str) -> String {
    pp_lex(rest)
        .iter()
        .filter(|t| !t.is_ws_or_newline())
        .map(|t| t.render())
        .collect::<Vec<_>>()
        .join(" ")
}

/// Detects the canonical include-guard pattern: the first directive is
/// `#ifndef G`, the second defines exactly `G`, the directives balance,
/// the guard group closes at the last directive, and no code sits outside
/// the guard. Strict on purpose: a missed guard only costs markers, but a
/// wrong skip would drop code.
fn detect_guard(lines: &[LogicalLine]) -> Option<String> {
    let mut dirs: Vec<(usize, String, String)> = Vec::new();
    for (idx, logical) in lines.iter().enumerate() {
        if let Some((name, rest)) = split_directive(&logical.text) {
            dirs.push((idx, name, rest));
        }
    }
    if dirs.len() < 3 || dirs[0].1 != "ifndef" {
        return None;
    }
    let guard = dirs[0].2.split_whitespace().next()?;
    if !is_ident(guard) || dirs[0].2.split_whitespace().count() != 1 {
        return None;
    }
    if dirs[1].1 != "define" {
        return None;
    }
    let def_rest = dirs[1].2.trim_start();
    let name_len = def_rest
        .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == '$'))
        .unwrap_or(def_rest.len());
    if &def_rest[..name_len] != guard {
        return None;
    }
    // A function-like `#define G(` is not a guard definition.
    if def_rest[name_len..].starts_with('(') {
        return None;
    }
    let mut depth = 0i32;
    let mut close_at = None;
    for (k, (_, name, _)) in dirs.iter().enumerate() {
        match name.as_str() {
            "if" | "ifdef" | "ifndef" => depth += 1,
            "endif" => {
                depth -= 1;
                if depth < 0 {
                    return None;
                }
                if depth == 0 {
                    close_at = Some(k);
                    break;
                }
            }
            _ => {}
        }
    }
    if depth != 0 || close_at != Some(dirs.len() - 1) {
        return None;
    }
    // Nothing but blanks outside the guard.
    let first_idx = dirs[0].0;
    let last_idx = dirs[dirs.len() - 1].0;
    if lines[..first_idx].iter().any(|l| !l.text.trim().is_empty())
        || lines[last_idx + 1..].iter().any(|l| !l.text.trim().is_empty())
    {
        return None;
    }
    Some(guard.to_string())
}

enum HeaderParseError {
    ExpectedFilename,
    UnterminatedString,
}

/// Splits an expanded `#include` operand into (filename, is-quote-form,
/// trailing text).
fn parse_header_operand(s: &str) -> Result<(String, bool, &str), HeaderParseError> {
    if let Some(after) = s.strip_prefix('"') {
        let mut i = 0;
        let bytes = after.as_bytes();
        while i < bytes.len() {
            if bytes[i] == b'\\' {
                i += 2;
                continue;
            }
            if bytes[i] == b'"' {
                let name = unescape_filename(&after[..i]);
                return Ok((name, true, &after[i + 1..]));
            }
            i += 1;
        }
        return Err(HeaderParseError::UnterminatedString);
    }
    if let Some(after) = s.strip_prefix('<') {
        if let Some(end) = after.find('>') {
            return Ok((after[..end].to_string(), false, &after[end + 1..]));
        }
        return Err(HeaderParseError::ExpectedFilename);
    }
    Err(HeaderParseError::ExpectedFilename)
}
