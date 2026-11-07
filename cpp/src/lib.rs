//! C preprocessor: C17 core plus ubiquitous extensions.
//!
//! Runs as an independent stage ahead of the tokenizer: text in, text out.
//! Output goes either to memory or to a file, selected with
//! [`PreprocessOptions::output`].

mod clock;
mod cond;
mod eval;
mod frontend;
mod macros;
mod pp_token;
mod preprocessor;

use std::fmt;
use std::path::{Path, PathBuf};

/// Where the preprocessed text goes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OutputTarget {
    /// Return the text in memory.
    Memory,
    /// Write the text to this file.
    File(PathBuf),
}

/// Options controlling preprocessing.
#[derive(Debug, Clone)]
pub struct PreprocessOptions {
    /// Header search paths (`-I`).
    pub include_paths: Vec<PathBuf>,
    /// Predefined macros (`-D name[=value]`; `None` means `1`).
    pub defines: Vec<(String, Option<String>)>,
    /// Suppressed macros (`-U`).
    pub undefs: Vec<String>,
    /// Whether to emit `#line` markers (`false` is the `-P` equivalent).
    pub emit_line_markers: bool,
    /// Where the preprocessed text goes.
    pub output: OutputTarget,
}

impl Default for PreprocessOptions {
    fn default() -> Self {
        PreprocessOptions {
            include_paths: Vec::new(),
            defines: Vec::new(),
            undefs: Vec::new(),
            emit_line_markers: true,
            output: OutputTarget::Memory,
        }
    }
}

/// The result of [`preprocess`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PreprocessOutput {
    /// The preprocessed text (`OutputTarget::Memory`).
    Memory(String),
    /// The file it was written to (`OutputTarget::File`).
    File(PathBuf),
}

/// A preprocessing failure, with its source position.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocessError {
    pub path: PathBuf,
    /// 1-based line number.
    pub line: u32,
    /// 1-based column number.
    pub column: u32,
    pub message: String,
}

impl PreprocessError {
    pub fn new(path: &Path, line: u32, column: u32, message: impl Into<String>) -> Self {
        PreprocessError {
            path: path.to_path_buf(),
            line,
            column,
            message: message.into(),
        }
    }
}

impl fmt::Display for PreprocessError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}: {}",
            self.path.display(),
            self.line,
            self.column,
            self.message
        )
    }
}

impl std::error::Error for PreprocessError {}

/// Preprocess `content` as if it were read from `path`.
///
/// Emits `#line` markers unless `options.emit_line_markers` is false.
/// Returns the text or writes it to a file, per `options.output`.
pub fn preprocess(
    content: &str,
    path: &Path,
    options: &PreprocessOptions,
) -> Result<PreprocessOutput, PreprocessError> {
    let text = preprocessor::preprocess_file(content, path, options)?;
    match &options.output {
        OutputTarget::Memory => Ok(PreprocessOutput::Memory(text)),
        OutputTarget::File(out) => std::fs::write(out, &text)
            .map(|()| PreprocessOutput::File(out.clone()))
            .map_err(|e| {
                PreprocessError::new(out, 1, 1, format!("cannot write output file: {e}"))
            }),
    }
}


