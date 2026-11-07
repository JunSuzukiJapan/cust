//! Differential test: our output must match `clang -E` on every fixture.
//!
//! Two modes run over the same fixtures: content mode (our
//! `emit_line_markers: false` against `clang -E -P`) isolates expanded
//! text, and markers-on mode (markers kept both sides) compares the
//! resync-marker streams too.
//!
//! Normalization, applied to both sides:
//!
//! - `#line N "file"` (ours) and `# N "file" flags...` (clang) both become
//!   `@LINE N <basename>`. Only the basename is compared: clang prints paths
//!   as resolved, we print them as written.
//! - clang's `<built-in>` / `<command line>` markers are dropped.
//! - consecutive identical markers collapse: a resync to the position the
//!   reader already has is a no-op (clang emits such doubles at file
//!   boundaries).
//! - empty lines are dropped: blank-line placement carries no meaning.
//! - spacing is compared insensitively outside string/character literals:
//!   leading/trailing whitespace is dropped and interior runs collapse to
//!   one space. `clang -E` reformats spacing (it collapses interior runs
//!   but preserves leading columns) while we keep the source spelling per
//!   the standard's comment→space rule; both are conforming, and our
//!   consumer (the tokenizer) is spacing-insensitive there. Literals are
//!   compared exactly.

extern crate cpp;

use std::path::{Path, PathBuf};
use std::process::Command;

fn fixtures_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

/// Canonicalizes a linemarker body: `N "file" ...` -> `@LINE N <basename>`.
fn canon_marker(rest: &str) -> String {
    let mut parts = rest.splitn(2, '"');
    let num = parts.next().unwrap_or("").trim();
    let file = parts.next().unwrap_or("").split('"').next().unwrap_or("");
    let base = Path::new(file)
        .file_name()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_else(|| file.to_string());
    format!("@LINE {num} {base}")
}

fn normalize(text: &str) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    let mut push = |s: String| {
        // Consecutive identical markers are resync no-ops.
        if !s.starts_with("@LINE") || out.last() != Some(&s) {
            out.push(s);
        }
    };
    for line in text.lines() {
        let t = line.trim();
        if let Some(rest) = t.strip_prefix("#line ") {
            push(canon_marker(rest));
            continue;
        }
        if t == "#line" {
            continue;
        }
        if let Some(rest) = t.strip_prefix('#') {
            // clang linemarker: `# N "file" flags...`
            let rest = rest.trim_start();
            if rest.chars().next().is_some_and(|c| c.is_ascii_digit()) {
                let canon = canon_marker(rest);
                if canon.contains("<built-in>") || canon.contains("<command line>") {
                    continue;
                }
                push(canon);
                continue;
            }
        }
        let code = canon_spacing(line);
        if code.is_empty() {
            continue;
        }
        push(code);
    }
    out
}

/// Drops leading/trailing whitespace and collapses interior whitespace runs
/// to one space, except inside `"..."` / `'...'` literals (escape-aware),
/// which are kept verbatim. Single-line only: no fixture spans a literal
/// across lines.
fn canon_spacing(line: &str) -> String {
    let mut out = String::new();
    let mut pending_space = false;
    let mut chars = line.chars();
    while let Some(c) = chars.next() {
        if c == '"' || c == '\'' {
            if pending_space && !out.is_empty() {
                out.push(' ');
            }
            pending_space = false;
            let quote = c;
            out.push(c);
            while let Some(d) = chars.next() {
                out.push(d);
                if d == '\\' {
                    if let Some(e) = chars.next() {
                        out.push(e);
                    }
                    continue;
                }
                if d == quote {
                    break;
                }
            }
            continue;
        }
        if c.is_whitespace() {
            pending_space = true;
            continue;
        }
        if pending_space && !out.is_empty() {
            out.push(' ');
        }
        pending_space = false;
        out.push(c);
    }
    out
}

fn diff_lines(a: &[String], b: &[String]) -> Vec<String> {
    let mut out = Vec::new();
    for i in 0..a.len().max(b.len()) {
        let x = a.get(i);
        let y = b.get(i);
        if x != y {
            out.push(format!("line {}: ours={x:?} clang={y:?}", i + 1));
        }
    }
    out
}

fn fixture_cases() -> (PathBuf, Vec<PathBuf>) {
    let dir = fixtures_dir();
    let mut cases: Vec<PathBuf> = std::fs::read_dir(&dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|e| e == "c"))
        .collect();
    cases.sort();
    assert!(!cases.is_empty(), "no fixtures in {}", dir.display());
    (dir, cases)
}

fn check_oracle(markers: bool) {
    if Command::new("clang").arg("--version").output().is_err() {
        panic!("differential test requires `clang` on PATH");
    }
    let (dir, cases) = fixture_cases();
    let mut failures = Vec::new();
    for case in &cases {
        let content = std::fs::read_to_string(case).unwrap();
        let opts = cpp::PreprocessOptions {
            emit_line_markers: markers,
            include_paths: vec![dir.clone()],
            ..Default::default()
        };
        let ours = match cpp::preprocess(&content, case, &opts).unwrap() {
            cpp::PreprocessOutput::Memory(s) => s,
            cpp::PreprocessOutput::File(_) => unreachable!(),
        };
        // Pinned to C17: our scope is C17 plus extensions (notably, bare
        // `true`/`false` are 0 there, and `__has_include` stays out).
        let mut clang = Command::new("clang");
        clang.args(["-E", "-std=c17", "-I"]).arg(&dir);
        if !markers {
            clang.arg("-P");
        }
        let clang_out = clang.arg(case).output().unwrap();
        assert!(
            clang_out.status.success(),
            "clang -E failed on {}: {}",
            case.display(),
            String::from_utf8_lossy(&clang_out.stderr)
        );
        let a = normalize(&ours);
        let b = normalize(&String::from_utf8_lossy(&clang_out.stdout));
        if a != b {
            failures.push((case.clone(), a, b));
        }
    }
    if !failures.is_empty() {
        let mut msg = format!("{} fixture(s) differ from clang -E:\n", failures.len());
        for (case, a, b) in &failures {
            msg.push_str(&format!("--- {}\n", case.display()));
            for line in diff_lines(a, b).iter().take(40) {
                msg.push_str(line);
                msg.push('\n');
            }
        }
        panic!("{msg}");
    }
}

#[test]
fn matches_clang_e() {
    check_oracle(false);
}

#[test]
fn matches_clang_e_with_markers() {
    check_oracle(true);
}
