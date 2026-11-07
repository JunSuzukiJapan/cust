//! Error-path tests: every failure is a `PreprocessError` carrying the
//! failing file, line, column and message.

extern crate cpp;

use cpp::{preprocess, PreprocessError, PreprocessOptions};
use std::path::{Path, PathBuf};

fn fails_with(src: &str) -> PreprocessError {
    match preprocess(src, Path::new("err.c"), &PreprocessOptions::default()) {
        Ok(_) => panic!("expected failure, got success"),
        Err(e) => e,
    }
}

fn check(src: &str, message_part: &str, line: u32, column: u32) {
    let e = fails_with(src);
    assert!(
        e.message.contains(message_part),
        "message {:?} lacks {message_part:?}",
        e.message
    );
    assert_eq!(e.line, line, "wrong line for {message_part:?}");
    assert_eq!(e.column, column, "wrong column for {message_part:?}");
    assert_eq!(e.path, PathBuf::from("err.c"));
}

#[test]
fn stray_endif_fails() {
    check("int x;\n#endif\n", "#endif without #if", 2, 2);
}

#[test]
fn unclosed_conditional_fails_at_opener() {
    check("#if 1\nint x;\n", "unterminated conditional directive", 1, 2);
}

#[test]
fn ifdef_without_name_fails() {
    check("#ifdef\n#endif\n", "macro name missing", 1, 7);
}

#[test]
fn if_with_division_by_zero_fails() {
    check("#if 1/0\n#endif\n", "division by zero", 1, 5);
}

#[test]
fn if_with_remainder_by_zero_fails() {
    check("#if 1%0\n#endif\n", "remainder by zero", 1, 5);
}

#[test]
fn else_after_else_fails() {
    check("#if 1\n#else\n#else\n#endif\n", "#else after #else", 3, 2);
}

#[test]
fn define_without_name_fails() {
    check("#define\n", "macro name missing", 1, 8);
}

#[test]
fn define_with_non_identifier_fails() {
    check("#define 42 x\n", "macro name must be an identifier", 1, 9);
}

#[test]
fn define_with_unclosed_params_fails() {
    check("#define F(x\n", "expected ')'", 1, 9);
}

#[test]
fn define_with_duplicate_param_fails() {
    check("#define F(a, a) a\n", "duplicate macro parameter", 1, 9);
}

#[test]
fn define_with_misplaced_ellipsis_fails() {
    check(
        "#define F(..., a) a\n",
        "must be the last macro parameter",
        1,
        9,
    );
}

#[test]
fn undef_without_name_fails() {
    check("#undef\n", "macro name missing", 1, 7);
}

#[test]
fn defined_cannot_be_a_macro_name() {
    check("#define defined 1\n", "'defined' cannot be used as a macro name", 1, 9);
    check("#undef defined\n", "'defined' cannot be used as a macro name", 1, 8);
}

#[test]
fn hash_without_parameter_fails_at_define() {
    check(
        "#define F(x) # + x\n",
        "'#' is not followed by a macro parameter",
        1,
        9,
    );
}

#[test]
fn paste_at_end_fails_at_define() {
    check(
        "#define F(x) x ##\n",
        "'##' cannot appear at end of macro expansion",
        1,
        9,
    );
}

#[test]
fn paste_at_start_fails_at_define() {
    check(
        "#define F(x) ## x\n",
        "'##' cannot appear at start of macro expansion",
        1,
        9,
    );
}

#[test]
fn stringize_of_plain_va_args_in_non_variadic_fails_at_define() {
    check(
        "#define F(x) #__VA_ARGS__\n",
        "'#' is not followed by a macro parameter",
        1,
        9,
    );
}

#[test]
fn too_many_arguments_fails() {
    check(
        "#define F(a) a\nint v = F(1, 2);\n",
        "too many arguments",
        2,
        1,
    );
}

#[test]
fn too_few_arguments_fails() {
    check(
        "#define F(a, b) a\nint v = F(1);\n",
        "too few arguments",
        2,
        1,
    );
}

#[test]
fn invalid_paste_fails() {
    check(
        "#define P(x) x ## ,\nint v = P(a);\n",
        "pasting formed",
        2,
        1,
    );
}

#[test]
fn unterminated_invocation_fails() {
    check(
        "#define F(x) x\nint v = F(a;\n",
        "unterminated function-like macro invocation",
        2,
        1,
    );
}

#[test]
fn error_directive_fails_with_its_message() {
    check("#error M world\n", "M world", 1, 2);
    let e = fails_with("#error\n");
    assert!(e.message.is_empty());
    assert_eq!((e.line, e.column), (1, 2));
}

#[test]
fn unknown_directive_fails() {
    check("#foo bar\n", "invalid preprocessing directive", 1, 2);
    check("# foo\n", "invalid preprocessing directive", 1, 3);
}

#[test]
fn skipped_groups_ignore_unknown_directives() {
    let out = preprocess(
        "#if 0\n#foo bar\n#endif\nint x;\n",
        Path::new("err.c"),
        &PreprocessOptions {
            emit_line_markers: false,
            ..Default::default()
        },
    );
    let text = match out.unwrap() {
        cpp::PreprocessOutput::Memory(s) => s,
        cpp::PreprocessOutput::File(_) => panic!("memory expected"),
    };
    assert_eq!(text, "int x;\n");
}

#[test]
fn malformed_ifdef_in_skipped_group_still_tracks_nesting() {
    // The bare `#ifdef` is ignored but counts as nesting, so the `#endif`
    // closes it and the outer `#if` is left unterminated.
    check(
        "#if 0\n#ifdef\n#endif\nint x;\n",
        "unterminated conditional directive",
        1,
        2,
    );
}

#[test]
fn missing_include_fails() {
    check(
        "#include \"no-such-header.h\"\n",
        "'no-such-header.h' file not found",
        1,
        10,
    );
}

#[test]
fn malformed_include_fails() {
    check("#include foo\n", "expected \"FILENAME\" or <FILENAME>", 1, 10);
}

#[test]
fn bad_line_directive_fails() {
    check(
        "#line abc\n",
        "#line directive requires a positive integer argument",
        1,
        7,
    );
    check("#line 5 6\n", "invalid filename for #line directive", 1, 9);
}

#[test]
fn pragma_operator_requires_a_string() {
    check(
        "_Pragma(foo)\n",
        "_Pragma takes a parenthesized string literal",
        1,
        1,
    );
}

#[test]
fn unterminated_conditional_in_header_points_into_the_header() {
    let dir = std::env::temp_dir();
    let tag = std::process::id();
    let hdr = dir.join(format!("cpp-err-{tag}.h"));
    std::fs::write(&hdr, "#if 1\nint in_hdr;\n").unwrap();
    let src = format!("#include \"{}\"\nint x;\n", hdr.display());
    let e = match preprocess(&src, Path::new("main.c"), &PreprocessOptions::default()) {
        Ok(_) => panic!("expected failure, got success"),
        Err(e) => e,
    };
    assert_eq!(e.message, "unterminated conditional directive");
    assert_eq!((e.line, e.column), (1, 2));
    assert_eq!(e.path, hdr);
    std::fs::remove_file(&hdr).ok();
}

#[test]
fn self_include_fails_with_depth_error() {
    let dir = std::env::temp_dir();
    let tag = std::process::id();
    let hdr = dir.join(format!("cpp-self-{tag}.h"));
    std::fs::write(&hdr, format!("#include \"{}\"\nint x;\n", hdr.display())).unwrap();
    let src = format!("#include \"{}\"\n", hdr.display());
    let e = match preprocess(&src, Path::new("main.c"), &PreprocessOptions::default()) {
        Ok(_) => panic!("expected failure, got success"),
        Err(e) => e,
    };
    assert_eq!(e.message, "#include nested too deeply");
    std::fs::remove_file(&hdr).ok();
}
