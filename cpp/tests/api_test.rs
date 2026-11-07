extern crate cpp;

use cpp::{preprocess, PreprocessOptions, OutputTarget, PreprocessOutput};
use std::path::{Path, PathBuf};

const SRC: &str = "#define FOO 42\nint x = FOO;\n";

fn temp_output(name: &str) -> PathBuf {
    std::env::temp_dir().join(format!("cpp-test-{}-{}.i", name, std::process::id()))
}

#[test]
fn memory_and_file_outputs_are_byte_identical() {
    let path = Path::new("byte_identical.c");
    let mem_opts = PreprocessOptions {
        output: OutputTarget::Memory,
        ..Default::default()
    };
    let memory = match preprocess(SRC, path, &mem_opts).unwrap() {
        PreprocessOutput::Memory(s) => s,
        PreprocessOutput::File(_) => panic!("expected memory output"),
    };

    let out = temp_output("byte-identical");
    let file_opts = PreprocessOptions {
        output: OutputTarget::File(out.clone()),
        ..Default::default()
    };
    match preprocess(SRC, path, &file_opts).unwrap() {
        PreprocessOutput::File(p) => assert_eq!(p, out),
        PreprocessOutput::Memory(_) => panic!("expected file output"),
    }
    let from_disk = std::fs::read_to_string(&out).unwrap();
    assert_eq!(memory, from_disk);
    std::fs::remove_file(&out).ok();
}

#[test]
fn line_markers_can_be_suppressed() {
    let dir = std::env::temp_dir().join(format!("cpp-test-markers-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("m.h"), "int y = 2;\n").unwrap();
    let main = dir.join("m.c");
    std::fs::write(&main, "#include \"m.h\"\nint x = 1;\n").unwrap();
    let content = std::fs::read_to_string(&main).unwrap();

    let marked = match preprocess(&content, &main, &PreprocessOptions::default()).unwrap() {
        PreprocessOutput::Memory(s) => s,
        PreprocessOutput::File(_) => panic!("expected memory output"),
    };
    assert!(marked.contains("#line "), "markers on: {marked:?}");

    let opts = PreprocessOptions {
        emit_line_markers: false,
        ..Default::default()
    };
    let plain = match preprocess(&content, &main, &opts).unwrap() {
        PreprocessOutput::Memory(s) => s,
        PreprocessOutput::File(_) => panic!("expected memory output"),
    };
    assert!(!plain.contains("#line "), "markers off: {plain:?}");
    assert!(plain.contains("int y = 2;"));
    assert!(plain.contains("int x = 1;"));
    std::fs::remove_dir_all(&dir).ok();
}

fn memory_of(content: &str, path: &Path, opts: &PreprocessOptions) -> String {
    match preprocess(content, path, opts).unwrap() {
        PreprocessOutput::Memory(s) => s,
        PreprocessOutput::File(_) => panic!("expected memory output"),
    }
}

#[test]
fn defines_seed_macros_and_undefs_win() {
    let src = "#ifdef A\nint has_a = 1;\n#endif\n#ifdef B\nint has_b = 1;\n#endif\nint v = A;\n";
    let opts = PreprocessOptions {
        defines: vec![("A".to_string(), Some("41".to_string())), ("B".to_string(), None)],
        undefs: vec!["B".to_string()],
        emit_line_markers: false,
        ..Default::default()
    };
    let out = memory_of(src, Path::new("def.c"), &opts);
    assert!(out.contains("int has_a = 1;"), "{out:?}");
    assert!(!out.contains("has_b"), "{out:?}");
    assert!(out.contains("int v = 41;"), "{out:?}");
}

#[test]
fn include_paths_serve_angle_includes() {
    let dir = std::env::temp_dir().join(format!("cpp-test-inc-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("found.h"), "int found = 1;\n").unwrap();
    let main = dir.join("main.c");
    let opts = PreprocessOptions {
        include_paths: vec![dir.clone()],
        emit_line_markers: false,
        ..Default::default()
    };
    let out = memory_of("#include <found.h>\nint x = 1;\n", &main, &opts);
    assert!(out.contains("int found = 1;"), "{out:?}");
    assert!(out.contains("int x = 1;"), "{out:?}");
    std::fs::remove_dir_all(&dir).ok();
}

#[test]
fn content_mode_is_dense_with_first_token_at_line_two_quirk() {
    // Both clang and gcc `-P` emit exactly one blank line when the first
    // token sits at line 2, and dense output otherwise.
    let opts = PreprocessOptions {
        emit_line_markers: false,
        ..Default::default()
    };
    assert_eq!(
        memory_of("#define A 1\nint x;\n", Path::new("q.c"), &opts),
        "\nint x;\n"
    );
    assert_eq!(
        memory_of("#define A 1\n#define B 2\nint x;\n", Path::new("q.c"), &opts),
        "int x;\n"
    );
    assert_eq!(memory_of("int x;\n", Path::new("q.c"), &opts), "int x;\n");
}
