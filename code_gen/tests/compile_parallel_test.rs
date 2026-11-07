extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;

/// Compiles `src` and calls its `test` function, mirroring the existing
/// codegen tests: parse, fresh `Context`, `CodeGen`, toplevels, JIT lookup,
/// call.
fn compile_and_run_i32_i32(src: &str, arg: i32) -> i32 {
    let asts = parse_from_str(src).unwrap();
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();
    let mut env = Env::new();
    for ast in &asts {
        let _ = gen.gen_toplevel(ast, &mut env, None, None).unwrap();
    }
    let f: JitFunction<FuncType_i32_i32> =
        unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    unsafe { f.call(arg) }
}

fn compile_and_run_void_i32(src: &str) -> i32 {
    let asts = parse_from_str(src).unwrap();
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();
    let mut env = Env::new();
    for ast in &asts {
        let _ = gen.gen_toplevel(ast, &mut env, None, None).unwrap();
    }
    let f: JitFunction<FuncType_void_i32> =
        unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    unsafe { f.call() }
}

// A tagged-enum struct match, the workload that used to crash under parallel
// threads (`enum_match_test::code_gen_struct_type_enum3`).
const ENUM_MATCH_SRC: &str = "
    enum Foo {
        Bar,
        Zot {
            int x;
            int y;
            int z;
        },
    };

    int test() {
        enum Foo x = Foo::Zot {
            x: 1;
            y: 2;
            z: 3;
        };

        if let (Foo::Zot { x, y, z } = x) {
            return x + y + z;
        } else {
            return 0;
        }
    }
";

// A struct member access (`struct_test::code_gen_pre_increment_member`).
const STRUCT_SRC: &str = "
    int printf(char* format, ...);

    struct foo {
        int x;
    };

    typedef struct foo Foo;

    int test(int x){
        Foo foo;

        foo.x = x;
        int y = ++foo.x;
        return foo.x + y;
    }
";

/// Drives whole JIT compiles from several threads at once, each with an
/// independent `Context`.
///
/// LLVM's process-global state (the target registry, MCJIT) is not safe to
/// drive concurrently even then: without `COMPILE_LOCK` this intermittently
/// crashes (SIGSEGV/SIGABRT) or fails with heap corruption. `CodeGen` holds
/// the lock from `try_new` until drop, so the threads serialize and this
/// passes deterministically.
#[test]
fn parallel_jit_compiles_do_not_interfere() {
    let mut handles = Vec::new();
    for t in 0..8u64 {
        handles.push(std::thread::spawn(move || {
            for round in 0..3 {
                // Stagger the start so the threads overlap mid-compile
                // rather than in lockstep.
                if round == 0 {
                    std::thread::sleep(std::time::Duration::from_millis(t * 2));
                }
                assert_eq!(compile_and_run_void_i32(ENUM_MATCH_SRC), 6);
                assert_eq!(compile_and_run_i32_i32(STRUCT_SRC, 1), 4);
            }
        }));
    }
    for h in handles {
        h.join().unwrap();
    }
}
