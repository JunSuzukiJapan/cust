extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;

#[test]
fn code_gen_match_global_tuple1() {
    let src = "
        $<int, int> tpl = $(1, 2);

        int test(){
            if let ($(x, y) = tpl) {
                return x + y;
            } else {
                return 0;
            }
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 3);
}

#[test]
fn code_gen_match_global_tuple2() {
    let src = "
        $<int, int> tpl = $(1, 2);

        int test(){
            if let ($(x @ a, y @ b) = tpl) {
                return x + y + a + b;
            } else {
                return 0;
            }
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 6);
}

#[test]
fn code_gen_match_global_tuple3() {
    let src = "
        $<int, int> tpl = $(1, 2);

        int test(){
            if let ($(3, y) = tpl) {
                return 1 + y;
            } else {
                return 10;
            }
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 10);
}

#[test]
fn code_gen_match_global_tuple4() {
    let src = "
        $<int, int> tpl = $(1, 2);

        int test(){
            if let ($(1, y) = tpl) {
                return 1 + y;
            } else {
                return 10;
            }
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 3);
}

