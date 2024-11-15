extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;
use std::path::Path;

#[test]
fn code_gen_if_let() {
    let src = "
        int test(int x){
            int y = 0;
            if let (x = x) {
                x = x * x;
                y = x;
            }

            return x + y;
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

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(3) }, 12);
}

#[test]
fn code_gen_if_let2() {
    let src = "
        int test(){
            if let (x = 5) {
                return x * x;
            }else{
                return 10;
            }

            return 100;
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
    assert_eq!(unsafe { f.call() }, 25);
}

#[test]
fn code_gen_if_let3() {
    let src = "
        int test(int i){
            if let (x @ alias_x = i) {
                return x * alias_x;
            }else{
                return 10;
            }

            return 100;
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

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(5) }, 25);
}

#[test]
fn code_gen_if_let4() {
    let src = "
        int test(){
            if let ('a' | 'c' @ x = 'c') {
                if(x == 'a'){
                    return 10;
                }else if(x == 'c') {
                    return 20;
                }
                return 30;
            }else{
                return 2;
            }

            return 3;
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
    assert_eq!(unsafe { f.call() }, 20);
}
