extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;

#[test]
fn code_gen_do_match_int() {
    let src = "
        int test(int input){
            int output = 0;
            do match (input) {
                0 => {
                    output = 1;
                },
                1 => {
                    output = 2;
                },
                2 => {
                    output = 3;
                },
                _ => {
                    output = 4;
                }
            }

            return output;
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
    assert_eq!(unsafe { f.call(0) }, 1);
    assert_eq!(unsafe { f.call(1) }, 2);
    assert_eq!(unsafe { f.call(2) }, 3);
    assert_eq!(unsafe { f.call(3) }, 4);
    assert_eq!(unsafe { f.call(10) }, 4);
}

#[test]
fn code_gen_do_match_multi_int() {
    let src = "
        int printf(const char *format, ...);

        int test(int input){
            printf(\"input: %d\\n\", input);
            int output = 0;

            do match (input) {
                0 | 1 | 2 | 3 => {
                    output = 10 + input;
                },
                4 | 5 | 6 => {
                    output = 20 + input;
                },
                _ => {
                    output = 30 + input;
                },
            }

            return output;
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
    assert_eq!(unsafe { f.call(0) }, 10);
    assert_eq!(unsafe { f.call(1) }, 11);
    assert_eq!(unsafe { f.call(2) }, 12);
    assert_eq!(unsafe { f.call(3) }, 13);
    assert_eq!(unsafe { f.call(4) }, 24);
    assert_eq!(unsafe { f.call(5) }, 25);
    assert_eq!(unsafe { f.call(6) }, 26);
    assert_eq!(unsafe { f.call(7) }, 37);
    assert_eq!(unsafe { f.call(10) }, 40);
}
