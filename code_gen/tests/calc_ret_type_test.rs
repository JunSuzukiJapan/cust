extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;


#[test]
fn code_gen_for_ret_type() -> Result<(), CodeGenError> {
    let src = "
        int test() {
            for(int i = 0; i < 3; i++){
                return 2;
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
    assert_eq!(unsafe { f.call() }, 2);

    Ok(())
}

#[test]
fn code_gen_for_ret_type2() -> Result<(), CodeGenError> {
    let src = "
        void test() {
            for(int i = 0; i < 3; i++){
                return;
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

    let f: JitFunction<FuncType_void_void> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, ());

    Ok(())
}

#[test]
fn code_gen_do_match_multi_ret_type() {
    let src = "
        int test(int input){
            int output = 0;
            do match (input) {
                x @ 0 | x @ 1 | x @ 2 => {
                    return 10 + x;
                },
                x @ 3 | x @ 4 | x @ 5 => {
                    return 20 + x;
                },
                _ => {
                    return 100 + input;
                }
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

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(0) }, 10);
    assert_eq!(unsafe { f.call(1) }, 11);
    assert_eq!(unsafe { f.call(2) }, 12);
    assert_eq!(unsafe { f.call(3) }, 23);
    assert_eq!(unsafe { f.call(4) }, 24);
    assert_eq!(unsafe { f.call(5) }, 25);
    assert_eq!(unsafe { f.call(6) }, 106);
    assert_eq!(unsafe { f.call(10) }, 110);
}
