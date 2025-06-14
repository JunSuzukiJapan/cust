extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;


#[test]
fn code_gen_union() -> Result<(), CodeGenError> {
    let src = "
        union foo {
            int i_value;
            double d_value;
        };

        typedef union foo Bar;

        int test() {
            Bar bar;

            bar.i_value = 100;
            int i = bar.i_value;

            bar.d_value = 3.14;
            double d = bar.d_value;

            return i;
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
    assert_eq!(unsafe { f.call() }, 100);

    Ok(())
}


#[test]
fn code_gen_union2() -> Result<(), CodeGenError> {
    let src = "
        typedef union foo {
            int i_value;
            double d_value;
        } Bar;

        int test() {
            Bar bar;

            bar.i_value = 100;
            int i = bar.i_value;

            bar.d_value = 3.14;
            double d = bar.d_value;

            return i;
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
    assert_eq!(unsafe { f.call() }, 100);

    Ok(())
}

#[test]
fn code_gen_global_union() -> Result<(), CodeGenError> {
    let src = "
        union foo {
            int i_value;
            double d_value;
        };

        typedef union foo Bar;
        Bar bar;

        int test() {
            bar.i_value = 100;
            int i = bar.i_value;

            bar.d_value = 3.14;
            double d = bar.d_value;

            return i;
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
    assert_eq!(unsafe { f.call() }, 100);

    Ok(())
}
