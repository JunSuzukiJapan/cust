extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;

#[test]
fn code_gen_define_global_tuple() {
    let src = "
        $<int, int> tpl = $(1, 2);

        int test(){
            return tpl.0 + tpl.1;
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
fn code_gen_define_tuple() {
    let src = "
        int printf(char* format, ...);

        int test(){
            $<int, int> tpl = $(1, 2);

            return tpl.0 + tpl.1;
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
fn code_gen_access_tuple_element() {
    let src = "
        int printf(char* format, ...);

        int test(){
            $<int, int> tpl = $(1, 2);
            tpl.0 = 3;
            tpl.1 = 4;

            return tpl.0 + tpl.1;
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
    assert_eq!(unsafe { f.call() }, 7);
}


#[test]
fn code_gen_tuple_in_struct() {
    let src = "
        int printf(char* format, ...);

        struct foo {
            int x;
            $<int, int> tpl;
        };

        typedef struct foo Foo;

        int test(int x){
            Foo foo;

            foo.x = x;
            foo.tpl.0 = 3;
            foo.tpl.1 = 4;

            return foo.x + foo.tpl.0 + foo.tpl.1;
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
    assert_eq!(unsafe { f.call(0) }, 7);
    assert_eq!(unsafe { f.call(1) }, 8);
}


#[test]
fn code_gen_pre_and_post_increment_member() {
    let src = "
        int printf(char* format, ...);

        struct foo {
            int x;
            $<int, int> tpl;
        };

        typedef struct foo Foo;

        int test(int x){
            Foo foo;

            foo.x = x;
            foo.tpl.0 = 3;
            foo.tpl.1 = 4;

            ++foo.tpl.0;
            foo.tpl.1++;

            return foo.x + foo.tpl.0 + foo.tpl.1;
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
    assert_eq!(unsafe { f.call(0) }, 9);
    assert_eq!(unsafe { f.call(1) }, 10);
}
