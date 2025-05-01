extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;

/*
#[test]
fn code_gen_struct_type_enum1() {
    let src = "
        enum Foo {
            Bar,
            Zot {
                int x;
                int y;
            },
        };

        int test() {
            enum Foo x = Foo::Bar;

            if let (Foo::Zot { x, y } = x) {
                return 1;
            }else{
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
    assert_eq!(unsafe { f.call() }, 0);
}
*/
/*
#[test]
fn code_gen_struct_type_enum2() {
    let src = "
        enum Foo {
            Bar,
            Zot {
                int x;
                int y;
            },
        };

        int test() {
            enum Foo x = Foo::Zot {
                x: 1;
                y: 2;
            };

            if let (Foo::Zot { x, y } = x) {
                return 1;
            }

            return 0;
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
    assert_eq!(unsafe { f.call() }, 1);
}
*/
/*
#[test]
fn code_gen_struct_type_enum() {
    let src = "
        enum Foo {
            Bar,
            Zot {
                int x;
                int y;
            },
        };

        int main() {
            enum Foo x = Foo::Zot {
                x: 1;
                y: 2;
            };

            if let (Foo::Zot { x, y } = x) {
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

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(0) }, 2);
    assert_eq!(unsafe { f.call(1) }, 4);
}
*/