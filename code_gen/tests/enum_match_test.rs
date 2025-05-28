extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;


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

#[test]
fn code_gen_struct_type_enum3() {
    let src = "
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
fn code_gen_struct_type_enum_with_at() {
    let src = "
        enum SomeEnum {
            Foo,
            Bar {
                int x;
                int y;
                int z;
            },
        };

        int test() {
            enum SomeEnum x = SomeEnum::Bar {
                x: 1;
                y: 2;
                z: 3;
            };

            if let (SomeEnum::Bar { x: a @ A, y: b @ B, z: c @ C } = x) {
                return a + b + c + A + B + C;
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
    assert_eq!(unsafe { f.call() }, 12);
}

#[test]
fn code_gen_a_few_sub_types_enum1() {
    let src = "
        enum SomeEnum {
            Foo,
            Bar {
                int x;
                int y;
                int z;
            },
            Zot {
                char a;
                char b;
                char c;
            }
        };

        int test() {
            enum SomeEnum x = SomeEnum::Foo;;
            int result = 0;

            x = SomeEnum::Bar {
                x: 5;
                y: 6;
                z: 7;
            };

            x = SomeEnum::Zot {
                a: 'a';
                b: 'b';
                c: 'c';
            };

            x = SomeEnum::Foo;

            if let (SomeEnum::Foo = x) {
                result = 1;
            } else {
                result = 0;
            }

            return result;
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

#[test]
fn code_gen_tuple_type_enum1() {
    let src = "
        enum Foo {
            Bar (int, int),
            Zot {
                int x;
                int y;
            },
        };

        int test() {
            enum Foo x = Foo::Bar(
                1,
                2
            );

            if let (Foo::Bar ( x, y ) = x) {
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
