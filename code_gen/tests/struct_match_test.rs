extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;
use std::error::Error;

#[test]
fn code_gen_struct_match1() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        struct Circle {
            int x, y;
            int radius;
        };

        impl Circle {
            const int PI100 = 314;

            Self default_new() {
                return Circle {
                    x: 0;
                    y: 0;
                    radius: 100;
                };
            }

            int area(&self) {
                return self.radius * Self::PI100 / 100;
            }
        }

        int test() {
            Circle c = Circle::default_new();

            if let (Circle { x: 1, y: 2, radius: 3 } = c) {
                return x + y + radius;
            } else {
                return -1;
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
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None)?;
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let result = unsafe { f.call() };
    assert_eq!(314, result);

    Ok(())
}
