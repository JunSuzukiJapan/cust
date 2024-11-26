extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;


#[test]
fn code_gen_pre_increment_member() {
    let src = "
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

#[test]
fn code_gen_post_increment_member() {
    let src = "
        int printf(char* format, ...);

        struct foo {
            int x;
        };

        typedef struct foo Foo;

        int test(int x){
            Foo foo;

            foo.x = x;
            int y = foo.x++;
            return foo.x + y;
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
    assert_eq!(unsafe { f.call(1) }, 3);
}

#[test]
fn code_gen_pre_decrement_member() {
    let src = "
        int printf(char* format, ...);

        struct foo {
            int x;
        };

        typedef struct foo Foo;

        int test(int x){
            Foo foo;

            foo.x = x;
            int y = --foo.x;
            return foo.x + y;
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
    assert_eq!(unsafe { f.call(0) }, -2);
    assert_eq!(unsafe { f.call(1) }, 0);
}

#[test]
fn code_gen_post_decrement_member() {
    let src = "
        int printf(char* format, ...);

        struct foo {
            int x;
        };

        typedef struct foo Foo;

        int test(int x){
            Foo foo;

            foo.x = x;
            int y = foo.x--;
            return foo.x + y;
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
    assert_eq!(unsafe { f.call(0) }, -1);
    assert_eq!(unsafe { f.call(1) }, 1);
}

#[test]
fn code_gen_struct() -> Result<(), CodeGenError> {
    let src = "
        struct date {
            int year, month;
            int day;
        };

        typedef struct date Date;

        int test() {
            Date date;

            date.year = 2022;
            date.month = 12;
            date.day = 31;

            Date* pointer = &date;
            pointer->year = 2023;
            pointer->month = 1;
            pointer->day = 1;

            return date.year + pointer->month + pointer->day;
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
    assert_eq!(unsafe { f.call() }, 2025);

    Ok(())
}

#[test]
fn code_gen_struct_init() -> Result<(), CodeGenError> {
    let src = "
        struct date {
            int year, month;
            int day;
        };

        typedef struct date Date;

        int test() {
            Date date = {2023, 1, 1};
            Date* pointer = &date;

            return date.year + pointer->month + pointer->day;
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
    assert_eq!(unsafe { f.call() }, 2025);

    Ok(())
}

#[test]
fn code_gen_global_struct_init() -> Result<(), CodeGenError> {
    let src = "
        int i = 1;

        struct date {
            int year, month;
            int day;
        };

        typedef struct date Date;
        Date date = {2023, 3, 3};

        int test() {
            Date* pointer = &date;
            Date date2 = {2023, 1, 1};

            i = 2;

            return i + date.year + date2.month + pointer->day;
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
    assert_eq!(unsafe { f.call() }, 2029);

    Ok(())
}

#[test]
fn code_gen_member_function() {
    let src = "
        int printf(char* format, ...);
        typedef unsigned char bool;

        struct Date {
            int year;
            int month;
            int day;
        };

        impl Date {
            int getYear(&self) {
                return self.year;
            }

            int getMonth(&self) {
                return self.month;
            }

            int getDay(&self) {
                return self.day;
            }

            bool isLeapYear(&self) {
                return self.year % 4 == 0 && self.year % 100 != 0;
            }

            void nextDay(&self) {
                self.day++;

                switch(self.month) {
                case 1:
                case 3:
                case 5:
                case 7:
                case 8:
                case 10:
                    if (self.day == 32) {
                        self.day = 1;
                        self.month++;
                    }
                    break;
                case 12:
                    if (self.day == 32) {
                        self.day = 1;
                        self.month = 1;
                        self.year++;
                    }
                    break;
                case 4:
                case 6:
                case 9:
                case 11:
                    if(self.day == 31){
                        self.day = 1;
                        self.month++;
                    }
                    break;
                case 2:
                    if(self.isLeapYear()){
                        if(self.day == 30){
                            self.day = 1;
                            self.month = 3;
                        }
                    }else{
                        if(self.day == 29){
                            self.day = 1;
                            self.month = 3;
                        }
                    }
                }
            }
        }

        int test() {
            struct Date date = {2000, 2, 28};
            date.nextDay();
            bool p1 = date.year == 2000 && date.month == 3 && date.day == 1;

            struct Date date2 = {2024, 2, 28};
            date2.nextDay();
            bool p2 = date2.year == 2024 && date2.month == 2 && date2.day == 29;

            return p1 && p2;
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
fn code_gen_typedef() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        typedef struct date {
            int year, month;
            int day;
        } Date;

        int test() {
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
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None)?;
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let result = unsafe { f.call() };
    assert_eq!(0, result);

    Ok(())
}

#[test]
fn code_gen_call_class_function() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        struct foo {
            int bar;
        };

        impl foo {
            int test() {
                return 5;
            }
        }

        int test(){
            struct foo temp;

            return foo::test();
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
    assert_eq!(5, result);

    Ok(())
}

#[test]
fn code_gen_call_member_function() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        struct foo {
            int bar;
        };

        impl foo {
            int zot = 1;

            int test() {
                return 5;
            }

            int test2(&self) {
                return 2;
            }
        }

        int test(){
            struct foo temp;

            return temp.test2();
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
    assert_eq!(2, result);

    Ok(())
}

#[test]
fn code_gen_class_var() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        typedef struct foo {
            int bar;
        } Foo;

        impl foo {
            int Zot = 1;

            int test() {
                return Foo::Zot;
            }
        }

        int test(){
            return foo::test();
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
    assert_eq!(1, result);

    Ok(())
}

#[test]
fn code_gen_class_va2() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        typedef struct foo {
            int bar;
        } Foo;

        impl foo {
            int Zot = 1;

            int test() {
                return foo::Zot;
            }
        }

        int test(){
            return foo::test();
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
    assert_eq!(1, result);

    Ok(())
}

#[test]
fn code_gen_class_va3() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        typedef struct foo {
            int bar;
        } Foo;

        impl foo {
            int Zot = 1;

            int test() {
                return Self::Zot + foo::Zot + Foo::Zot;
            }
        }

        int test(){
            return foo::test();
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
    assert_eq!(3, result);

    Ok(())
}

#[test]
fn code_gen_init_struct_by_function() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        struct Circle {
            int radius;
        };
        
        impl Circle {
            const int PI100 = 314;
        
            Self new(int radius) {
                Circle c = {radius};
        
                return c;
            }
        
            int area(&self) {
                return self.radius * Self::PI100 / 100;
            }
        }

        int test() {
            Circle c = Circle::new(100);

            return c.area();
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
/*
#[test]
fn code_gen_init_struct_by_function2() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        struct Circle {
            int x, y;
            int radius;
        };

        impl Circle {
            const int PI100 = 314;

            Self new(int x, int y, int radius) {
                Circle c = Circle {
                    x: x;
                    y: y;
                    radius: radius;
                };

                return c;
            }

            int area(&self) {
                return self.radius * Self::PI100 / 100;
            }
        }

        int test() {
            Circle c = Circle::new(0, 0, 100);

            return c.area();
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
*/
/*
#[test]
fn code_gen_init_struct_by_function3() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        struct Circle {
            int x, y;
            int radius;
        };

        impl Circle {
            const int PI100 = 314;

            Self new(int x, int y, int radius) {
                return Circle {
                    x: x;
                    y: y;
                    radius: radius;
                };
            }

            int area(&self) {
                return self.radius * Self::PI100 / 100;
            }
        }

        int test() {
            Circle c = Circle::new(0, 0, 100);

            return c.area();
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

#[test]
fn code_gen_init_struct_by_function4() -> Result<(), Box<dyn Error>> {
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

            return c.area();
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
*/
/* main関数から実行するときは起きないが、テストだとスタックオーバーフローになる。

#[test]
fn code_gen_struct_array_and_pointer() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        int printf(char* format, ...);

        struct date {
            int year, month;
            int day;
        };
        typedef struct date Date;

        Date days[2][3] = {{{2024, 1, 1},
                            {2024, 1, 2},
                            {2024, 1, 3}
                           },
                           {{2024, 2, 1},
                            {2024, 2, 2},
                            {2024, 2, 3}
                           }
                          };

        int test() {
            Date* ptr = days;

            return ptr[0].year + ptr[0].month + ptr[0].day
                 + ptr[1].year + ptr[1].month + ptr[1].day
                 + ptr[2].year + ptr[2].month + ptr[2].day
                 + ptr[3].year + ptr[3].month + ptr[3].day
                 + ptr[4].year + ptr[4].month + ptr[4].day
                 + ptr[5].year + ptr[5].month + ptr[5].day;
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
    assert_eq!(12165, result);

    Ok(())
}

#[test]
fn code_gen_init_struct_array() -> Result<(), Box<dyn Error>> {
    let src = "
        struct date {
            int year, month;
            int day;
        };
        typedef struct date Date;

        Date days[2][3] = {{{2024, 1, 1},
                            {2024, 1, 2},
                            {2024, 1, 3}
                           },
                           {{2024, 2, 1},
                            {2024, 2, 2},
                            {2024, 2, 3}
                           }
                          };

        int test() {
            return days[0][0].year + days[0][0].month + days[0][0].day
                 + days[0][1].year + days[0][1].month + days[0][1].day
                 + days[0][2].year + days[0][2].month + days[0][2].day
                 + days[1][0].year + days[1][0].month + days[1][0].day
                 + days[1][1].year + days[1][1].month + days[1][1].day
                 + days[1][2].year + days[1][2].month + days[1][2].day;
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
    assert_eq!(12165, result);

    Ok(())
}
*/
