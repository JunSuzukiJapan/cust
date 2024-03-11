#![allow(non_camel_case_types)]
#![allow(dead_code)]

extern crate tokenizer;
extern crate parser;
extern crate code_gen;

use parser::{Parser, ExprAST, Tokenizer, ParserError, Defines};
use code_gen::{CodeGen, Env};

// use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
// use inkwell::types::BasicType;
// use inkwell::values::{AnyValue, FunctionValue};
// use inkwell::execution_engine::FunctionLookupError;

type FuncType = unsafe extern "C" fn(i64, i64) -> i64;
type NoArgFunc = unsafe extern "C" fn() -> u64;
type FuncType_i32_i32 = unsafe extern "C" fn(i32) -> i32;
type FuncType_void_i32 = unsafe extern "C" fn() -> i32;
// type FuncType_i64_i64 = unsafe extern "C" fn(i64) -> i64;
// type FuncType_i64i64_i64 = unsafe extern "C" fn(i64, i64) -> i64;
// type FuncType_i64i64i64_i64 = unsafe extern "C" fn(i64, i64, i64) -> i64;
type FuncType_void_void = unsafe extern "C" fn() -> i64;

fn parse_expression_from_str(src: &str) -> Result<Option<ExprAST>, ParserError> {
    let token_list = Tokenizer::tokenize(src).unwrap();
    let mut iter = token_list.iter().peekable();
    let parser = Parser::new();
    let mut defs = Defines::new();
    let mut labels = Vec::new();
    parser.parse_expression(&mut iter, &mut defs, &mut Some(&mut labels))
}

fn main() {
/*
    let src = "
        int printf(char* format, ...);

        int sub(int x, int y){
            return x - y;
        }

        void sub_test(){
            int x = 3;
            int y = 4;
            int z = sub(x, y);
            printf(\"%d - %d = %d\\\n\", x, y, z);
            int sum;
            sum = x + y + z;
            printf(\"sum(%d + %d + %d): %d\\\n\", x, y, z, sum);
        }
    ";
    let src = "
        int printf(char* format, ...);

        void sub_test(){
            int x = 3;
            printf(\"x: %d\\\n\", x);

            {
                int x = 4;
                printf(\" > x: %d\\\n\", x);

                {
                    int x = 5;
                    printf(\" >> x: %d\\\n\", x);
                }

                printf(\" < x: %d\\\n\", x);
            }

            printf(\"x: %d\\\n\", x);
        }
    ";
    let src = "
        int printf(char* format, ...);

        void test(){
            int x = 0;

            if(x) {
                printf(\"NOT Zero\\\n\");
            }else{
                printf(\"Zero\\\n\");
            }
        }
    ";
    let src = "
        int printf(char* format, ...);

        int if_test(int x){
            if(x){
                return 10;
            }else{
                return 20;
            }
        }

        void test(){
            printf(\"%d\\\n\", if_test(0));
            printf(\"%d\\\n\", if_test(1));
        }
    ";

    let src = "
        int printf(char* format, ...);

        int for_test(int x){
            int sum = 0;

            int i;
            for(i = 0; i < x; i++){
                sum += i;
            }

            int j;
            for(j = 0; j < x; j++){
                sum += j;
            }

            return sum;
        }

        void test(){
            printf(\"%d\\\n\", for_test(0));
            printf(\"%d\\\n\", for_test(1));
        }
    ";
    let src = "
        int printf(char* format, ...);
        _Bool false = 0;

        void test(){
            int i = 0;
            do {
                printf(\"Hello %d\\\n\", i);
                i++;
            } while (i < 3);
        }
    ";
    let src = "
        int printf(char* format, ...);

        void test(){
            for(int i = 0; i < 5; i++){
                if(i % 2 == 0){
                    printf(\"Continue %d\\\n\", i);
                    continue;
                }
                printf(\"Hello %d\\\n\", i);
            }
        }
    ";
    let src = "
        int printf(char* format, ...);

        void test(){
            for(int i = 0; i < 5; i++){
                if(i == 3){
                    printf(\"break %d\\\n\", i);
                    break;
                }
                printf(\"Hello %d\\\n\", i);
            }
        }
    ";
    let src = "
        int printf(char* format, ...);

        void test(){
            for(int i = 0; ; i++){
                if(i == 3){
                    printf(\"break %d\\\n\", i);
                    break;
                }
                printf(\"Hello %d\\\n\", i);
            }
        }
    ";
    let src = "
        int printf(char* format, ...);

        void test(){
            int i = 0;
            while(i < 3){
                printf(\"Hello %d\\\n\", i);
                i++;
            }
        }
    ";
    let src = "
        int printf(char* format, ...);

        void test(){
            for(int i = 0; i < 5; i++){
                for(int j = 0; j < 5; j++){
                    printf(\"i: %d, j: %d\\\n\", i, j);

                    if(i == 2 && j == 2) goto END;
                }
            }
END: ;
        }
    ";

    let src = "
        int printf(char* format, ...);

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
            printf(\"year: %d, month: %d, day: %d\\\n\", date.year, date.month, date.day);

            Date* pointer = &date;
            pointer->year = 2023;
            pointer->month = 1;
            pointer->day = 1;
            printf(\"year: %d, month: %d, day: %d\\\n\", pointer->year, pointer->month, pointer->day);

            return 1;
        }
    ";

    let src = "
        int printf(char* format, ...);

        union foo {
            int i_value;
            double d_value;
        };

        typedef union foo Bar;

        void test() {
            Bar bar;

            bar.i_value = 100;
            int i = bar.i_value;

            bar.d_value = 3.14;
            double d = bar.d_value;

            printf(\"bar.i_value: %d, bar.d_value: %lf, i: %d, d: %lf\\\n\", bar.i_value, bar.d_value, i, d);
        }
    ";

    let src = "
        int printf(char* format, ...);

        void test() {
            int ary[2][3];

            for(int i = 0; i < 2; i++){
                for(int j = 0; j < 3; j++){
                    ary[i][j] = i * 10 + j;
                    printf(\"assign ary[%d][%d] = %d\\\n\", i, j, ary[i][j]);
                }
            }

            for(int i = 0; i < 2; i++){
                for(int j = 0; j < 3; j++){
                    printf(\"ary[%d][%d] = %d\\\n\", i, j, ary[i][j]);
                }
            }
        }
    ";
    let src = "
        int printf(char* format, ...);

        int switch_test(int x){
            switch(x){
            case 0:
                x += 1000;
            case 1:
                x += 20000;
                break;
            case 2:
                x += 300000;
                break;
            default:
                x += 4000000;
            }
            return x;
        }

        void test(){
            int x = switch_test(0);
            printf(\"x = %d\\\n\", x);
            x = switch_test(1);
            printf(\"x = %d\\\n\", x);
            x = switch_test(2);
            printf(\"x = %d\\\n\", x);
            x = switch_test(3);
            printf(\"x = %d\\\n\", x);
        }
    ";

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
        }

        void test() {
            struct Date date = {2023, 1, 2};
            printf(\"date = {%d, %d, %d}\\\n\", date.year, date.month, date.day);
            date.nextDay();
            printf(\"date = {%d, %d, %d}\\\n\", date.year, date.month, date.day);
        }
    ";

    let src = "
        int printf(char* format, ...);
        typedef int bool;

        void test() {
            bool p1 = 1;
            bool p2 = 1;

            if(p1){
                if(p2){
                    printf(\"Hello!\\\n\");
                    printf(\"p1 && p2: %d\\\n\", p1 && p2);
                }
            }
        }
    ";

    let src = "
        int printf(char* format, ...);
        typedef int bool;

        enum Weekday {
            Sunday,
            Monday,
            Tuesday,
            Wednesday = 10,
            Thursday,
            Friday,
            Saturday,
        };

        void test() {
            printf(\"%d\\\n\",Sunday);
            printf(\"%d\\\n\", Monday);
            printf(\"%d\\\n\", Tuesday);
            printf(\"%d\\\n\", Wednesday);
            printf(\"%d\\\n\", Thursday);
            printf(\"%d\\\n\", Friday);
            printf(\"%d\\\n\", Saturday);
        }
    ";

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
        }

        int test() {
            struct Date date = {2000, 2, 28};
            date.nextDay();
            bool p1 = date.year == 2000 && date.month == 3 && date.day == 1;
            printf(\"date.year == 2000: %d\\\n\", date.year == 2000);
            printf(\"date.month == 3: %d\\\n\", date.month == 3);
            printf(\"date.day == 1: %d\\\n\", date.day == 1);
            printf(\"date.year == 2000 && date.month == 3: %d\\\n\", date.year == 2000 && date.month == 3);
            printf(\"date.year == 2000 && date.month == 3 && date.day == 1: %d\\\n\", date.year == 2000 && date.month == 3 && date.day == 1);
            printf(\"p1: %d\\\n\", p1);

            struct Date date2 = {2024, 2, 28};
            date2.nextDay();
            bool p2 = date2.year == 2024 && date2.month == 2 && date2.day == 29;
            printf(\"p2: %d\\\n\", p2);

            printf(\"p1 && p2: %d\\\n\", p1 && p2);
            return p1 && p2;
        }
    ";

    let src = "
        int printf(char* format, ...);

        void test() {
            int x = 1;
            printf(\"x = %d\\\n\", x);
            int* ptr = &x;
            printf(\"*ptr = %d\\\n\", *ptr);
            int** handle = &ptr;
            printf(\"**handle = %d\\\n\", **handle);
        }
    ";
    let src = "
        int printf(char* format, ...);

        int test(int i) {
            int x = i;
            int* ptr = &x;
            int** handle = &ptr;

            printf(\"x = %d\\\n\", x);
            printf(\"*ptr = %d\\\n\", *ptr);
            printf(\"**handle = %d\\\n\", **handle);

            return *ptr;
        }
    ";

    let src = "
        int printf(char* format, ...);

        void test(){
            int (i) = 1;
            int (*foo) = &i;
            int (*(*bar)) = &foo;
            int *(*(*(zot)));

            printf(\"*foo = %d\\\n\", *foo);
        }
    ";

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

    let src = "
        int printf(char* format, ...);

        int test() {
            int num[3] = {1, 2, 3};

            printf(\"num[0] = %d\\\n\", num[0]);
            printf(\"num[1] = %d\\\n\", num[1]);
            printf(\"num[2] = %d\\\n\", num[2]);
            return num[0] + num[1] + num[2];
        }
    ";

    let src = "
        int printf(char* format, ...);

        int num[2][3] = {{1, 2, 3},
                         {4, 5, 6}};

        int test() {
            for(int i = 0; i < 2; i++){
                for(int j = 0; j < 3; j++){
                    printf(\"num[%d][%d] = %d\\\n\", i, j, num[i][j]);
                }
            }

            return num[0][0] + num[0][1] + num[0][2]
                 + num[1][0] + num[1][1] + num[1][2];
        }
    ";
*/
/*
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
            for(int i = 0; i < 2; i++){
                for(int j = 0; j < 3; j++){
                    printf(\"days[%d][%d].year = %d\\\n\", i, j, days[i][j].year);
                    printf(\"days[%d][%d].month = %d\\\n\", i, j, days[i][j].month);
                    printf(\"days[%d][%d].day = %d\\\n\", i, j, days[i][j].day);
                }
            }

            return days[0][0].year + days[0][0].month + days[0][0].day
                 + days[0][1].year + days[0][1].month + days[0][1].day
                 + days[0][2].year + days[0][2].month + days[0][2].day
                 + days[1][0].year + days[1][0].month + days[1][0].day
                 + days[1][1].year + days[1][1].month + days[1][1].day
                 + days[1][2].year + days[1][2].month + days[1][2].day;
        }
    ";
*/
/*
    let src = "
        int printf(char* format, ...);

        void test() {
            int num[2][2] = {{1, 2}, {3, 4}};

            int* ptr;
            
            ptr = num[0];
            printf(\"num[0]. %d\\\n\", *ptr);

            ptr = num;
            printf(\"num. %d\\\n\", *ptr);

            int* ptr2 = num[1];
            printf(\"num[1]. %d\\\n\", *ptr2);
        }
    ";

    let src = "
        int printf(char* format, ...);

        int num[2][2] = {{1, 2}, {3, 4}};

        int test() {
            int* ptr = num[0];
            printf(\"%d\\\n\", *ptr);

            return *ptr;
        }
    ";

    let src = "
        int printf(char* format, ...);

        int num[2][2] = {{1, 2}, {3, 4}};

        int test() {
            int* ptr = num[0];
            printf(\"%d\\\n\", *ptr);

            return ptr[0] + ptr[1] + ptr[2] + ptr[3];
        }
    ";

    let src = "
        int printf(char* format, ...);

        typedef struct date {
            int year, month;
            int day;
        } Date;

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

    let src = "
        typedef struct date {
            int year, month;
            int day;
        } Date;

        int test() {
            return 0;
        }
    ";

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
*/

    let src = "
        typedef struct circle {
            int radius;
        } Circle;
        
        impl Circle {
            const int PI100 = 314;
        
            Circle new(int radius) {
                Circle c = {radius};
        
                return c;
            }
        
            int area() {
                return self.radius * Self::PI100 / 100;
            }
        }

        int test() {

        }
    ";


    // tokenize
    let tokenized = Tokenizer::tokenize(src).unwrap();
    // parse
    let asts = Parser::parse(tokenized).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();
    println!("<<code parsed.>>");
    let mut env = Env::new();

    // for index in 0..asts.len() {
    //     let _any_value = gen.gen_code(&asts[index], &mut env, None, None).unwrap();
    // }
    gen.gen_toplevels(&asts, &mut env).unwrap();

    gen.module.print_to_stderr();

    println!("<<get llvm function>>");
    // let f: JitFunction<FuncType_void_void> = unsafe { gen.execution_engine.get_function("test").unwrap() };
    // let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    // let f: JitFunction<NoArgFunc> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    println!("<<call llvm function>>");
    let result = unsafe { f.call() };
    // let result = unsafe { f.call(1) };
    // assert_eq!(result, 1);
    println!("result: {result}");
    println!("<<end call llvm function>>");

    println!("<<all end>>");
}
