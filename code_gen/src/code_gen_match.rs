use crate::parser::{AST, ExprAST, Type};
use crate::parser::{Pattern, EnumPattern, StructPattern};
use crate::type_util::TypeUtil;
use super::{CompiledValue, CodeGenError};
use super::Env;
use super::env::{BreakCatcher, ContinueCatcher};
use crate::Position;
use crate::CodeGen;

use inkwell::basic_block::BasicBlock;
use inkwell::types::{BasicType, BasicMetadataTypeEnum};
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{IntPredicate, AddressSpace};
use parser::{NumberType, SpecifierQualifier};
use std::error::Error;
use std::f32::consts::E;
use std::rc::Rc;

impl<'ctx> CodeGen<'ctx> {
    fn code_gen_fun_string_match_body(&self, current_function: FunctionValue<'ctx>) -> Result<(), Box<dyn Error>> {
        let loop_pre_condition = self.context.append_basic_block(current_function, "loop.pre_condition");
        let loop_start = self.context.append_basic_block(current_function, "loop.start");
        let loop_update = self.context.append_basic_block(current_function, "loop.update");
        let loop_end = self.context.append_basic_block(current_function, "loop.end");
        let and_left = self.context.append_basic_block(current_function, "and.left");
        let and_right = self.context.append_basic_block(current_function, "and.right");
        let and_false = self.context.append_basic_block(current_function, "and.false");
        let and_end = self.context.append_basic_block(current_function, "and.end");

        // define i32 @strcmp(ptr %0, ptr %1) {
        //     entry:
        //       %s1 = alloca ptr, align 8
        //       store ptr %0, ptr %s1, align 8
        //       %s2 = alloca ptr, align 8
        //       store ptr %1, ptr %s2, align 8
        //       br label %loop.pre_condition
        let char_type = self.context.i8_type();
        let char_ptr_type = char_type.ptr_type(AddressSpace::default());
        let s1 = self.builder.build_alloca(char_ptr_type, "s1").unwrap();
        let s2 = self.builder.build_alloca(char_ptr_type, "s2").unwrap();
        let param1 = current_function.get_nth_param(0).unwrap();
        let param2 = current_function.get_nth_param(1).unwrap();
        self.builder.build_store(s1, param1)?;
        self.builder.build_store(s2, param2)?;
        self.builder.build_unconditional_branch(loop_pre_condition)?;
            
        //     loop.pre_condition:                               ; preds = %loop.update, %entry
        //       br label %and.left
        self.builder.position_at_end(loop_pre_condition);
        self.builder.build_unconditional_branch(and_left)?;
            
        //     loop.start:                                       ; preds = %and.end
        //       %s16 = load ptr, ptr %s1, align 8
        //       %op_add_pointer = ptrtoint ptr %s16 to i64
        //       %op_add_pointer7 = add i64 %op_add_pointer, mul (i64 ptrtoint (ptr getelementptr (i8, ptr null, i32 1) to i64), i32 1)
        //       store i64 %op_add_pointer7, ptr %s1, align 8
        //       %s28 = load ptr, ptr %s2, align 8
        //       %op_add_pointer9 = ptrtoint ptr %s28 to i64
        //       %op_add_pointer10 = add i64 %op_add_pointer9, mul (i64 ptrtoint (ptr getelementptr (i8, ptr null, i32 1) to i64), i32 1)
        //       store i64 %op_add_pointer10, ptr %s2, align 8
        //       br label %loop.update
        self.builder.position_at_end(loop_start);
        let s16 = self.builder.build_load(char_ptr_type, s1, "load_s1")?;
        let op_add_pointer = self.builder.build_ptr_to_int(s16.into_pointer_value(), self.context.i64_type(), "op_add_pointer")?;
        let op_add_pointer7 = self.builder.build_int_add(op_add_pointer, self.context.i64_type().const_int(1, false), "op_add_pointer7")?;
        self.builder.build_store(s1, self.builder.build_int_to_ptr(op_add_pointer7, char_ptr_type, "int_to_ptr")?)?;
        let s28 = self.builder.build_load(char_ptr_type, s2, "load_s2")?;
        let op_add_pointer9 = self.builder.build_ptr_to_int(s28.into_pointer_value(), self.context.i64_type(), "op_add_pointer9")?;
        let op_add_pointer10 = self.builder.build_int_add(op_add_pointer9, self.context.i64_type().const_int(1, false), "op_add_pointer10")?;
        self.builder.build_store(s2, self.builder.build_int_to_ptr(op_add_pointer10, char_ptr_type, "int_to_ptr")?)?;
        self.builder.build_unconditional_branch(loop_update)?;
            
        //     loop.update:                                      ; preds = %loop.start
        //       br label %loop.pre_condition
        self.builder.position_at_end(loop_update);
        self.builder.build_unconditional_branch(loop_pre_condition)?;
            
        //     loop.end:                                         ; preds = %and.end
        //       %s111 = load ptr, ptr %s1, align 8
        //       %get_value_from_pointer12 = load i8, ptr %s111, align 1
        //       %s213 = load ptr, ptr %s2, align 8
        //       %get_value_from_pointer14 = load i8, ptr %s213, align 1
        //       %sub_int = sub i8 %get_value_from_pointer12, %get_value_from_pointer14
        //       %cast_from_unsigned_char_to_int = sext i8 %sub_int to i32
        //       ret i32 %cast_from_unsigned_char_to_int
        self.builder.position_at_end(loop_end);
        let s111 = self.builder.build_load(char_ptr_type, s1, "load_s1")?;
        let get_value_from_pointer12 = self.builder.build_load(char_type, s111.into_pointer_value(), "get_value_from_pointer12")?;
        let s213 = self.builder.build_load(char_ptr_type, s2, "load_s2")?;
        let get_value_from_pointer14 = self.builder.build_load(char_type, s213.into_pointer_value(), "get_value_from_pointer14")?;
        let sub_int = self.builder.build_int_sub(get_value_from_pointer12.into_int_value(), get_value_from_pointer14.into_int_value(), "sub_int")?;
        let eq_zero = self.builder.build_int_compare(IntPredicate::EQ, sub_int, self.context.i8_type().const_zero(), "eq_zero")?;
        self.builder.build_return(Some(&eq_zero))?;
            
        //     and.left:                                         ; preds = %loop.pre_condition
        //       %s11 = load ptr, ptr %s1, align 8
        //       %get_value_from_pointer = load i8, ptr %s11, align 1
        //       %and.not_zero_left = icmp ne i8 %get_value_from_pointer, 0
        //       br i1 %and.not_zero_left, label %and.right, label %and.false
        self.builder.position_at_end(and_left);
        let s11 = self.builder.build_load(char_ptr_type, s1, "load_s1")?;
        let get_value_from_pointer = self.builder.build_load(char_type, s11.into_pointer_value(), "get_value_from_pointer")?;
        let int_val = get_value_from_pointer.into_int_value();
        let and_not_zero_left = self.builder.build_int_compare(IntPredicate::NE, int_val, self.context.i8_type().const_zero(), "and_not_zero_left")?;
        self.builder.build_conditional_branch(and_not_zero_left, and_right, and_false)?;
            
        //     and.right:                                        ; preds = %and.left
        //       %s12 = load ptr, ptr %s1, align 8
        //       %get_value_from_pointer3 = load i8, ptr %s12, align 1
        //       %s24 = load ptr, ptr %s2, align 8
        //       %get_value_from_pointer5 = load i8, ptr %s24, align 1
        //       %eq_int = icmp eq i8 %get_value_from_pointer3, %get_value_from_pointer5
        //       %and.not_zero_right = icmp ne i1 %eq_int, false
        //       br i1 %and.not_zero_right, label %and.end, label %and.false
        self.builder.position_at_end(and_right);
        let s12 = self.builder.build_load(char_ptr_type, s1, "load_s1")?;
        let get_value_from_pointer3 = self.builder.build_load(char_type, s12.into_pointer_value(), "get_value_from_pointer3")?;
        let s24 = self.builder.build_load(char_ptr_type, s2, "load_s2")?;
        let get_value_from_pointer5 = self.builder.build_load(char_type, s24.into_pointer_value(), "get_value_from_pointer5")?;
        let int_val = get_value_from_pointer3.into_int_value();
        let int_val2 = get_value_from_pointer5.into_int_value();
        let eq_int = self.builder.build_int_compare(IntPredicate::EQ, int_val, int_val2, "eq_int")?;
        let and_not_zero_right = self.builder.build_int_compare(IntPredicate::NE, eq_int, self.context.bool_type().const_zero(), "and_not_zero_right")?;
        self.builder.build_conditional_branch(and_not_zero_right, and_end, and_false)?;
            
        //     and.false:                                        ; preds = %and.right, %and.left
        //       br label %and.end
        self.builder.position_at_end(and_false);
        self.builder.build_unconditional_branch(and_end)?;
            
        //     and.end:                                          ; preds = %and.false, %and.right
        //       %and.result = phi i1 [ true, %and.right ], [ false, %and.false ]
        //       br i1 %and.result, label %loop.start, label %loop.end
        //     }
        self.builder.position_at_end(and_end);
        let and_result = self.builder.build_phi(self.context.bool_type(), "and.result")?;
        and_result.add_incoming(&[(&self.context.bool_type().const_all_ones(), and_right), (&self.context.bool_type().const_zero(), and_false)]);
        self.builder.build_conditional_branch(and_result.as_basic_value().into_int_value(), loop_start, loop_end)?;

        Ok(())
    }

    fn get_fun_string_match(&self, current_function: FunctionValue<'ctx>, current_block: BasicBlock<'ctx>, env: &mut Env<'ctx>) -> Result<FunctionValue<'ctx>, Box<dyn Error>> {
        if let Some(function) = env.inner_fun_string_match {
            return Ok(function);
        }

        // let jump_label = self.context.append_basic_block(current_function, "jump_label");
        // self.builder.build_unconditional_branch(jump_label)?;

        //
        // define string_match
        //

        // prologue
        let fn_name = "inner::string_match";
        let args_type: Vec<BasicMetadataTypeEnum> = vec![
            self.context.i8_type().ptr_type(AddressSpace::default()).as_basic_type_enum().into(),
            self.context.i8_type().ptr_type(AddressSpace::default()).as_basic_type_enum().into()
        ];
        let fn_type = self.context.bool_type().fn_type(args_type.as_slice(), false);
        let function = self.module.add_function(fn_name, fn_type, None);

        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        self.code_gen_fun_string_match_body(function)?;

        env.inner_fun_string_match = Some(function);

        //
        // end of define string_match
        //

        // self.builder.position_at_end(jump_label);
        self.builder.position_at_end(current_block);

        Ok(function)
    }

    fn gen_string_match(
        &self,
        arg1: BasicMetadataValueEnum<'ctx>,
        arg2: BasicMetadataValueEnum<'ctx>,
        current_function: FunctionValue<'ctx>,
        current_block: BasicBlock<'ctx>,
        env: &mut Env<'ctx>
    ) -> Result<IntValue<'ctx>, Box<dyn Error>> {

        let args = [arg1, arg2];
        let function = self.get_fun_string_match(current_function, current_block, env)?;
        let call_site_value = self.builder.build_call(function, &args, "call_inner::fun_string_match")?;
        let any_val = call_site_value.try_as_basic_value().left().unwrap().as_any_value_enum();
        let int_val = any_val.into_int_value();

        Ok(int_val)
    }

    fn gen_tag_match(
        &self,
        arg1: BasicMetadataValueEnum<'ctx>,
        arg2: BasicMetadataValueEnum<'ctx>,
        current_function: FunctionValue<'ctx>,
        current_block: BasicBlock<'ctx>,
        env: &mut Env<'ctx>
    ) -> Result<IntValue<'ctx>, Box<dyn Error>> {



        unimplemented!()
    }

    pub fn gen_if_let<'b, 'c>(
        &self,
        pattern_list: &Vec<(Box<Pattern>, Position)>,
        pattern_name: &Option<String>,
        condition: &ExprAST,
        then: &'ctx AST,
        else_: &'ctx Option<Box<AST>>,
        pos: &Position,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let (_fun_type, func) = env.get_current_function().ok_or(CodeGenError::no_current_function(pos.clone()))?;
        let func = func.clone();
        let cond_block = self.context.append_basic_block(func, "if_let.cond");
        let cond_block2 = self.context.append_basic_block(func, "if_let.cond2");
        let then_block = self.context.append_basic_block(func, "if_let.then");
        let else_block = self.context.append_basic_block(func, "if_let.else");
        let end_block  = self.context.append_basic_block(func, "if_let.end");

        self.builder.build_unconditional_branch(cond_block)?;
        self.builder.position_at_end(cond_block);

        env.add_new_local();

        // match patterns
        let cond = self.gen_expr(condition, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::condition_is_not_number(condition, (*condition).get_position().clone()))?;
        let matched = self.gen_pattern_match(
            pattern_list,
            pattern_name,
            pos,
            &cond,
            then,
            env,
            func,
            cond_block,
            cond_block2,
            break_catcher,
            continue_catcher
        )?.ok_or(CodeGenError::condition_is_not_number(condition, (*condition).get_position().clone()))?;

        self.builder.position_at_end(cond_block2);
        let mut comparison = matched.get_value().into_int_value();
        let i1_type = self.context.bool_type();
        comparison = self.builder.build_int_cast(comparison, i1_type, "cast to i1")?;  // cast to i1
        self.builder.build_conditional_branch(comparison, then_block, else_block)?;

        // then block
        self.builder.position_at_end(then_block);
        self.gen_stmt(then, env, break_catcher, continue_catcher)?;
        if let Some(blk) = self.builder.get_insert_block() {
            if ! self.last_is_jump_statement(blk) {
                self.builder.position_at_end(blk);
                self.builder.build_unconditional_branch(end_block)?;
            }
        }
        if ! self.last_is_jump_statement(then_block) {
            self.builder.position_at_end(then_block);
            self.builder.build_unconditional_branch(end_block)?;
        }

        env.remove_local();

        // else block
        self.builder.position_at_end(else_block);
        if let Some(expr) = else_ {
            self.gen_stmt(expr, env, break_catcher, continue_catcher)?;
            if let Some(blk) = self.builder.get_insert_block() {
                if ! self.last_is_jump_statement(blk) {
                    self.builder.position_at_end(blk);
                    self.builder.build_unconditional_branch(end_block)?;
                }
            }
        }
        if ! self.last_is_jump_statement(else_block) {
            self.builder.position_at_end(else_block);
            self.builder.build_unconditional_branch(end_block)?;
        }

        // end block
        self.builder.position_at_end(end_block);

        Ok(None)
    }

    fn gen_pattern_match<'b, 'c>(&self,
        pattern_list: &Vec<(Box<Pattern>, Position)>,
        pattern_name: &Option<String>,
        pattern_pos: &Position,
        value: &CompiledValue<'ctx>,
        then: &'ctx AST,
        env: &mut Env<'ctx>,
        func: FunctionValue<'ctx>,
        current_block: BasicBlock<'ctx>,
        after_match_block: BasicBlock<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        let bool_type = self.context.bool_type();
        let zero = bool_type.const_zero();
        let one = bool_type.const_all_ones();
        let condition_ptr = self.builder.build_alloca(bool_type, "matched")?;
        self.builder.build_store(condition_ptr, zero)?;  // set return value false

        let all_end_block = after_match_block;

        for (pat, pos) in pattern_list {
            match &**pat {
                Pattern::Var(name) => {
                    self.gen_ver_match(value, env, one, condition_ptr, pos, name)?;

                    break;
                },
                Pattern::Char(ch) => {
                    self.gen_char_match(value, func, one, condition_ptr, all_end_block, pos, ch)?;
                },
                Pattern::CharRange(ch1, ch2) => {
                    self.gen_char_range_match(value, func, one, condition_ptr, all_end_block, pos, ch1, ch2)?;
                },
                Pattern::Number(num) => {
                    self.gen_number_match(value, func, one, condition_ptr, all_end_block, pos, num)?;
                },
                Pattern::NumberRange(num1, num2) => {
                    self.gen_number_range_match(value, func, one, condition_ptr, all_end_block, pos, num1, num2)?;
                },
                Pattern::Str(s) => {
                    self.gen_str_match(value, env, func, current_block, one, condition_ptr, all_end_block, pos, s)?;
                },
                Pattern::Struct(strct) => {






                    unimplemented!()
                },
                Pattern::Tuple(tpl) => {





                    unimplemented!()

                },
                Pattern::Enum(enum_pat) => {
                    self.gen_enum_match(value, env, func, one, condition_ptr, all_end_block, pos, enum_pat)?;
                },
            }
        }

        self.builder.build_unconditional_branch(all_end_block)?;

        self.builder.position_at_end(all_end_block);

        // '@' 以降に対応する。
        if let Some(alias_name) = pattern_name {
            let sq = SpecifierQualifier::default();

            let typ = value.get_type();
            let basic_type = env.basic_type_enum_from_type(&typ, self.context, pattern_pos)?;
            let ptr = self.builder.build_alloca(basic_type, alias_name)?;

            let any_value = value.get_value();
            let basic_value = self.try_as_basic_value(&any_value, pattern_pos)?;
            self.builder.build_store(ptr, basic_value)?;

            env.insert_local(alias_name, Rc::clone(typ), sq, ptr);
        }

        let num_type = Type::Number(NumberType::_Bool);
        let result_any_value: AnyValueEnum = self.builder.build_load(bool_type, condition_ptr, "get_condition")?.as_any_value_enum();
        Ok(Some(CompiledValue::new(Rc::new(num_type), result_any_value)))
        // let result_any_value = condition_ptr.as_any_value_enum();
        // Ok(Some(CompiledValue::new(Rc::new(num_type), result_any_value)))
    }
    
    fn gen_ver_match(&self, value: &CompiledValue<'ctx>, env: &mut Env<'ctx>, one: IntValue<'_>, condition_ptr: inkwell::values::PointerValue<'ctx>, pos: &Position, name: &String) -> Result<(), Box<dyn Error>> {
        self.builder.build_store(condition_ptr, one)?;
        let sq = SpecifierQualifier::default();
        let typ = value.get_type();
        let basic_type = env.basic_type_enum_from_type(&typ, self.context, pos)?;
        let ptr = self.builder.build_alloca(basic_type, name)?;
        let any_value = value.get_value();
        let basic_value = self.try_as_basic_value(&any_value, pos)?;
        self.builder.build_store(ptr, basic_value)?;
        env.insert_local(name, Rc::clone(typ), sq, ptr);
        Ok(())
    }
    
    fn gen_char_match(&self, value: &CompiledValue<'ctx>, func: FunctionValue<'ctx>, one: IntValue<'_>, condition_ptr: inkwell::values::PointerValue<'ctx>, all_end_block: BasicBlock<'ctx>, pos: &Position, ch: &char) -> Result<(), Box<dyn Error>> {
        let then_block = self.context.append_basic_block(func, "match.then");
        let else_block  = self.context.append_basic_block(func, "match.else");
        let i8_type = self.context.i8_type();
        let i8_ch = i8_type.const_int(*ch as u64, true);
        let c = CompiledValue::new(Type::Number(NumberType::Char).into(), i8_ch.as_any_value_enum());
        let (left, right) = self.bin_expr_implicit_cast(c, value.clone(), pos)?;
        let left_value = left.get_value();
        let right_value = right.get_value();
        let comparison = self.builder.build_int_compare(IntPredicate::EQ, left_value.into_int_value(), right_value.into_int_value(), "match_compare_char")?;
        self.builder.build_conditional_branch(comparison, then_block, else_block)?;
        self.builder.position_at_end(then_block);
        self.builder.build_store(condition_ptr, one)?;
        self.builder.build_unconditional_branch(all_end_block)?;
        self.builder.position_at_end(else_block);
        Ok(())
    }

    fn gen_char_range_match(&self, value: &CompiledValue<'ctx>, func: FunctionValue<'ctx>, one: IntValue<'_>, condition_ptr: inkwell::values::PointerValue<'ctx>, all_end_block: BasicBlock<'ctx>, pos: &Position, ch1: &char, ch2: &char) -> Result<(), Box<dyn Error>> {
        let greater_than_block = self.context.append_basic_block(func, "match.greater");
        let less_than_block = self.context.append_basic_block(func, "match.less");
        let else_block  = self.context.append_basic_block(func, "match.else");
        let i8_type = self.context.i8_type();
        let i8_ch = i8_type.const_int(*ch1 as u64, true);
        let c = CompiledValue::new(Type::Number(NumberType::Char).into(), i8_ch.as_any_value_enum());
        let (other, target) = self.bin_expr_implicit_cast(c, value.clone(), pos)?;
        let other_value = other.get_value();
        let target_value = target.get_value();
        let comparison = self.builder.build_int_compare(IntPredicate::UGE, target_value.into_int_value(), other_value.into_int_value(), "match_compare_char")?;
        self.builder.build_conditional_branch(comparison, greater_than_block, else_block)?;
        self.builder.position_at_end(greater_than_block);
        let i8_ch2 = i8_type.const_int(*ch2 as u64, true);
        let c2 = CompiledValue::new(Type::Number(NumberType::Char).into(), i8_ch2.as_any_value_enum());
        let (other, target) = self.bin_expr_implicit_cast(c2, value.clone(), pos)?;
        let other_value = other.get_value();
        let target_value = target.get_value();
        let comparison = self.builder.build_int_compare(IntPredicate::ULE, target_value.into_int_value(), other_value.into_int_value(), "match_compare_char")?;
        self.builder.build_conditional_branch(comparison, less_than_block, else_block)?;
        self.builder.position_at_end(less_than_block);
        self.builder.build_store(condition_ptr, one)?;
        self.builder.build_unconditional_branch(all_end_block)?;
        self.builder.position_at_end(else_block);
        Ok(())
    }
    
    fn gen_number_match(&self, value: &CompiledValue<'ctx>, func: FunctionValue<'ctx>, one: IntValue<'_>, condition_ptr: inkwell::values::PointerValue<'ctx>, all_end_block: BasicBlock<'ctx>, pos: &Position, num: &i128) -> Result<(), Box<dyn Error>> {
        let then_block = self.context.append_basic_block(func, "match.then");
        let else_block  = self.context.append_basic_block(func, "match.else");
        let i64_type = self.context.i64_type();
        let i64_num = i64_type.const_int(*num as u64, true);
        let n = CompiledValue::new(Type::Number(NumberType::LongLong).into(), i64_num.as_any_value_enum());
        let (left, right) = self.bin_expr_implicit_cast(n, value.clone(), pos)?;
        let left_value = left.get_value();
        let right_value = right.get_value();
        let comparison = self.builder.build_int_compare(IntPredicate::EQ, left_value.into_int_value(), right_value.into_int_value(), "match_compare_number")?;
        self.builder.build_conditional_branch(comparison, then_block, else_block)?;
        self.builder.position_at_end(then_block);
        self.builder.build_store(condition_ptr, one)?;
        self.builder.build_unconditional_branch(all_end_block)?;
        self.builder.position_at_end(else_block);
        Ok(())
    }
    
    fn gen_number_range_match(&self, value: &CompiledValue<'ctx>, func: FunctionValue<'ctx>, one: IntValue<'_>, condition_ptr: inkwell::values::PointerValue<'ctx>, all_end_block: BasicBlock<'ctx>, pos: &Position, num1: &i128, num2: &i128) -> Result<(), Box<dyn Error>> {
        let greater_than_block = self.context.append_basic_block(func, "match.greater");
        let less_than_block = self.context.append_basic_block(func, "match.less");
        let else_block  = self.context.append_basic_block(func, "match.else");
        let i64_type = self.context.i64_type();
        let i64_num = i64_type.const_int(*num1 as u64, true);
        let n = CompiledValue::new(Type::Number(NumberType::LongLong).into(), i64_num.as_any_value_enum());
        let (other, target) = self.bin_expr_implicit_cast(n, value.clone(), pos)?;
        let other_value = other.get_value();
        let target_value = target.get_value();
        let comparison = self.builder.build_int_compare(IntPredicate::UGE, target_value.into_int_value(), other_value.into_int_value(), "match_compare_char")?;
        self.builder.build_conditional_branch(comparison, greater_than_block, else_block)?;
        self.builder.position_at_end(greater_than_block);
        let i64_num2 = i64_type.const_int(*num2 as u64, true);
        let n2 = CompiledValue::new(Type::Number(NumberType::LongLong).into(), i64_num2.as_any_value_enum());
        let (other, target) = self.bin_expr_implicit_cast(n2, value.clone(), pos)?;
        let other_value = other.get_value();
        let target_value = target.get_value();
        let comparison = self.builder.build_int_compare(IntPredicate::ULE, target_value.into_int_value(), other_value.into_int_value(), "match_compare_char")?;
        self.builder.build_conditional_branch(comparison, less_than_block, else_block)?;
        self.builder.position_at_end(less_than_block);
        self.builder.build_store(condition_ptr, one)?;
        self.builder.build_unconditional_branch(all_end_block)?;
        self.builder.position_at_end(else_block);
        Ok(())
    }
    
    fn gen_str_match(&self, value: &CompiledValue<'ctx>, env: &mut Env<'ctx>, func: FunctionValue<'ctx>, current_block: BasicBlock<'ctx>, one: IntValue<'_>, condition_ptr: inkwell::values::PointerValue<'ctx>, all_end_block: BasicBlock<'ctx>, pos: &Position, s: &String) -> Result<(), Box<dyn Error>> {
        let then_block = self.context.append_basic_block(func, "match.then");
        let else_block  = self.context.append_basic_block(func, "match.else");
        let str1 = self.builder.build_global_string_ptr(s, "global_str_for_match")?;
        let str1 = self.try_as_basic_metadata_value(&str1.as_any_value_enum(), pos)?;
        let str2 = value.get_value();
        let str2 = self.try_as_basic_metadata_value(&str2, pos)?;
        let comparison = self.gen_string_match(str1, str2, func, current_block, env)?;
        self.builder.build_conditional_branch(comparison, then_block, else_block)?;
        self.builder.position_at_end(then_block);
        self.builder.build_store(condition_ptr, one)?;
        self.builder.build_unconditional_branch(all_end_block)?;
        self.builder.position_at_end(else_block);
        Ok(())
    }

    fn gen_enum_match(&self, value: &CompiledValue<'ctx>, env: &mut Env<'ctx>, func: FunctionValue<'ctx>, one: IntValue<'_>, condition_ptr: inkwell::values::PointerValue<'ctx>, all_end_block: BasicBlock<'ctx>, pos: &Position, enum_pat: &EnumPattern) -> Result<(), Box<dyn Error>> {
        Ok(match enum_pat {
            EnumPattern::Simple(name, _) => {
                let then_block = self.context.append_basic_block(func, "match.then");
                let else_block  = self.context.append_basic_block(func, "match.else");
    
    
    
    
    
    
                // let enum_name = name.clone();
                // let enum_value = value.get_value();
                // let enum_value = self.try_as_basic_metadata_value(&enum_value, pos)?;
    
                // let comparison = self.gen_string_match(enum_name.as_str(), enum_value, func, current_block, env)?;
                // self.builder.build_conditional_branch(comparison, then_block, else_block)?;
    
                // //
                // // matched
                // //
                // self.builder.position_at_end(then_block);
                // self.builder.build_store(condition_ptr, one)?;  // set return value true
                // self.builder.build_unconditional_branch(all_end_block)?;
    
                // //
                // // not matched
                // //
                // self.builder.position_at_end(else_block);
    
                unimplemented!()
            },
            EnumPattern::Tuple(name, sub_name, pattern_list) => {
                // let then_block = self.context.append_basic_block(func, "match.then");
                // let else_block  = self.context.append_basic_block(func, "match.else");
    
                // let enum_name = name.clone();
                // let enum_value = value.get_value();
                // let enum_value = self.try_as_basic_metadata_value(&enum_value, pos)?;
    
                // let comparison = self.gen_string_match(enum_name.as_str(), enum_value, func, current_block, env)?;
                // self.builder.build_conditional_branch(comparison, then_block, else_block)?;
    
                // //
                // // matched
                // //
                // self.builder.position_at_end(then_block);
                // self.builder.build_store(condition_ptr, one)?;  // set return value true
                // self.builder.build_unconditional_branch(all_end_block)?;
    
                // //
                // // not matched
                // //
                // self.builder.position_at_end(else_block);
    
                unimplemented!()
            },
            EnumPattern::Struct(type_name, field_name, struct_pat) => {
                let then_block = self.context.append_basic_block(func, "match.then");
                let else_block  = self.context.append_basic_block(func, "match.else");
    
                let arg_type = value.get_type();
                // let pat_type = env.get_type(name).ok_or(CodeGenError::no_such_a_type(name, pos.clone()))?;
                // if arg_type != pat_type {
                //     return Err(CodeGenError::type_mismatch(arg_type.clone(), pat_type, pos.clone()).into());
                // }
    
                let arg_enum_def = arg_type.get_enum_definition().ok_or(CodeGenError::not_enum(arg_type.as_ref().clone(), pos.clone()))?;
                let required_tag = arg_enum_def.get_index(&field_name).ok_or(CodeGenError::no_such_a_field(arg_enum_def.get_name().to_string(), type_name.to_string(), pos.clone()))?;
    
                let i64_type = self.context.i64_type();
                let i64_num = i64_type.const_int(required_tag as u64, true);
                let left = CompiledValue::new(Type::Number(NumberType::LongLong).into(), i64_num.as_any_value_enum());
    
                let real_tag = self.gen_get_tag(value, env, pos)?;
                let ty = Rc::new(Type::Number(NumberType::Int));
                let right = CompiledValue::new(ty, real_tag.as_any_value_enum());
    
                let (left, right) = self.bin_expr_implicit_cast(left, right, pos)?;
                let left_value = left.get_value();
                let right_value = right.get_value();
                let comparison = self.builder.build_int_compare(IntPredicate::EQ, left_value.into_int_value(), right_value.into_int_value(), "match_compare_number")?;
                self.builder.build_conditional_branch(comparison, then_block, else_block)?;
    
                //
                // matched
                //
                self.builder.position_at_end(then_block);
                self.builder.build_store(condition_ptr, one)?;

                // TODO: 
                let pattern_map = struct_pat.get_map();
eprintln!("struct_pat: {:?}\n", struct_pat);
                let struct_value = self.gen_get_struct_value(value, env, pos)?;
eprintln!("pattern_map: {:?}\n", pattern_map);
eprintln!("struct_value: {:?}\n", struct_value);

                for (pat_name, item) in pattern_map.iter() {
eprintln!("name: {:?}\n", pat_name);
eprintln!("item: {:?}\n", item);
                    if let Some((pattern_list, opt_at_name)) = item {  // if let (type_name::FieldName {pat_name: item})






                    }else{ // item is None.                               if let (type_name::FieldName {pat_name})





                    }
                }





                self.builder.build_unconditional_branch(all_end_block)?;
        
                //
                // not matched
                //
                self.builder.position_at_end(else_block);
            },
        })
    }
    
    fn gen_get_tag(&self,
        value: &CompiledValue<'ctx>,
        env: &mut Env<'ctx>,
        pos: &Position
    ) -> Result<IntValue<'ctx>, Box<dyn Error>> {

        let v = value.get_value();
        let st_value = v.into_struct_value();
        let ptr = st_value.get_field_at_index(0).unwrap().into_pointer_value();
        let ty = st_value.get_type();
        let ty2 = ty.get_field_type_at_index(0).unwrap();
        let tag_value = self.builder.build_load(ty2, ptr, "get_tag")?;

        // let v = value.get_value();
        // let st_value = v.into_struct_value();
        // let ty = st_value.get_type();
        // let tag_type = ty.get_field_type_at_index(0).unwrap();
        // let ptr = st_value.get_field_at_index(0).unwrap().into_pointer_value();
        // let tag_ptr = self.builder.build_struct_gep(tag_type, ptr, 0, "get_tag_ptr")?;
        // let tag_value = self.builder.build_load(tag_type, tag_ptr, "load_tag_value")?;

        Ok(tag_value.into_int_value())
    }

    fn gen_get_struct_value(&self,
        value: &CompiledValue<'ctx>,
        env: &mut Env<'ctx>,
        pos: &Position
    ) -> Result<StructValue<'ctx>, Box<dyn Error>> {

        let v = value.get_value();
        let st_value = v.into_struct_value();
        let ty = st_value.get_type();
        let struct_type = ty.get_field_type_at_index(1).unwrap();

        let ptr = st_value.get_field_at_index(0).unwrap().into_pointer_value();
        let struct_ptr = self.builder.build_struct_gep(struct_type, ptr, 1, "get_struct_ptr")?;
        let struct_value = self.builder.build_load(struct_type, struct_ptr, "load_struct_value")?;

        Ok(struct_value.into_struct_value())
    }
}