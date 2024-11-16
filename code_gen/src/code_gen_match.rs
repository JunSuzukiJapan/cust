use crate::parser::{AST, ExprAST, Type};
use crate::parser::Pattern;
use super::{CompiledValue, CodeGenError};
use super::Env;
use super::env::{BreakCatcher, ContinueCatcher};
use crate::Position;
use crate::CodeGen;

use inkwell::values::{AnyValue, AnyValueEnum, FunctionValue};
use inkwell::IntPredicate;
use parser::{NumberType, SpecifierQualifier};
use std::error::Error;
use std::rc::Rc;

impl<'ctx> CodeGen<'ctx> {
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
        let then_block = self.context.append_basic_block(func, "if_let.then");
        let else_block = self.context.append_basic_block(func, "if_let.else");
        let end_block  = self.context.append_basic_block(func, "if_let.end");

        self.builder.build_unconditional_branch(cond_block)?;
        self.builder.position_at_end(cond_block);

        env.add_new_local();

        // match patterns
        let cond = self.gen_expr(condition, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::condition_is_not_number(condition, (*condition).get_position().clone()))?;
        let matched = self.gen_pattern_match(pattern_list, pattern_name, pos, &cond, env, func)?.ok_or(CodeGenError::condition_is_not_number(condition, (*condition).get_position().clone()))?;
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
        env: &mut Env<'ctx>,
        func: FunctionValue<'ctx>
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        let bool_type = self.context.bool_type();
        let zero = bool_type.const_zero();
        let one = bool_type.const_all_ones();
        let condition_ptr = self.builder.build_alloca(bool_type, "matched")?;
        self.builder.build_store(condition_ptr, zero)?;

        let all_end_block  = self.context.append_basic_block(func, "match.all_end");

        for (pat, pos) in pattern_list {
            match &**pat {
                Pattern::Char(ch) => {
                    let then_block = self.context.append_basic_block(func, "match.then");
                    let next_block  = self.context.append_basic_block(func, "match.next");

                    let i8_type = self.context.i8_type();
                    let i8_ch = i8_type.const_int(*ch as u64, true);
                    let c = CompiledValue::new(Type::Number(NumberType::Char).into(), i8_ch.as_any_value_enum());

                    let (left, right) = self.bin_expr_implicit_cast(c, value.clone())?;
                    let left_value = left.get_value();
                    let right_value = right.get_value();
    
                    let comparison = self.builder.build_int_compare(IntPredicate::EQ, left_value.into_int_value(), right_value.into_int_value(), "match_compare_char")?;
                    self.builder.build_conditional_branch(comparison, then_block, next_block)?;

                    //
                    // matched
                    //
                    self.builder.position_at_end(then_block);
                    self.builder.build_store(condition_ptr, one)?;
                    self.builder.build_unconditional_branch(all_end_block)?;

                    //
                    // not matched
                    //
                    self.builder.position_at_end(next_block);
                },
                Pattern::CharRange(ch1, ch2) => {





                    unimplemented!()
                },
                Pattern::Enum(_) => {
                    unimplemented!()
                },
                Pattern::Number(num) => {
                    unimplemented!()
                },
                Pattern::NumberRange(_, _) => {
                    unimplemented!()
                },
                Pattern::Str(s) => {
                    unimplemented!()
                },
                Pattern::Struct(strct) => {
                    unimplemented!()
                },
                Pattern::Tuple(tpl) => {
                    unimplemented!()
                },
                Pattern::Var(name) => {
                    self.builder.build_store(condition_ptr, one)?;

                    let sq = SpecifierQualifier::default();

                    let typ = value.get_type();
                    let basic_type = env.basic_type_enum_from_type(&typ, self.context, pos)?;
                    let ptr = self.builder.build_alloca(basic_type, name)?;

                    let any_value = value.get_value();
                    let basic_value = self.try_as_basic_value(&any_value, pos)?;
                    self.builder.build_store(ptr, basic_value)?;

                    env.insert_local(name, Rc::clone(typ), sq, ptr);

                    // self.builder.build_unconditional_branch(all_end_block)?;

                    break;
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
        let result_any_value: AnyValueEnum = self.builder.build_load(condition_ptr, "get_condition")?.as_any_value_enum();
        Ok(Some(CompiledValue::new(Rc::new(num_type), result_any_value)))
    }
}