use crate::parser::{AST, ExprAST, Type, StructDefinition, StructField};
use crate::parser::Pattern;
use super::{CompiledValue, CodeGenError};
use super::Env;
use super::env::{BreakCatcher, ContinueCatcher};
use super::type_util::TypeUtil;
use crate::Position;
use crate::CodeGen;

use inkwell::context::Context;
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, GlobalValue, PointerValue, StructValue};
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum};
use inkwell::types::AnyType;
use inkwell::types::StructType;
use parser::{NumberType, SpecifierQualifier};
use std::error::Error;
use std::collections::HashMap;
use std::rc::Rc;

impl<'ctx> CodeGen<'ctx> {
    pub fn gen_if_let<'b, 'c>(
        &self,
        pattern_list: &Vec<(Box<Pattern>, Position)>,
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
        let matched = self.gen_pattern_match(pattern_list, &cond, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::condition_is_not_number(condition, (*condition).get_position().clone()))?;
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
        value: &CompiledValue<'ctx>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        let bool_type = self.context.bool_type();
        let zero = bool_type.const_zero();
        let one = bool_type.const_all_ones();

        let mut condition = zero;
        for (pat, pos) in pattern_list {
            match &**pat {
                Pattern::Char(ch) => {
                    unimplemented!()
                },
                Pattern::CharRange(_, _) => {
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
                    condition = one;

                    let sq = SpecifierQualifier::default();

                    let typ = value.get_type();
                    let basic_type = env.basic_type_enum_from_type(&typ, self.context, pos)?;
                    let ptr = self.builder.build_alloca(basic_type, name)?;

                    let any_value = value.get_value();
                    let basic_value = self.try_as_basic_value(&any_value, pos)?;
                    self.builder.build_store(ptr, basic_value)?;

                    env.insert_local(name, Rc::clone(typ), sq, ptr);

                    break;
                },
            }
        }

        let num_type = Type::Number(NumberType::_Bool);
        let any_value: AnyValueEnum = condition.into();
        Ok(Some(CompiledValue::new(Rc::new(num_type), any_value)))
    }
}