use crate::parser::{AST, ExprAST, Type, Block};
use super::{CompiledValue, CodeGenError};
use super::Env;
use super::env::{BreakCatcher, ContinueCatcher};
use crate::parser::{Switch, Case};
use crate::Position;
use crate::CodeGen;

use inkwell::values::{AnyValue, AnyValueEnum, InstructionOpcode, InstructionValue};
use inkwell::basic_block::BasicBlock;
use inkwell::{IntPredicate};
use std::error::Error;

impl<'ctx> CodeGen<'ctx> {

    pub fn gen_stmt<'b, 'c>(&self,
        ast: &'ctx AST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        match ast {
            AST::DefVar{specifiers, declarations, pos: _} => {
                self.gen_def_var(specifiers, declarations, env, break_catcher, continue_catcher)?;
                Ok(None)
            },
            AST::Block(block, _pos) => self.gen_block(block, env, break_catcher, continue_catcher),
            AST::Return(opt_expr, pos) => {
                let fun_typ = env.get_current_function_type().ok_or(CodeGenError::return_without_function(pos.clone()))?;
                let required_ret_type = fun_typ.get_return_type();

                let result: InstructionValue;
                if let Some(expr) = opt_expr {
                    let mut real_ret = self.gen_expr(expr, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(expr.get_position().clone()))?;
                    let real_ret_type = real_ret.get_type();

                    if required_ret_type.as_ref() != real_ret_type.as_ref() {
                        let casted = self.gen_implicit_cast(&real_ret.get_value(), &real_ret_type, &required_ret_type, expr.get_position())?;
                        real_ret = CompiledValue::new(real_ret.get_type().clone(), casted);
                    }

                    let ret = self.try_as_basic_value(&real_ret.get_value(), expr.get_position())?;
                    result = self.builder.build_return(Some(&ret))?;
                }else{
                    if ! required_ret_type.is_void() {
                        return Err(Box::new(CodeGenError::return_type_mismatch(Type::Void, required_ret_type.as_ref().clone(), pos.clone())));
                    }

                    result = self.builder.build_return(None)?;
                }
                Ok(Some(result.as_any_value_enum()))
            },
            AST::If(condition, then, else_, pos) => {
                let (_fun_type, func) = env.get_current_function().ok_or(CodeGenError::no_current_function(pos.clone()))?;
                let func = func.clone();
                let cond_block = self.context.append_basic_block(func, "if.cond");
                let then_block = self.context.append_basic_block(func, "if.then");
                let else_block = self.context.append_basic_block(func, "if.else");
                let end_block  = self.context.append_basic_block(func, "if.end");

                self.builder.build_unconditional_branch(cond_block)?;
                self.builder.position_at_end(cond_block);

                // check condition
                let cond = self.gen_expr(condition, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::condition_is_not_number(condition, (**condition).get_position().clone()))?;
                let mut comparison = cond.get_value().into_int_value();
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
            },
            AST::IfLet { pattern_list, expr, then, else_, pos } => {
                self.gen_if_let(pattern_list, expr, then, else_, pos, env, break_catcher, continue_catcher)
            },
            AST::Match { expr, pattern_list_list, pos } => {






                unimplemented!()
            },
            AST::Loop {init_expr, pre_condition, body, update_expr, post_condition, pos} => {
                self.gen_loop(init_expr, pre_condition, body, update_expr, post_condition, env, break_catcher, continue_catcher, pos)
            },
            AST::Break(pos) => {
                let break_block = break_catcher.ok_or(CodeGenError::break_not_in_loop_or_switch(pos.clone()))?.get_block();
                self.builder.build_unconditional_branch(*break_block)?;

                Ok(None)
            },
            AST::Continue(pos) => {
                let continue_block = continue_catcher.ok_or(CodeGenError::continue_not_in_loop(pos.clone()))?.get_block();
                self.builder.build_unconditional_branch(*continue_block)?;

                Ok(None)
            },
            AST::Goto(id, pos) => {
                let block = env.get_block(id).ok_or(CodeGenError::no_such_a_label(id, pos.clone()))?;
                self.builder.build_unconditional_branch(*block)?;

                Ok(None)
            },
            AST::Labeled(id, opt_stmt, pos) => {
                let block = env.get_block(id).ok_or(CodeGenError::no_such_a_label(id, pos.clone()))?;
                self.builder.build_unconditional_branch(*block)?;
                self.builder.position_at_end(*block);

                if let Some(stmt) = opt_stmt {
                    self.gen_stmt(stmt, env, break_catcher, continue_catcher)
                }else{
                    Ok(None)
                }
            },
            AST::Switch(switch, pos) => {
                self.gen_switch(switch, env, break_catcher, continue_catcher, pos)
            },
            AST::Case(case, _pos) => {
                self.gen_case(case, env, break_catcher, continue_catcher)
            },
            AST::Default(stmt, pos) => {
                self.gen_default(stmt, env, break_catcher, continue_catcher, pos)
            },
            AST::_self(pos) => {
                if let Some((_typ, _sq, ptr)) = env.get_self_ptr() {
                    let basic_val = self.builder.build_load(ptr, "get_self")?;
                    let any_val = basic_val.as_any_value_enum();

                    Ok(Some(any_val))
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable("self", pos.clone())))
                }
            },
            AST::Expr(expr_ast, _pos) => {
                if let Some(value) = self.gen_expr(expr_ast, env, break_catcher, continue_catcher)? {
                    Ok(Some(value.get_value()))
                }else{
                    Ok(None)
                }
            },
            AST::_Self(pos) => {
                Err(Box::new(CodeGenError::self_is_not_statement(pos.clone())))
            },
        }
    }

    pub fn gen_switch<'b, 'c>(&self,
        switch: &'ctx Switch,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let (_fun_type, func) = env.get_current_function().ok_or(CodeGenError::no_current_function(pos.clone()))?;
        let func = func.clone();
        let cond_block  = self.context.append_basic_block(func, "switch.cond");
        let end_block  = self.context.append_basic_block(func, "switch.end");
        let break_catcher = BreakCatcher::new(&end_block);
        let break_catcher = Some(&break_catcher);

        //
        // condition expr
        //
        self.builder.build_unconditional_branch(cond_block)?;
        self.builder.position_at_end(cond_block);
        let opt_cond_expr = switch.get_cond_expr();
        let opt_cond = if let Some(e) = opt_cond_expr {
            self.gen_expr(e, env, break_catcher, continue_catcher)?
        }else{
            return Ok(None);
        };
        if opt_cond.is_none(){
            return Ok(None);
        }
        let cond_expr = opt_cond.unwrap();

        //
        // cases
        //
        env.add_new_switch_case();

        let opt_stmt = switch.get_stmt();
        let _opt_result = if let Some(stmt) = opt_stmt {
            self.gen_stmt(stmt, env, break_catcher, continue_catcher)?
        }else{
            None
        };

        let case_list = env.get_case_list();
        let len = case_list.len();
        let mut opt_default = None;
        let mut current_block = cond_block;
        for i in 0..len {
            let case = &case_list[i];
            let block = case.get_block();

            //
            // add branch if last statement is not branch
            //
            if ! self.last_is_jump_statement(*block) {
                let next_block = if i == len - 1 {
                    end_block
                }else{
                    *case_list[i+1].get_block()
                };

                self.builder.position_at_end(*block);
                self.builder.build_unconditional_branch(next_block)?;
            }

            let insert_block = case.get_insert_block();
            if let Some(blk) = insert_block {
                if ! self.last_is_jump_statement(*blk) {
                    let next_block = if i == len - 1 {
                        end_block
                    }else{
                        *case_list[i+1].get_block()
                    };

                    self.builder.position_at_end(*blk);
                    self.builder.build_unconditional_branch(next_block)?;
                }
            }

            //
            // conditional jump
            //
            self.builder.position_at_end(current_block);

            if case.is_case() {  // case
                if opt_default.is_some() {
                    return Err(Box::new(CodeGenError::case_after_default(case.get_position().clone())));
                }

                let case_cond = case.get_cond().unwrap();
                let value = case_cond.as_i32_value();
                let i32_type = self.context.i32_type();
                let case_value = i32_type.const_int(value as u64, true);
                let real_value = self.try_as_basic_value(&cond_expr.get_value(), case_cond.get_position())?.into_int_value();
                let comparison = self.builder.build_int_compare(IntPredicate::EQ, real_value, case_value, "compare_switch_case")?;



                if i < len - 1 {
                    current_block = self.context.append_basic_block(func.clone(), &format!("cond_block_{}", i + 1));
                }else{
                    current_block = end_block;
                }
                self.builder.build_conditional_branch(comparison, *case.get_block(), current_block)?;

            }else{               // default
                if opt_default.is_some() {
                    return Err(Box::new(CodeGenError::already_default_defined(case.get_position().clone())));
                }
                opt_default = Some(case);

                self.builder.build_unconditional_branch(*case.get_block())?;
            }
        }

        // if let Some(default) = opt_default {
        //     self.builder.position_at_end(cond_block);
        //     self.builder.build_unconditional_branch(*default.get_block());
        // }


        env.remove_switch_case();

        //
        // end block
        //
        self.builder.position_at_end(end_block);

        Ok(None)
    }

    pub fn gen_case<'b, 'c>(&self,
        case: &'ctx Case,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let (_fun_type, func) = env.get_current_function().ok_or(CodeGenError::no_current_function(case.get_position().clone()))?;
        let case_block  = self.context.append_basic_block(func.clone(), "switch.case");

        self.builder.position_at_end(case_block);

        let const_cond = case.get_cond();
        let ast = case.get_stmt();
        let code = self.gen_stmt(ast, env, break_catcher, continue_catcher)?;
        let insert_block= self.builder.get_insert_block();

        env.insert_case(const_cond, case_block, code, insert_block, case.get_position().clone());

        Ok(None)
    }

    pub fn gen_default<'b, 'c>(&self,
        stmt: &'ctx AST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let (_fun_type, func) = env.get_current_function().ok_or(CodeGenError::no_current_function(pos.clone()))?;
        let default_block  = self.context.append_basic_block(func.clone(), "switch.default");

        self.builder.position_at_end(default_block);
        let code = self.gen_stmt(stmt, env, break_catcher, continue_catcher)?;
        let insert_block= self.builder.get_insert_block();

        env.insert_default(default_block, code, insert_block, pos.clone());

        Ok(None)
    }

    pub fn gen_loop<'b, 'c>(&self,
        init_expr: &'ctx Option<Box<ExprAST>>,
        pre_condition: &'ctx Option<Box<ExprAST>>,
        body: &'ctx Option<Box<AST>>,
        update_expr: &'ctx Option<Box<ExprAST>>,
        post_condition: &'ctx Option<Box<ExprAST>>,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let (_fun_type, func) = env.get_current_function().ok_or(CodeGenError::no_current_function(pos.clone()))?;
        let func = func.clone();
        let pre_condition_block = self.context.append_basic_block(func, "loop.pre_condition");
        let start_block = self.context.append_basic_block(func, "loop.start");
        // let post_condition_block = self.context.append_basic_block(func, "loop.post_condition");
        let update_block = self.context.append_basic_block(func, "loop.update");
        let end_block = self.context.append_basic_block(func, "loop.end");

        let break_catcher = BreakCatcher::new(&end_block);
        let continue_catcher = ContinueCatcher::new(&update_block);
        let break_catcher = Some(&break_catcher);
        let continue_catcher = Some(&continue_catcher);

        //
        // init exprs
        //
        if let Some(expr) = init_expr {
            self.gen_expr(expr, env, break_catcher, continue_catcher)?;
        }

        //
        // check pre condition
        //
        self.builder.build_unconditional_branch(pre_condition_block)?;
        self.builder.position_at_end(pre_condition_block);

        if let Some(cond) = pre_condition {
            let cond = self.gen_expr(cond, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::condition_is_not_number(cond, cond.get_position().clone()))?;
            let comparison = cond.get_value().into_int_value();
            self.builder.build_conditional_branch(comparison, start_block, end_block)?;
        }else{
            self.builder.build_unconditional_branch(start_block)?;
        }

        // loop start
        self.builder.position_at_end(start_block);

        //
        // body
        //
        if let Some(stmt) = body {
            self.gen_stmt(stmt, env, break_catcher, continue_catcher)?;
        }

        //
        // update
        //
        self.builder.build_unconditional_branch(update_block)?;
        self.builder.position_at_end(update_block);
        if let Some(expr) = update_expr {
            self.gen_expr(expr, env, break_catcher, continue_catcher)?;
        }

        //
        // check post condtition
        //
        if let Some(cond) = post_condition {
            let cond = self.gen_expr(cond, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::condition_is_not_number(cond, cond.get_position().clone()))?;
            let comparison = cond.get_value().into_int_value();
            self.builder.build_conditional_branch(comparison, pre_condition_block, end_block)?;
        }else{
            self.builder.build_unconditional_branch(pre_condition_block)?;
        }

        // loop end
        self.builder.position_at_end(end_block);

        Ok(None)
    }


    pub fn last_is_jump_statement(&self, block: BasicBlock) -> bool {
        if let Some(inst) = block.get_last_instruction() {
            let op_code = inst.get_opcode();
            match op_code {
                InstructionOpcode::Br | InstructionOpcode::Return => true,
                _ => false,
            }
        }else{
            false
        }
    }

    pub fn gen_block<'b, 'c>(&self,
        block: &'ctx Block,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {
        env.add_new_local();
        let result = self.gen_block_sub(block, env, break_catcher, continue_catcher);
        env.remove_local();

        result
    }

    fn gen_block_sub<'b, 'c>(&self,
        block: &'ctx Block,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {
        for ast in block.body.iter() {
            let _any_value = self.gen_stmt(ast, env, break_catcher, continue_catcher)?;
        }

        Ok(None)
    }
}
