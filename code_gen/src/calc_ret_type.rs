use crate::parser::{AST, Type, NumberType, Pattern};
use super::{CodeGenError};
use super::Env;
use super::type_util::TypeUtil;
use crate::{Position};
use crate::CodeGen;

use parser::ast::ForInitExpr;
use parser::{EnumPattern, ExprAST};
use std::error::Error;
use std::rc::Rc;

impl<'ctx> CodeGen<'ctx> {
    pub fn calc_ret_type(&self, stmt: &AST, env: &mut Env<'ctx>) -> Result<Rc<Type>, Box<dyn Error>> {
        match stmt {
            AST::DefVar { specifiers, declarations, pos: _ } => {
                let base_type = specifiers.get_type();
                for decl in declarations {
                    let declarator = decl.get_declarator();
                    let typ = declarator.make_type(base_type);
                    let name = declarator.get_name();

                    env.insert_local_type(name, typ);
                }

                Ok(Rc::new(Type::Void))
            },
            AST::Block(blk, _pos) => {
                let mut typ = Rc::new(Type::Void);

                for e in &blk.body {
                    typ = self.calc_ret_type(e, env)?;

                    match e {
                        AST::Return(_, _) => {
                            break;
                        },
                        _ => (),  // do nothing
                    }
                }

                Ok(typ)
            },
            AST::Return(None, _pos) => {
                Ok(Rc::new(Type::Void))
            },
            AST::Return(Some(expr), _pos) => {
                let typ = TypeUtil::get_type(&**expr, env)?;
                Ok(typ)
            },
            AST::Labeled(_name, opt_ast, _pos) => {
                if let Some(ast) = opt_ast {
                    self.calc_ret_type(ast, env)
                }else{
                    Ok(Rc::new(Type::Void))
                }
            },
            AST::Switch(switch, _pos1) => {
                let opt_stmt = switch.get_stmt();
                if let Some(stmt) = opt_stmt {
                    if let AST::Block(blk, _pos2) = stmt {
                        // let mut typ = Rc::new(Type::Void);
                        let mut typ = None;

                        for e in &blk.body {
                            match e {
                                AST::Return(Some(boxed_ast), _) => {
                                    let t = TypeUtil::get_type(&**boxed_ast, env)?;

                                    if typ.is_none() {
                                        typ = Some(t);
                                    }else{
                                        if typ.as_ref().unwrap() != &t {
                                            return Err(Box::new(CodeGenError::return_type_mismatch(typ.as_ref().unwrap().as_ref().clone(), t.as_ref().clone(), boxed_ast.get_position().clone())));
                                        }
                                    }
                                },
                                AST::Case(case, _pos3) => {
                                    let stmt = case.get_stmt();
                                    let typ2 = self.calc_ret_type(stmt, env)?;

                                    if typ.is_none() {
                                        typ = Some(typ2);
                                    }else{
                                        if typ.as_ref().unwrap() != &typ2 {
                                            return Err(Box::new(CodeGenError::return_type_mismatch(typ.as_ref().unwrap().as_ref().clone(), typ2.as_ref().clone(), case.get_position().clone())));
                                        }
                                    }
                                },
                                _ => (),  // do nothing
                            }
                        }

                        if let Some(typ) = typ {
                            return Ok(typ);
                        }else{
                            return Ok(Rc::new(Type::Void));
                        }
                    }
                }

                Ok(Rc::new(Type::Void))
            },
            AST::If(_cond, if_then, if_else, pos) => {
                self.calc_ret_type_in_if(if_then, if_else, pos, env)
            },
            AST::IfLet { pattern_list, expr, then, else_, pos } => {
                self.calc_ret_type_in_if_let(then, else_, pattern_list, expr, pos, env)
            },
            AST::Loop { init_expr, pre_condition: _, body, update_expr: _, post_condition: _, pos: _ } => {
                env.add_new_local_types();

                if let Some(expr) = init_expr {
                    self.set_type_in_for_init_expr(expr, env)?
                }

                let mut result = Rc::new(Type::Void);
                if let Some(stmt) = body {
                    result = self.calc_ret_type(stmt, env)?;
                }

                env.remove_local_types();

                Ok(result)
            },
            AST::Match { pattern_list_list, expr, pos: _ } => {
                let mut result = Rc::new(Type::Void);
                let expr_type = TypeUtil::get_type(expr, env)?;

                env.add_new_local_types();

                for (pat_list, then) in pattern_list_list {
                    for pat in pat_list {
                        self.insert_pat_type(pat, &expr_type, env)?;
                    }

                    result = self.calc_ret_type(then, env)?;
                }

                env.remove_local_types();

                Ok(result)
            },
            _ => {
                Ok(Rc::new(Type::Void))
            },
        }
    }

    fn set_type_in_for_init_expr(&self, expr: &ForInitExpr, env: &mut Env<'ctx>) -> Result<(), Box<dyn Error>> {
        match expr {
            ForInitExpr::DefVar { specifiers, declarations, pos: _ } => {
                let base_type = specifiers.get_type();
                for decl in declarations {
                    let declarator = decl.get_declarator();
                    let typ = declarator.make_type(base_type);
                    let name = declarator.get_name();

                    env.insert_local_type(name, typ);
                }
            },
            ForInitExpr::ExprList(list) => {
                for expr in list {
                    self.set_type_in_for_init_expr(expr, env)?
                }
            },
            ForInitExpr::Expr(_) => {
                // do nothing
            }
        }

        Ok(())
    }

    pub fn calc_ret_type_in_if(&self, if_then: &AST, if_else: &Option<Box<AST>>, pos: &Position, env: &mut Env<'ctx>) -> Result<Rc<Type>, Box<dyn Error>> {
        let then_type = self.calc_ret_type(if_then, env)?;

        if then_type.is_void() {
            if let Some(else_expr) = if_else {
                let else_type = self.calc_ret_type(else_expr, env)?;
                if else_type.is_void() {
                    Ok(Rc::new(Type::Void))
                }else{
                    Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type.as_ref().clone(), else_type.as_ref().clone())))
                }

            }else{
                Ok(Rc::new(Type::Void))
            }

        }else{
            if let Some(else_expr) = if_else {
                let else_type = self.calc_ret_type(else_expr, env)?;
                if else_type.is_void() {
                    Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type.as_ref().clone(), else_type.as_ref().clone())))
                }else{
                    Ok(else_type)
                }

            }else{
                Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type.as_ref().clone(), Type::Void)))
            }
        }
    }

    pub fn calc_ret_type_in_if_let(&self, if_then: &AST, if_else: &Option<Box<AST>>, pattern_list: &Vec<Box<Pattern>>, expr: &ExprAST, pos: &Position, env: &mut Env<'ctx>) -> Result<Rc<Type>, Box<dyn Error>> {
        let result;

        env.add_new_local_types();

        for pattern in pattern_list {
            let expr_type = TypeUtil::get_type(expr, env)?;
            self.insert_pat_type(pattern, &expr_type, env)?;
        }

        let then_type = self.calc_ret_type(if_then, env)?;

        if then_type.is_void() {
            if let Some(else_expr) = if_else {
                let else_type = self.calc_ret_type(else_expr, env)?;
                if else_type.is_void() {
                    result = Ok(Rc::new(Type::Void));
                }else{
                    result = Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type.as_ref().clone(), else_type.as_ref().clone())));
                }

            }else{
                result = Ok(Rc::new(Type::Void));
            }

        }else{
            if let Some(else_expr) = if_else {
                let else_type = self.calc_ret_type(else_expr, env)?;
                if else_type.is_void() {
                    result = Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type.as_ref().clone(), else_type.as_ref().clone())));
                }else{
                    result = Ok(else_type);
                }

            }else{
                result = Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type.as_ref().clone(), Type::Void)));
            }
        }

        env.remove_local_types();

        Ok(result?)
    }

    fn insert_pat_type(&self, pattern: &Pattern, expr_type: &Rc<Type>, env: &mut Env<'ctx>) -> Result<(), Box<dyn Error + 'static>> {
        match pattern {
            Pattern::Var(name, opt_at_name, _pos) => {
                env.insert_local_type(name, Rc::clone(&expr_type));
    
                if let Some(pat_name) = opt_at_name {
                    env.insert_local_type(&pat_name, Rc::clone(expr_type));
                }
            },
            Pattern::Char(_, opt_at_name, _pos) | Pattern::CharRange(_, _, opt_at_name, _pos) => {
                let typ = Type::new_number_type(NumberType::Char);
                if let Some(pat_name) = opt_at_name {
                    env.insert_local_type(&pat_name, Rc::new(typ));
                }
            },
            Pattern::Number(_, opt_at_name, _pos) | Pattern::NumberRange(_, _, opt_at_name, _pos) => {
                if let Some(pat_name) = opt_at_name {
                    env.insert_local_type(&pat_name, Rc::clone(expr_type));
                }
            },
            Pattern::Str(_, opt_at_name, _pos) => {
                let typ = Type::new_pointer_type(Rc::new(Type::new_number_type(NumberType::Char)), false, false);
                if let Some(pat_name) = opt_at_name {
                    env.insert_local_type(&pat_name, Rc::new(typ));
                }
            },
            Pattern::Enum(enum_pat, opt_at_name, _pos) => {
                match enum_pat {
                    // Name::SubName
                    EnumPattern::Simple(typ, _name, _sub_name) => {
                        if let Some(pat_name) = opt_at_name {
                            env.insert_local_type(&pat_name, Rc::clone(typ));
                        }
                    },
                    // Name::SubName(pat_name @ pattern1, pattern2, ...)
                    EnumPattern::Tuple(_typ, _name, sub_name, pat_list) => {
                        let e_type = expr_type.get_enum_sub_type_by_name(sub_name).unwrap();
                        self.insert_enum_pat_list_type(e_type, env, pat_list)?;

                        if let Some(pat_name) = opt_at_name {
                            env.insert_local_type(&pat_name, Rc::clone(e_type));
                        }
                    },
                    // Name::SubName { field1: struct_pattern1, field2: struct_pattern2, ... }
                    EnumPattern::Struct(_typ, _name, sub_name, struct_pat) => {
                        let e_type = expr_type.get_enum_sub_type_by_name(sub_name).unwrap();
                        self.insert_struct_pat_type(e_type, env, struct_pat)?;

                        if let Some(pat_name) = opt_at_name {
                            env.insert_local_type(&pat_name, Rc::clone(e_type));
                        }
                    },
                }
            },
            Pattern::Struct(struct_pat, opt_at_name, _pos) => {
                self.insert_struct_pat_type(expr_type, env, struct_pat)?;

                if let Some(pat_name) = opt_at_name {
                    env.insert_local_type(&pat_name, Rc::clone(expr_type));
                }
            },
            Pattern::Tuple(pattern_list, opt_at_name, _pos) => {
                self.insert_enum_pat_list_type(expr_type, env, pattern_list)?;

                if let Some(pat_name) = opt_at_name {
                    env.insert_local_type(&pat_name, Rc::clone(expr_type));
                }
            },
            Pattern::OrList(pat_list, opt_at_name, _pos) => {
                for pat in pat_list {
                    self.insert_pat_type(pat, expr_type, env)?;
                }

                if let Some(pat_name) = opt_at_name {
                    env.insert_local_type(&pat_name, Rc::clone(expr_type));
                }
            },
        }

        Ok(())
    }

    fn insert_enum_pat_list_type(&self, expr_type: &Rc<Type>, env: &mut Env<'ctx>, pattern_list: &Vec<Vec<Box<Pattern>>>) -> Result<(), Box<dyn Error + 'static>> {
        for i in 0..pattern_list.len() {
            let pat_list = &pattern_list[i];
            let pat= &pat_list[0];
            let e_type = expr_type.get_field_type_from_tuple_at_index(i).unwrap();

            self.insert_pat_type(pat, e_type, env)?;
        }

        Ok(())
    }

    fn insert_struct_pat_type(&self, expr_type: &Rc<Type>, env: &mut Env<'ctx>, struct_pat: &parser::StructPattern) -> Result<(), Box<dyn Error + 'static>> {
        let pattern_map = struct_pat.get_map();
        let key_list = struct_pat.get_keys();

        for key in key_list {
            let pat = pattern_map.get(key).unwrap();
            let e_type = expr_type.get_field_type_from_struct_by_name(&key).unwrap();
    
            if let Some(pat_list) = pat {
                let pat = &pat_list[0];
                self.insert_pat_type(pat, e_type, env)?;

            }else{
                env.insert_local_type(key, Rc::clone(e_type));
            }
        }

        Ok(())
    }
}
