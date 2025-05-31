use crate::parser::{Type, EnumDefinition, Enumerator, EnumLiteral, ast::TupleLiteral};
use super::{CodeGenError, CompiledValue};
use super::Env;
use super::env::{BreakCatcher, ContinueCatcher};
use super::type_util::TypeUtil;
use crate::Position;
use crate::CodeGen;

use inkwell::context::Context;
use inkwell::values::{IntValue, AnyValue, PointerValue, BasicValueEnum, BasicValue};
use inkwell::types::{AnyTypeEnum, BasicTypeEnum, StructType};
use inkwell::types::AnyType;
use parser::ConstExpr;
use std::error::Error;
use std::collections::HashMap;
use std::rc::Rc;


impl<'ctx> CodeGen<'ctx> {
    pub fn gen_define_enum<'b, 'c>(
        &self,
        enum_name: &str,
        enum_def: &EnumDefinition,
        type_variables: &Option<Vec<String>>,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<AnyTypeEnum<'ctx>>, Box<dyn Error>> {

        match enum_def {
            EnumDefinition::StandardEnum { fields, .. } => {
                let (enumerator_list, index_map) = self.enum_from_enum_standard(fields, type_variables, env)?;
                let i32_type = self.context.i32_type();
                env.insert_enum_const(enum_name.into(), &i32_type, enumerator_list, index_map, pos)?;

                Ok(None)
            },
            EnumDefinition::TaggedEnum { fields, .. } => {
                let (type_list, index_map, max_size, max_size_type) = Self::tagged_enum_from_enum_definition(enum_name, fields, &self.enum_tag_type, type_variables, self.context, pos)?;
                env.insert_tagged_enum(enum_name.into(), type_list, index_map, max_size, max_size_type, pos)?;
        
                if let Some(t) = max_size_type {
                    Ok(Some(t.as_any_type_enum()))
                }else{
                    Ok(None)
                }
            },
        }
    }

    pub fn tagged_enum_from_enum_definition<'b, 'c>(
        _enum_name: &str,
        fields: &Vec<Enumerator>,
        tag_type: &Rc<Type>,
        _type_variables: &Option<Vec<String>>,
        ctx: &'ctx Context,
        pos: &Position
    ) -> Result<(Vec<(Rc<Type>, BasicTypeEnum<'ctx>)>, HashMap<String, usize>, u64, Option<BasicTypeEnum<'ctx>>), Box<dyn Error>> {

        let mut list: Vec<(Rc<Type>, BasicTypeEnum<'ctx>)> = Vec::new();
        let mut index_map: HashMap<String, usize> = HashMap::new();
        let mut index = 0;
        let mut max_size = 0;
        let mut max_size_type: Option<BasicTypeEnum> = None;
 
        let tag_basic_type = TypeUtil::to_basic_type_enum(&tag_type, ctx, pos)?;
        let rc_tag_type = Rc::clone(tag_type);
        let tag_size = Self::size_of(&tag_basic_type)?;

        for field in fields {
            match field {
                Enumerator::Const { name, .. } => {
                    list.push((Rc::clone(&rc_tag_type), tag_basic_type.clone()));

                    let size = tag_size;
                    if size > max_size {
                        max_size = size;
                        max_size_type = Some(tag_basic_type);
                    }

                    if max_size_type.is_none() {
                        max_size = size;
                        max_size_type = Some(tag_basic_type.clone())
                    }

                    index_map.insert(name.clone(), index);
                },
                Enumerator::TypeStruct { name, struct_type } => {
                    let def = struct_type.get_struct_definition().unwrap();
                    let type_variables = struct_type.get_type_variables();

                    let typ = Type::struct_from_struct_definition(Some(name.to_string()), def.clone(), type_variables.clone());
                    let t = TypeUtil::to_basic_type_enum(&typ, ctx, pos)?;
                    let t = Self::add_tag_type(tag_basic_type, t, ctx);
                    list.push((Rc::new(typ), t.clone()));

                    let size = Self::size_of(&t)?;
                    if size > max_size {
                        max_size = size;
                        max_size_type = Some(t);
                    }

                    if max_size_type.is_none() {
                        max_size = size;
                        max_size_type = Some(t.clone())
                    }

                    index_map.insert(name.clone(), index);
                },
                Enumerator::TypeTuple { name, tuple_type } => {
                    let typ = TypeUtil::to_basic_type_enum(&tuple_type, ctx, pos)?;
                    let t = Self::add_tag_type(tag_basic_type, typ, ctx);
                    list.push((Rc::clone(tuple_type), t.clone()));

                    let size = Self::size_of(&t)?;
                    if size > max_size {
                        max_size = size;
                        max_size_type = Some(t);
                    }

                    if max_size_type.is_none() {
                        max_size = size;
                        max_size_type = Some(t.clone())
                    }

                    index_map.insert(name.clone(), index);
                },
            }

            index += 1;
        }

        Ok((list, index_map, max_size, max_size_type))
    }

    fn add_tag_type(tag_type: BasicTypeEnum,  typ: BasicTypeEnum, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        let mut vec = Vec::new();

        vec.push(tag_type);
        vec.push(typ);

        let struct_type = ctx.struct_type(&vec, false);
        BasicTypeEnum::StructType(struct_type)
    }

    pub fn enum_from_enum_standard(&self, fields: &Vec<Enumerator>, _type_variables: &Option<Vec<String>>, _env: &mut Env<'ctx>) -> Result<(Vec<(String, IntValue<'ctx>)>, HashMap<String, usize>), CodeGenError> {
        let mut enumerator_list: Vec<(String, IntValue<'ctx>)> = Vec::new();
        let mut index_map: HashMap<String, usize> = HashMap::new();
        let mut index: usize = 0;

        for field in fields {
            match field {
                Enumerator::Const { name, const_value } => {
                    let i32_type = self.context.i32_type();
                    let i32_value = i32_type.const_int(*const_value as u64, false);
                    enumerator_list.push((name.to_string(), i32_value));

                    index_map.insert(name.clone(), index);
                },
                Enumerator::TypeTuple { name: _, tuple_type: _ } => {
                    panic!("standard enum do not have TypeTuple");
                },
                Enumerator::TypeStruct { name: _, struct_type: _ } => {
                    panic!("standard enum do not have TypeStruct");
                },
            }

            index += 1;
        }

        Ok((enumerator_list, index_map))
    }

    pub fn gen_enum_literal(
        &self,
        literal: &EnumLiteral,
        tag: &u64,
        env: &Env<'ctx>,
        break_catcher: Option<&BreakCatcher>,
        continue_catcher: Option<&ContinueCatcher>,
        _pos: &Position
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {

        match literal {
            EnumLiteral::Symbol(_elem_name, _index) => {
                let tag_type = self.enum_tag_llvm_type;
                let vec: Vec<BasicTypeEnum> = vec!(tag_type.into());
                let tagged_type = self.context.struct_type(&vec, false);
                let tagged_ptr = self.builder.build_alloca(tagged_type, "enum_literal")?;
                let tag_ptr = self.builder.build_struct_gep(tagged_type, tagged_ptr, 0, "struct_gep_in_tagged_enum")?;

                let tag_value = tag_type.const_int(*tag as u64, false);
                let _ = self.builder.build_store(tag_ptr, tag_value);

                let basic_val = self.builder.build_load(tagged_type, tagged_ptr, &format!("load_enum_literal"))?;
                let any_val = basic_val.as_any_value_enum();
                let typ = &self.enum_only_tag_type;
                Ok(Some(CompiledValue::new(Rc::clone(typ), any_val)))
            },
            EnumLiteral::Struct(struct_literal) => {
                let typ = struct_literal.get_type();
                let pos = struct_literal.get_position();

                let tag_type = self.enum_tag_llvm_type;
                let raw_type = env.basic_type_enum_from_type(&typ, self.context, pos)?;
                let vec: Vec<BasicTypeEnum> = vec!(tag_type.into(), raw_type);
                let tagged_type = self.context.struct_type(&vec, false);
                let tagged_ptr = self.builder.build_alloca(tagged_type, "enum_literal")?;
                let tag_ptr = self.builder.build_struct_gep(tagged_type, tagged_ptr, 0, "struct_gep_in_tagged_enum")?;
                let struct_ptr = self.builder.build_struct_gep(tagged_type, tagged_ptr, 1, "struct_gep_in_tagged_enum")?;

                let tag_value = tag_type.const_int(*tag as u64, false);
                let _ = self.builder.build_store(tag_ptr, tag_value);

                let initialized_literal = self.gen_struct_literal(struct_literal, struct_ptr, env, break_catcher, continue_catcher)?;
                let _initialized_literal = initialized_literal.ok_or(CodeGenError::cannot_init_enum(literal.get_type().get_type_name(), pos.clone()))?;

                let basic_val = self.builder.build_load(tagged_type, tagged_ptr, &format!("load_enum_literal"))?;
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(Rc::clone(typ), any_val)))
            },
            EnumLiteral::Tuple(tuple_literal) => {
                let typ = tuple_literal.get_type();
                let pos = tuple_literal.get_position();

                let tag_type = self.enum_tag_llvm_type;
                let raw_type = env.basic_type_enum_from_type(&typ, self.context, pos)?;
                let vec: Vec<BasicTypeEnum> = vec!(tag_type.into(), raw_type);
                let tagged_type = self.context.struct_type(&vec, false);
                let tagged_ptr = self.builder.build_alloca(tagged_type, "enum_literal")?;
                let tag_ptr = self.builder.build_struct_gep(tagged_type, tagged_ptr, 0, "struct_gep_for_tag_in_tagged_enum")?;
                let tuple_ptr = self.builder.build_struct_gep(tagged_type, tagged_ptr, 1, "struct_gep_for_tuple_in_tagged_enum")?;
                let tag_value = tag_type.const_int(*tag as u64, false);
                let _ = self.builder.build_store(tag_ptr, tag_value);

                if tuple_literal.is_const() {
                    let const_list = tuple_literal.get_const_list();
                    let initialized_literal = self.gen_tuple_literal_from_const_expr_list(const_list, tuple_ptr, typ, env, break_catcher, continue_catcher, pos)?;
                    let _initialized_literal = initialized_literal.ok_or(CodeGenError::cannot_init_enum(literal.get_type().get_type_name(), pos.clone()))?;

                }else{
                    let expr_list = tuple_literal.get_expr_list();
                    let initialized_literal = self.gen_tuple_literal(expr_list, Some(tuple_ptr), env, break_catcher, continue_catcher, pos)?;
                    let _initialized_literal = initialized_literal.ok_or(CodeGenError::cannot_init_enum(literal.get_type().get_type_name(), pos.clone()))?;
                }

                let basic_val = self.builder.build_load(tagged_type, tagged_ptr, &format!("load_enum_literal"))?;
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(Rc::clone(typ), any_val)))
            },
        }
    }

    // pub fn gen_tuple_literal<'b, 'c>(&self,
    //     tuple_literal: &TupleLiteral,
    //     tuple_ptr: PointerValue<'ctx>,
    //     env: &Env<'ctx>,
    //     break_catcher: Option<&'b BreakCatcher>,
    //     continue_catcher: Option<&'c ContinueCatcher>
    // ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {

    //     match tuple_literal {
    //         TupleLiteral::ConstLiteral { typ, list, pos } => {
    //             let mut vec: Vec<BasicValueEnum> = Vec::new();
    //             let type_list = typ.get_tuple_type_list().unwrap();

    //             if type_list.len() != list.len() {
    //                 return Err(CodeGenError::tuple_length_mismatch(type_list.len(), list.len(), pos.clone()).into());
    //             }

    //             for const_expr in list {
    //                 let basic_value = self.const_expr_to_basic_value_enum(const_expr);
    //                 vec.push(basic_value);
    //             }

    //             let values = self.context.const_struct(&vec, false);
    //             let _result = self.builder.build_store(tuple_ptr, values.as_basic_value_enum());

    //             let tuple_type = Self::tuple_type_from_type_list(type_list, &self.context, pos)?;

    //             let basic_val = self.builder.build_load(tuple_type, tuple_ptr, "load_tuple_const_literal")?;
    //             let any_val = basic_val.as_any_value_enum();

    //             Ok(Some(CompiledValue::new(typ.clone(), any_val)))

    //         },
    //         TupleLiteral::NormalLiteral { typ, list, pos } => {




    //             unimplemented!()
    //         },
    //     }
    // }

    fn tuple_type_from_type_list(list: &Vec<Rc<Type>>, ctx: &'ctx Context, pos: &Position) -> Result<StructType<'ctx>, Box<dyn Error>> {
        let mut vec = Vec::new();

        for typ in list {
            let basic_type = TypeUtil::to_basic_type_enum(typ, ctx, pos)?;
            vec.push(basic_type);
        }

        let struct_type = ctx.struct_type(&vec, false);
        Ok(struct_type)
    }
}