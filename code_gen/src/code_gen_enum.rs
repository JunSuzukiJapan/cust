use crate::parser::{Type, EnumDefinition, Enumerator};
use super::{CodeGenError};
use super::Env;
use super::env::{BreakCatcher, ContinueCatcher};
use super::type_util::TypeUtil;
use crate::Position;
use crate::CodeGen;

use inkwell::context::Context;
use inkwell::values::{IntValue};
use inkwell::types::{AnyTypeEnum, BasicTypeEnum};
use inkwell::types::AnyType;
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





                    unimplemented!()
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

    pub fn enum_from_enum_standard(&self, fields: &Vec<Enumerator>, type_variables: &Option<Vec<String>>, _env: &mut Env<'ctx>) -> Result<(Vec<(String, IntValue<'ctx>)>, HashMap<String, usize>), CodeGenError> {
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

}