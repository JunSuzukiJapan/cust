#![allow(dead_code)]

use std::collections::HashMap;
use std::error::Error;
use std::rc::Rc;
use std::thread::panicking;
use inkwell::values::{PointerValue, FunctionValue, GlobalValue, AnyValueEnum, IntValue, BasicValueEnum, BasicValue};
use inkwell::types::{StructType, AnyTypeEnum, AnyType, BasicTypeEnum, IntType, BasicType};
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use parser::FunProto;
use tokenizer::Position;
use crate::global::global;
use crate::parser::{Type, ConstExpr, NumberType, ExprAST, CustFunctionType, SpecifierQualifier};
use crate::{CodeGenError};
use super::type_util::TypeUtil;

use super::CodeGen;

#[derive(Debug)]
pub enum SomeValue<'ctx> {
    Global(&'ctx GlobalValue<'ctx>),
    Local(&'ctx PointerValue<'ctx>),
}

impl<'ctx> SomeValue<'ctx> {
    pub fn as_local_pointer(&self, pos: &Position) -> Result<&PointerValue<'ctx>, CodeGenError> {
        match self {
            SomeValue::Local(ptr) => Ok(*ptr),
            _ => Err(CodeGenError::cannot_convert_to_local_pointer(pos.clone())),
        }
    }

    pub fn as_global(&self, pos: &Position) -> Result<&GlobalValue<'ctx>, CodeGenError> {
        match self {
            SomeValue::Global(val) => Ok(*val),
            _ => Err(CodeGenError::cannot_convert_to_global_value(pos.clone())),
        }
    }

}

#[derive(Debug, Clone)]
pub struct BreakCatcher<'a> (&'a BasicBlock<'a>);

impl<'a> BreakCatcher<'a> {
    pub fn new(block: &'a BasicBlock) -> BreakCatcher<'a> {
        BreakCatcher(block)
    }

    pub fn get_block(&self) -> &'a BasicBlock<'a> {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct ContinueCatcher<'a> (&'a BasicBlock<'a>);


impl<'a> ContinueCatcher<'a> {
    pub fn new(block: &'a BasicBlock) -> ContinueCatcher<'a> {
        ContinueCatcher(block)
    }

    pub fn get_block(&self) -> &'a BasicBlock<'a> {
        self.0
    }
}

#[derive(Debug, Clone)]
pub enum CompiledCase<'a> {
    Case {
        cond: &'a ConstExpr,
        block: BasicBlock<'a>,
        code: Option<AnyValueEnum<'a>>,
        insert_block: Option<BasicBlock<'a>>,
        pos: Position
    },
    Default {
        block: BasicBlock<'a>,
        code: Option<AnyValueEnum<'a>>,
        insert_block: Option<BasicBlock<'a>>,
        pos: Position
    }
}

impl<'a> CompiledCase<'a> {
    pub fn new(cond: &'a ConstExpr, block: BasicBlock<'a>, code: Option<AnyValueEnum<'a>>, insert_block: Option<BasicBlock<'a>>, pos: Position) -> CompiledCase<'a> {
        CompiledCase::Case {
            cond,
            block,
            code,
            insert_block,
            pos,
        }
    }

    pub fn new_default(block: BasicBlock<'a>, code: Option<AnyValueEnum<'a>>, insert_block: Option<BasicBlock<'a>>, pos: Position) -> CompiledCase<'a> {
        CompiledCase::Default {
            block,
            code,
            insert_block,
            pos,
        }
    }

    pub fn is_case(&self) -> bool {
        match self {
            CompiledCase::Case { .. } => true,
            _ => false,
        }
    }

    pub fn is_default(&self) -> bool {
        match self {
            CompiledCase::Default { .. } => true,
            _ => false,
        }
    }

    pub fn get_block(&self) -> &BasicBlock<'a> {
        match self {
            CompiledCase::Case { block, .. } => block,
            CompiledCase::Default { block, .. } => block,
        }
    }

    pub fn get_code(&self) -> &Option<AnyValueEnum<'a>> {
        match self {
            CompiledCase::Case { code, .. } => code,
            CompiledCase::Default { code, .. } => code,
        }
    }

    pub fn get_cond(&self) -> Option<&'a ConstExpr> {
        match self {
            CompiledCase::Case { cond, .. } => Some(cond),
            CompiledCase::Default { .. } => None,
        }
    }

    pub fn get_insert_block(&self) -> &Option<BasicBlock<'a>> {
        match self {
            CompiledCase::Case { insert_block, .. } => insert_block,
            CompiledCase::Default { insert_block, .. } => insert_block,
        }
    }

    pub fn get_position(&self) -> &Position {
        match self {
            Self::Case { pos, .. } => pos,
            Self::Default { pos, .. } => pos,
        }
    }
}

#[derive(Debug, Clone)]
pub struct  NormalTaggedEnum<'ctx> {
    name: String,
    type_list: Vec<(Rc<Type>, BasicTypeEnum<'ctx>)>,
    index_map: HashMap<String, usize>,
    max_size: u64,
    max_size_type: Option<BasicTypeEnum<'ctx>>,
}

impl <'ctx> NormalTaggedEnum<'ctx> {
    pub fn new(name: String, type_list: Vec<(Rc<Type>, BasicTypeEnum<'ctx>)>, index_map: HashMap<String, usize>, max_size: u64, max_size_type: Option<BasicTypeEnum<'ctx>>) -> Self {
        NormalTaggedEnum { name, type_list, index_map, max_size, max_size_type }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_type_list(&self) -> &Vec<(Rc<Type>, BasicTypeEnum<'ctx>)> {
        &self.type_list
    }

    pub fn get_index_map(&self) -> &HashMap<String, usize> {
        &self.index_map
    }

    pub fn get_max_size(&self) -> u64 {
        self.max_size
    }

    pub fn get_max_size_type(&self) -> Option<BasicTypeEnum<'ctx>> {
        self.max_size_type.clone()
    }
}

#[derive(Debug, Clone)]
pub struct TypeVarTaggedEnum {
    name: String,
    type_list: Vec<Rc<Type>>,
    index_map: HashMap<String, usize>,
    type_variables: Vec<String>,
}

impl TypeVarTaggedEnum {
    pub fn new(name: String, type_list: Vec<Rc<Type>>, index_map: HashMap<String, usize>, type_variables: Vec<String>) -> Self {
        TypeVarTaggedEnum { name, type_list, index_map, type_variables }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_type_list(&self) -> &Vec<Rc<Type>> {
        &self.type_list
    }

    pub fn get_index_map(&self) -> &HashMap<String, usize> {
        &self.index_map
    }

    pub fn get_type_variables(&self) -> &Vec<String> {
        &self.type_variables
    }
}

#[derive(Debug, Clone)]
pub struct BoundTaggedEnum<'ctx> {
    type_var_enum: Rc<TypeVarTaggedEnum>,
    type_list: Vec<(Rc<Type>, BasicTypeEnum<'ctx>)>,
    index_map: HashMap<String, usize>,
    max_size: u64,
    max_size_type: Option<BasicTypeEnum<'ctx>>,
    bound_map: HashMap<String, (Rc<Type>, BasicTypeEnum<'ctx>)>,
}

impl<'ctx> BoundTaggedEnum<'ctx> {
    pub fn try_new(
        type_var_enum: Rc<TypeVarTaggedEnum>,
        type_list: Vec<(Rc<Type>, BasicTypeEnum<'ctx>)>,
        index_map: HashMap<String, usize>,
        bound_map: HashMap<String, (Rc<Type>, BasicTypeEnum<'ctx>)>,
        env: &mut Env<'ctx>,
        pos: &Position
    ) -> Result<Self, Box<dyn Error>> {

        env.add_new_local_types();

        let mut map = HashMap::new();
        for (name, typ) in bound_map.iter() {
            map.insert(name.clone(), Rc::clone(&typ.0));
        }
        env.set_type_variables(type_var_enum.get_type_variables(), &map, pos)?;

        let mut max_size = 0;
        let mut max_size_type = None;
        for (_ty, basic_ty) in &type_list {
            let size = CodeGen::size_of(basic_ty)?;
            if size > max_size {
                max_size = size;
                max_size_type = Some(basic_ty.clone());
            }
        }

        env.remove_local_types();

        let tagged = BoundTaggedEnum { type_var_enum, type_list, index_map, max_size, max_size_type, bound_map };
        Ok(tagged)
    }

    pub fn get_type_var_enum(&self) -> &TypeVarTaggedEnum {
        &self.type_var_enum
    }

    pub fn get_type_list(&self) -> &Vec<(Rc<Type>, BasicTypeEnum<'ctx>)> {
        &self.type_list
    }

    pub fn get_index_map(&self) -> &HashMap<String, usize> {
        &self.index_map
    }

    pub fn get_max_size(&self) -> u64 {
        self.max_size
    }

    pub fn get_max_size_type(&self) -> Option<BasicTypeEnum<'ctx>> {
        self.max_size_type.clone()
    }

    pub fn get_bound_map(&self) -> &HashMap<String, (Rc<Type>, BasicTypeEnum<'ctx>)> {
        &self.bound_map
    }
}

#[derive(Debug, Clone)]
pub enum TaggedEnum<'ctx> {
    Normal(NormalTaggedEnum<'ctx>),
    TypeVar(TypeVarTaggedEnum),
    Bound(BoundTaggedEnum<'ctx>),
}

impl<'ctx> TaggedEnum<'ctx> {
    pub fn new_normal_tagged_enum(
        name: String,
        type_list: Vec<(Rc<Type>, BasicTypeEnum<'ctx>)>,
        index_map: HashMap<String, usize>,
        max_size: u64,
        max_size_type: Option<BasicTypeEnum<'ctx>>,
    ) -> Self {
        let tagged = NormalTaggedEnum::new(name, type_list, index_map, max_size, max_size_type);
        TaggedEnum::Normal(tagged)
    }

    pub fn new_type_var_tagged_enum(
        name: String,
        type_list: Vec<Rc<Type>>,
        index_map: HashMap<String, usize>,
        type_variables: Vec<String>,
    ) -> Self {
        let type_var_enum = TypeVarTaggedEnum::new(name, type_list, index_map, type_variables);
        TaggedEnum::TypeVar(type_var_enum)
    }

    pub fn new_bound_tagged_enum(
        type_var_enum: Rc<TypeVarTaggedEnum>,
        type_list: Vec<(Rc<Type>, BasicTypeEnum<'ctx>)>,
        index_map: HashMap<String, usize>,
        bound_map: HashMap<String, (Rc<Type>, BasicTypeEnum<'ctx>)>,
        env: &mut Env<'ctx>,
        pos: &Position
    ) ->Result<Self, Box<dyn Error>> {
        let bound_enum = BoundTaggedEnum::try_new(type_var_enum, type_list, index_map, bound_map, env, pos)?;
        Ok(TaggedEnum::Bound(bound_enum))
    }

    pub fn get_index_map(&self) -> &HashMap<String, usize> {
        match self {
            TaggedEnum::Normal(tagged_enum) => &tagged_enum.index_map,
            TaggedEnum::TypeVar(type_var_enum) => &type_var_enum.index_map,
            TaggedEnum::Bound(bound_enum) => &bound_enum.index_map,
        }
    }

    pub fn get_pair_type_list(&self) -> &Vec<(Rc<Type>, BasicTypeEnum<'ctx>)> {
        match self {
            TaggedEnum::Normal(tagged_enum) => &tagged_enum.type_list,
            TaggedEnum::Bound(bound_enum) => &bound_enum.type_list,
            TaggedEnum::TypeVar(_type_var_enum) => {
                panic!("TypeVar does not have type list, use get_pair_type instead");
            },
        }
    }

    pub fn get_type_list(&self) -> &Vec<Rc<Type>> {
        match self {
            TaggedEnum::TypeVar(type_var_enum) => &type_var_enum.type_list,
            _ => {
                panic!("Normal or Bound TaggedEnum does not have type list, use get_pair_type_list instead");
            }
        }
    }

    pub fn get_max_size(&self) -> Option<u64> {
        match self {
            TaggedEnum::Normal(tagged_enum) => Some(tagged_enum.max_size),
            TaggedEnum::TypeVar(_) => None, // TypeVar does not have max size
            TaggedEnum::Bound(bound_enum) => Some(bound_enum.max_size),
        }
    }

    pub fn get_max_size_type(&self) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            TaggedEnum::Normal(tagged_enum) => tagged_enum.max_size_type.clone(),
            TaggedEnum::TypeVar(_tagged) => None, // TypeVar does not have max size type
            TaggedEnum::Bound(bound_enum) => bound_enum.get_max_size_type(),
        }
    }

    pub fn get_name(&self) -> &str {
        match self {
            TaggedEnum::Normal(tagged_enum) => tagged_enum.get_name(),
            TaggedEnum::TypeVar(type_var_enum) => type_var_enum.get_name(),
            TaggedEnum::Bound(bound_enum) => bound_enum.get_type_var_enum().get_name(),
        }
    }

    pub fn get_type_varialbes(&self) -> Option<&Vec<String>> {
        match self {
            TaggedEnum::TypeVar(type_var_enum) => Some(type_var_enum.get_type_variables()),
            TaggedEnum::Bound(bound_enum) => Some(bound_enum.get_type_var_enum().get_type_variables()),
            TaggedEnum::Normal(_) => None, // Normal does not have type variables
        }
    }

    pub fn as_any_type_enum(&self) -> Option<AnyTypeEnum<'ctx>> {
        if let Some(t) = self.get_max_size_type() {
            Some(t.as_any_type_enum())
        }else{
            panic!("no type in enum");
        }
    }
}

#[derive(Debug, Clone)]
pub enum GenType<'ctx> {
    Type(AnyTypeEnum<'ctx>),
    Union{
        type_list: Vec<(Rc<Type>, BasicTypeEnum<'ctx>)>,
        index_map: HashMap<String, usize>,
        max_size: u64,
        max_size_type: Option<BasicTypeEnum<'ctx>>,
    },
    StandardEnum {
        i32_type: IntType<'ctx>,
        enumerator_list: Vec<(String, IntValue<'ctx>)>,
        index_map: HashMap<String,usize>,
    },
    TaggedEnum(TaggedEnum<'ctx>),
    TypeDefStruct(String, Box<GenType<'ctx>>),
    TypeDefUnion(String, Box<GenType<'ctx>>),
}

impl<'ctx> GenType<'ctx> {
    pub fn is_struct_type(&self) -> bool {
        match self {
            GenType::Type(t) => t.is_struct_type(),
            GenType::TypeDefStruct(_, _) => true,
            _ => false,
        }
    }

    pub fn is_union_type(&self) -> bool{
        match self {
            GenType::Union {..} => true,
            GenType::TypeDefUnion(_, _) => true,
            _ => false,
        }
    }

    #[allow(unused)]
    pub fn is_enum_type(&self) -> bool {
        match self {
            GenType::StandardEnum {..} => true,
            GenType::TaggedEnum(_) => true,
            _ => false,
        }
    }

    #[allow(unused)]
    pub fn is_tagged_enum_type(&self) -> bool {
        match self {
            GenType::TaggedEnum(gen_enum) => true,
            _ => false,
        }
    }

    pub fn get_index_map(&self) -> Option<&HashMap<String, usize>> {
        match self {
            GenType::Union { index_map, .. } => Some(index_map),
            GenType::StandardEnum { index_map, .. } => Some(index_map),
            GenType::TaggedEnum(gen_enum) => {
                Some(gen_enum.get_index_map())
            },
            GenType::TypeDefStruct(_, gen_type) => {
                gen_type.get_index_map()
            },
            GenType::TypeDefUnion(_, gen_type) => {
                gen_type.get_index_map()
            },
            _ => None,
        }
    }

    pub fn get_type_list(&self) -> Option<&Vec<(Rc<Type>, BasicTypeEnum<'ctx>)>> {
        match self {
            GenType::Union { type_list, .. } => Some(type_list),
            GenType::TaggedEnum(gen_enum) => {
                Some(gen_enum.get_pair_type_list())
            },
            GenType::TypeDefStruct(_, gen_type) => {
                gen_type.get_type_list()
            },
            GenType::TypeDefUnion(_, gen_type) => {
                gen_type.get_type_list()
            },
            _ => None,
        }
    }

    pub fn as_any_type_enum(&self) -> AnyTypeEnum<'ctx> {
        match self {
            GenType::Type(t) => t.as_any_type_enum(),
            GenType::Union {max_size_type, ..} => {
                if let Some(t) = max_size_type {
                    t.as_any_type_enum()
                }else{
                    panic!("no type in union");
                }
            },
            GenType::StandardEnum { i32_type, .. } => i32_type.as_any_type_enum(),
            GenType::TaggedEnum(gen_enum) => {
                if let Some(t) = gen_enum.get_max_size_type() {
                    t.as_any_type_enum()
                }else{
                    panic!("no type in tagged enum");
                }
            },
            GenType::TypeDefStruct(_struct_name, gen_type) => {
                gen_type.as_any_type_enum()
            },
            GenType::TypeDefUnion(_name, gen_type) => {
                gen_type.as_any_type_enum()
            },
        }
    }
}

#[derive(Debug, Clone)]
enum ConstOrGlobalValue<'ctx> {
    Const{value: BasicValueEnum<'ctx>},
    GlobalValue{global: GlobalValue<'ctx>},
}

#[derive(Debug, Clone)]
pub struct Interface {
    name: String,
    function_prototypes: HashMap<String, FunProto>,
}

#[derive(Debug, Clone)]
pub struct Class<'ctx> {
    name: String,
    vars:HashMap<String, (Rc<Type>, SpecifierQualifier, GlobalValue<'ctx>)>,
    class_functions: HashMap<String, (CustFunctionType, FunctionValue<'ctx>)>,
    functions: HashMap<String, (CustFunctionType, FunctionValue<'ctx>)>,
    interfaces: HashMap<String, (Interface,                                   // interface
                                 HashMap<String, &'ctx (CustFunctionType, FunctionValue<'ctx>)>)      // interfaceに対する実際の関数実装
                       >,
}

impl<'ctx> Class<'ctx> {
    pub fn new(name: &str) -> Self {
        Class {
            name: name.to_string(),
            vars: HashMap::new(),
            class_functions: HashMap::new(),
            functions: HashMap::new(),
            interfaces: HashMap::new(),
        }
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn add_class_function(&mut self, func_name: &str, typ: CustFunctionType, function: FunctionValue<'ctx>, pos: &Position) -> Result<(), CodeGenError> {
        if self.class_functions.contains_key(func_name) {
            return  Err(CodeGenError::already_class_function_defined(self.name.clone(), func_name.to_string(), pos.clone()));
        }

        self.class_functions.insert(func_name.to_string(), (typ, function));

        Ok(())
    }

    pub fn add_member_function(&mut self, func_name: &str, typ: CustFunctionType, function: FunctionValue<'ctx>, pos: &Position) -> Result<(), CodeGenError> {
        if self.functions.contains_key(func_name) {
            return Err(CodeGenError::already_member_function_defined(self.name.clone(), func_name.to_string(), pos.clone()));
        }

        self.functions.insert(func_name.to_string(), (typ, function));

        Ok(())
    }

    #[inline]
    pub fn get_class_function(&self, func_name: &str) -> Option<&(CustFunctionType, FunctionValue<'ctx>)> {
        self.class_functions.get(func_name)
    }

    #[inline]
    pub fn get_member_function(&self, func_name: &str) -> Option<&(CustFunctionType, FunctionValue<'ctx>)> {
        self.functions.get(func_name)
    }

    pub fn add_class_var(&mut self, var_name: &str, typ: Rc<Type>, sq: SpecifierQualifier, ptr: GlobalValue<'ctx>, pos: &Position) -> Result<(), CodeGenError> {
        if self.vars.contains_key(var_name) {
            return Err(CodeGenError::already_class_var_defined(self.name.clone(), var_name.to_string(), pos.clone()));
        }

        if sq.is_volatile() {
            unimplemented!("volatile class variable is not supported yet");
        }
        if sq.is_const() {
            ptr.set_constant(true);
        }

        self.vars.insert(var_name.to_string(), (typ, sq, ptr));

        Ok(())
    }

    #[inline]
    pub fn get_class_var(&self, var_name: &str) -> Option<&(Rc<Type>, SpecifierQualifier, GlobalValue<'ctx>)> {
        self.vars.get(var_name)
    }
}

#[derive(Debug, Clone)]
pub struct Env<'ctx> {
    global_def: HashMap<String, (Rc<Type>, SpecifierQualifier, ConstOrGlobalValue<'ctx>)>,
    global_functions: HashMap<String, (CustFunctionType, FunctionValue<'ctx>)>,

    classes: HashMap<String, Class<'ctx>>,

    locals: Vec<Vec<HashMap<String, (Rc<Type>, SpecifierQualifier, PointerValue<'ctx>)>>>,
    local_functions: Vec<Vec<HashMap<String, (CustFunctionType, FunctionValue<'ctx>)>>>,
    local_labels: Vec<Vec<HashMap<String, BasicBlock<'ctx>>>>,
    local_cases: Vec<Vec<Vec<CompiledCase<'ctx>>>>,

    current_function: Option<(CustFunctionType, FunctionValue<'ctx>)>,
    types: HashMap<String, (GenType<'ctx>, Option<HashMap<String, usize>>)>,

    current_class: Option<*const Class<'ctx>>,

    inner_fun_string_match: Option<FunctionValue<'ctx>>,

    // only for calc type
    local_type_map_list: Vec<HashMap<String, Rc<Type>>>,
}

impl<'ctx> Env<'ctx> {
    pub fn new() -> Env<'ctx> {
        Env {
            global_def: HashMap::new(),
            global_functions: HashMap::new(),

            classes: HashMap::new(),

            locals: Vec::new(),
            local_functions: Vec::new(),
            local_labels: Vec::new(),
            local_cases: Vec::new(),

            current_function: None,
            types: HashMap::new(),

            current_class: None,

            inner_fun_string_match: None,
            local_type_map_list: Vec::new(),
        }
    }

    pub fn add_new_local_types(&mut self) {
        self.local_type_map_list.push(HashMap::new());
    }

    pub fn remove_local_types(&mut self) {
        self.local_type_map_list.pop();
    }

    pub fn insert_local_type(&mut self, key: &str, typ: Rc<Type>) {
        self.local_type_map_list.last_mut().unwrap().insert(key.to_string(), typ);
    }

    pub fn add_new_function_local(&mut self) {
        self.locals.push(vec![HashMap::new()]);
        self.local_functions.push(vec![HashMap::new()]);
        self.local_labels.push(vec![HashMap::new()]);
        self.local_cases.push(vec![Vec::new()]);
    }

    pub fn remove_function_local(&mut self) {
        self.locals.pop();
        self.local_functions.pop();
        self.local_labels.pop();
        self.local_cases.pop();
    }

    pub fn add_new_local(&mut self) {
        self.locals.last_mut().unwrap().push(HashMap::new());
        self.local_functions.last_mut().unwrap().push(HashMap::new());
        self.local_labels.last_mut().unwrap().push(HashMap::new());
    }

    pub fn remove_local(&mut self) {
        self.locals.last_mut().unwrap().pop();
        self.local_functions.last_mut().unwrap().pop();
        self.local_labels.last_mut().unwrap().pop();
    }

    pub fn add_new_switch_case(&mut self) {
        self.local_cases.last_mut().unwrap().push(Vec::new());
    }

    pub fn remove_switch_case(&mut self) {
        self.local_cases.last_mut().unwrap().pop();
    }

    pub fn get_case_list(&self) -> &Vec<CompiledCase> {
        self.local_cases.last().unwrap().last().unwrap()
    }

    pub fn insert_case(&mut self, cond: &'ctx ConstExpr, block: BasicBlock<'ctx>, code: Option<AnyValueEnum<'ctx>>, insert_block: Option<BasicBlock<'ctx>>, pos: Position) {
        self.local_cases.last_mut().unwrap().last_mut().unwrap().push(CompiledCase::new(cond, block, code, insert_block, pos));
    }

    pub fn insert_default(&mut self, block: BasicBlock<'ctx>, code: Option<AnyValueEnum<'ctx>>, insert_block: Option<BasicBlock<'ctx>>, pos: Position) {
        self.local_cases.last_mut().unwrap().last_mut().unwrap().push(CompiledCase::new_default(block, code, insert_block, pos));
    }

    pub fn get_inner_fun_string_match(&self) -> Option<FunctionValue<'ctx>> {
        self.inner_fun_string_match
    }

    pub fn set_inner_fun_string_match(&mut self, function: FunctionValue<'ctx>) {
        self.inner_fun_string_match = Some(function);
    }

    pub fn insert_global_var(&mut self, key: &str, typ: Rc<Type>, sq: SpecifierQualifier, ptr: GlobalValue<'ctx>) {
        if sq.is_volatile() {
            unimplemented!("volatile global variable is not supported yet");
        }
        if sq.is_const() {
            ptr.set_constant(true);
            self.global_def.insert(key.to_string(), (typ, sq, ConstOrGlobalValue::GlobalValue { global: ptr }));
        }else{
            self.global_def.insert(key.to_string(), (typ, sq, ConstOrGlobalValue::GlobalValue { global: ptr }));
        }
    }

    fn get_real_class_name(&self, class_name: &str) -> String {
        if let Some((gen_type, _)) = self.types.get(class_name) {
            match gen_type {
                GenType::TypeDefStruct(struct_name, _raw_ptr) => {
                    struct_name.to_string()
                },
                GenType::TypeDefUnion(name, _raw_ptr) => {
                    name.to_string()
                },
                _ => {
                    class_name.to_string()
                },
            }
        }else{
            class_name.to_string()
        }
    }

    pub fn insert_class_var(&mut self, class_name: &str, var_name: &str, typ: Rc<Type>, sq: SpecifierQualifier, ptr: GlobalValue<'ctx>, pos: &Position) -> Result<(), CodeGenError> {
        let class_name = &self.get_real_class_name(class_name);

        if ! self.classes.contains_key(class_name) {
            self.classes.insert(class_name.to_string(), Class::new(class_name));
        }

        self.classes.get_mut(class_name).unwrap().add_class_var(var_name, typ, sq, ptr, pos)?;

        Ok(())
    }

    pub fn get_class_var(&self, class_name: &str, var_name: &str) -> Option<&(Rc<Type>, SpecifierQualifier, GlobalValue<'ctx>)> {
        let class_name = &self.get_real_class_name(class_name);

        if ! self.classes.contains_key(class_name) {
            return None;
        }

        self.classes.get(class_name).unwrap().get_class_var(var_name)
    }

    pub fn insert_global_enumerator(&mut self, key: &str, typ: Rc<Type>, value: IntValue<'ctx>) {
        let sq = SpecifierQualifier::default_const();
        self.global_def.insert(key.to_string(), (typ, sq, ConstOrGlobalValue::Const { value: value.as_basic_value_enum() }));
    }

    pub fn insert_local(&mut self, key: &str, typ: Rc<Type>, sq: SpecifierQualifier, ptr: PointerValue<'ctx>) {
        if sq.is_volatile() {
            unimplemented!("volatile local variable is not supported yet");
        }
        self.locals.last_mut().unwrap().last_mut().unwrap().insert(key.to_string(), (typ, sq, ptr));
    }

    pub fn insert_label(&mut self, key: &str, block: BasicBlock<'ctx>) -> Result<(), CodeGenError> {
        self.local_labels.last_mut().unwrap().last_mut().unwrap().insert(key.to_string(), block);
        Ok(())
    }

    pub fn insert_function(&mut self, key: &str, typ: CustFunctionType, function: FunctionValue<'ctx>) {
        self.global_functions.insert(key.to_string(), (typ, function));
    }

    pub fn insert_class_function(&mut self, class_name: &str, func_name: &str, typ: CustFunctionType, function: FunctionValue<'ctx>, pos: &Position) -> Result<(), CodeGenError> {
        if ! self.classes.contains_key(class_name) {
            self.classes.insert(class_name.to_string(), Class::new(class_name));
        }

        self.classes.get_mut(class_name).unwrap().add_class_function(func_name, typ, function, pos)?;

        Ok(())
    }

    pub fn insert_member_function(&mut self, class_name: &str, func_name: &str, typ: CustFunctionType, function: FunctionValue<'ctx>, pos: &Position) -> Result<(), CodeGenError> {
        if ! self.classes.contains_key(class_name) {
            self.classes.insert(class_name.to_string(), Class::new(class_name));
        }

        self.classes.get_mut(class_name).unwrap().add_member_function(func_name, typ, function, pos)?;

        Ok(())
    }

    pub fn get_function(&self, key: &str) -> Option<&(CustFunctionType, FunctionValue<'ctx>)> {
        self.global_functions.get(key)
    }

    pub fn get_class_function(&self, class_name: &str, func_name: &str) -> Option<&(CustFunctionType, FunctionValue<'ctx>)> {
        if ! self.classes.contains_key(class_name) {
            return  None;
        }

        self.classes.get(class_name).unwrap().get_class_function(func_name)
    }

    pub fn get_member_function(&self, class_name: &str, func_name: &str) -> Option<&(CustFunctionType, FunctionValue<'ctx>)> {
        if ! self.classes.contains_key(class_name) {
            return  None;
        }

        self.classes.get(class_name).unwrap().get_member_function(func_name)
    }

    pub fn insert_struct(&mut self, key: &str, struct_type: &StructType<'ctx>, index_map: HashMap<String, usize>, pos: &Position) -> Result<(), CodeGenError> {
        if let Some(_any_type_enum) = self.types.get(key) {
            return Err(CodeGenError::already_type_defined_in_struct(key, pos.clone()));
        }

        self.types.insert(key.to_string(), (GenType::Type(struct_type.as_any_type_enum()), Some(index_map)));

        Ok(())
    }

    pub fn insert_union(
        &mut self,
        key: &str,
        type_list: Vec<(Rc<Type>, BasicTypeEnum<'ctx>)>,
        index_map: HashMap<String, usize>,
        max_size: u64, max_size_type: Option<BasicTypeEnum<'ctx>>,
        pos: &Position
      ) -> Result<(), CodeGenError>
    {
        if let Some(_any_type_enum) = self.types.get(key) {
            return Err(CodeGenError::already_type_defined_in_union(key, pos.clone()));
        }

        let gen_type = GenType::Union { type_list, index_map: index_map.clone(), max_size, max_size_type };
        self.types.insert(key.to_string(), (gen_type, Some(index_map)));

        Ok(())
    }

    pub fn insert_typedef(&mut self, key: &str, typ: &Type, ctx: &'ctx Context, pos: &Position) -> Result<(), Box<dyn Error>> {
        if let Some(_any_type_enum) = self.types.get(key) {
            return Err(Box::new(CodeGenError::already_type_defined_in_typedef(typ, key, pos.clone())));
        }

        match typ {
            Type::Symbol(name) => {
                let t = self.get_gen_type(name, ctx, pos)?.ok_or(CodeGenError::no_such_a_type(name, pos.clone()))?.clone();
                self.types.insert(key.to_string(), (t.clone(), None));
            },
            Type::Struct(st_ty) => {
                let name = st_ty.get_name();
                let definition = st_ty.get_struct_definition();
                let type_variables = st_ty.get_type_variables();

                if let Some(struct_name) = name {
                    if let Some(t) = &self.get_gen_type(struct_name, ctx, pos)? {
                        if ! t.is_struct_type() {
                            return  Err(Box::new(CodeGenError::already_type_defined_in_typedef(typ, struct_name, pos.clone())));
                        }

                        self.types.insert(key.to_string(), (t.clone(), None));
                        return Ok(());
                    }
                }

                let (struct_type, index_map) = CodeGen::struct_from_struct_definition(name, definition, type_variables, ctx, self, pos)?;
                if let Some(struct_name) = name {
                    self.insert_struct(struct_name, &struct_type, index_map, pos)?;
                    let gen_type = self.get_gen_type(&struct_name, ctx, pos)?.unwrap();
                    let t = GenType::TypeDefStruct(struct_name.to_string(), Box::new(gen_type));
                    self.types.insert(key.to_string(), (t, None));
                }else{
                    self.insert_struct(key, &struct_type, index_map, pos)?;
                };
            },
            Type::Union { name, fields, type_variables } => {
                if let Some(id) = name {
                    if let Some(t) = self.get_gen_type(id, ctx, pos)? {
                        if ! t.is_union_type() {
                            return  Err(Box::new(CodeGenError::already_type_defined_in_typedef(typ, id, pos.clone())));
                        }

                        self.types.insert(key.to_string(), (t.clone(), None));
                        return  Ok(());
                    }
                }

                let (type_list, index_map, max_size, max_size_type) = CodeGen::union_from_struct_definition(name, fields, type_variables, ctx, self, pos)?;
                if let Some(union_name) = name {
                    self.insert_union(&union_name, type_list, index_map, max_size, max_size_type, pos)?;

                    // let raw_ptr = &self.get_gen_type(&union_name, ctx, pos)?.unwrap() as *const GenType;
                    let gen_type = self.get_gen_type(&union_name, ctx, pos)?.unwrap();
                    let t = GenType::TypeDefUnion(union_name.to_string(), Box::new(gen_type));
                    self.types.insert(key.to_string(), (t.clone(), None));

                }else{
                    self.insert_union(key, type_list, index_map, max_size, max_size_type, pos)?;
                }

            },
            _ => {
                let gen_type = GenType::Type(TypeUtil::to_llvm_any_type(typ, ctx, self, pos)?);
                self.types.insert(key.to_string(), (gen_type, None));
            },
        };
        // self.types.insert(key.to_string(), (gen_type, None));

        Ok(())
    }

    pub fn insert_const_enum(&mut self, key: &str, enum_type: &IntType<'ctx>, enumerator_list: Vec<(String, IntValue<'ctx>)>, index_map: HashMap<String, usize>, pos: &Position) -> Result<(), CodeGenError> {
        if let Some(_any_type_enum) = self.types.get(key) {
            return Err(CodeGenError::already_type_defined_in_struct(key, pos.clone()));
        }

        for (id, val) in &enumerator_list {
            self.insert_global_enumerator(id, Rc::new(Type::Number(NumberType::Int)), *val);
        }

        let gen_type = GenType::StandardEnum {
            i32_type: enum_type.clone(),
            enumerator_list,
            index_map,
        };
        self.types.insert(key.to_string(), (gen_type, None));

        Ok(())
    }

    pub fn insert_tagged_enum(
        &mut self,
        key: &str,
        tagged_enum: TaggedEnum<'ctx>,
        pos: &Position
    ) -> Result<(), CodeGenError> {
        if let Some(_any_type_enum) = self.types.get(key) {
            return Err(CodeGenError::already_type_defined_in_enum(key, pos.clone()));
        }

        let index_map = tagged_enum.get_index_map().clone();
        let gen_type = GenType::TaggedEnum(tagged_enum);
        self.types.insert(key.to_string(), (gen_type, Some(index_map)));
        Ok(())
    }

    pub fn basic_type_enum_from_type(&mut self, typ: &Type, ctx: &'ctx Context, pos: &Position) -> Result<BasicTypeEnum<'ctx>, Box<dyn Error>> {
        match typ {
            Type::Struct(st_ty) => {
                let name = st_ty.get_name();
                let definition = st_ty.get_struct_definition();
                let type_variables = st_ty.get_type_variables();

                if let Some(id) = name {
                    if let Some(gen_type) = self.get_gen_type(id, ctx, pos)? {
                        let mut gen_type = &gen_type;
                        loop {
                            match gen_type {
                                GenType::Type(t) => {
                                    if let Ok(basic_type) = BasicTypeEnum::try_from(*t) {
                                        return Ok(basic_type);
                                    }else{
                                        return Err(Box::new(CodeGenError::mismatch_type_struct_fields(Some(id), pos.clone())));
                                    }
                                },
                                GenType::Union { max_size_type, .. } => {
                                    let t = max_size_type.ok_or(CodeGenError::union_has_no_field(None, pos.clone()))?;
                                    return Ok(t);
                                },
                                GenType::StandardEnum { i32_type, .. } => {
                                    return Ok(i32_type.as_basic_type_enum());
                                },
                                GenType::TaggedEnum(gen_enum) => {
                                    if let Some(t) = gen_enum.get_max_size_type() {
                                        return Ok(t);
                                    }else{
                                        return Err(Box::new(CodeGenError::enum_has_no_field(gen_enum.get_name().to_string(), pos.clone())));
                                    }
                                },
                                GenType::TypeDefStruct(_name, gen_t) => {
                                    gen_type = gen_t;
                                },
                                GenType::TypeDefUnion(_name, gen_t) => {
                                    gen_type = gen_t
                                },
                            }
                        }
                    }else{
                        let (any_type, _index_map) = CodeGen::struct_from_struct_definition(&None, definition, type_variables, ctx, self, pos)?;
                        let basic_type = BasicTypeEnum::try_from(any_type).unwrap();
                        Ok(basic_type)
                    }
                }else{
                    let (any_type, _index_map) = CodeGen::struct_from_struct_definition(&None, definition, type_variables, ctx, self, pos)?;
                    let basic_type = BasicTypeEnum::try_from(any_type).unwrap();
                    Ok(basic_type)
                }
            },
            Type::Union { name, fields, type_variables } => {
                if let Some(id) = name {
                    if let Some(gen_type) = self.get_gen_type(id, ctx, pos)? {
                        let mut gen_type = &gen_type;
                        loop {
                            match gen_type {
                                GenType::Type(t) => {
                                    if let Ok(basic_type) = BasicTypeEnum::try_from(*t) {
                                        return Ok(basic_type);
                                    }else{
                                        return Err(Box::new(CodeGenError::mismatch_type_struct_fields(Some(id), pos.clone())));
                                    }
                                },
                                GenType::Union { max_size_type, .. } => {
                                    let t = max_size_type.ok_or(CodeGenError::union_has_no_field(None, pos.clone()))?;
                                    return Ok(t);
                                },
                                GenType::StandardEnum { i32_type, .. } => {
                                    return Ok(i32_type.as_basic_type_enum());
                                },
                                GenType::TaggedEnum(gen_enum) => {
                                    if let Some(t) = gen_enum.get_max_size_type() {
                                        return Ok(t);
                                    }else{
                                        return Err(Box::new(CodeGenError::enum_has_no_field(gen_enum.get_name().to_string(), pos.clone())));
                                    }
                                },
                                GenType::TypeDefStruct(_name, gen_t) => {
                                    gen_type = gen_t;
                                },
                                GenType::TypeDefUnion(_name, gen_t) => {
                                    gen_type = gen_t;
                                },
                            }
                        }
                    }else{
                        let (_type_list, _index_map, _max_size, max_size_type_opt) = CodeGen::union_from_struct_definition(&None, fields, type_variables, ctx, self, pos)?;
                        let max_size_type = max_size_type_opt.ok_or(CodeGenError::union_has_no_field(None, pos.clone()))?;
                        Ok(max_size_type)
                    }
                }else{
                    let (_type_list, _index_map, _max_size, max_size_type_opt) = CodeGen::union_from_struct_definition(&None, fields, type_variables, ctx, self, pos)?;
                    let max_size_type = max_size_type_opt.ok_or(CodeGenError::union_has_no_field(None, pos.clone()))?;
                    Ok(max_size_type)
                }
            },
            Type::Enum { name, enum_def, type_variables: _ } => {
                if enum_def.is_standard() {
                    Ok(BasicTypeEnum::IntType(ctx.i32_type()))
                }else{
                    if let Some(gen_type) = self.get_gen_type(name, ctx, pos)? {
                        let mut gen_type = &gen_type;
                        loop {
                            match &gen_type {
                                GenType::Type(t) => {
                                    if let Ok(basic_type) = BasicTypeEnum::try_from(*t) {
                                        return Ok(basic_type);
                                    }else{
                                        return Err(Box::new(CodeGenError::mismatch_type_struct_fields(Some(name), pos.clone())));
                                    }
                                },
                                GenType::Union { max_size_type, .. } => {
                                    let t = max_size_type.ok_or(CodeGenError::union_has_no_field(None, pos.clone()))?;
                                    return Ok(t);
                                },
                                GenType::StandardEnum { i32_type, .. } => {
                                    return Ok(i32_type.as_basic_type_enum());
                                },
                                GenType::TaggedEnum(gen_enum) => {
                                    let t = gen_enum.get_max_size_type().ok_or(CodeGenError::enum_has_no_field(gen_enum.get_name().to_string(), pos.clone()))?;
                                    return Ok(t);
                                },
                                GenType::TypeDefStruct(_name, gen_t) => {
                                    gen_type = gen_t;
                                },
                                GenType::TypeDefUnion(_name, gen_t) => {
                                    gen_type = gen_t;
                                },
                            }
                        }

                    }else{
                        Err(Box::new(CodeGenError::no_such_a_type(name, pos.clone())))
                    }
                }
            },
            Type::Tuple(_type_list) => {
                TypeUtil::to_basic_type_enum(typ, ctx, self, pos)
            },
            Type::BoundEnumType { enum_type, map } => {
                let enum_def = enum_type.get_enum_definition().unwrap();
                let type_variables = enum_type.get_type_variables();

                if enum_def.is_standard() {
                    if map.len() != 0 {
                        return Err(Box::new(CodeGenError::mismatch_type_variables_in_bound_enum(0, map.len(), pos.clone())));
                    }

                    let tag_type = global().enum_tag_type();
                    let tag_type = TypeUtil::to_basic_type_enum(&Type::Number(tag_type.clone()), ctx, self, pos)?;
                    return Ok(tag_type);
                }

                self.add_new_local_types();

                if let Some(typ_vars) = type_variables {
                    if typ_vars.len() != map.len() {
                        return Err(Box::new(CodeGenError::mismatch_type_variables_in_bound_enum(typ_vars.len(), map.len(), pos.clone())));
                    }
                    self.set_type_variables(typ_vars, map, pos)?;
                }else{
                    if map.len() != 0 {
                        return Err(Box::new(CodeGenError::mismatch_type_variables_in_bound_enum(0, map.len(), pos.clone())));
                    }
                }

                let fields = enum_def.get_fields();
                let mut max_size = 0;
                let mut max_size_type = None;
                for field in fields {
                    if let Some(field_type) = field.get_type() {
                        let basic_type = TypeUtil::to_basic_type_enum(&field_type, ctx, self, pos)?;
                        let size = CodeGen::size_of(&basic_type)?;
                        if size > max_size {
                            max_size = size;
                            max_size_type = Some(basic_type);
                        }
                    }
                }

                self.remove_local_types();

                // add tag type
                let tag_type = global().enum_tag_type();
                let tag_type = TypeUtil::to_basic_type_enum(&Type::Number(tag_type.clone()), ctx, self, pos)?;
                let vec;
                if let Some(max_size_type) = max_size_type {
                    vec = vec![tag_type, max_size_type];
                }else{
                    vec = vec![tag_type];
                }
                let struct_ty = ctx.struct_type(&vec, false);
                let result = struct_ty.as_basic_type_enum();

                Ok(result)
            },
            Type::BoundStructType { struct_type, map } => {



                unimplemented!()
            },
            Type::BoundUnionType { union_type, map } => {


                unimplemented!()
            },
            _ => {
                Ok(TypeUtil::to_basic_type_enum(typ, ctx, self, pos)?)
            },
        }
    }

    pub fn set_type_variables(&mut self, type_variables: &Vec<String>, map: &HashMap<String, Rc<Type>>, pos: &Position) -> Result<(), Box<dyn Error>> {
        let map_len = map.len();

        if type_variables.len() != map_len {
            return Err(Box::new(CodeGenError::mismatch_type_variables_in_bound_enum(type_variables.len(), map.len(), pos.clone())));
        }

        for typ_var_name in type_variables {
            if ! map.contains_key(typ_var_name) {
                return Err(Box::new(CodeGenError::no_such_a_type_variable_in_bound_enum(typ_var_name.to_string(), pos.clone())));
            }

            self.insert_local_type(typ_var_name, map.get(typ_var_name).unwrap().clone());
        }

        Ok(())
    }

    pub fn get_block(&self, key: &str) -> Option<&BasicBlock> {
        let list = self.local_labels.last().unwrap();
        let mut index = list.len() - 1;
        loop {
            if let Some(tuple) = list[index].get(key) {
                return Some(tuple);
            }

            if index == 0 { break; }
            index -= 1;
        }

        None
    }

    pub fn get_type_by_id(&self, key: &str) -> Option<&Rc<Type>> {
        if let Some((typ, _sq, _ptr)) = self.get_ptr_from_local(key) {
            Some(typ)
        }else if let Some((typ, _sq, val)) = self.global_def.get(key) {
            match val {
                ConstOrGlobalValue::GlobalValue { global: _ } => Some(typ),
                ConstOrGlobalValue::Const { value: _ } => Some(typ),
            }
        }else{
            for map in self.local_type_map_list.iter().rev() {
                if let Some(t) = map.get(key) {
                    return Some(t);
                }
            }

            None
        }
    }

    pub fn get_ptr(&self, key: &str) -> Option<(&Rc<Type>, &SpecifierQualifier, PointerValue<'ctx>)> {
        if let Some((typ, sq, ptr)) = self.get_ptr_from_local(key) {
            Some((typ, sq, *ptr))
        }else if let Some((typ, sq, val)) = self.global_def.get(key) {
            match val {
                ConstOrGlobalValue::GlobalValue { global } => Some((typ, sq, global.as_pointer_value())),
                _ => None,
            }
        }else{
            None
        }
    }

    pub fn get_value(&self, key: &str) -> Option<(&Rc<Type>, &SpecifierQualifier, BasicValueEnum<'ctx>)> {
        if let Some((typ, sq, val)) = self.global_def.get(key) {
            match val {
                ConstOrGlobalValue::Const { value } => Some((typ, sq, *value)),
                ConstOrGlobalValue::GlobalValue {..} => None,
            }
        }else{
            None
        }
    }

    fn get_ptr_from_local(&self, key: &str) -> Option<&(Rc<Type>, SpecifierQualifier, PointerValue<'ctx>)> {
        if let Some(list) = self.locals.last() {
            let mut index = list.len() - 1;
            loop {
                if let Some(tuple) = list[index].get(key) {
                    return Some(tuple);
                }

                if index == 0 { break; }
                index -= 1;
            }
        }

        None
    }

    pub fn get_self_ptr(&self) -> Option<(&Rc<Type>, &SpecifierQualifier, PointerValue<'ctx>)> {
        if let Some((typ, sq, ptr)) = self.get_ptr_from_local("self") {
            Some((typ, sq, *ptr))
        }else{
            None
        }
    }

    pub fn get_current_function(&self) -> Option<(&CustFunctionType, &FunctionValue<'ctx>)> {
        if let Some((ref typ, ref value)) = self.current_function {
            Some((typ, value))
        }else{
            None
        }
    }

    pub fn get_current_function_type(&self) -> Option<CustFunctionType> {
        if let Some((ref typ, ref _value)) = self.current_function {
            Some(typ.clone())
        }else{
            None
        }
    }

    pub fn set_current_function(&mut self,  typ: &CustFunctionType, func: FunctionValue<'ctx>) {
        self.current_function = Some((typ.clone(), func));
    }

    pub fn get_gen_type(&mut self, key: &str, ctx: &'ctx Context, pos: &Position) -> Result<Option<GenType<'ctx>>, Box<dyn Error>> {
        if let Some((typ, _index_map)) = self.types.get(key) {
            match typ {
                GenType::TypeDefStruct(_name, gen_t) => {
                    Ok(Some(*gen_t.clone()))
                },
                GenType::TypeDefUnion(_name, gen_t) => {
                    Ok(Some(*gen_t.clone()))
                },
                GenType::TaggedEnum(tagged) => {
                    if let Some(typ_vars) = tagged.get_type_varialbes() {
                        let mut bound_map: HashMap<String, (Rc<Type>, BasicTypeEnum<'ctx>)> = HashMap::new();

                        for id in typ_vars {
                            if let Some(t) = self.get_type_by_id(id) {
                                let llvm_ty = TypeUtil::to_basic_type_enum(&t, ctx, &mut self.clone(), pos)?;  // self.clone() はバグの温床になる可能性がある。
                                bound_map.insert(id.to_string(), (Rc::clone(t), llvm_ty));

                            }else{
                                panic!("no such a type variable: {id} in tagged enum: {}", tagged.get_name());
                            }
                        }

                        let type_list = tagged.get_type_list().clone();
                        let type_var_enum = TypeVarTaggedEnum::new(tagged.get_name().to_string(), type_list.clone(), tagged.get_index_map().clone(), typ_vars.clone());
                        let index_map = tagged.get_index_map().clone();

                        // let type_pair_list = type_list.iter().map(|t| (Rc::clone(t), TypeUtil::to_basic_type_enum(t, ctx, &mut self.clone(), pos).unwrap())).collect::<Vec<_>>();  // self.clone() はバグの温床になる可能性がある。
                        let mut type_pair_list = Vec::new();
                        for typ in type_list {
                            let llvm_ty = TypeUtil::to_basic_type_enum(&typ, ctx, &mut self.clone(), pos)?;

                            let tag_type = global().enum_tag_type();
                            let tag_type = TypeUtil::to_basic_type_enum(&Type::Number(tag_type.clone()), ctx, self, pos)?;
                            let struct_ty = ctx.struct_type(&[tag_type, llvm_ty], false);

                            type_pair_list.push((typ, struct_ty.as_basic_type_enum()));
                        }

                        let bound = BoundTaggedEnum::try_new(Rc::new(type_var_enum), type_pair_list, index_map, bound_map, self, pos).unwrap();
                        let tag = TaggedEnum::Bound(bound);
                        let gen_type = GenType::TaggedEnum(tag);

                        Ok(Some(gen_type))

                    }else{
                        Ok(Some(typ.clone()))
                    }
                },
                _ => {
                    Ok(Some(typ.clone()))
                }
            }
        }else{
            Ok(None)
        }
    }

    pub fn get_type_and_index_map(&self, key: &str) -> Option<&(GenType<'ctx>, Option<HashMap<String, usize>>)> {
        self.types.get(key)
    }

    pub fn is_signed(&self, ast: &ExprAST) -> Result<bool, CodeGenError> {
        match ast {
            ExprAST::Symbol(name, _pos) => {
                if let Some((typ, _sq, _pointer)) = self.get_ptr(name) {
                    Ok(typ.is_signed()?)
                }else{
                    Err(CodeGenError::condition_is_not_number(ast, ast.get_position().clone()))
                }
            },
            _ => Ok(ast.is_signed()?),
        }
    }

    #[inline]
    pub fn get_class(&self, name: &str) -> Option<&Class<'ctx>> {
        self.classes.get(name)
    }

    pub fn intern_class(&mut self, class_name: &str) -> &Class<'ctx> {
        if ! self.classes.contains_key(class_name) {
            self.classes.insert(class_name.to_string(), Class::new(class_name));
        }

        self.get_class(class_name).unwrap()
    }

    pub fn set_current_class(&mut self, class: *const Class<'ctx>) {
        self.current_class = Some(class);
    }

    pub fn remove_current_class(&mut self) {
        self.current_class = None;
    }

    pub fn get_current_class(&self) -> Option<&Class<'ctx>> {
        if let Some(cls) = self.current_class {
            unsafe {
                Some(cls.as_ref().unwrap())
            }
        }else{
            None
        }
    }
}