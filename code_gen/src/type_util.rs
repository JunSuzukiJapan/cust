use crate::global::global;
use crate::parser::{Type, NumberType, Pointer, BinOp, Position};
use crate::Env;
use inkwell::context::Context;
use std::error::Error;
use std::rc::Rc;
use inkwell::types::{BasicTypeEnum, AnyTypeEnum, BasicType, BasicMetadataTypeEnum, IntType, PointerType};
use inkwell::AddressSpace;
use inkwell::types::AnyType;
use parser::{ExprAST, Initializer, ConstInitializer, ArrayInitializer};
use super::{CodeGen, CodeGenError};

pub struct TypeUtil;

impl TypeUtil {
    pub fn to_llvm_int_type<'a>(typ: &Type, ctx: &'a Context, pos: &Position) -> Result<IntType<'a>, Box<dyn Error>> {
        match typ {
            Type::Number(NumberType::_Bool)  => Ok(ctx.bool_type()),
            Type::Number(NumberType::Char)   => Ok(ctx.i8_type()),
            Type::Number(NumberType::Int)    => Ok(ctx.i32_type()),
            Type::Number(NumberType::Long)  => Ok(ctx.i64_type()),
            Type::Number(NumberType::Short)  => Ok(ctx.i16_type()),
            Type::Number(NumberType::UnsignedChar)   => Ok(ctx.i8_type()),
            Type::Number(NumberType::UnsignedInt)    => Ok(ctx.i32_type()),
            Type::Number(NumberType::UnsignedLong)  => Ok(ctx.i64_type()),
            Type::Number(NumberType::UnsignedShort)  => Ok(ctx.i16_type()),
            _ => Err(Box::new(CodeGenError::not_int_type(typ.clone(), pos.clone()))),
        }
    }

    pub fn to_basic_type_enum<'a>(typ: &Type, ctx: &'a Context, pos: &Position) -> Result<BasicTypeEnum<'a>, Box<dyn Error>> {
        match typ {
            Type::Number(NumberType::_Bool)  => Ok(BasicTypeEnum::IntType(ctx.bool_type())),
            Type::Number(NumberType::Char)   => Ok(BasicTypeEnum::IntType(ctx.i8_type())),
            Type::Number(NumberType::Double) => Ok(BasicTypeEnum::FloatType(ctx.f128_type())),
            Type::Number(NumberType::Float)  => Ok(BasicTypeEnum::FloatType(ctx.f64_type())),
            Type::Number(NumberType::Int)    => Ok(BasicTypeEnum::IntType(ctx.i32_type())),
            Type::Number(NumberType::Long)  => Ok(BasicTypeEnum::IntType(ctx.i64_type())),
            Type::Number(NumberType::Short)  => Ok(BasicTypeEnum::IntType(ctx.i16_type())),
            Type::Number(NumberType::UnsignedChar)   => Ok(BasicTypeEnum::IntType(ctx.i8_type())),
            Type::Number(NumberType::UnsignedInt)    => Ok(BasicTypeEnum::IntType(ctx.i32_type())),
            Type::Number(NumberType::UnsignedLong)  => Ok(BasicTypeEnum::IntType(ctx.i64_type())),
            Type::Number(NumberType::UnsignedShort)  => Ok(BasicTypeEnum::IntType(ctx.i16_type())),
            // Type::Void   => Ok(BasicTypeEnum::VoidType(ctx.void_type())),
            Type::Pointer(_p, to_type) => {
                let typ = Self::to_llvm_any_type(typ, ctx, pos)?;

                if typ.is_int_type() {
                    // Ok(typ.into_int_type().ptr_type(AddressSpace::default()).into())
                    Ok(typ.into_int_type().into())
                }else if typ.is_float_type() {
                    // Ok(typ.into_float_type().ptr_type(AddressSpace::default()).into())
                    Ok(typ.into_float_type().into())
                }else if typ.is_pointer_type() {
                    // Ok(typ.into_pointer_type().ptr_type(AddressSpace::default()).into())
                    Ok(typ.into_pointer_type().into())
                }else if typ.is_array_type() {
                    Ok(typ.into_array_type().into())
                }else if typ.is_vector_type() {
                    Ok(typ.into_vector_type().into())
                }else if typ.is_struct_type() {
                    Ok(typ.into_struct_type().ptr_type(AddressSpace::default()).into())
                }else{
                    Err(Box::new(CodeGenError::illegal_type_for_pointer(&to_type, pos.clone())))
                }
            },
            Type::Array { name: _, typ: type2, size_list } => {
                let mut to_type = Self::to_basic_type_enum(type2, ctx, pos)?;

                for size in size_list.iter().rev() {
                    to_type = to_type.array_type(*size as u32).as_basic_type_enum();
                }

                Ok(to_type)
            },
            Type::Struct { name, fields } => {
                let (struct_type, _index_map) = CodeGen::struct_from_struct_definition(name, fields, ctx, pos)?;
                Ok(BasicTypeEnum::StructType(struct_type))
            },
            Type::Union { name, fields } => {
                let (_union_type, _index_map, _max_size, max_size_type) = CodeGen::union_from_struct_definition(name, fields, ctx, pos)?;
                if let Some(typ) = max_size_type {
                    Ok(typ)
                }else{
                    if let Some(id) = name {
                        Err(Box::new(CodeGenError::union_has_no_field(Some(id.to_string()), pos.clone())))
                    }else{
                        Err(Box::new(CodeGenError::union_has_no_field(None, pos.clone())))
                    }
                }
            },
            Type::Enum { name, enum_def } => {
                if enum_def.is_standard() {
                    Ok(BasicTypeEnum::IntType(ctx.i32_type()))
                }else{
                    let enum_tag_type = Rc::new(Type::Number(global().enum_tag_type.clone()));
                    let (_type_list, _index_map, _max_size, max_size_type) = CodeGen::tagged_enum_from_enum_definition(name, enum_def.get_fields(), &enum_tag_type, ctx, pos)?;

                    if let Some(typ) = max_size_type {
                        Ok(typ)
                    }else{
                        Err(Box::new(CodeGenError::enum_has_no_field(name.to_string(), pos.clone())))
                    }
                }
            },
            Type::Symbol(_name) => {
                Err(Box::new(CodeGenError::cannot_convert_to_basic_type(typ.to_string(), pos.clone())))

            },
            Type::Tuple(type_list) => {
                let mut vec = Vec::new();

                for t in type_list {
                    let t2 = TypeUtil::to_basic_type_enum(t, ctx, pos)?;
                    vec.push(t2);
                }

                let any_type = ctx.struct_type(&vec, false);
                let basic_type = BasicTypeEnum::try_from(any_type).unwrap();
                Ok(basic_type)
            },
            _ => {
                Err(Box::new(CodeGenError::cannot_convert_to_basic_type(typ.to_string(), pos.clone())))
            },
        }
    }

    #[inline]
    pub fn to_llvm_type<'a>(typ: &Type, ctx: &'a Context, pos: &Position) -> Result<BasicMetadataTypeEnum<'a>, Box<dyn Error>> {
        Ok(Self::to_basic_type_enum(typ, ctx, pos)?.into())
    }

    pub fn to_llvm_any_type<'a>(typ: &Type, ctx: &'a Context, pos: &Position) -> Result<AnyTypeEnum<'a>, Box<dyn Error>> {
        match typ {
            Type::Number(NumberType::Char)   => Ok(AnyTypeEnum::IntType(ctx.i8_type())),
            Type::Number(NumberType::Short)  => Ok(AnyTypeEnum::IntType(ctx.i16_type())),
            Type::Number(NumberType::Int)    => Ok(AnyTypeEnum::IntType(ctx.i32_type())),
            Type::Number(NumberType::Long)   => Ok(AnyTypeEnum::IntType(ctx.i64_type())),
            Type::Number(NumberType::Float)  => Ok(AnyTypeEnum::FloatType(ctx.f64_type())),
            Type::Number(NumberType::Double) => Ok(AnyTypeEnum::FloatType(ctx.f128_type())),
            Type::Number(NumberType::UnsignedChar)   => Ok(AnyTypeEnum::IntType(ctx.i8_type())),
            Type::Number(NumberType::UnsignedShort)  => Ok(AnyTypeEnum::IntType(ctx.i16_type())),
            Type::Number(NumberType::UnsignedInt)    => Ok(AnyTypeEnum::IntType(ctx.i32_type())),
            Type::Number(NumberType::UnsignedLong)   => Ok(AnyTypeEnum::IntType(ctx.i64_type())),
            Type::Void   => Ok(AnyTypeEnum::VoidType(ctx.void_type())),
            Type::Struct { name, fields } => {
                let name = if let Some(id) = name {
                    Some(id.clone())
                }else{
                    None
                };
                let (struct_type, _tbl) = CodeGen::struct_from_struct_definition(&name, &fields, ctx, pos)?;
                Ok(struct_type.as_any_type_enum())
            },
            Type::Pointer(_ptr, typ) => {
                let ptr_type = Self::make_llvm_ptr_type(typ, ctx, pos)?;
                Ok(AnyTypeEnum::PointerType(ptr_type))
            },
            Type::Symbol(_name) => {
                // maybe unreached
                unimplemented!("'{}' to AnyTypeEnum", typ.to_string())
            },
            _ => {
                // maybe unreached
                unimplemented!("'{}' to AnyTypeEnum", typ.to_string())
            },
        }
    }

    pub fn make_llvm_ptr_type<'a>(typ: &Type, ctx: &'a Context, pos: &Position) -> Result<PointerType<'a>, Box<dyn Error>> {
        match typ {
            Type::Number(NumberType::Char)   => Ok(ctx.i8_type().ptr_type(AddressSpace::default())),
            Type::Number(NumberType::Short)  => Ok(ctx.i16_type().ptr_type(AddressSpace::default())),
            Type::Number(NumberType::Int)    => Ok(ctx.i32_type().ptr_type(AddressSpace::default())),
            Type::Number(NumberType::Long)   => Ok(ctx.i64_type().ptr_type(AddressSpace::default())),
            Type::Number(NumberType::Float)  => Ok(ctx.f64_type().ptr_type(AddressSpace::default())),
            Type::Number(NumberType::Double) => Ok(ctx.f128_type().ptr_type(AddressSpace::default())),
            Type::Number(NumberType::UnsignedChar)   => Ok(ctx.i8_type().ptr_type(AddressSpace::default())),
            Type::Number(NumberType::UnsignedShort)  => Ok(ctx.i16_type().ptr_type(AddressSpace::default())),
            Type::Number(NumberType::UnsignedInt)    => Ok(ctx.i32_type().ptr_type(AddressSpace::default())),
            Type::Number(NumberType::UnsignedLong)   => Ok(ctx.i64_type().ptr_type(AddressSpace::default())),
            Type::Void   => Ok(ctx.i8_type().ptr_type(AddressSpace::default())),
            Type::Struct { name, fields } => {
                let name = if let Some(id) = name {
                    Some(id.clone())
                }else{
                    None
                };
                let (struct_type, _tbl) = CodeGen::struct_from_struct_definition(&name, &fields, ctx, pos)?;
                Ok(struct_type.ptr_type(AddressSpace::default()))
            },
            Type::Pointer(_ptr, typ) => {
                let ptr_type = Self::make_llvm_ptr_type(typ, ctx, pos)?;
                Ok(ptr_type.ptr_type(AddressSpace::default()))
            },
            Type::Tuple(type_list) => {
                let mut vec: Vec<BasicTypeEnum<'a>> = Vec::new();

                for t in type_list {
                    let t2 = Self::to_basic_type_enum(&t, ctx, pos)?;
                    vec.push(t2);
                }

                let any_type = ctx.struct_type(&vec, false);
                Ok(any_type.ptr_type(AddressSpace::default()))
            },
            _ => {
                Err(Box::new(CodeGenError::cannot_make_pointer_type(typ.clone(), pos.clone())))
            },
        }
    }

    pub fn get_type(expr_ast: &ExprAST, env: &Env) -> Result<Rc<Type>, CodeGenError> {
        match expr_ast {
            ExprAST::Assign(left, _right, _pos) => {
                // (*left).get_type(env)
                Self::get_type(&left, env)
            },
            ExprAST::OpAssign(_op, left, _right, _pos) => {
                // (*left).get_type(env)
                Self::get_type(&left, env)
            },
            ExprAST::Char(_, _pos) => Ok(Rc::new(Type::Number(NumberType::Char))),
            ExprAST::Int(_, _pos) => Ok(Rc::new(Type::Number(NumberType::Int))),
            ExprAST::Short(_, _pos) => Ok(Rc::new(Type::Number(NumberType::Short))),
            ExprAST::Long(_, _pos) => Ok(Rc::new(Type::Number(NumberType::Long))),
            ExprAST::LongLong(_, _pos) => Ok(Rc::new(Type::Number(NumberType::LongLong))),
            ExprAST::UChar(_, _pos) => Ok(Rc::new(Type::Number(NumberType::UnsignedChar))),
            ExprAST::UInt(_, _pos) => Ok(Rc::new(Type::Number(NumberType::UnsignedInt))),
            ExprAST::UShort(_, _pos) => Ok(Rc::new(Type::Number(NumberType::UnsignedShort))),
            ExprAST::ULong(_, _pos) => Ok(Rc::new(Type::Number(NumberType::UnsignedLong))),
            ExprAST::ULongLong(_, _pos) => Ok(Rc::new(Type::Number(NumberType::UnsignedLongLong))),
            ExprAST::StringLiteral(_string, _pos) => Ok(Rc::new(Type::Pointer(Pointer::new(false, false), Box::new(Rc::new(Type::Number(NumberType::Char)))))),
            ExprAST::Float(_, _pos) => Ok(Rc::new(Type::Number(NumberType::Float))),
            ExprAST::Double(_, _pos) => Ok(Rc::new(Type::Number(NumberType::Double))),
            ExprAST::BinExpr(op, left, _right, _pos) => {
                match op {
                    BinOp::Equal | BinOp::NotEqual | BinOp::Less | BinOp::LessEqual | BinOp::Greater | BinOp::GreaterEqual => {
                        Ok(Rc::new(Type::Number(NumberType::_Bool)))
                    },
                    _ => Self::get_type(&left, env),
                }
            },
            ExprAST::UnaryMinus(expr, _pos) => Self::get_type(&expr, env),
            ExprAST::UnaryTilda(expr, _pos) => Self::get_type(&expr, env),
            // ExprAST::UnaryNot(expr) => expr.get_type(env),
            ExprAST::UnarySizeOfExpr(_expr, _pos) => Ok(Rc::new(Type::Number(NumberType::Int))),
            ExprAST::UnarySizeOfTypeName(_typ, _pos) => Ok(Rc::new(Type::Number(NumberType::Int))),
            ExprAST::ArrayAccess(expr, index_list, pos) => {
                let index_len = index_list.len();

                let typ = Self::get_type(&expr, env)?;
                if let Type::Array { name: _, typ: item_type, size_list } = typ.as_ref() {  // Array
                    let len = size_list.len();

                    if len == index_len {
                        return Ok(*item_type.clone());

                    } if len < index_len {
                        return Err(CodeGenError::array_index_is_too_long(pos.clone()))

                    }else{  // len < index_len
                        let t = Type::new_pointer_type(*item_type.clone(), false, false);
                        Ok(Rc::new(t))
                    }
                }else if let Type::Pointer(_, elem_type) = typ.as_ref() {             // Pointer
                    if index_len > 1 {
                        return Err(CodeGenError::array_index_is_too_long(pos.clone()));
                    }

                    // let t = Type::new_pointer_type(*elem_type.clone(), false, false);
                    // Ok(Rc::new(t))
                    Ok(Rc::clone(&elem_type))

                }else{
                    return Err(CodeGenError::not_array(*expr.clone(), pos.clone()));
                }
            },
            ExprAST::Symbol(name, pos) => {
                let typ = env.get_type_by_id(&name).ok_or(CodeGenError::no_such_a_variable(&name, pos.clone()))?;
                Ok(typ.clone())
            },
            ExprAST::SelfStaticSymbol(_sym, pos) => {
                let (typ, _sq, _expr) = env.get_ptr("Self").ok_or(CodeGenError::access_self_type_without_impl(pos.clone()))?;
                Ok(typ.clone())
            },
            ExprAST::StructStaticSymbol(class_name, var_name, pos) => {
                let (typ, _sq, _global_value) = env.get_class_var(class_name, var_name).ok_or(CodeGenError::no_such_a_variable(&format!("{}::{}", class_name, var_name), pos.clone()))?;
                Ok(Rc::clone(typ))
            },
            ExprAST::_self(pos) => {
                let (typ, _sq, _expr) = env.get_ptr("self").ok_or(CodeGenError::access_self_without_impl(pos.clone()))?;
                Ok(Rc::clone(typ))
            },
            ExprAST::Not(_expr, _pos) => Ok(Rc::new(Type::Number(NumberType::_Bool))),
            // ExprAST::ExpressionPair(_, right, _pos) => TypeUtil::get_type(&right, env),
            ExprAST::Cast(typ, _, _pos) => Ok(typ.clone()),
            ExprAST::PreInc(name, _sym_pos, pos) => {
                let typ = env.get_type_by_id(&name).ok_or(CodeGenError::no_such_a_variable(&name, pos.clone()))?;
                Ok(typ.clone())
            },
            ExprAST::PreDec(name, _sym_pos, pos) => {
                let typ = env.get_type_by_id(&name).ok_or(CodeGenError::no_such_a_variable(&name, pos.clone()))?;
                Ok(typ.clone())
            },
            ExprAST::PostInc(name, _sym_pos, pos) => {
                let typ = env.get_type_by_id(&name).ok_or(CodeGenError::no_such_a_variable(&name, pos.clone()))?;
                Ok(typ.clone())
            },
            ExprAST::PostDec(name, _sym_pos, pos) => {
                let typ = env.get_type_by_id(&name).ok_or(CodeGenError::no_such_a_variable(&name, pos.clone()))?;
                Ok(typ.clone())
            },
            ExprAST::PreIncMemberAccess(expr, _pos) => {
                Self::get_type(expr, env)
            },
            ExprAST::PostIncMemberAccess(expr, _pos) => {
                Self::get_type(expr, env)
            },
            ExprAST::PreDecMemberAccess(expr, _pos) => {
                Self::get_type(expr, env)
            },
            ExprAST::PostDecMemberAccess(expr, _pos) => {
                Self::get_type(expr, env)
            },
            ExprAST::UnaryGetAddress(boxed_ast, _pos) => {
                let ast = &*boxed_ast;
                let t = TypeUtil::get_type(ast, env)?;
                Ok(Rc::new(Type::new_pointer_type(t, false, false)))
            },
            ExprAST::UnaryPointerAccess(boxed_ast, pos) => {  // *pointer
                let ast = &**boxed_ast;
                let typ = TypeUtil::get_type(ast, env)?;

                match typ.as_ref() {
                    Type::Pointer(_p, t) => {
                        Ok(*t.clone())
                    },
                    _ => Err(CodeGenError::not_pointer(&typ, pos.clone())),
                }
            },
            ExprAST::MemberAccess(boxed_ast, field_name, pos) => {  // some_var.field
                let ast = &*boxed_ast;
                let typ = TypeUtil::get_type(ast, env)?;

                match typ.as_ref() {
                    Type::Struct { name: _, fields } => {
                        let t = fields.get_type(&field_name).ok_or(CodeGenError::type_has_not_member(&field_name, pos.clone()))?;
                        Ok(t.clone())
                    },
                    Type::Union { name: _, fields } => {
                        let t = fields.get_type(&field_name).ok_or(CodeGenError::type_has_not_member(&field_name, pos.clone()))?;
                        Ok(t.clone())
                    },
                    _ => {
                        return Err(CodeGenError::type_has_not_member(&format!("{:?}", &typ), pos.clone()));
                    },
                }
            },
            ExprAST::PointerAccess(boxed_ast, field_name, pos) => {  // some_var->field
                let ast = &*boxed_ast;
                let typ = TypeUtil::get_type(ast, env)?;
                let pointed_type = typ.get_pointed_type(pos)?;

                match pointed_type {
                    Type::Struct { name: _, fields } => {
                        let t = fields.get_type(&field_name).ok_or(CodeGenError::type_has_not_member(&field_name, pos.clone()))?;
                        Ok(t.clone())
                    },
                    Type::Union { name: _, fields } => {
                        let t = fields.get_type(&field_name).ok_or(CodeGenError::type_has_not_member(&field_name, pos.clone()))?;
                        Ok(t.clone())
                    },
                    _ => {
                        return Err(CodeGenError::type_has_not_member(&format!("{:?}", &typ), pos.clone()));
                    },
                }
            },
            ExprAST::TernaryOperator(_, e1, _, _pos) => {
                TypeUtil::get_type(&e1, env)
            },
            ExprAST::CallFunction(fun, _args, _pos) => {
                let f_type = match &**fun {
                    ExprAST::Symbol(name, pos2) => {
                        if let Some((fun_type, _f_value)) = env.get_function(name) {
                            fun_type
                        }else{
                            return Err(CodeGenError::not_function(name, pos2.clone()));
                        }
                    },
                    ExprAST::MemberAccess(ast, fun_name, pos2) => {
                        let typ = TypeUtil::get_type(ast, env)?;
                        let class_name = typ.get_type_name();
                        let method_name = CodeGen::make_function_name_in_impl(&class_name, fun_name);

                        if let Some((fun_type, _f_value)) = env.get_function(&method_name) {
                            fun_type
                        }else{
                            return Err(CodeGenError::not_function(&method_name, pos2.clone()));
                        }
                    },
                    _ => panic!("'{:?}' is not function.", **fun),
                };

                Ok(f_type.get_return_type().clone())
            },
            ExprAST::StructLiteral(struct_literal) => {
                let typ = struct_literal.get_type();
                Ok(Rc::clone(typ))
            },
            ExprAST::UnionLiteral(typ, _map, _pos) => {
                Ok(Rc::clone(typ))
            },
            ExprAST::UnionConstLiteral(typ, _map, _pos) => {
                Ok(Rc::clone(typ))
            },
            ExprAST::EnumLiteral(typ, _index, _literal, _pos) => {
                // let typ = literal.get_type();
                Ok(Rc::clone(typ))
            },
            ExprAST::TupleLiteral(expr_list, _pos) => {
                let mut type_list = Vec::new();

                for e in expr_list {
                    let typ = TypeUtil::get_type(e, env)?;
                    type_list.push(typ);
                }

                let t = Type::Tuple(type_list);
                Ok(Rc::new(t))
            },
            ExprAST::TupleMemberAccess(expr_ast, index, pos) => {
                let typ = TypeUtil::get_type(expr_ast, env)?;
                let ptr = Rc::as_ptr(&typ);
                if let Type::Tuple(vec) = unsafe{&*ptr} {
                    let len = vec.len();
                    if *index >= len {
                        return Err(CodeGenError::tuple_index_too_big(len, *index, pos.clone()));
                    }

                    Ok(Rc::clone(&vec[*index]))

                }else{
                    Err(CodeGenError::not_tuple_in_tuple_access_by_index(*expr_ast.clone(), pos.clone()))
                }
            },
            ExprAST::TuplePointerAccess(expr_ast, index, pos) => {





                unimplemented!()
            },
            // ExprAST::DefVar { specifiers: _, declarations: _, pos: _ } => {
            //     // maybe unreached???
            //     unimplemented!()
            // },
         }
    }

    pub fn get_initializer_type(init: &Initializer, env: &Env) -> Result<Rc<Type>, CodeGenError> {
        match init {
            Initializer::Simple(expr, _pos) => TypeUtil::get_type(expr, env),
            Initializer::Array(_init, typ, _pos) => Ok(Rc::clone(typ)),
            Initializer::Struct(_init, typ, _pos) => Ok(Rc::clone(typ)),
        }
    }

    pub fn get_initializer_type_of_const_initializer(init: &ConstInitializer, env: &Env) -> Result<Rc<Type>, CodeGenError> {
        match init {
            ConstInitializer::Simple(expr, _pos) => Ok(Rc::new(expr.get_type())),
            ConstInitializer::Array(_init, typ, _pos) => Ok(Rc::clone(typ)),
            ConstInitializer::Struct(_init, typ, _pos) => Ok(Rc::clone(typ)),
        }
    }

    pub fn get_initializer_type_of_array_initializer(init: &ArrayInitializer, env: &Env) -> Result<Rc<Type>, CodeGenError> {
        match init {
            ArrayInitializer::Const(expr, _pos) => Ok(Rc::clone(&expr.get_type())),
            ArrayInitializer::Array(_init, typ, _pos) => Ok(Rc::clone(typ)),
        }
    }
}