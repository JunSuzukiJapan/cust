use crate::parser::{Type, NumberType, Pointer, BinOp, Position};
use crate::Env;
use inkwell::context::Context;
use std::error::Error;
use inkwell::types::{BasicTypeEnum, AnyTypeEnum, BasicType, BasicMetadataTypeEnum, IntType, PointerType};
use inkwell::AddressSpace;
use inkwell::types::AnyType;
use parser::{ExprAST, Initializer};
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
                    Err(Box::new(CodeGenError::illegal_type_for_pointer(pos.clone(), &to_type)))
                }
            },
            Type::Array { name: _, typ: type2, size_list } => {
                let mut to_type = Self::to_basic_type_enum(type2, ctx, pos)?;

                for sz in size_list.iter().rev() {
                    let size = sz.to_usize()?;
                    to_type = to_type.array_type(size as u32).as_basic_type_enum();
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
                        Err(Box::new(CodeGenError::union_has_no_field(pos.clone(), Some(id.to_string()))))
                    }else{
                        Err(Box::new(CodeGenError::union_has_no_field(pos.clone(), None)))
                    }
                }
            },
            Type::Enum { name: _, enum_def: _ } => {
                Ok(BasicTypeEnum::IntType(ctx.i32_type()))
            },
            Type::Symbol(_name) => {
                Err(Box::new(CodeGenError::cannot_convert_to_basic_type(typ.to_string(), pos.clone())))

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

    fn make_llvm_ptr_type<'a>(typ: &Type, ctx: &'a Context, pos: &Position) -> Result<PointerType<'a>, Box<dyn Error>> {
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
            _ => {
                Err(Box::new(CodeGenError::cannot_make_pointer_type(typ.clone(), pos.clone())))
            },
        }
    }

    pub fn get_type(expr: &ExprAST, env: &Env) -> Result<Type, CodeGenError> {
        match expr {
            ExprAST::Assign(left, _right, _pos) => {
                // (*left).get_type(env)
                Self::get_type(&left, env)
            },
            ExprAST::OpAssign(_op, left, _right, _pos) => {
                // (*left).get_type(env)
                Self::get_type(&left, env)
            },
            ExprAST::Char(_, _pos) => Ok(Type::Number(NumberType::Char)),
            ExprAST::Int(_, _pos) => Ok(Type::Number(NumberType::Int)),
            ExprAST::Short(_, _pos) => Ok(Type::Number(NumberType::Short)),
            ExprAST::Long(_, _pos) => Ok(Type::Number(NumberType::Long)),
            ExprAST::LongLong(_, _pos) => Ok(Type::Number(NumberType::LongLong)),
            ExprAST::UChar(_, _pos) => Ok(Type::Number(NumberType::UnsignedChar)),
            ExprAST::UInt(_, _pos) => Ok(Type::Number(NumberType::UnsignedInt)),
            ExprAST::UShort(_, _pos) => Ok(Type::Number(NumberType::UnsignedShort)),
            ExprAST::ULong(_, _pos) => Ok(Type::Number(NumberType::UnsignedLong)),
            ExprAST::ULongLong(_, _pos) => Ok(Type::Number(NumberType::UnsignedLongLong)),
            ExprAST::StringLiteral(_string, _pos) => Ok(Type::Pointer(Pointer::new(false, false), Box::new(Type::Number(NumberType::Char)))),
            ExprAST::Float(_, _pos) => Ok(Type::Number(NumberType::Float)),
            ExprAST::Double(_, _pos) => Ok(Type::Number(NumberType::Double)),
            ExprAST::BinExpr(op, left, _right, _pos) => {
                match op {
                    BinOp::Equal | BinOp::NotEqual | BinOp::Less | BinOp::LessEqual | BinOp::Greater | BinOp::GreaterEqual => Ok(Type::Number(NumberType::_Bool)),
                    _ => Self::get_type(&left, env),
                }
            },
            ExprAST::UnaryMinus(expr, _pos) => Self::get_type(&expr, env),
            ExprAST::UnaryTilda(expr, _pos) => Self::get_type(&expr, env),
            // ExprAST::UnaryNot(expr) => expr.get_type(env),
            ExprAST::UnarySizeOfExpr(_expr, _pos) => Ok(Type::Number(NumberType::Int)),
            ExprAST::UnarySizeOfTypeName(_typ, _pos) => Ok(Type::Number(NumberType::Int)),
            ExprAST::ArrayAccess(expr, _index, _pos) => {
                let typ = Self::get_type(&expr, env)?;
                Ok(typ)
            },
            ExprAST::Symbol(name, pos) => {
                let typ = env.get_type_by_id(&name).ok_or(CodeGenError::no_such_a_variable(pos.clone(), &name))?;
                Ok(typ.clone())
            },
            ExprAST::_Self(pos) => {
                let (typ, _expr) = env.get_ptr("Self").ok_or(CodeGenError::access_self_type_without_impl(pos.clone()))?;
                Ok(typ.clone())
            },
            ExprAST::_self(pos) => {
                let (typ, _expr) = env.get_ptr("self").ok_or(CodeGenError::access_self_without_impl(pos.clone()))?;
                Ok(typ.clone())
            },
            ExprAST::Not(_expr, _pos) => Ok(Type::Number(NumberType::_Bool)),
            // ExprAST::ExpressionPair(_, right, _pos) => TypeUtil::get_type(&right, env),
            ExprAST::Cast(typ, _, _pos) => Ok(typ.clone()),
            ExprAST::PreInc(name, _sym_pos, pos) => {
                let typ = env.get_type_by_id(&name).ok_or(CodeGenError::no_such_a_variable(pos.clone(), &name))?;
                Ok(typ.clone())
            },
            ExprAST::PreDec(name, _sym_pos, pos) => {
                let typ = env.get_type_by_id(&name).ok_or(CodeGenError::no_such_a_variable(pos.clone(), &name))?;
                Ok(typ.clone())
            },
            ExprAST::PostInc(name, _sym_pos, pos) => {
                let typ = env.get_type_by_id(&name).ok_or(CodeGenError::no_such_a_variable(pos.clone(), &name))?;
                Ok(typ.clone())
            },
            ExprAST::PostDec(name, _sym_pos, pos) => {
                let typ = env.get_type_by_id(&name).ok_or(CodeGenError::no_such_a_variable(pos.clone(), &name))?;
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
                Ok(Type::new_pointer_type(t, false, false))
            },
            ExprAST::UnaryPointerAccess(boxed_ast, pos) => {  // *pointer
                let ast = &**boxed_ast;
                let typ = TypeUtil::get_type(ast, env)?;

                match typ {
                    Type::Pointer(_p, t) => {
                        Ok(*t.clone())
                    },
                    _ => Err(CodeGenError::not_pointer(pos.clone(), &typ)),
                }
            },
            ExprAST::MemberAccess(boxed_ast, field_name, pos) => {  // some_var.field
                let ast = &*boxed_ast;
                let typ = TypeUtil::get_type(ast, env)?;
                match typ {
                    Type::Struct { name: _, fields } => {
                        let t = fields.get_type(&field_name).ok_or(CodeGenError::type_has_not_member(pos.clone(), &field_name))?;
                        Ok(t.clone())
                    },
                    Type::Union { name: _, fields } => {
                        let t = fields.get_type(&field_name).ok_or(CodeGenError::type_has_not_member(pos.clone(), &field_name))?;
                        Ok(t.clone())
                    },
                    _ => return Err(CodeGenError::type_has_not_member(pos.clone(), &format!("{:?}", &typ))),
                }
            },
            ExprAST::PointerAccess(boxed_ast, field_name, pos) => {  // some_var->field
                let ast = &*boxed_ast;
                let typ = TypeUtil::get_type(ast, env)?;
                match typ {
                    Type::Struct { name: _, fields } => {
                        let t = fields.get_type(&field_name).ok_or(CodeGenError::type_has_not_member(pos.clone(), &field_name))?;
                        Ok(t.clone())
                    },
                    Type::Union { name: _, fields } => {
                        let t = fields.get_type(&field_name).ok_or(CodeGenError::type_has_not_member(pos.clone(), &field_name))?;
                        Ok(t.clone())
                    },
                    _ => return Err(CodeGenError::type_has_not_member(pos.clone(), &format!("{:?}", &typ))),
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
            // ExprAST::InitializerList(_, _pos) => {
            //     // maybe unreached??
            //     unimplemented!()
            // },
            ExprAST::DefVar { specifiers: _, declarations: _, pos: _ } => {
                // maybe unreached???
                unimplemented!()
            },
         }
    }

    pub fn get_initializer_type(init: &Initializer, env: &Env) -> Result<Type, CodeGenError> {
        match init {
            Initializer::Simple(expr, _pos) => TypeUtil::get_type(expr, env),
            Initializer::ArrayOrStruct(_init, _pos) => {
                unimplemented!()
            }
        }
    }

}