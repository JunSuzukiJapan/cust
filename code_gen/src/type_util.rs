use crate::parser::{Type, NumberType, Pointer, BinOp};
use crate::Env;
use inkwell::context::Context;
use inkwell::values::{BasicValueEnum, BasicMetadataValueEnum, AnyValueEnum, AnyValue, FunctionValue, InstructionOpcode, PointerValue, InstructionValue, BasicValue, IntValue};
use std::error::Error;
use inkwell::types::{BasicTypeEnum, AnyTypeEnum, FunctionType, BasicType, BasicMetadataTypeEnum};
use inkwell::AddressSpace;
use inkwell::types::AnyType;
use parser::ExprAST;
use super::{CodeGen, CodeGenError};

pub struct TypeUtil;

impl TypeUtil {
    pub fn to_basic_type_enum<'a>(typ: &Type, ctx: &'a Context) -> Result<BasicTypeEnum<'a>, Box<dyn Error>> {
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
                let typ = Self::to_llvm_type(to_type, ctx)?;

                if typ.is_int_type() {
                    Ok(typ.into_int_type().ptr_type(AddressSpace::default()).into())
                }else if typ.is_float_type() {
                    Ok(typ.into_float_type().ptr_type(AddressSpace::default()).into())
                }else if typ.is_pointer_type() {
                    Ok(typ.into_pointer_type().ptr_type(AddressSpace::default()).into())
                }else if typ.is_array_type() {




                    unimplemented!()
                }else if typ.is_vector_type() {





                    unimplemented!()
                }else if typ.is_struct_type() {
                    Ok(typ.into_struct_type().ptr_type(AddressSpace::default()).into())
                }else{
                    Err(Box::new(CodeGenError::illegal_type_for_pointer(None, &to_type)))
                }

            },
            Type::Array { name: _, typ: type2, opt_size_list } => {
                let mut to_type = Self::to_basic_type_enum(type2, ctx)?;

                for opt_size in opt_size_list.iter().rev() {
                    let size = if let Some(sz) = opt_size {
                        sz.to_usize()?
                    }else{
                        0
                    };

                    to_type = to_type.array_type(size as u32).as_basic_type_enum();
                }

                Ok(to_type)
            },
            Type::Symbol(_name) => {





                unimplemented!()
            },
            Type::Struct { name, fields } => {
                let (struct_type, _index_map) = CodeGen::struct_from_struct_definition(name, fields, ctx)?;
                Ok(BasicTypeEnum::StructType(struct_type))
            },
            Type::Union { name, fields } => {
                let (_union_type, _index_map, _max_size, max_size_type) = CodeGen::union_from_struct_definition(name, fields, ctx)?;
                if let Some(typ) = max_size_type {
                    Ok(typ)
                }else{
                    if let Some(id) = name {
                        Err(Box::new(CodeGenError::union_has_no_field(None, Some(id.to_string()))))
                    }else{
                        Err(Box::new(CodeGenError::union_has_no_field(None, None)))
                    }
                }
            },
            Type::Enum { name, enum_def } => {



                unimplemented!()
            },
            _ => {
                Err(Box::new(CodeGenError::no_such_a_type(None, &typ.to_string())))
            },
        }
    }


    pub fn to_llvm_type<'a>(typ: &Type, ctx: &'a Context) -> Result<BasicMetadataTypeEnum<'a>, Box<dyn Error>> {
        Ok(Self::to_basic_type_enum(typ, ctx)?.into())
    }

    pub fn to_llvm_any_type<'a>(typ: &Type, ctx: &'a Context) -> Result<AnyTypeEnum<'a>, Box<dyn Error>> {
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
                let (struct_type, _tbl) = CodeGen::struct_from_struct_definition(&name, &fields, ctx)?;
                Ok(struct_type.as_any_type_enum())
            },
            Type::Symbol(_name) => {
                unimplemented!("'{}' to AnyTypeEnum", typ.to_string())
            },
            _ => {
                unimplemented!("'{}' to AnyTypeEnum", typ.to_string())
            },
        }
    }

    pub fn get_type(expr: &ExprAST, env: &Env) -> Result<Type, CodeGenError> {
        match expr {
            ExprAST::Assign(left, _right) => {
                // (*left).get_type(env)
                Self::get_type(&left, env)
            },
            ExprAST::Char(_) => Ok(Type::Number(NumberType::Char)),
            ExprAST::Int(_) => Ok(Type::Number(NumberType::Int)),
            ExprAST::Short(_) => Ok(Type::Number(NumberType::Short)),
            ExprAST::Long(_) => Ok(Type::Number(NumberType::Long)),
            ExprAST::LongLong(_) => Ok(Type::Number(NumberType::LongLong)),
            ExprAST::UChar(_) => Ok(Type::Number(NumberType::UnsignedChar)),
            ExprAST::UInt(_) => Ok(Type::Number(NumberType::UnsignedInt)),
            ExprAST::UShort(_) => Ok(Type::Number(NumberType::UnsignedShort)),
            ExprAST::ULong(_) => Ok(Type::Number(NumberType::UnsignedLong)),
            ExprAST::ULongLong(_) => Ok(Type::Number(NumberType::UnsignedLongLong)),
            ExprAST::StringLiteral(_string) => Ok(Type::Pointer(Pointer::new(false, false), Box::new(Type::Number(NumberType::Char)))),
            ExprAST::Float(_) => Ok(Type::Number(NumberType::Float)),
            ExprAST::Double(_) => Ok(Type::Number(NumberType::Double)),
            ExprAST::BinExpr(op, left, _right) => {
                match op {
                    BinOp::Equal | BinOp::NotEqual | BinOp::Less | BinOp::LessEqual | BinOp::Greater | BinOp::GreaterEqual => Ok(Type::Number(NumberType::_Bool)),
                    _ => Self::get_type(&left, env),
                }
            },
            ExprAST::UnaryMinus(expr) => Self::get_type(&expr, env),
            ExprAST::UnaryTilda(expr) => Self::get_type(&expr, env),
            // ExprAST::UnaryNot(expr) => expr.get_type(env),
            ExprAST::UnarySizeOfExpr(_expr) => Ok(Type::Number(NumberType::Int)),
            ExprAST::UnarySizeOfTypeName(_typ) => Ok(Type::Number(NumberType::Int)),
            ExprAST::ArrayAccess(expr, _index) => {
                let typ = Self::get_type(&expr, env)?;
                Ok(typ)
            },
            ExprAST::Symbol(name) => {
                let (typ, _expr) = env.get_ptr(&name).ok_or(CodeGenError::no_such_a_variable(None, &name))?;
                Ok(typ.clone())
            },
            ExprAST::_Self => {
                let (typ, _expr) = env.get_ptr("Self").ok_or(CodeGenError::access_self_type_without_impl(None))?;
                Ok(typ.clone())
            },
            ExprAST::_self => {
                let (typ, _expr) = env.get_ptr("self").ok_or(CodeGenError::access_self_without_impl(None))?;
                Ok(typ.clone())
            },
            ExprAST::Not(_expr) => Ok(Type::Number(NumberType::_Bool)),
            ExprAST::ExpressionPair(_, right) => TypeUtil::get_type(&right, env),
            ExprAST::Cast(typ, _) => Ok(typ.clone()),
            ExprAST::Inc(expr) => TypeUtil::get_type(&expr, env),
            ExprAST::Dec(expr) => TypeUtil::get_type(&expr, env),
            ExprAST::UnaryGetAddress(boxed_ast) => {
                let ast = &*boxed_ast;
                let t = TypeUtil::get_type(ast, env)?;
                Ok(Type::new_pointer_type(t, false, false))
            },
            ExprAST::UnaryPointerAccess(boxed_ast) => {
                let ast = &**boxed_ast;
                match ast {
                    ExprAST::Symbol(name) => {
                        let (typ, _ptr) = env.get_ptr(name).ok_or(CodeGenError::no_such_a_variable(None, name))?;
                        match typ {
                            Type::Pointer(_p, t) => {
                                Ok(*t.clone())
                            },
                            _ => Err(CodeGenError::not_pointer(None, typ)),
                        }
                    },
                    _ => unimplemented!(),
                }
            },
            ExprAST::MemberAccess(boxed_ast, field_name) => {
                let ast = &*boxed_ast;
                let typ = TypeUtil::get_type(ast, env)?;
                match typ {
                    Type::Struct { name: _, fields } => {
                        let t = fields.get_type(&field_name).ok_or(CodeGenError::type_has_not_member(None, &field_name))?;
                        Ok(t.clone())
                    },
                    Type::Union { name: _, fields } => {
                        let t = fields.get_type(&field_name).ok_or(CodeGenError::type_has_not_member(None, &field_name))?;
                        Ok(t.clone())
                    },
                    Type::Enum { name, enum_def } => {



                        unimplemented!()
                    },
                    _ => return Err(CodeGenError::type_has_not_member(None, &format!("{:?}", &typ))),
                }
            },
            ExprAST::PointerAccess(boxed_ast, field_name) => {
                let ast = &*boxed_ast;
                let typ = TypeUtil::get_type(ast, env)?;
                match typ {
                    Type::Struct { name: _, fields } => {
                        let t = fields.get_type(&field_name).ok_or(CodeGenError::type_has_not_member(None, &field_name))?;
                        Ok(t.clone())
                    },
                    Type::Union { name: _, fields } => {
                        let t = fields.get_type(&field_name).ok_or(CodeGenError::type_has_not_member(None, &field_name))?;
                        Ok(t.clone())
                    },
                    Type::Enum { name, enum_def } => {



                        unimplemented!()
                    },
                    _ => return Err(CodeGenError::type_has_not_member(None, &format!("{:?}", &typ))),
                }
            },
            ExprAST::TernaryOperator(_, e1, _) => {
                TypeUtil::get_type(&e1, env)
            },
            ExprAST::InitializerList(_) => {





                unimplemented!()
            },
            ExprAST::CallFunction(_, _) => {




                unimplemented!()
            },
            ExprAST::DefVar { specifiers, declarations } => {



                unimplemented!()
            },
         }
    }

}