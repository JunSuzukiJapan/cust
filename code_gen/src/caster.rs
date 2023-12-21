// use crate::parser::{AST, ExprAST, BinOp, Type, Pointer, Block, Params, StructDefinition, StructField, NumberType, Function, FunProto, FunOrProto, EnumDefinition, Enumerator};
// use crate::parser::{CompileError, Declaration, DeclarationSpecifier};
use super::CodeGenError;
// use crate::compiler::CompiledValue;
// use super::Env;
// use super::env::{BreakCatcher, ContinueCatcher, TypeOrUnion};
use crate::parser::Type;
// #[cfg(test)]
// use crate::parser::{SpecifierQualifier, DirectDeclarator, Declarator, Defines, Param};
// use crate::parser::{Switch, Case};

// use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
// use inkwell::execution_engine::ExecutionEngine;
// use inkwell::module::Module;
use inkwell::values::{AnyValueEnum, AnyValue};
// use inkwell::types::{BasicTypeEnum, AnyTypeEnum};
// use inkwell::values::IntMathValue;
// use inkwell::types::IntType;
use parser::NumberType;
// use inkwell::values::{BasicValueEnum, BasicMetadataValueEnum, AnyValueEnum, AnyValue, FunctionValue, InstructionOpcode, PointerValue, InstructionValue, BasicValue, IntValue};
// use inkwell::types::{BasicTypeEnum, AnyTypeEnum, FunctionType, BasicType, BasicMetadataTypeEnum};
// use inkwell::{IntPredicate, FloatPredicate};
// use inkwell::AddressSpace;
// use inkwell::types::AnyType;
// use inkwell::types::StructType;
use std::error::Error;

pub struct Caster;

impl Caster {
    pub fn gen_cast<'ctx>(builder: &Builder<'ctx>, ctx: &'ctx Context, value: &AnyValueEnum<'ctx>, from_type: &Type, to_type: &Type) -> Result<AnyValueEnum<'ctx>, Box<dyn Error>> {
        let from_t = from_type.get_number_type()?;
        let to_t = to_type.get_number_type()?;

        match (from_t, to_t) {
            // same types
            (NumberType::Char, NumberType::Char) |
            (NumberType::Short, NumberType::Short) |
            (NumberType::Int, NumberType::Int) |
            (NumberType::Long, NumberType::Long) |
            (NumberType::LongLong, NumberType::LongLong) |
            (NumberType::UnsignedChar, NumberType::UnsignedChar) |
            (NumberType::UnsignedShort, NumberType::UnsignedShort) |
            (NumberType::UnsignedInt, NumberType::UnsignedInt) |
            (NumberType::UnsignedLong, NumberType::UnsignedLong) |
            (NumberType::UnsignedLongLong, NumberType::UnsignedLongLong) |
            (NumberType::Float, NumberType::Float) |
            (NumberType::Double, NumberType::Double) => Ok(*value),
            // char -> unsigned char
            (NumberType::Char, NumberType::UnsignedChar) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_char_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },


            _ => unimplemented!(),
        }

        // match (from_type, to_type) {
        //     (AnyTypeEnum::IntType(_t1), BasicTypeEnum::IntType(t2)) => {
        //         let is_signed = unimplemented!();
        //         let result = builder.build_int_cast_sign_flag(value.into_int_value(), *t2, is_signed, "int_cast");
        //         // let result = value.into_int_value().const_cast(*t2, is_signed);
        //         Ok(result.as_any_value_enum())
        //     },
        //     (AnyTypeEnum::IntType(_t1), BasicTypeEnum::FloatType(t2)) => {
        //         let result = value.into_int_value().const_signed_to_float(*t2);
        //         Ok(result.as_any_value_enum())
        //     },
        //     (AnyTypeEnum::FloatType(_t1), BasicTypeEnum::IntType(_t2)) => {




        //         unimplemented!()
        //     },
        //     (AnyTypeEnum::FloatType(_t1), BasicTypeEnum::FloatType(_t2)) => {




        //         unimplemented!()
        //     },
        //     _ => {
        //         return Err(Box::new(CompileError::cannot_cast(None, &from_type, to_type)));
        //     }
        // }
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cast_byte_to_int() {

    }
}