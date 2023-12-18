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
// use inkwell::context::Context;
// use inkwell::execution_engine::ExecutionEngine;
// use inkwell::module::Module;
use inkwell::values::{AnyValueEnum, AnyValue};
use inkwell::types::{BasicTypeEnum, AnyTypeEnum};
// use inkwell::values::{BasicValueEnum, BasicMetadataValueEnum, AnyValueEnum, AnyValue, FunctionValue, InstructionOpcode, PointerValue, InstructionValue, BasicValue, IntValue};
// use inkwell::types::{BasicTypeEnum, AnyTypeEnum, FunctionType, BasicType, BasicMetadataTypeEnum};
// use inkwell::{IntPredicate, FloatPredicate};
// use inkwell::AddressSpace;
// use inkwell::types::AnyType;
// use inkwell::types::StructType;
use std::error::Error;

pub struct Caster;

impl Caster {
    pub fn gen_cast<'ctx>(builder: &Builder<'ctx>, value: &AnyValueEnum<'ctx>, from_type: &Type, to_type: &Type) -> Result<AnyValueEnum<'ctx>, Box<dyn Error>> {
        unimplemented!()

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