use inkwell::values::AnyValueEnum;
use crate::parser::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct CompiledValue<'ctx> {
    typ: Type,
    any_value: AnyValueEnum<'ctx>,
}

impl<'ctx> CompiledValue<'ctx> {
    pub fn new(opt_type: Type, opt_any_value: AnyValueEnum<'ctx>) -> CompiledValue<'ctx> {
        CompiledValue {
            typ: opt_type,
            any_value: opt_any_value,
        }
    }

    pub fn get_type(&self) -> &Type {
        &self.typ
    }

    pub fn get_value(&self) -> AnyValueEnum<'ctx> {
        self.any_value
    }
}