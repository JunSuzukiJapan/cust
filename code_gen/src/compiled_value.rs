use inkwell::values::AnyValueEnum;
use crate::parser::Type;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct CompiledValue<'ctx> {
    typ: Rc<Type>,
    any_value: AnyValueEnum<'ctx>,
}

impl<'ctx> CompiledValue<'ctx> {
    pub fn new(typ: Rc<Type>, any_value: AnyValueEnum<'ctx>) -> CompiledValue<'ctx> {
        CompiledValue {
            typ: typ,
            any_value: any_value,
        }
    }

    pub fn get_type(&self) -> &Rc<Type> {
        &self.typ
    }

    pub fn get_value(&self) -> AnyValueEnum<'ctx> {
        self.any_value
    }
}