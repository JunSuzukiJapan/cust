use std::sync::OnceLock;

use crate::parser::{NumberType};

pub struct Global {
    pub enum_tag_type: NumberType,
}

impl Global {
    fn new() -> Self {
        let enum_tag_type = NumberType::Long;

        Global {
            enum_tag_type,
        }
    }
}

pub fn global() -> &'static Global {
    static INSTANCE: OnceLock<Global> = OnceLock::new();
    INSTANCE.get_or_init(|| Global::new())
}

// pub fn enum_tag_type() -> Type {
//     let global = global();
//     let num_type = global.enum_tag_type.clone();
//     Type::Number(num_type)
// }