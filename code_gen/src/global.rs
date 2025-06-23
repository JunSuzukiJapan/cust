use std::sync::OnceLock;

use crate::parser::{NumberType};

pub struct Global {
    enum_tag_type: NumberType,
}

impl Global {
    fn new() -> Self {
        let enum_tag_type = NumberType::UnsignedInt;

        Global {
            enum_tag_type,
        }
    }

    #[inline]
    pub fn enum_tag_type(&self) -> &NumberType {
        &self.enum_tag_type
    }
}

pub fn global() -> &'static Global {
    static INSTANCE: OnceLock<Global> = OnceLock::new();
    INSTANCE.get_or_init(|| Global::new())
}

