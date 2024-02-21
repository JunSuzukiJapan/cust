use crate::ast::Initializer;

use super::{ParserError, Type, ConstExpr, DeclarationSpecifier, Declarator, Params};
use super::{StructDefinition, EnumDefinition, Position};

use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
enum DefineVar {
    Variable {
        name: String,
        typ: Type,
        init_expr: Option<Initializer>,
    },
    Const {
        name: String,
        typ: Type,
        init_expr: ConstExpr,
    },
    Function {
        name: String,
        specifiers: DeclarationSpecifier,
        declarator: Declarator,
        params: Params,
    },
}

impl DefineVar {
    pub fn new_var(name: &str, typ: &Type, init_expr: Option<Initializer>) -> DefineVar {
        DefineVar::Variable {
            name: name.to_string(),
            typ: typ.clone(),
            init_expr: init_expr,
        }
    }

    pub fn new_const(name: &str, typ: &Type, init_expr: ConstExpr) -> DefineVar {
        DefineVar::Const {
            name: name.to_string(),
            typ: typ.clone(),
            init_expr: init_expr,
        }
    }

    pub fn new_function(name: &str, specifiers: DeclarationSpecifier, declarator: Declarator, params: Params) -> DefineVar {
        DefineVar::Function {
            name: name.to_string(),
            specifiers: specifiers,
            declarator: declarator,
            params: params,
        }
    }
}

/*
 * DefineType
 */
#[derive(Debug, PartialEq, Clone)]
enum DefineType {
    Struct {
        struct_type: Type,
    },
    Union {
        union_type: Type,
    },
    Enum {
        enum_type: Type,
    },
    TypeDef {
        name: String,
        source: Type,
    },
    _Self {
        self_type: Type,
    }
}

impl DefineType {
    pub fn new_struct(name: &str, fields: StructDefinition) -> DefineType {
        let struct_type = Type::Struct { name: Some(name.to_string()), fields };
        DefineType::Struct {
            struct_type
        }
    }

    pub fn new_union(name: &str, fields: StructDefinition) -> DefineType {
        let union_type = Type::Union { name: Some(name.to_string()), fields };
        DefineType::Union {
            union_type
        }
    }

    pub fn new_enum(name: &str, enum_def: EnumDefinition) -> DefineType {
        let enum_type = Type::Enum { name: Some(name.to_string()), enum_def: enum_def };
        DefineType::Enum {
            enum_type
        }
    }

    pub fn new_typedef(name: &str, typ: &Type) -> DefineType {
        DefineType::TypeDef {
            name: name.to_string(),
            source: typ.clone(),
        }
    }

    pub fn new_self_type(typ: &Type) -> DefineType {
        DefineType::_Self {
            self_type: typ.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Maps {
    pub def_map: HashMap<String, DefineVar>,
    pub type_map: HashMap<String, DefineType>,
    pub struct_map: HashMap<String, DefineType>,
}

impl Maps {
    pub fn new() -> Maps {
        Maps {
            def_map: HashMap::new(),
            type_map: HashMap::new(),
            struct_map: HashMap::new(),
        }
    }
}

/*
 * Defines
 */
#[derive(Debug, PartialEq, Clone)]
pub struct Defines {
    // locals
    local_maps: Vec<Vec<Maps>>,
    // globals
    global_maps: Maps,
}

impl Defines {
    pub fn new() -> Defines {
        Defines {
            local_maps: vec![(Vec::new())],
            global_maps: Maps::new(),
        }
    }

    pub fn add_new_function_local(&mut self) {
        self.local_maps.push(vec![Maps::new()]);
    }

    pub fn remove_function_local(&mut self) {
        self.local_maps.pop();
    }

    pub fn new_local(&mut self) {
        self.local_maps.last_mut().unwrap().push(Maps::new());
    }

    pub fn remove_local(&mut self) {
        self.local_maps.last_mut().unwrap().pop();
    }

    pub fn set_enum(&mut self, name: &str, enum_def: EnumDefinition, pos: &Position) -> Result<(), ParserError> {
        if self.exists_type(name) {
            return Err(ParserError::already_var_defined(pos.clone(), name));
        }

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().struct_map.insert(name.to_string(), DefineType::new_enum(name, enum_def));
        }else{
            self.global_maps.type_map.insert(name.to_string(), DefineType::new_enum(name, enum_def.clone()));
            self.global_maps.struct_map.insert(name.to_string(), DefineType::new_enum(name, enum_def));
        }

        Ok(())
    }

    pub fn set_struct(&mut self, name: &str, struct_def: StructDefinition, pos: &Position) -> Result<(), ParserError> {
        if self.exists_type(name) {
            return Err(ParserError::already_var_defined(pos.clone(), name));
        }

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().struct_map.insert(name.to_string(), DefineType::new_struct(name, struct_def));
        }else{
            self.global_maps.type_map.insert(name.to_string(), DefineType::new_struct(name, struct_def.clone()));
            self.global_maps.struct_map.insert(name.to_string(), DefineType::new_struct(name, struct_def));
        }

        Ok(())
    }

    pub fn set_union(&mut self, name: &str, struct_def: StructDefinition, pos: &Position) -> Result<(), ParserError> {
        if self.exists_type(name) {
            return Err(ParserError::already_var_defined(pos.clone(), name));
        }

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().struct_map.insert(name.to_string(), DefineType::new_union(name, struct_def));
        }else{
            self.global_maps.type_map.insert(name.to_string(), DefineType::new_union(name, struct_def.clone()));
            self.global_maps.struct_map.insert(name.to_string(), DefineType::new_union(name, struct_def));
        }

        Ok(())
    }

    pub fn set_var(&mut self, name: &str, typ: &Type, init_expr: Option<Initializer>, pos: &Position) -> Result<(), ParserError> {
        if self.cannot_define_var(name) {
            return Err(ParserError::already_var_defined(pos.clone(), name));
        }

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().def_map.insert(name.to_string(), DefineVar::new_var(name, typ, init_expr));
        }else{
            self.global_maps.def_map.insert(name.to_string(), DefineVar::new_var(name, typ, init_expr));
        }

        Ok(())
    }

    pub fn set_const(&mut self, name: &str, typ: &Type, init_expr: ConstExpr, pos: &Position) -> Result<(), ParserError> {
        if self.cannot_define_var(name) {
            return Err(ParserError::already_var_defined(pos.clone(), name));
        }

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().def_map.insert(name.to_string(), DefineVar::new_const(name, typ, init_expr));
        }else{
            self.global_maps.def_map.insert(name.to_string(), DefineVar::new_const(name, typ, init_expr));
         }

        Ok(())
    }

    pub fn set_self_type(&mut self, typ: &Type) -> Result<(), ParserError> {
        let def_type = DefineType::new_self_type(typ);

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().type_map.insert("Self".to_string(), def_type);
        }else{
            self.global_maps.type_map.insert("Self".to_string(), def_type);
        }

        return Ok(());
    }

    pub fn get_self_type(&self, pos: &Position) -> Result<&Type, ParserError> {
        let typ = self.get_type("Self").ok_or(ParserError::access_self_type_without_impl(pos.clone()))?;
        Ok(typ)
    }

    // #[allow(mutable_borrow_reservation_conflict)]
    pub fn set_typedef(&mut self, typedef_name: &str, typ: &Type, pos: &Position) -> Result<(), ParserError> {
        if self.exists_type(typedef_name) {
            return Err(ParserError::already_type_defined_in_env(pos.clone(), typedef_name));
        }

        if let Type::Struct { name, fields } = typ {
            if let Some(id) = name {
                if ! fields.has_fields() {
                    let t = &self.get_struct_type(id).ok_or(ParserError::no_such_a_struct(pos.clone(), id))?;
                    let def_type = DefineType::new_typedef(typedef_name, t);
                    if self.local_maps.last().unwrap().len() > 0 {
                        self.local_maps.last_mut().unwrap().last_mut().unwrap().type_map.insert(typedef_name.to_string(), def_type);
                    }else{
                        self.global_maps.type_map.insert(typedef_name.to_string(), def_type);
                    }

                    return Ok(());
                }
            }
        }

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().type_map.insert(typedef_name.to_string(), DefineType::new_typedef(typedef_name, typ));
        }else{
            self.global_maps.type_map.insert(typedef_name.to_string(), DefineType::new_typedef(typedef_name, typ));
         }

        Ok(())
    }

    pub fn set_function(&mut self, name: &str, specifiers: DeclarationSpecifier, declarator: Declarator, params: Params, pos: &Position) -> Result<(), ParserError> {
        if self.exists_type(name) {
            return Err(ParserError::already_type_defined_in_env(pos.clone(), name));
        }

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().def_map.insert(name.to_string(), DefineVar::new_function(name, specifiers, declarator, params));
        }else{
            self.global_maps.def_map.insert(name.to_string(), DefineVar::new_function(name, specifiers, declarator, params));
        }

        Ok(())
    }

    pub fn get_const(&self, name: &str, pos: &Position) -> Result<ConstExpr, ParserError> {
        let item;
        if self.local_maps.last().unwrap().len() > 0 {
            item = self.local_maps.last().unwrap().last().unwrap().def_map.get(name).ok_or(ParserError::no_such_a_constant(pos.clone(), name))?;
        }else{
            item = self.global_maps.def_map.get(name).ok_or(ParserError::no_such_a_constant(pos.clone(), name))?;
        }

        match item {
            DefineVar::Const{init_expr, ..} => {
                Ok(init_expr.clone())
            },
            _ => Err(ParserError::no_such_a_constant(pos.clone(), name)),
        }
    }

    pub fn is_type(&self, name: &str) -> bool {
        // check locals
        if self.local_maps.last().unwrap().len() > 0 {
            let list = self.local_maps.last().unwrap();
            let len = list.len();
            for i in 0 .. (len - 1) {
                let index = (len - 1) - i;
                let map = &list[index].type_map;

                if let Some(obj) = map.get(name) {
                    match obj {
                        DefineType::Enum {..} => return true,
                        DefineType::Struct {..} => return true,
                        DefineType::TypeDef {..} => return true,
                        DefineType::Union {..} => return true,
                        DefineType::_Self {..} => return true,
                        // _ => false,
                    }
                }
            }
        }

        // check globals
        if let Some(obj) = self.global_maps.type_map.get(name) {
            match obj {
                DefineType::Enum {..} => true,
                DefineType::Struct {..} => true,
                DefineType::TypeDef {..} => true,
                DefineType::Union {..} => true,
                DefineType::_Self {..} => true,
                // _ => false,
            }
        }else{
            false
        }        
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        // check locals
        if self.local_maps.last().unwrap().len() > 0 {
            let list = self.local_maps.last().unwrap();
            let len = list.len();
            if len > 0 {
                for i in 0 .. (len - 1) {
                    let index = (len - 1) - i;
                    let map = &list[index].type_map;

                    if let Some(obj) = map.get(name) {
                        match obj {
                            DefineType::Enum {enum_type} => {
                                return Some(enum_type);
                            },
                            DefineType::Struct {struct_type} => {
                                return Some(&struct_type);
                            },
                            DefineType::TypeDef {source, ..} => {
                                return Some(source);
                            },
                            DefineType::Union { union_type } => {
                                return Some(union_type);
                            },
                            DefineType::_Self { self_type } => {
                                return Some(self_type);
                            }
                            // _ => None,
                        }
                    }
                }
            }
        }

        // check globals
        if let Some(obj) = self.global_maps.type_map.get(name) {
            match obj {
                DefineType::Enum {enum_type} => {
                    Some(enum_type)
                },
                DefineType::Struct {struct_type} => {
                    Some(&struct_type)
                },
                DefineType::TypeDef {source, ..} => {
                    Some(source)
                },
                DefineType::Union { union_type } => {
                    Some(union_type)
                },
                DefineType::_Self { self_type } => {
                    Some(self_type)
                },
                // _ => None,
            }
        }else{
            None
        }     
    }

    pub fn get_struct_type(&self, name: &str) -> Option<&Type> {
        // check locals
        if self.local_maps.last().unwrap().len() > 0 {
            let list = self.local_maps.last().unwrap();
            let len = list.len();
            if len > 0 {
                for i in 0 .. (len - 1) {
                    let index = (len - 1) - i;
                    let map = &list[index].struct_map;

                    if let Some(obj) = map.get(name) {
                        match obj {
                            DefineType::Enum {enum_type} => {
                                return Some(&enum_type);
                            },
                            DefineType::Struct {struct_type} => {
                                return Some(&struct_type);
                            },
                            _ => return None,
                        }
                    }
                }
            }
        }

        // check globals
        if let Some(obj) = self.global_maps.struct_map.get(name) {
            match obj {
                DefineType::Enum {enum_type} => {
                    Some(&enum_type)
                },
                DefineType::Struct {struct_type} => {
                    Some(&struct_type)
                },
                _ => None,
            }
        }else{
            None
        }    
    }

    pub fn cannot_define_var(&self, name: &str) -> bool {
        if self.local_maps.last().unwrap().len() > 0 {
            let map = &self.local_maps.last().unwrap().last().unwrap().def_map;
            if let Some(_obj) = map.get(name) {
                return true;
            }
        }else{
            if let Some(_obj) = self.global_maps.def_map.get(name) {
                return true;
            }
        }

        false
    }

    pub fn exists_var(&self, name: &str) -> bool {
        // check locals
        if self.local_maps.last().unwrap().len() > 0 {
            let list = self.local_maps.last().unwrap();
            let len = list.len();

            for i in 0 .. len {
                let index = (len - 1) - i;
                let map = &list[index].def_map;

                if let Some(_obj) = map.get(name) {
                    return true;
                }
            }
        }

        // check globals
        if let Some(_obj) = self.global_maps.def_map.get(name) {
            true
        }else{
            false
        }
    }

    pub fn exists_type(&self, name: &str) -> bool {
        // check locals
        if self.local_maps.last().unwrap().len() > 0 {
            let list = self.local_maps.last().unwrap();
            let len = list.len();
            if len > 0 {
                for i in 0 .. (len - 1) {
                    let index = (len - 1) - i;
                    let map = &list[index].type_map;

                    if let Some(_obj) = map.get(name) {
                        return true;
                    }
                }
            }
        }

        // check globals
        if let Some(_obj) = self.global_maps.type_map.get(name) {
            true
        }else{
            false
        }
    }
}
