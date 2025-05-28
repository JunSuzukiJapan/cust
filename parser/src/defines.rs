use crate::{ExprAST, Initializer};

use super::{ParserError, Type, ConstExpr, DeclarationSpecifier, Declarator, Params, NumberType, Pointer, BinOp};
use super::{StructDefinition, EnumDefinition, Position};

use std::collections::HashMap;
use std::rc::Rc;

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
        struct_type: Rc<Type>,
    },
    Union {
        union_type: Rc<Type>,
    },
    Enum {
        enum_type: Rc<Type>,
    },
    TypeDef {
        name: String,
        source: Rc<Type>,
    },
    _Self {
        self_type: Rc<Type>,
    }
}

impl DefineType {
    pub fn new_struct(name: &str, fields: StructDefinition, type_variables: Option<Vec<String>>) -> DefineType {
        let struct_type = Type::Struct { name: Some(name.to_string()), fields, type_variables };
        DefineType::Struct {
            struct_type: Rc::new(struct_type),
        }
    }

    pub fn new_union(name: &str, fields: StructDefinition, type_variables: Option<Vec<String>>) -> DefineType {
        let union_type = Type::Union { name: Some(name.to_string()), fields, type_variables };
        DefineType::Union {
            union_type: Rc::new(union_type),
        }
    }

    pub fn new_enum(name: &str, enum_def: EnumDefinition, type_variables: Option<Vec<String>>) -> DefineType {
        let enum_type = Type::Enum { name: name.to_string(), enum_def: enum_def, type_variables };
        DefineType::Enum {
            enum_type: Rc::new(enum_type),
        }
    }

    pub fn new_typedef(name: &str, typ: &Rc<Type>) -> DefineType {
        DefineType::TypeDef {
            name: name.to_string(),
            source: Rc::clone(typ),
        }
    }

    pub fn new_self_type(typ: &Rc<Type>) -> DefineType {
        DefineType::_Self {
            self_type: Rc::clone(typ),
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
    // generics types
    generics: Vec<HashMap<String, Rc<Type>>>,
}

impl Defines {
    pub fn new() -> Defines {
        Defines {
            local_maps: vec![(Vec::new())],
            global_maps: Maps::new(),
            generics: Vec::new(),
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

    pub fn add_new_generics(&mut self) {
        self.generics.push(HashMap::new());
    }

    pub fn remove_generics(&mut self) {
        self.generics.pop();
    }

    pub fn set_enum(&mut self, name: &str, enum_def: EnumDefinition, type_variables: Option<Vec<String>>, pos: &Position) -> Result<(), ParserError> {
        if self.exists_type(name) {
            return Err(ParserError::already_var_defined(name, pos.clone()));
        }

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().struct_map.insert(name.to_string(), DefineType::new_enum(name, enum_def, type_variables));
        }else{
            self.global_maps.type_map.insert(name.to_string(), DefineType::new_enum(name, enum_def.clone(), type_variables.clone()));
            self.global_maps.struct_map.insert(name.to_string(), DefineType::new_enum(name, enum_def, type_variables));
        }

        Ok(())
    }

    pub fn set_struct(&mut self, name: &str, struct_def: StructDefinition, type_variables: Option<Vec<String>>, pos: &Position) -> Result<(), ParserError> {
        if self.exists_type(name) {
            return Err(ParserError::already_var_defined(name, pos.clone()));
        }

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().struct_map.insert(name.to_string(), DefineType::new_struct(name, struct_def, type_variables));
        }else{
            self.global_maps.type_map.insert(name.to_string(), DefineType::new_struct(name, struct_def.clone(), type_variables.clone()));
            self.global_maps.struct_map.insert(name.to_string(), DefineType::new_struct(name, struct_def, type_variables));
        }

        Ok(())
    }

    pub fn set_union(&mut self, name: &str, struct_def: StructDefinition, type_variables: Option<Vec<String>>, pos: &Position) -> Result<(), ParserError> {
        if self.exists_type(name) {
            return Err(ParserError::already_var_defined(name, pos.clone()));
        }

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().struct_map.insert(name.to_string(), DefineType::new_union(name, struct_def, type_variables));
        }else{
            self.global_maps.type_map.insert(name.to_string(), DefineType::new_union(name, struct_def.clone(), type_variables.clone()));
            self.global_maps.struct_map.insert(name.to_string(), DefineType::new_union(name, struct_def, type_variables));
        }

        Ok(())
    }

    pub fn set_var(&mut self, name: &str, typ: &Type, init_expr: Option<Initializer>, pos: &Position) -> Result<(), ParserError> {
        if self.cannot_define_var(name) {
            return Err(ParserError::already_var_defined(name, pos.clone()));
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
            return Err(ParserError::already_var_defined(name, pos.clone()));
        }

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().def_map.insert(name.to_string(), DefineVar::new_const(name, typ, init_expr));
        }else{
            self.global_maps.def_map.insert(name.to_string(), DefineVar::new_const(name, typ, init_expr));
         }

        Ok(())
    }

    pub fn set_self_type(&mut self, typ: &Rc<Type>) -> Result<(), ParserError> {
        let def_type = DefineType::new_self_type(typ);

        if self.local_maps.last().unwrap().len() > 0 {
            self.local_maps.last_mut().unwrap().last_mut().unwrap().type_map.insert("Self".to_string(), def_type);
        }else{
            self.global_maps.type_map.insert("Self".to_string(), def_type);
        }

        return Ok(());
    }

    pub fn get_self_type(&self, pos: &Position) -> Result<&Rc<Type>, ParserError> {
        let typ = self.get_type("Self").ok_or(ParserError::access_self_type_without_impl(pos.clone()))?;
        Ok(typ)
    }

    // #[allow(mutable_borrow_reservation_conflict)]
    pub fn set_typedef(&mut self, typedef_name: &str, typ: &Rc<Type>, pos: &Position) -> Result<(), ParserError> {
        if self.exists_type(typedef_name) {
            return Err(ParserError::already_type_defined_in_env(typedef_name, pos.clone()));
        }

        if let Type::Struct { name, fields, .. } = typ.as_ref() {
            if let Some(id) = name {
                if ! fields.has_fields() {
                    let t = &self.get_struct_type(id).ok_or(ParserError::no_such_a_struct(id, pos.clone()))?;
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
            return Err(ParserError::already_type_defined_in_env(name, pos.clone()));
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
            item = self.local_maps.last().unwrap().last().unwrap().def_map.get(name).ok_or(ParserError::no_such_a_constant(name, pos.clone()))?;
        }else{
            item = self.global_maps.def_map.get(name).ok_or(ParserError::no_such_a_constant(name, pos.clone()))?;
        }

        match item {
            DefineVar::Const{init_expr, ..} => {
                Ok(init_expr.clone())
            },
            _ => Err(ParserError::no_such_a_constant(name, pos.clone())),
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

    pub fn get_type(&self, name: &str) -> Option<&Rc<Type>> {
        // check generics
        if self.generics.len() > 0 {
            let map = self.generics.last().unwrap();

            if let Some(rc_type) = map.get(name) {
                return Some(&rc_type);
            }
        }

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

    // pub fn get_type_from_expr(expr_ast: &ExprAST, defs: &Defines) -> Result<Rc<Type>, ParserError> {
    //     match expr_ast {
    //         ExprAST::Assign(left, _right, _pos) => {
    //             Self::get_type_from_expr(&left, defs)
    //         },
    //         ExprAST::OpAssign(_op, left, _right, _pos) => {
    //             Self::get_type_from_expr(&left, defs)
    //         },
    //         ExprAST::Char(_, _pos) => Ok(Rc::new(Type::Number(NumberType::Char))),
    //         ExprAST::Int(_, _pos) => Ok(Rc::new(Type::Number(NumberType::Int))),
    //         ExprAST::Short(_, _pos) => Ok(Rc::new(Type::Number(NumberType::Short))),
    //         ExprAST::Long(_, _pos) => Ok(Rc::new(Type::Number(NumberType::Long))),
    //         ExprAST::LongLong(_, _pos) => Ok(Rc::new(Type::Number(NumberType::LongLong))),
    //         ExprAST::UChar(_, _pos) => Ok(Rc::new(Type::Number(NumberType::UnsignedChar))),
    //         ExprAST::UInt(_, _pos) => Ok(Rc::new(Type::Number(NumberType::UnsignedInt))),
    //         ExprAST::UShort(_, _pos) => Ok(Rc::new(Type::Number(NumberType::UnsignedShort))),
    //         ExprAST::ULong(_, _pos) => Ok(Rc::new(Type::Number(NumberType::UnsignedLong))),
    //         ExprAST::ULongLong(_, _pos) => Ok(Rc::new(Type::Number(NumberType::UnsignedLongLong))),
    //         ExprAST::StringLiteral(_string, _pos) => Ok(Rc::new(Type::Pointer(Pointer::new(false, false), Box::new(Rc::new(Type::Number(NumberType::Char)))))),
    //         ExprAST::Float(_, _pos) => Ok(Rc::new(Type::Number(NumberType::Float))),
    //         ExprAST::Double(_, _pos) => Ok(Rc::new(Type::Number(NumberType::Double))),
    //         ExprAST::BinExpr(op, left, _right, _pos) => {
    //             match op {
    //                 BinOp::Equal | BinOp::NotEqual | BinOp::Less | BinOp::LessEqual | BinOp::Greater | BinOp::GreaterEqual => {
    //                     Ok(Rc::new(Type::Number(NumberType::_Bool)))
    //                 },
    //                 _ => Self::get_type_from_expr(&left, defs),
    //             }
    //         },
    //         ExprAST::UnaryMinus(expr, _pos) => Self::get_type_from_expr(&expr, defs),
    //         ExprAST::UnaryTilda(expr, _pos) => Self::get_type_from_expr(&expr, defs),
    //         // ExprAST::UnaryNot(expr) => expr.get_type(defs),
    //         ExprAST::UnarySizeOfExpr(_expr, _pos) => Ok(Rc::new(Type::Number(NumberType::Int))),
    //         ExprAST::UnarySizeOfTypeName(_typ, _pos) => Ok(Rc::new(Type::Number(NumberType::Int))),
    //         ExprAST::ArrayAccess(expr, index_list, pos) => {
    //             let index_len = index_list.len();

    //             let typ = Self::get_type_from_expr(&expr, defs)?;
    //             if let Type::Array { name: _, typ: item_type, size_list } = typ.as_ref() {  // Array
    //                 let len = size_list.len();

    //                 if len == index_len {
    //                     return Ok(*item_type.clone());

    //                 } if len < index_len {
    //                     return Err(ParserError::array_index_is_too_long(pos.clone()))

    //                 }else{  // len < index_len
    //                     let t = Type::new_pointer_type(*item_type.clone(), false, false);
    //                     Ok(Rc::new(t))
    //                 }
    //             }else if let Type::Pointer(_, elem_type) = typ.as_ref() {             // Pointer
    //                 if index_len > 1 {
    //                     return Err(ParserError::array_index_is_too_long(pos.clone()));
    //                 }

    //                 // let t = Type::new_pointer_type(*elem_type.clone(), false, false);
    //                 // Ok(Rc::new(t))
    //                 Ok(Rc::clone(&elem_type))

    //             }else{
    //                 return Err(ParserError::not_array(*expr.clone(), pos.clone()));
    //             }
    //         },
    //         ExprAST::Symbol(name, pos) => {
    //             let typ = defs.get_type(&name).ok_or(ParserError::no_such_a_variable(&name, pos.clone()))?;
    //             Ok(typ.clone())
    //         },
    //         ExprAST::SelfStaticSymbol(_sym, pos) => {
    //             let (typ, _sq, _expr) = defs.get_ptr("Self").ok_or(ParserError::access_self_type_without_impl(pos.clone()))?;
    //             Ok(typ.clone())
    //         },
    //         ExprAST::TypeMemberAccess(class_name, var_name, pos) => {
    //             let (typ, _sq, _global_value) = defs.get_class_var(class_name, var_name).ok_or(ParserError::no_such_a_variable(&format!("{}::{}", class_name, var_name), pos.clone()))?;
    //             Ok(Rc::clone(typ))
    //         },
    //         ExprAST::_self(pos) => {
    //             let (typ, _sq, _expr) = defs.get_ptr("self").ok_or(ParserError::access_self_without_impl(pos.clone()))?;
    //             Ok(Rc::clone(typ))
    //         },
    //         ExprAST::Not(_expr, _pos) => Ok(Rc::new(Type::Number(NumberType::_Bool))),
    //         // ExprAST::ExpressionPair(_, right, _pos) => TypeUtil::get_type(&right, defs),
    //         ExprAST::Cast(typ, _, _pos) => Ok(typ.clone()),
    //         ExprAST::PreInc(name, _sym_pos, pos) => {
    //             let typ = defs.get_type(&name).ok_or(ParserError::no_such_a_variable(&name, pos.clone()))?;
    //             Ok(typ.clone())
    //         },
    //         ExprAST::PreDec(name, _sym_pos, pos) => {
    //             let typ = defs.get_type(&name).ok_or(ParserError::no_such_a_variable(&name, pos.clone()))?;
    //             Ok(typ.clone())
    //         },
    //         ExprAST::PostInc(name, _sym_pos, pos) => {
    //             let typ = defs.get_type(&name).ok_or(ParserError::no_such_a_variable(&name, pos.clone()))?;
    //             Ok(typ.clone())
    //         },
    //         ExprAST::PostDec(name, _sym_pos, pos) => {
    //             let typ = defs.get_type(&name).ok_or(ParserError::no_such_a_variable(&name, pos.clone()))?;
    //             Ok(typ.clone())
    //         },
    //         ExprAST::PreIncMemberAccess(expr, _pos) => {
    //             Self::get_type_from_expr(expr, defs)
    //         },
    //         ExprAST::PostIncMemberAccess(expr, _pos) => {
    //             Self::get_type_from_expr(expr, defs)
    //         },
    //         ExprAST::PreDecMemberAccess(expr, _pos) => {
    //             Self::get_type_from_expr(expr, defs)
    //         },
    //         ExprAST::PostDecMemberAccess(expr, _pos) => {
    //             Self::get_type_from_expr(expr, defs)
    //         },
    //         ExprAST::UnaryGetAddress(boxed_ast, _pos) => {
    //             let ast = &*boxed_ast;
    //             let t = TypeUtil::get_type(ast, defs)?;
    //             Ok(Rc::new(Type::new_pointer_type(t, false, false)))
    //         },
    //         ExprAST::UnaryPointerAccess(boxed_ast, pos) => {  // *pointer
    //             let ast = &**boxed_ast;
    //             let typ = TypeUtil::get_type(ast, defs)?;

    //             match typ.as_ref() {
    //                 Type::Pointer(_p, t) => {
    //                     Ok(*t.clone())
    //                 },
    //                 _ => Err(ParserError::not_pointer(&typ, pos.clone())),
    //             }
    //         },
    //         ExprAST::MemberAccess(boxed_ast, field_name, pos) => {  // some_var.field
    //             let ast = &*boxed_ast;
    //             let typ = TypeUtil::get_type(ast, defs)?;

    //             match typ.as_ref() {
    //                 Type::Struct { name: _, fields, type_variables: _ } => {
    //                     let t = fields.get_type(&field_name).ok_or(ParserError::type_has_not_member(&field_name, pos.clone()))?;
    //                     Ok(t.clone())
    //                 },
    //                 Type::Union { name: _, fields, type_variables: _ } => {
    //                     let t = fields.get_type(&field_name).ok_or(ParserError::type_has_not_member(&field_name, pos.clone()))?;
    //                     Ok(t.clone())
    //                 },
    //                 _ => {
    //                     return Err(ParserError::type_has_not_member(&format!("{:?}", &typ), pos.clone()));
    //                 },
    //             }
    //         },
    //         ExprAST::PointerAccess(boxed_ast, field_name, pos) => {  // some_var->field
    //             let ast = &*boxed_ast;
    //             let typ = Self::get_type_from_expr(ast, defs)?;
    //             let pointed_type = typ.get_pointed_type(pos)?;

    //             match pointed_type {
    //                 Type::Struct { name: _, fields, type_variables: _ } => {
    //                     let t = fields.get_type(&field_name).ok_or(ParserError::type_has_not_member(&field_name, pos.clone()))?;
    //                     Ok(t.clone())
    //                 },
    //                 Type::Union { name: _, fields, type_variables: _ } => {
    //                     let t = fields.get_type(&field_name).ok_or(ParserError::type_has_not_member(&field_name, pos.clone()))?;
    //                     Ok(t.clone())
    //                 },
    //                 _ => {
    //                     return Err(ParserError::type_has_not_member(&format!("{:?}", &typ), pos.clone()));
    //                 },
    //             }
    //         },
    //         ExprAST::TernaryOperator(_, e1, _, _pos) => {
    //             Self::get_type_from_expr(&e1, defs)
    //         },
    //         ExprAST::CallFunction(fun, _args, _pos) => {
    //             let f_type = match &**fun {
    //                 ExprAST::Symbol(name, pos2) => {
    //                     if let Some((fun_type, _f_value)) = defs.get_function(name) {
    //                         fun_type
    //                     }else{
    //                         return Err(ParserError::not_function(name, pos2.clone()));
    //                     }
    //                 },
    //                 ExprAST::MemberAccess(ast, fun_name, pos2) => {
    //                     let typ = Self::get_type_from_expr(ast, defs)?;
    //                     let class_name = typ.get_type_name();
    //                     let method_name = CodeGen::make_function_name_in_impl(&class_name, fun_name);

    //                     if let Some((fun_type, _f_value)) = defs.get_function(&method_name) {
    //                         fun_type
    //                     }else{
    //                         return Err(ParserError::not_function(&method_name, pos2.clone()));
    //                     }
    //                 },
    //                 _ => panic!("'{:?}' is not function.", **fun),
    //             };

    //             Ok(f_type.get_return_type().clone())
    //         },
    //         ExprAST::StructLiteral(struct_literal) => {
    //             let typ = struct_literal.get_type();
    //             Ok(Rc::clone(typ))
    //         },
    //         ExprAST::UnionLiteral(typ, _map, _pos) => {
    //             Ok(Rc::clone(&typ))
    //         },
    //         ExprAST::UnionConstLiteral(typ, _map, _pos) => {
    //             Ok(Rc::clone(&typ))
    //         },
    //         ExprAST::EnumLiteral(typ, _index, _literal, _pos) => {
    //             // let typ = literal.get_type();
    //             Ok(Rc::clone(&typ))
    //         },
    //         ExprAST::TupleLiteral(tuple_literal, _pos) => {
    //             let expr_list = tuple_literal.get_expr_list();
    //             let mut type_list = Vec::new();

    //             for e in expr_list {
    //                 let typ = Self::get_type_from_expr(e, defs)?;
    //                 type_list.push(typ);
    //             }

    //             let t = Type::Tuple(type_list);
    //             Ok(Rc::new(t))
    //         },
    //         ExprAST::TupleMemberAccess(expr_ast, index, pos) => {
    //             let typ = Self::get_type_from_expr(expr_ast, defs)?;
    //             let ptr = Rc::as_ptr(&typ);
    //             if let Type::Tuple(vec) = unsafe{&*ptr} {
    //                 let len = vec.len();
    //                 if *index >= len {
    //                     return Err(ParserError::tuple_index_too_big(len, *index, pos.clone()));
    //                 }

    //                 Ok(Rc::clone(&vec[*index]))

    //             }else{
    //                 Err(ParserError::not_tuple_in_tuple_access_by_index(*expr_ast.clone(), pos.clone()))
    //             }
    //         },
    //         ExprAST::TuplePointerAccess(_expr_ast, _index, _pos) => {





    //             unimplemented!()
    //         },
    //         // ExprAST::DefVar { specifiers: _, declarations: _, pos: _ } => {
    //         //     // maybe unreached???
    //         //     unimplemented!()
    //         // },
    //      }
    // }

    pub fn get_struct_type(&self, name: &str) -> Option<&Rc<Type>> {
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

    pub fn get_union_type(&self, name: &str) -> Option<&Rc<Type>> {
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
                            DefineType::Union {union_type} => {
                                return Some(&union_type);
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
                DefineType::Union {union_type} => {
                    Some(&union_type)
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

    pub fn check_exist(&self, name: &str, pos: &Position) -> Result<(), ParserError> {
        if self.exists_var(name) {
            return Err(ParserError::already_var_defined(name, pos.clone()));
        }
        if self.exists_type(name) {
            return Err(ParserError::already_type_defined_in_env(name, pos.clone()));
        }

        Ok(())
    }

    pub fn add_generic_type(&mut self, name: &str, pos: &Position) -> Result<Rc<Type>, ParserError> {
        let map = self.generics.last_mut().unwrap();
        if map.contains_key(name) {
            return Err(ParserError::already_generics_type_defined(name.to_string(), pos.clone()));
        }

        let typ = Type::TypeVariable(name.to_string());
        let origin = Rc::new(typ);
        let clone = Rc::clone(&origin);
        map.insert(name.to_string(), origin);

        Ok(clone)
    }
}
