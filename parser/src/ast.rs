#![allow(dead_code)]

use crate::ParserError;

// use super::tokenizer::*;
// // use super::compile_error::CompileError;
// // use super::types::*;
// // use super::ConstExpr;
// // use crate::compiler::Env;
// // use crate::Defines;

// #[derive(Debug, Clone)]
// pub struct DefVar {
//     def_vars: Vec<(String, Type)>,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct Block {
//     pub body: Vec<AST>,
// }

// #[derive(Debug, Clone)]
// struct BlockAnalyze<'a> {
//     def_vars: Vec<(Type, String)>,
//     inner_blocks: Vec<&'a Block>,
// }

// impl Block {
//     pub fn new_with_block(body: Vec<AST>) -> Block {
//         Block {
//             body: body,
//         }
//     }

//     fn analyze<'a>(&'a self) -> BlockAnalyze<'a> {
//         let mut def_vars = Vec::new();
//         let mut inner_blocks = Vec::new();

//         for (_i, ast) in self.body.iter().enumerate() {
//             if ast.is_def_var() {
//                 let (ds, decls) = ast.get_def_var().unwrap();
//                 let typ = ds.get_type();
//                 for decl in decls {
//                     let name = decl.get_declarator().get_name().to_string();
//                     def_vars.push((typ.clone(), name));
//                 }
//             }
//             if ast.is_block() {
//                 inner_blocks.push(ast.get_block().unwrap());
//             }
//         }

//         BlockAnalyze {
//             def_vars: def_vars,
//             inner_blocks: inner_blocks,
//         }
//     }
// }

use tokenizer::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    Comma,
    ShiftLeft,
    ShiftRight,
    BitAnd,
    BitOr,
    BitXor,
}

impl BinOp {
    pub fn from_token_type(token_type: &TokenType) -> Result<BinOp, ParserError> {
        match token_type {
            TokenType::Add => Ok(BinOp::Add),
            TokenType::Sub => Ok(BinOp::Sub),
            TokenType::Mul => Ok(BinOp::Mul),
            TokenType::Div => Ok(BinOp::Div),
            TokenType::Mod => Ok(BinOp::Mod),
            TokenType::Equal => Ok(BinOp::Equal),
            TokenType::NotEqual => Ok(BinOp::NotEqual),
            TokenType::Less => Ok(BinOp::Less),
            TokenType::LessEqual => Ok(BinOp::LessEqual),
            TokenType::Greater => Ok(BinOp::Greater),
            TokenType::GreaterEqual => Ok(BinOp::GreaterEqual),
            TokenType::And => Ok(BinOp::And),
            TokenType::Or => Ok(BinOp::Or),
            TokenType::ShiftLeft => Ok(BinOp::ShiftLeft),
            TokenType::ShiftRight => Ok(BinOp::ShiftRight),
            TokenType::BitAnd => Ok(BinOp::BitAnd),
            TokenType::BitOr => Ok(BinOp::BitOr),
            TokenType::BitXor => Ok(BinOp::BitXor),
            _ => Err(ParserError::no_such_a_operator(None, token_type.clone())),
        }
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct StorageClassSpecifier {
//     auto: bool,
//     register: bool,
//     static_: bool,
//     extern_: bool,
//     typedef: bool,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub enum TypeSpecifier {
//     Void,
//     Char,
//     Short,
//     Int,
//     Long,
//     Float,
//     Double,
//     Signed,
//     Unsigned,
//     StructOrUnionSpecifier,
//     EnumSpecifier,
//     TypedefName,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub enum TypeQualifier {
//     Const,
//     Volatile,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct SpecifierQualifier {
//     // storage class specifier
//     pub auto: bool,
//     pub register: bool,
//     pub static_: bool,
//     pub extern_: bool,
//     pub typedef: bool,

//     // type qualifier
//     pub const_: bool,
//     pub volatile: bool,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub enum SpecifierQualifierOrVariadic {
//     SQ(SpecifierQualifier),
//     Variadic,
// }

// impl SpecifierQualifier {
//     pub fn new() -> SpecifierQualifier {
//         SpecifierQualifier {
//             auto: false,
//             register: false,
//             static_: false,
//             extern_: false,
//             typedef: false,
//             const_: false,
//             volatile: false,
//         }
//     }

//     pub fn set_auto(&mut self) -> Result<(), CompileError> {
//         self.auto = true;
//         Ok(())
//     }

//     pub fn set_register(&mut self) -> Result<(), CompileError> {
//         self.register = true;
//         Ok(())
//     }

//     pub fn set_static(&mut self) -> Result<(), CompileError> {
//         self.static_ = true;
//         Ok(())
//     }

//     pub fn set_extern(&mut self) -> Result<(), CompileError> {
//         self.extern_ = true;
//         Ok(())
//     }

//     pub fn set_const(&mut self) -> Result<(), CompileError> {
//         self.const_ = true;
//         Ok(())
//     }

//     pub fn set_volatile(&mut self) -> Result<(), CompileError> {
//         self.volatile = true;
//         Ok(())
//     }

//     pub fn set_typedef(&mut self) -> Result<(), CompileError> {
//         self.typedef = true;
//         Ok(())
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct DeclarationSpecifier {
//     pub typ: Type,
//     pub specifier_qualifier: SpecifierQualifier,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub enum DeclarationSpecifierOrVariadic {
//     DS(DeclarationSpecifier),
//     Variadic,
// }

// impl DeclarationSpecifierOrVariadic {
//     #[inline]
//     pub fn from_declaration_specifier(ds: DeclarationSpecifier) -> DeclarationSpecifierOrVariadic {
//         DeclarationSpecifierOrVariadic::DS(ds)
//     }

//     #[inline]
//     pub fn new_variadic() -> DeclarationSpecifierOrVariadic {
//         DeclarationSpecifierOrVariadic::Variadic
//     }

//     #[inline]
//     pub fn is_variadic(&self) -> bool {
//         *self == DeclarationSpecifierOrVariadic::Variadic
//     }

//     #[inline]
//     pub fn get_declaration_specifier(&self) -> Option<&DeclarationSpecifier> {
//         match self {
//             DeclarationSpecifierOrVariadic::DS(ds) => Some(ds),
//             _ => None,
//         }
//     }

//     #[inline]
//     pub fn get_type(&self) -> Option<&Type> {
//         match self {
//             DeclarationSpecifierOrVariadic::DS(ds) => Some(ds.get_type()),
//             _ => None,
//         }
//     }
// }

// impl DeclarationSpecifier {
//     pub fn new(typ: Type, sq: SpecifierQualifier) -> DeclarationSpecifier {
//         DeclarationSpecifier {
//             typ: typ,
//             specifier_qualifier: sq,
//         }
//     }

//     #[inline]
//     pub fn get_type(&self) -> &Type {
//         &self.typ
//     }

//     #[inline]
//     pub fn set_type(&mut self, typ: &Type) {
//         self.typ = typ.clone();
//     }

//     #[inline]
//     pub fn get_specifier_qualifier(&self) -> &SpecifierQualifier {
//         &self.specifier_qualifier
//     }

//     #[inline]
//     pub fn is_typedef(&self) -> bool {
//         self.specifier_qualifier.typedef
//     }

//     #[inline]
//     pub fn is_const(&self) -> bool {
//         self.specifier_qualifier.const_
//     }

//     #[inline]
//     pub fn is_volatile(&self) -> bool {
//         self.specifier_qualifier.volatile
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct Declarator {
//     pointer: Option<Pointer>,
//     direct_declarator: Box<DirectDeclarator>,
// }

// impl Declarator {
//     pub fn new(pointer: Option<Pointer>, dd: DirectDeclarator) -> Declarator {
//         Declarator {
//             pointer: pointer,
//             direct_declarator: Box::new(dd),
//         }
//     }

//     #[inline]
//     pub fn get_pointer(&self) -> &Option<Pointer> {
//         &self.pointer
//     }

//     #[inline]
//     pub fn get_name(&self) -> &str {
//         self.direct_declarator.get_name()
//     }

//     #[inline]
//     pub fn get_direct_declarator(&self) -> &DirectDeclarator {
//         &*self.direct_declarator
//     }

//     pub fn make_type(&self, typ: &Type) -> Type {
//         let typ = self.direct_declarator.make_array_type(typ);
//         if let Some(p) = self.get_pointer() {
//             p.make_type_to(&typ)
//         }else{
//             typ.clone()
//         }
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub enum DirectDeclarator {
//     Symbol(String),
//     Enclosed(Declarator),
//     ArrayDef(Box<DirectDeclarator>, Vec<Option<ConstExpr>>),
//     FunctionDef(Box<DirectDeclarator>, Params),
// }

// impl DirectDeclarator {
//     pub fn get_name(&self) -> &str {
//         match self {
//             Self::Symbol(id) => &id,
//             Self::Enclosed(decl) => decl.get_name(),
//             Self::ArrayDef(decl, _) => (**decl).get_name(),
//             Self::FunctionDef(decl, _) => (**decl).get_name(),
//         }
//     }

//     pub fn make_array_type(&self, typ: &Type) -> Type {
//         match self {
//             Self::ArrayDef(dd, opt_size_list) => {
//                 let t = dd.make_array_type(typ);
//                 Type::Array { name: None, typ: Box::new(t.clone()), opt_size_list: opt_size_list.clone() }
//             },
//             _ => typ.clone(),
//         }
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct Declaration {
//     declarator: Declarator,
//     init_expr: Option<Box<ExprAST>>,
// }

// impl Declaration {
//     pub fn new(decl: Declarator, init: Option<Box<ExprAST>>) -> Declaration {
//         Declaration {
//             declarator: decl,
//             init_expr: init,
//         }
//     }

//     #[inline]
//     pub fn get_declarator(&self) -> &Declarator {
//         &self.declarator
//     }

//     #[inline]
//     pub fn get_init_expr(&self) -> &Option<Box<ExprAST>> {
//         &self.init_expr
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct StructDeclarator {
//     declarator: Option<Declarator>,
//     opt_bit_size: Option<u64>,
// }

// impl StructDeclarator {
//     pub fn new(opt_decl: Option<Declarator>, opt_bit_size: Option<ConstExpr>) -> StructDeclarator {
//         let opt_bit_size = if let Some(const_expr) = opt_bit_size {
//             Some(const_expr.to_usize().unwrap())
//         }else{
//             None
//         };

//         StructDeclarator {
//             declarator: opt_decl,
//             opt_bit_size: opt_bit_size,
//         }
//     }

//     #[inline]
//     pub fn get_declarator(&self) -> &Option<Declarator> {
//         &self.declarator
//     }

//     #[inline]
//     pub fn get_name<'a>(&'a self) -> Option<&'a str> {
//         if let Some(decl) = &self.declarator {
//             Some(decl.get_name())
//         }else{
//             None
//         }
//     }

//     #[inline]
//     pub fn get_bit_size(&self) -> &Option<u64> {
//         &self.opt_bit_size
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct StructDeclaration {
//     specifier_qualifier: SpecifierQualifier,
//     typ: Option<Type>,
//     declarator_list: Vec<StructDeclarator>,
// }

// impl StructDeclaration {
//     pub fn new(specifier_qualifier: SpecifierQualifier, typ: Option<Type>, declarator_list: Vec<StructDeclarator>) -> StructDeclaration {
//         StructDeclaration {
//             specifier_qualifier,
//             typ,
//             declarator_list,
//         }
//     }

//     pub fn get_specifier_qualifier(&self) -> &SpecifierQualifier {
//         &self.specifier_qualifier
//     }

//     pub fn get_type(&self) -> &Option<Type> {
//         &self.typ
//     }

//     pub fn get_declarator_list(&self) -> &Vec<StructDeclarator> {
//         &self.declarator_list
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct AbstractDeclarator {
//     pointer: Option<Pointer>,
//     direct_abstract_declarator: Option<DirectAbstractDeclarator>,
// }

// impl AbstractDeclarator {
//     pub fn new(pointer: Option<Pointer>, direct_abstract_declarator: Option<DirectAbstractDeclarator>) -> AbstractDeclarator {
//         AbstractDeclarator {
//             pointer: pointer,
//             direct_abstract_declarator: direct_abstract_declarator,
//         }
//     }
// }


// #[derive(Debug, Clone, PartialEq)]
// pub enum DirectAbstractDeclarator {
//     Simple(Option<Box<AbstractDeclarator>>),
//     FunCall(Box<DirectAbstractDeclarator>, Params),
//     ArrayAccess(Box<DirectAbstractDeclarator>, Option<ConstExpr>),
// }

// impl DirectAbstractDeclarator {
//     pub fn new_simple(abs_decl: Option<AbstractDeclarator>) -> DirectAbstractDeclarator {
//         if let Some(decl) = abs_decl {
//             DirectAbstractDeclarator::Simple(Some(Box::new(decl)))
//         }else{
//             DirectAbstractDeclarator::Simple(None)
//         }
//     }

//     pub fn new_funcall(abs_decl: DirectAbstractDeclarator, params: Params) -> DirectAbstractDeclarator {
//         DirectAbstractDeclarator::FunCall(Box::new(abs_decl), params)
//     }

//     pub fn new_array_access(abs_decl: DirectAbstractDeclarator, index: Option<ConstExpr>) -> DirectAbstractDeclarator {
//         DirectAbstractDeclarator::ArrayAccess(Box::new(abs_decl), index)
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct Function {
//     pub specifiers: DeclarationSpecifier,
//     pub declarator: Declarator,
//     pub params: Params,
//     pub body: Block,
//     pub labels: Vec<String>,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct FunProto {
//     pub specifiers: DeclarationSpecifier,
//     pub declarator: Declarator,
//     pub params: Params,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub enum FunOrProto {
//     Fun(Function),
//     Proto(FunProto),
// }

// impl FunOrProto {
//     pub fn get_specifiers(&self) -> &DeclarationSpecifier {
//         match self {
//             FunOrProto::Fun(f) => &f.specifiers,
//             FunOrProto::Proto(p) => &p.specifiers,
//         }
//     }

//     pub fn get_declarator(&self) -> &Declarator {
//         match self {
//             FunOrProto::Fun(f) => &f.declarator,
//             FunOrProto::Proto(p) => &p.declarator,
//         }
//     }

//     pub fn get_params(&self) -> &Params {
//         match self {
//             FunOrProto::Fun(f) => &f.params,
//             FunOrProto::Proto(p) => &p.params,
//         }
//     }

//     pub fn get_body(&self) -> Option<&Block> {
//         match self {
//             FunOrProto::Fun(f) => Some(&f.body),
//             FunOrProto::Proto(_) => None,
//         }
//     }

//     pub fn get_labels(&self) -> Option<&Vec<String>> {
//         match self {
//             FunOrProto::Fun(f) => Some(&f.labels),
//             FunOrProto::Proto(_) => None,
//         }
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct Case {
//     cond: ConstExpr,
//     stmt: Box<AST>,
// }

// impl Case {
//     pub fn new(cond: ConstExpr, stmt: Box<AST>) -> Case {
//         Case {
//             cond,
//             stmt,
//         }
//     }

//     pub fn get_cond(&self) -> &ConstExpr {
//         &self.cond
//     }

//     pub fn get_stmt(&self) -> &AST {
//         &&self.stmt
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct Switch {
//     cond_expr: Option<Box<ExprAST>>,
//     stmt: Option<Box<AST>>,

// }

// impl Switch {
//     pub fn new(cond_expr: Option<Box<ExprAST>>, stmt: Option<AST>) -> Switch {
//         Switch {
//             cond_expr,
//             stmt: match stmt {
//                 Some(stmt) => Some(Box::new(stmt)),
//                 None => None,
//             }
//         }
//     }

//     pub fn get_cond_expr(&self) -> Option<&ExprAST> {
//         if let Some(e) = &self.cond_expr {
//             Some(&*e)
//         }else{
//             None
//         }
//     }

//     pub fn get_stmt(&self) -> Option<&AST> {
//         if let Some(stmt) = &self.stmt {
//             Some(&*stmt)
//         }else{
//             None
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum ExprAST {
//     Assign(Box<ExprAST>, Box<ExprAST>),
//     // Inc(Box<ExprAST>),
//     // Dec(Box<ExprAST>),
    Char(u32),
    Int(u128),
//     Short(i16),
//     Long(i64),
//     UChar(u8),
//     UInt(u32),
//     UShort(u16),
//     ULong(u64),
//     LongLong(i128),
//     ULongLong(u128),
//     Float(f32),
    Double(f64),
    StringLiteral(String),
    Symbol(String),
    BinExpr(BinOp, Box<ExprAST>, Box<ExprAST>),
//     Not(Box<ExprAST>),
//     UnaryMinus(Box<ExprAST>),
//     UnaryTilda(Box<ExprAST>),
//     UnarySizeOfExpr(Box<ExprAST>),
//     UnarySizeOfTypeName(Type),
//     ExpressionPair(Box<ExprAST>, Box<ExprAST>),
//     Cast(Type, Box<ExprAST>),
//     UnaryGetAddress(Box<ExprAST>),
//     UnaryPointerAccess(Box<ExprAST>),
//     MemberAccess(Box<ExprAST>, String),
//     PointerAccess(Box<ExprAST>, String),
//     TernaryOperator(Box<ExprAST>, Box<ExprAST>, Box<ExprAST>),
//     ArrayAccess(Box<ExprAST>, Box<ExprAST>),
//     CallFunction(Box<ExprAST>, Vec<ExprAST>),
//     InitializerList(Vec<ExprAST>),
//     DefVar {
//         specifiers: DeclarationSpecifier,
//         declarations: Vec<Declaration>,
//     },
    _Self,
    _self,
}

impl ExprAST {
//     pub fn is_signed(&self) -> Result<bool, CompileError> {
//         match self {
//             ExprAST::Char(_) | ExprAST::Int(_) | ExprAST::Short(_) | ExprAST::Long(_) => Ok(true),
//             ExprAST::UChar(_) | ExprAST::UInt(_) | ExprAST::UShort(_) | ExprAST::ULong(_) => Ok(false),
//             _ => Err(CompileError::not_number(None, self)),
//         }
//     }

//     pub fn to_const(&self, defs: &Defines) -> Result<ConstExpr, CompileError> {
//         match self {
//             ExprAST::Char(num) => Ok(ConstExpr::Int(*num as i64)),
//             ExprAST::Int(num) => Ok(ConstExpr::Int(*num as i64)),
//             ExprAST::Short(num) => Ok(ConstExpr::Int(*num as i64)),
//             ExprAST::Long(num) => Ok(ConstExpr::Int(*num as i64)),
//             ExprAST::LongLong(num) => Ok(ConstExpr::LongLong(*num as i128)),
//             ExprAST::UChar(num) => Ok(ConstExpr::Unsigned(*num as u64)),
//             ExprAST::UInt(num) => Ok(ConstExpr::Unsigned(*num as u64)),
//             ExprAST::UShort(num) => Ok(ConstExpr::Unsigned(*num as u64)),
//             ExprAST::ULong(num) => Ok(ConstExpr::Unsigned(*num as u64)),
//             ExprAST::ULongLong(num) => Ok(ConstExpr::ULongLong(*num as u128)),
//             ExprAST::Float(num) => Ok(ConstExpr::Double(*num as f64)),
//             ExprAST::Double(num) => Ok(ConstExpr::Double(*num as f64)),
//             ExprAST::BinExpr(op, left, right) => {
//                 match op {
//                     BinOp::Add => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         Ok(e1 + e2)
//                     },
//                     BinOp::Sub => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         Ok(e1 - e2)
//                     },
//                     BinOp::Mul => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         Ok(e1 * e2)
//                     },
//                     BinOp::Div => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         Ok(e1 / e2)
//                     },
//                     BinOp::Mod => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         Ok(e1 % e2)
//                     },
//                     BinOp::BitAnd => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         Ok((e1 & e2)?)
//                     },
//                     BinOp::BitOr => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         Ok((e1 | e2)?)
//                     },
//                     BinOp::BitXor => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         Ok((e1 ^ e2)?)
//                     },
//                     BinOp::Comma => {
//                         let e2 = right.to_const(defs)?;
//                         Ok(e2)
//                     },
//                     BinOp::ShiftLeft => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         Ok((e1 << e2)?)
//                     },
//                     BinOp::ShiftRight => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         Ok((e1 >> e2)?)
//                     },
//                     BinOp::Equal => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         if e1 == e2 {
//                             Ok(ConstExpr::Int(1))
//                         }else{
//                             Ok(ConstExpr::Int(0))
//                         }
//                     },
//                     BinOp::NotEqual => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         if e1 != e2 {
//                             Ok(ConstExpr::Int(1))
//                         }else{
//                             Ok(ConstExpr::Int(0))
//                         }
//                     },
//                     BinOp::Less => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         if e1 < e2 {
//                             Ok(ConstExpr::Int(1))
//                         }else{
//                             Ok(ConstExpr::Int(0))
//                         }
//                     },
//                     BinOp::LessEqual => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         if e1 <= e2 {
//                             Ok(ConstExpr::Int(1))
//                         }else{
//                             Ok(ConstExpr::Int(0))
//                         }
//                     },
//                     BinOp::Greater => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         if e1 > e2 {
//                             Ok(ConstExpr::Int(1))
//                         }else{
//                             Ok(ConstExpr::Int(0))
//                         }
//                     },
//                     BinOp::GreaterEqual => {
//                         let e1 = left.to_const(defs)?;
//                         let e2 = right.to_const(defs)?;
//                         if e1 >= e2 {
//                             Ok(ConstExpr::Int(1))
//                         }else{
//                             Ok(ConstExpr::Int(0))
//                         }
//                     },
//                     BinOp::And => {




//                         unimplemented!()
//                     },
//                     BinOp::Or => {




//                         unimplemented!()
//                     },
//                 }
//             },
//             ExprAST::UnaryMinus(expr) => expr.to_const(defs),
//             ExprAST::Symbol(name) => {
//                 Ok(defs.get_const(name)?)
//             },
//             ExprAST::Not(expr) => Ok((!expr.to_const(defs)?)?),
//             ExprAST::ExpressionPair(_, right) => right.to_const(defs),


//             _ => Err(CompileError::is_not_constant(None, self)),
//         }
//     }

//     pub fn get_type(&self, env: &Env) -> Result<Type, CompileError> {
//         match self {
//             ExprAST::Assign(left, _right) => {
//                 (*left).get_type(env)
//             },
//             ExprAST::Char(_) => Ok(Type::Number(NumberType::Char)),
//             ExprAST::Int(_) => Ok(Type::Number(NumberType::Int)),
//             ExprAST::Short(_) => Ok(Type::Number(NumberType::Short)),
//             ExprAST::Long(_) => Ok(Type::Number(NumberType::Long)),
//             ExprAST::LongLong(_) => Ok(Type::Number(NumberType::LongLong)),
//             ExprAST::UChar(_) => Ok(Type::Number(NumberType::UnsignedChar)),
//             ExprAST::UInt(_) => Ok(Type::Number(NumberType::UnsignedInt)),
//             ExprAST::UShort(_) => Ok(Type::Number(NumberType::UnsignedShort)),
//             ExprAST::ULong(_) => Ok(Type::Number(NumberType::UnsignedLong)),
//             ExprAST::ULongLong(_) => Ok(Type::Number(NumberType::UnsignedLongLong)),
//             ExprAST::StringLiteral(_string) => Ok(Type::Pointer(Pointer::new(false, false), Box::new(Type::Number(NumberType::Char)))),
//             ExprAST::Float(_) => Ok(Type::Number(NumberType::Float)),
//             ExprAST::Double(_) => Ok(Type::Number(NumberType::Double)),
//             ExprAST::BinExpr(op, left, _right) => {
//                 match op {
//                     BinOp::Equal | BinOp::NotEqual | BinOp::Less | BinOp::LessEqual | BinOp::Greater | BinOp::GreaterEqual => Ok(Type::Number(NumberType::_Bool)),
//                     _ => left.get_type(env),
//                 }
//             },
//             ExprAST::UnaryMinus(expr) => expr.get_type(env),
//             ExprAST::UnaryTilda(expr) => expr.get_type(env),
//             // ExprAST::UnaryNot(expr) => expr.get_type(env),
//             ExprAST::UnarySizeOfExpr(_expr) => Ok(Type::Number(NumberType::Int)),
//             ExprAST::UnarySizeOfTypeName(_typ) => Ok(Type::Number(NumberType::Int)),
//             ExprAST::ArrayAccess(expr, _index) => {
//                 let typ = expr.get_type(env)?;
//                 Ok(typ)
//             },
//             ExprAST::Symbol(name) => {
//                 let (typ, _expr) = env.get_ptr(name).ok_or(CompileError::no_such_a_variable(None, name))?;
//                 Ok(typ.clone())
//             },
//             ExprAST::_Self => {
//                 let (typ, _expr) = env.get_ptr("Self").ok_or(CompileError::access_self_type_without_impl(None))?;
//                 Ok(typ.clone())
//             },
//             ExprAST::_self => {
//                 let (typ, _expr) = env.get_ptr("self").ok_or(CompileError::access_self_without_impl(None))?;
//                 Ok(typ.clone())
//             },
//             ExprAST::Not(_expr) => Ok(Type::Number(NumberType::_Bool)),
//             ExprAST::ExpressionPair(_, right) => right.get_type(env),
//             ExprAST::Cast(typ, _) => Ok(typ.clone()),
//             // ExprAST::Inc(expr) => expr.get_type(env),
//             // ExprAST::Dec(expr) => expr.get_type(env),
//             ExprAST::UnaryGetAddress(boxed_ast) => {
//                 let ast = &**boxed_ast;
//                 let t = ast.get_type(env)?;
//                 Ok(Type::new_pointer_type(t, false, false))
//             },
//             ExprAST::UnaryPointerAccess(boxed_ast) => {
//                 let ast = &**boxed_ast;
//                 match ast {
//                     ExprAST::Symbol(name) => {
//                         let (typ, _ptr) = env.get_ptr(name).ok_or(CompileError::no_such_a_variable(None, name))?;
//                         match typ {
//                             Type::Pointer(_p, t) => {
//                                 Ok(*t.clone())
//                             },
//                             _ => Err(CompileError::not_pointer(None, typ)),
//                         }
//                     },
//                     _ => unimplemented!(),
//                 }
//             },
//             ExprAST::MemberAccess(boxed_ast, field_name) => {
//                 let ast = &**boxed_ast;
//                 let typ = ast.get_type(env)?;
//                 match typ {
//                     Type::Struct { name: _, fields } => {
//                         let t = fields.get_type(field_name).ok_or(CompileError::type_has_not_member(None, field_name))?;
//                         Ok(t.clone())
//                     },
//                     Type::Union { name: _, fields } => {
//                         let t = fields.get_type(field_name).ok_or(CompileError::type_has_not_member(None, field_name))?;
//                         Ok(t.clone())
//                     },
//                     Type::Enum { name, enum_def } => {



//                         unimplemented!()
//                     },
//                     _ => return Err(CompileError::type_has_not_member(None, &format!("{:?}", &typ))),
//                 }
//             },
//             ExprAST::PointerAccess(boxed_ast, field_name) => {
//                 let ast = &**boxed_ast;
//                 let typ = ast.get_type(env)?;
//                 match typ {
//                     Type::Struct { name: _, fields } => {
//                         let t = fields.get_type(field_name).ok_or(CompileError::type_has_not_member(None, field_name))?;
//                         Ok(t.clone())
//                     },
//                     Type::Union { name: _, fields } => {
//                         let t = fields.get_type(field_name).ok_or(CompileError::type_has_not_member(None, field_name))?;
//                         Ok(t.clone())
//                     },
//                     Type::Enum { name, enum_def } => {



//                         unimplemented!()
//                     },
//                     _ => return Err(CompileError::type_has_not_member(None, &format!("{:?}", &typ))),
//                 }
//             },
//             ExprAST::TernaryOperator(_, e1, _) => {
//                 e1.get_type(env)
//             },
//             ExprAST::InitializerList(_) => {





//                 unimplemented!()
//             },
//             ExprAST::CallFunction(_, _) => {




//                 unimplemented!()
//             },
//             ExprAST::DefVar { specifiers, declarations } => {



//                 unimplemented!()
//             },
//          }
//     }

//     pub fn is_array_access(&self) -> bool {
//         match self {
//             ExprAST::ArrayAccess(_, _) => true,
//             _ => false,
//         }
//     }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
//     TypeDef(String, Type),
//     DefineStruct {
//         name: Option<String>,
//         fields: StructDefinition,
//     },
//     DefineUnion {
//         name: Option<String>,
//         fields: StructDefinition,
//     },
//     DefineEnum {
//         name: Option<String>,
//         fields: EnumDefinition,
//     },
//     Impl {
//         name: String,
//         typ: Type,
//         for_type: Option<String>,
//         functions: Vec<FunOrProto>,
//     },
//     DefVar {
//         specifiers: DeclarationSpecifier,
//         declarations: Vec<Declaration>,
//     },
//     GlobalDefVar {
//         specifiers: DeclarationSpecifier,
//         declaration: Vec<Declaration>,
//     },
//     Function(Function),
//     FunProto(FunProto),
//     Block(Block),
//     Expr(Box<ExprAST>),
//     Return(Option<Box<ExprAST>>),
//     Labeled(String, Option<Box<AST>>),
//     Case(Case),
//     Default(Box<AST>),
//     Switch(Switch),
//     If(Box<ExprAST>, Box<AST>, Option<Box<AST>>),
//     Loop {
//         init_expr: Option<Box<ExprAST>>,
//         pre_condition: Option<Box<ExprAST>>,
//         body: Option<Box<AST>>,
//         update_expr: Option<Box<ExprAST>>,
//         post_condition: Option<Box<ExprAST>>,
//     },
//     Break,
//     Continue,
//     Goto(String),
//     _Self,
//     _self,




}

// impl AST {
//     pub fn new_impl(impl_name: &str, impl_type: Type, for_something: Option<String>, functions: Vec<FunOrProto>) -> AST {
//         AST::Impl { name: impl_name.to_string(), typ: impl_type, for_type: for_something, functions: functions }
//     }

//     pub fn is_block(&self) -> bool {
//         match self {
//             AST::Block(_) => true,
//             _ => false,
//         }
//     }

//     pub fn get_block(&self) -> Result<&Block, CompileError> {
//         match self {
//             AST::Block(blk) => Ok(blk),
//             _ => Err(CompileError::cannot_get_block(None)),
//         }
//     }

//     pub fn is_def_var(&self) -> bool {
//         match self {
//             AST::DefVar{..} => true,
//             _ => false,
//         }
//     }

//     pub fn get_def_var(&self) -> Result<(&DeclarationSpecifier, &Vec<Declaration>), CompileError> {
//         match self {
//             AST::DefVar{specifiers, declarations: declaration} => {
//                 Ok((specifiers, declaration))
//             },
//             _ => Err(CompileError::not_defvar_when_get(None)),
//         }
//     }
//  }

//  #[derive(Debug, Clone, PartialEq)]
//  pub struct Param (DeclarationSpecifier, Declarator);

//  impl Param {
//     pub fn new(ds: DeclarationSpecifier, decl: Declarator, defs: &mut Defines) -> Result<Param, CompileError> {
//         let typ = ds.get_type();
//         let name = decl.get_name();
//         defs.set_var(name, typ, None)?;
//         Ok(Param(ds, decl))
//     }

//     pub fn get_type(&self) -> Type {
//         let typ = self.0.get_type();
//         self.1.make_type(typ)
//     }

//     pub fn get_name(&self) -> &str {
//         self.1.get_name()
//     }
//  }

//  #[derive(Debug, Clone, PartialEq)]
// pub enum CustSelf {
//     Pointer(Type),  // *self
//     Ref(Type),      // &self
//     Direct(Type),   // self
// }

// impl CustSelf {
//     pub fn get_type(&self) -> &Type {
//         match self {
//             CustSelf::Direct(typ) => typ,
//             CustSelf::Pointer(typ) => typ,
//             CustSelf::Ref(typ) => typ,
//         }
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct Params {
//     _self: Option<CustSelf>,
//     params: Vec<Param>,
//     has_variadic: bool,
// }

// impl Params {
//     pub fn new(params: Vec<Param>, has_variadic: bool) -> Params {
//         Params {
//             _self: None,
//             params: params,
//             has_variadic: has_variadic,
//         }
//     }

//     pub fn new_with_self(_self: Option<CustSelf>, params: Vec<Param>, has_variadic: bool) -> Params {
//         Params {
//             _self: _self,
//             params: params,
//             has_variadic: has_variadic,
//         }
//     }

//     pub fn from_vec(params: Vec<(DeclarationSpecifier, Declarator)>, has_variadic: bool, defs: &mut Defines) -> Result<Params, CompileError> {
//         let mut v = Vec::new();
//         for (ds, decl) in params {
//             v.push(Param::new(ds, decl, defs)?);
//         }

//         Ok(Params {
//             _self: None,
//             params: v,
//             has_variadic: has_variadic,
//         })
//     }

//     pub fn from_vec_with_self(_self: Option<CustSelf>, params: Vec<(DeclarationSpecifier, Declarator)>, has_variadic: bool, defs: &mut Defines) -> Result<Params, CompileError> {
//         let mut v = Vec::new();
//         for (ds, decl) in params {
//             v.push(Param::new(ds, decl, defs)?);
//         }

//         Ok(Params {
//             _self: _self,
//             params: v,
//             has_variadic: has_variadic,
//         })
//     }

//     pub fn len(&self) -> usize {
//         self.params.len()
//     }

//     pub fn has_self(&self) -> bool {
//         self._self.is_some()
//     }

//     pub fn get_self(&self) -> Option<&CustSelf> {
//         self._self.as_ref()
//     }

//     pub fn get_params(&self) -> &Vec<Param> {
//         &self.params
//     }

//     pub fn get_params_type(&self) -> Vec<Type> {
//         let mut v = Vec::new();

//         if let Some(cust_self)  = &self._self {
//             v.push(cust_self.get_type().clone());
//         }

//         for param in &self.params {
//             v.push(param.get_type().clone());
//         }
//         v
//     }

//     pub fn has_variadic(&self) -> bool {
//         self.has_variadic
//     }

//     pub fn set_variadic(&mut self, has_variadic: bool) {
//         self.has_variadic = has_variadic;
//     }
// }
