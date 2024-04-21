#![allow(dead_code)]

use super::{Position, Token};
use super::ParserError;
use super::ast::{AST, ToplevelAST, ExprAST, Block, Param, Params, BinOp, TypeQualifier, DeclarationSpecifier, SpecifierQualifier, Declarator, DirectDeclarator, Initializer};
use super::ast::{DeclarationSpecifierOrVariadic, Declaration, StructDeclaration, StructDeclarator, AbstractDeclarator, DirectAbstractDeclarator, ImplElement};
use super::ast::{StructLiteral, EnumLiteral};
use super::ConstExpr;
use super::types::*;
use super::defines::*;
use super::{CustSelf, Function, FunProto, FunOrProt, Switch, Case};

use std::slice::Iter;
use std::iter::Peekable;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug)]
pub struct Parser;

impl Parser {
    pub fn new() -> Self {
        Parser
    }

    #[inline]
    fn next_token<'a>(&self, iter: &'a mut Peekable<Iter<(Token, Position)>>) -> Result<(&'a Token, &'a Position), ParserError> {
        let (tok, pos) = iter.next().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        Ok((tok, pos))
    }

    #[inline]
    fn peek_token<'a>(&self, iter: &'a mut Peekable<Iter<(Token, Position)>>) -> Result<(&'a Token, &'a Position), ParserError> {
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        Ok((tok, pos))
    }

    pub fn parse(input: Vec<(Token, Position)>) -> Result<Vec<ToplevelAST>, ParserError> {
        let mut iter = input.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();

        parser.parse_translation_unit(&mut iter, &mut defs)
    }

    pub fn parse_translation_unit(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines) -> Result<Vec<ToplevelAST>, ParserError> {
        let mut declarations = Vec::new();
        let mut labels = Vec::new();

        while let Some((tok, _pos)) = iter.peek() {
            if tok.is_eof() { break; }

            if let Some(decl) = self.parse_external_declaration(iter, defs, &mut Some(&mut labels))? {
                declarations.push(decl);
            }
        }

        Ok(declarations)
    }

    fn make_global_def_var(&self, ds: DeclarationSpecifier, declarations: Vec<Declaration>, defs: &mut Defines, pos: &Position) -> Result<ToplevelAST, ParserError> {
        let typ = ds.get_type();

        for declaration in &declarations {
            let decl = declaration.get_declarator();
            let name = decl.get_name();
            let init = if let Some(expr) = declaration.get_init_expr() {
                Some((*expr).clone())
            }else{
                None
            };
            defs.set_var(name, typ, init, pos)?;
        }

        Ok(ToplevelAST::GlobalDefVar {
            specifiers: ds,
            declaration: declarations,
            pos: pos.clone(),
        })
    }

    fn make_global_def_array(&self, ds: DeclarationSpecifier, declarations: Vec<Declaration>, size_list: &Vec<usize>, defs: &mut Defines, pos: &Position) -> Result<ToplevelAST, ParserError> {
        let typ = ds.get_type();

        for declaration in &declarations {
            let decl = declaration.get_declarator();
            let name = decl.get_name();
            let array_type = Type::Array { name: Some(name.to_string()), typ: Box::new(Rc::clone(typ)), size_list: size_list.clone() };
            let init = if let Some(expr) = declaration.get_init_expr() {
                Some((*expr).clone())
            }else{
                None
            };
            defs.set_var(name, &array_type, init, pos)?;
        }

        Ok(ToplevelAST::GlobalDefVar {
            specifiers: ds,
            declaration: declarations,
            pos: pos.clone(),
        })
    }

    fn parse_def_var(&self, ds: DeclarationSpecifier, declarations: Vec<Declaration>, defs: &mut Defines, pos: &Position) -> Result<(DeclarationSpecifier, Vec<Declaration>), ParserError> {
        let typ = ds.get_type();

        for declaration in &declarations {
            let decl = declaration.get_declarator();
            let name = decl.get_name();
            let init = if let Some(expr) = declaration.get_init_expr() {
                Some((*expr).clone())
            }else{
                None
            };
            defs.set_var(name, typ, init, pos)?;
        }

        Ok((ds, declarations))
    }

    pub fn  parse_external_declaration(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ToplevelAST>, ParserError> {
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        if *tok == Token::Impl {
            return self.parse_impl(iter, defs, labels);
        }

        let ds = self.parse_declaration_specifier(iter, defs, labels)?;
        let ds = ds.get_declaration_specifier().unwrap();

        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok == Token::SemiColon {
            iter.next();  // skip ';'

            match ds.get_type().as_ref() {
                Type::Struct { name, fields } => {
                    return Ok(Some(ToplevelAST::DefineStruct{name: name.clone(), fields: fields.clone(), pos: pos.clone()}));
                },
                Type::Union { name, fields } => {
                    return Ok(Some(ToplevelAST::DefineUnion{name: name.clone(), fields: fields.clone(), pos: pos.clone()}));
                },
                Type::Enum { name, enum_def } => {
                    return Ok(Some(ToplevelAST::DefineEnum {name: name.clone(), fields: enum_def.clone(), pos: pos.clone()}));
                }
                _ => {
                    println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                    return Err(ParserError::syntax_error(pos.clone()));
                },
            }
        }

        let (decl, opt_initializer) = self.parse_declarator(ds.get_type(), iter, defs, &mut None)?;

        if ds.is_typedef() {
            self.parse_expected_token(iter, Token::SemiColon)?;  // skip ';'

            let typ = ds.get_type();
            let name = decl.get_name();
            defs.set_typedef(&name, typ, pos)?;

            Ok(Some(ToplevelAST::TypeDef(name.to_string(), typ.clone(), pos.clone())))
        }else{
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
    
            self.process_declarator(decl, opt_initializer, ds, tok, pos, iter, defs)
        }
    }

    fn process_declarator(&self, decl: Declarator, opt_initializer: Option<Initializer>, ds: &DeclarationSpecifier, tok: &Token, pos: &Position, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines) ->  Result<Option<ToplevelAST>, ParserError> {
        match decl.get_direct_declarator() {
            DirectDeclarator::Symbol(_id, _pos2) => {
                match tok {
                    Token::SemiColon => {
                        iter.next();  // skip ';'
                        let declaration = Declaration::new(decl, opt_initializer);

                        Ok(Some(self.make_global_def_var(ds.clone(), vec![declaration], defs, pos)?))
                    },
                    Token::Assign => {
                        iter.next();  // skip '='

                        let mut labels = Vec::new();
                        let typ = ds.get_type();
                        let init_expr = self.parse_initializer(typ, iter, defs, &mut Some(&mut labels))?;
                        self.parse_expected_token(iter, Token::SemiColon)?;
                        let declaration = Declaration::new(decl, Some(init_expr));

                        Ok(Some(self.make_global_def_var(ds.clone(), vec![declaration], defs, pos)?))
                    },
                    _ => {
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        Err(ParserError::syntax_error(pos.clone()))
                    },
                }
            },
            DirectDeclarator::Enclosed(decl2, pos2) => {
                self.process_declarator(decl2.clone(), opt_initializer, ds, tok, pos2, iter, defs)
            },
            DirectDeclarator::ArrayDef(_direct_declarator, opt_usize_list, _pos2) => {
                match tok {
                    Token::SemiColon => {
                        iter.next();  // skip ';'

                        let declaration = Declaration::new(decl.clone(), opt_initializer);

                        // if let Some(size_list) = const_expr_list {
                        //     Ok(Some(self.make_global_def_array(ds.clone(), vec![declaration], size_list, defs, pos)?))
                        // }else{
                        //     Err(ParserError::array_need_explicit_size_or_initializer(pos.clone()))
                        // }
                        Ok(Some(self.make_global_def_array(ds.clone(), vec![declaration], opt_usize_list, defs, pos)?))

                    },
                    Token::Assign => {
                        panic!("unreachable")
                        // iter.next();  // skip '='

                        // let mut labels = Vec::new();
                        // let init_expr = self.parse_initializer(iter, defs, &mut Some(&mut labels))?;
                        // self.parse_expected_token(iter, Token::SemiColon)?;
                        // let declaration = Declaration::new(decl.clone(), Some(Box::new(init_expr)));

                        // let size_list = if let Some(vec) = const_expr_list {
                        //     vec
                        // } else {
                        //     match &init_expr {
                        //         Initializer::Array(list, _pos) => {
                        //             let mut v = Vec::new();

                        //             for item in list {

                        //             }


                        //             unimplemented!()
                        //         },
                        //         _ => Err(ParserError::array_need_array_initializer(pos.clone())),
                        //     }
                        // };

                        // Ok(Some(self.make_global_def_array(ds.clone(), vec![declaration], size_list, defs, pos)?))
                    },
                    _ => {
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        Err(ParserError::syntax_error(pos.clone()))
                    },
                }
            },
            DirectDeclarator::FunctionDef(_direct_declarator, params, _pos2) => {
                match tok {
                    Token::SemiColon => {  // pre function definition
                        iter.next();  // skip ';'

                        let proto = ToplevelAST::FunProto(FunProto {
                                specifiers: ds.clone(),
                                declarator: decl.clone(),
                                params: params.clone(),
                            },
                            pos.clone()
                        );

                        defs.remove_function_local();
                        defs.set_function(decl.get_name(), ds.clone(), decl.clone(), params.clone(), pos)?;

                        Ok(Some(proto))
                    },
                    Token::BraceLeft => {  // function
                        let mut labels = Vec::new();
                        let block = self.parse_compound_statement(iter, defs, &mut Some(&mut labels))?;

                        let func = ToplevelAST::Function(Function {
                                specifiers: ds.clone(),
                                declarator: decl.clone(),
                                params: params.clone(),
                                body: block,
                                labels: labels,
                            },
                            pos.clone()
                        );

                        defs.remove_function_local();
                        defs.set_function(decl.get_name(), ds.clone(), decl.clone(), params.clone(), pos)?;

                        Ok(Some(func))
                    },
                    _ => {
                        defs.remove_function_local();
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        Err(ParserError::syntax_error(pos.clone()))
                    },
                }
            },
        }
    }

    fn parse_declaration_specifier(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<DeclarationSpecifierOrVariadic, ParserError> {
        let (sq, type_or_variadic, _pos) = self.parse_type_specifier_qualifier(iter, defs, labels)?;

        match type_or_variadic {
            TypeOrVariadic::Type(typ) => {
                let ds = DeclarationSpecifier::new(&typ, sq);
                Ok(DeclarationSpecifierOrVariadic::DS(ds))
            },
            TypeOrVariadic::Variadic => Ok(DeclarationSpecifierOrVariadic::Variadic),
        }
    }

    pub fn parse_type_specifier_qualifier(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(SpecifierQualifier, TypeOrVariadic, Position), ParserError> {
        let mut sq = SpecifierQualifier::new();
        let mut opt_signed: Option<(bool, Position)> = None;
        let mut opt_unsigned: Option<(bool, Position)> = None;
        let mut opt_type: Option<(Rc<Type>, Position)> = None;
        let mut opt_name: Option<String> = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::Equal | Token::SemiColon | Token::ParenLeft | Token::BracketLeft | Token::Comma | Token::ParenRight => {
                        break;
                    },

                    //
                    // storage class specifiers
                    //
                    Token::Auto => {
                        iter.next();
                        sq.set_auto()?;
                    },
                    Token::Register => {
                        iter.next();
                        sq.set_register()?;
                    }
                    Token::Static => {
                        iter.next();
                        sq.set_static()?;
                    },
                    Token::Extern => {
                        iter.next();
                        sq.set_extern()?;
                    },
                    Token::Typedef => {
                        iter.next();
                        sq.set_typedef()?;
                    },

                    //
                    // type qualifiers
                    //
                    Token::Const => {
                        iter.next();
                        sq.set_const()?;
                    },
                    Token::Volatile => {
                        iter.next();
                        sq.set_volatile()?;
                    },
                    Token::TripleDot => {
                        iter.next();  // skip '...'
                        return Ok((sq, TypeOrVariadic::Variadic, pos.clone()));
                    },

                    //
                    // Type
                    //
                    Token::Symbol(name) => {
                        if opt_type.is_some() {
                            break;
                        }

                        opt_name = Some(name.to_string());
                        if let Some(t) = defs.get_type(name) {
                            iter.next();  // skip Symbol
                            opt_type = Some((t.clone(), pos.clone()));
                        }else{
                            break;
                        }
                    },
                    Token::_Self => {
                        if opt_type.is_some() {
                            break;
                        }

                        if let Some(t) = defs.get_type("Self") {
                            iter.next();  // skip 'Self'
                            opt_type = Some((t.clone(), pos.clone()));
                        }else{
                            break;
                        }
                    },
                    Token::Struct => {
                        iter.next();  // skip 'struct'

                        let (tok2, pos2) = iter.peek().unwrap();
                        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
                        match tok2 {
                            Token::Symbol(name) => {
                                iter.next();  // skip Symbol

                                let (tok3, pos3) = iter.peek().unwrap();
                                if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }
                                match tok3 {
                                    Token::BraceLeft => {
                                        iter.next();  // skip '{'

                                        let declaration = self.parse_struct_declaration_list(iter, defs, labels)?;
                                        let definition = StructDefinition::try_new(Some(name.clone()), Some(declaration), pos3)?;
                                        let type_struct = Type::struct_from_struct_definition(Some(name.clone()), definition.clone());
                                        defs.set_struct(name, definition, pos3)?;

                                        opt_type = Some((
                                            Rc::new(type_struct),
                                            pos.clone()
                                        ));

                                        self.parse_expected_token(iter, Token::BraceRight)?;
                                    },
                                    _ => {
                                        let type_struct = if let Some(t) = defs.get_type(&name) {
                                            t.clone()
                                        }else{
                                            let definition = StructDefinition::try_new(Some(name.clone()), None, pos3)?;
                                            let typ = Type::struct_from_struct_definition(Some(name.clone()), definition);
                                            Rc::new(typ)
                                        };

                                        opt_type = Some((
                                            type_struct,
                                            pos.clone()
                                        ));
                                    },
                                }
                            },
                            Token::BraceLeft => {
                                iter.next();  // skip '{'
                                let declaration = self.parse_struct_declaration_list(iter, defs, labels)?;
                                let definition = StructDefinition::try_new(None, Some(declaration), pos2)?;
                                let type_struct = Type::struct_from_struct_definition(None, definition);
                                opt_type = Some((
                                    Rc::new(type_struct),
                                    pos.clone()
                                ));

                                self.parse_expected_token(iter, Token::BraceRight)?;
                            },
                            _ => {
                                println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                                return Err(ParserError::syntax_error(pos2.clone()));
                            }
                        }
                    },
                    Token::Union => {
                        iter.next();  // skip 'union'

                        // let (tok2, pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(pos.clone()))?;
                        let (tok2, pos2) = iter.peek().unwrap();
                        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
                        match tok2 {
                            Token::Symbol(name) => {
                                iter.next();  // skip Symbol

                                // let (tok3, _pos3) = iter.peek().ok_or(ParserError::illegal_end_of_input(pos.clone()))?;
                                let (tok3, pos3) = iter.peek().unwrap();
                                if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }
                                match tok3 {
                                    Token::BraceLeft => {
                                        iter.next();  // skip '{'

                                        let declaration = self.parse_struct_declaration_list(iter, defs, labels)?;
                                        let definition = StructDefinition::try_new(Some(name.clone()), Some(declaration), pos2)?;
                                        let type_union = Type::union_from_struct_definition(Some(name.clone()), definition.clone());
                                        defs.set_union(name, definition, pos3)?;

                                        opt_type = Some((
                                            Rc::new(type_union),
                                            pos.clone()
                                        ));

                                        self.parse_expected_token(iter, Token::BraceRight)?;
                                    },
                                    _ => {
                                        let type_union = if let Some(t) = defs.get_type(&name) {
                                            t.clone()
                                        }else{
                                            let definition = StructDefinition::try_new(Some(name.clone()), None, pos2)?;
                                            let typ = Type::union_from_struct_definition(Some(name.clone()), definition);
                                            Rc::new(typ)
                                        };

                                        // let definition = StructDefinition::try_new(Some(name.clone()), None)?;
                                        // let type_union = Type::union_from_struct_definition(Some(name.clone()), definition);

                                        opt_type = Some((
                                            type_union,
                                            pos.clone()
                                        ));
                                    },
                                }
                            },
                            Token::BraceLeft => {
                                iter.next();  // skip '{'
                                let declaration = self.parse_struct_declaration_list(iter, defs, labels)?;
                                let definition = StructDefinition::try_new(None, Some(declaration), pos2)?;
                                let type_struct = Type::union_from_struct_definition(None, definition);
                                opt_type = Some((
                                    Rc::new(type_struct),
                                    pos.clone()
                                ));

                                self.parse_expected_token(iter, Token::BraceRight)?;
                            },
                            _ => {
                                println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                                return Err(ParserError::syntax_error(pos2.clone()));
                            }
                        }
                    },
                    Token::Enum => {
                        iter.next();  // skip 'enum'

                        let (tok2, pos2) = iter.peek().unwrap();
                        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
                        match tok2 {
                            Token::Symbol(name) => {
                                iter.next();  // skip Symbol
                
                                let (tok3, pos3) = iter.peek().unwrap();
                                if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }
                                match tok3 {
                                    Token::BraceLeft => {
                                        iter.next();  // skip '{'

                                        let type_struct = self.parse_enum_body(name, pos3, iter, defs, labels)?;
                                        opt_type = Some((
                                            Rc::new(type_struct),
                                            pos.clone()
                                        ));
                                    },
                                    Token::Less => {  // '<'
                                        defs.add_new_generics();

                                        let g_list = self.parse_generic_types(iter, defs)?;

                                        self.parse_expected_token(iter, Token::BraceLeft)?;  // skip '{'

                                        let type_struct = self.parse_enum_body(name, pos3, iter, defs, labels)?;
                                        opt_type = Some((
                                            Rc::new(type_struct),
                                            pos.clone()
                                        ));

                                        defs.remove_generics();
                                    },
                                    _ => {
                                        let type_struct = if let Some(t) = defs.get_type(&name) {
                                            Rc::clone(t)
                                        }else{
                                            let definition = EnumDefinition::new_standard(Some(name.clone()), Vec::new());
                                            let typ = Type::enum_from_enum_definition(Some(name.clone()), definition);
                                            Rc::new(typ)
                                        };
                
                                        opt_type = Some((
                                            type_struct,
                                            pos.clone()
                                        ));
                                    },
                                }
                            },
                            Token::BraceLeft => {
                                iter.next();  // skip '{'
                                let (enum_list, is_tagged) = self.parse_enumerator_list(iter, defs, labels)?;
                                let definition;
                                if is_tagged {
                                    definition = EnumDefinition::new_tagged(None, enum_list);
                                }else{
                                    definition = EnumDefinition::new_standard(None, enum_list);
                                }
                                let type_struct = Type::enum_from_enum_definition(None, definition);
                                opt_type = Some((
                                    Rc::new(type_struct),
                                    pos.clone()
                                ));
                
                                self.parse_expected_token(iter, Token::BraceRight)?;
                            },
                            _ => {
                                println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                                return Err(ParserError::syntax_error(pos2.clone()));
                            }
                        }
                    },
                    Token::Signed => {
                        iter.next();
                        if let Some((true, pre_pos)) = opt_unsigned {
                            return Err(ParserError::cannot_combine_with_previous_unsigned_declaration_specifier(pos.clone(), pre_pos.clone()));
                        }
                        if let Some((typ, _pos)) = &opt_type {
                            if ! typ.can_sign() {
                                return Err(ParserError::not_number_signed(&typ, pos.clone()));
                            }
                        }
                        opt_signed = Some((true, pos.clone()));
                    },
                    Token::Unsigned => {
                        iter.next();
                        if let Some((true, pre_pos)) = opt_signed {
                            return Err(ParserError::cannot_combine_with_previous_signed_declaration_specifier(pos.clone(), pre_pos.clone()));
                        }
                        if let Some((typ, _pos)) = &opt_type {
                            if ! typ.can_sign() {
                                return Err(ParserError::not_number_unsigned(&typ, pos.clone()));
                            }
                        }
                        opt_unsigned = Some((true, pos.clone()));
                    },
                    Token::Void => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            return Err(ParserError::already_type_defined(&Type::Void, pos.clone(), &pre_type, &pre_pos));
                        }
                        opt_type = Some((Rc::new(Type::Void), pos.clone()));
                    },
                    Token::_Bool => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            return Err(ParserError::already_type_defined(&Type::Number(NumberType::_Bool), pos.clone(), &pre_type, &pre_pos));
                        }
                        opt_type = Some((Rc::new(Type::Number(NumberType::_Bool)), pos.clone()));
                    }
                    Token::Char => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            return Err(ParserError::already_type_defined(&Type::Number(NumberType::Char), pos.clone(), &pre_type, &pre_pos));
                        }
                        opt_type = Some((Rc::new(Type::Number(NumberType::Char)), pos.clone()));
                    },
                    Token::Short => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            match pre_type.as_ref() {
                                Type::Number(NumberType::Int) => {
                                    opt_type = Some((Rc::new(Type::Number(NumberType::Short)), pre_pos.clone()));
                                    continue;
                                },
                                _ => {
                                    return Err(ParserError::already_type_defined(&Type::Number(NumberType::Short), pos.clone(), &pre_type, &pre_pos));
                                },
                            }
                        }
                        opt_type = Some((Rc::new(Type::Number(NumberType::Short)), pos.clone()));
                    },
                    Token::Int => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            match pre_type.as_ref() {
                                Type::Number(NumberType::Short) | Type::Number(NumberType::Long) | Type::Number(NumberType::LongLong) => {
                                    continue;
                                },
                                _ => {
                                    return Err(ParserError::already_type_defined(&Type::Number(NumberType::Int), pos.clone(), &pre_type, &pre_pos));
                                },
                            }
                        }
                        opt_type = Some((Rc::new(Type::Number(NumberType::Int)), pos.clone()));
                    },
                    Token::Long => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            match pre_type.as_ref() {
                                Type::Number(NumberType::Int) => {
                                    opt_type = Some((Rc::new(Type::Number(NumberType::Long)), pre_pos.clone()));
                                    continue;
                                },
                                Type::Number(NumberType::Long) => {
                                    opt_type = Some((Rc::new(Type::Number(NumberType::LongLong)), pre_pos.clone()));
                                    continue;
                                },
                                _ => {
                                    return Err(ParserError::already_type_defined(&Type::Number(NumberType::Long), pos.clone(), &pre_type, &pre_pos));
                                },
                            }
                        }
                        opt_type = Some((Rc::new(Type::Number(NumberType::Long)), pos.clone()));
                    },
                    Token::Float => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            return Err(ParserError::already_type_defined(&Type::Number(NumberType::Float), pos.clone(), &pre_type, &pre_pos));
                        }
                        opt_type = Some((Rc::new(Type::Number(NumberType::Float)), pos.clone()));
                    },
                    Token::Double => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            return Err(ParserError::already_type_defined(&Type::Number(NumberType::Double), pos.clone(), &pre_type, &pre_pos));
                        }
                        opt_type = Some((Rc::new(Type::Number(NumberType::Double)), pos.clone()));
                    },
                    _ => {
                        break;
                    },
                }

            }else{
                break;
            }
        }

        let (typ, pos) = if let Some((typ, pos)) = opt_type {
            (typ, pos)
        }else{
println!("here.");
            return Err(ParserError::no_type_defined(opt_name, iter.peek().unwrap().1.clone()));
        };
        let typ = if let Some((true, _)) = opt_unsigned {
            Rc::new(typ.to_unsigned(&pos)?)
        }else{
            typ
        };

        Ok((sq, TypeOrVariadic::Type(typ), pos))
    }

    fn parse_enum_body(&self, name: &String, pos3: &Position, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Type, ParserError> {
        let (enum_list, is_tagged) = self.parse_enumerator_list(iter, defs, labels)?;

        let definition;
        if is_tagged {
            definition = EnumDefinition::new_tagged(Some(name.clone()), enum_list);
        }else{
            definition = EnumDefinition::new_standard(Some(name.clone()), enum_list);
        }

        let type_struct = Type::enum_from_enum_definition(Some(name.clone()), definition.clone());
        defs.set_enum(name, definition, pos3)?;

        self.parse_expected_token(iter, Token::BraceRight)?;

        Ok(type_struct)
    }


    fn parse_generic_types(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines) -> Result<Vec<Rc<Type>>, ParserError> {
        iter.next();  // skip '<'

        let mut list = Vec::new();

        loop {
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            if *tok == Token::Greater {
                iter.next();  // skip '>'
                break;
            }

            match tok {
                Token::Symbol(name) => {
                    iter.next();  // skip Symbol
                    defs.check_exist(name, pos)?;

                    let g_type = defs.add_generic_type(name, pos)?;

                    list.push(g_type);
                },
                _ => {
                    return Err(ParserError::not_generic_type(tok.clone(), pos.clone()));
                }
            }
        }

        Ok(list)
    }

    fn parse_struct_declaration_list(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Vec<StructDeclaration>, ParserError> {
        let mut list: Vec<StructDeclaration> = Vec::new();

        loop {
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            if *tok == Token::BraceRight {
                break;
            }

            let declaration = self.parse_struct_declaration(iter, defs, labels)?;
            list.push(declaration);
        }

        Ok(list)
    }

    fn parse_struct_declaration(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<StructDeclaration, ParserError> {
        let mut list: Vec<StructDeclarator> = Vec::new();
        let (sq, type_or_variadic, pos) = self.parse_type_specifier_qualifier(iter, defs, labels)?;

        //
        // error check:
        //        check <storage-class-specifier>
        //        check variadic parameter
        //
        if sq.auto || sq.register || sq.static_ || sq.extern_ || sq.typedef || type_or_variadic.is_variadic() {
            return Err(ParserError::syntax_error_while_parsing_struct(&sq, &type_or_variadic, pos.clone()));
        }

        let typ = type_or_variadic.get_type().unwrap();

        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok == Token::BraceRight {
            return Ok(StructDeclaration::new(sq, Some(typ.clone()), list));
        }

        loop {
            let decl = self.parse_struct_declarator(&typ, iter, defs, labels)?;
            list.push(decl);

            let (next_token, pos) = iter.peek().unwrap();
            if next_token.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            if *next_token != Token::Comma {
                break;
            }else{
                iter.next();  // skip ','
            }
        }

        self.parse_expected_token(iter, Token::SemiColon)?;

        Ok(StructDeclaration::new(sq, Some(Rc::clone(typ)), list))
    }

    fn parse_struct_declarator(&self, typ: &Rc<Type>, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<StructDeclarator, ParserError> {
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok == Token::Colon {
            iter.next();  // skip ':'

            let const_expr = self.parse_constant_expression(iter, defs, labels)?.ok_or(ParserError::no_constant_expr_parsing_struct_after_colon(pos.clone()))?;

            Ok(StructDeclarator::new(None, Some(const_expr)))

        }else{
            let (decl, _initializer) = self.parse_declarator(typ, iter, defs, labels)?;

            // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            if *tok == Token::Colon {
                iter.next();  // skip ':'

                let const_expr = self.parse_constant_expression(iter, defs, labels)?.ok_or(ParserError::no_constant_expr_parsing_struct_after_colon(pos.clone()))?;

                Ok(StructDeclarator::new(Some(decl), Some(const_expr)))

            }else{
                Ok(StructDeclarator::new(Some(decl), None))
            }
        }
    }

    fn parse_enumerator_list(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(Vec<Enumerator>, bool), ParserError> {
        let mut list: Vec<Enumerator> = Vec::new();
        let mut value: u32 = 0;

        let mut is_standard = false;  // when ```field_name = constant````, true
        let mut is_tagged = false;  // (Types...) or {field: Type...}

        loop {
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            if *tok == Token::BraceRight {
                // iter.next();  // skip '}'
                break;
            }

            let (next_token, pos) = iter.next().unwrap();
            if next_token.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            let name = if let Token::Symbol(id) = next_token {
                id
            }else{
                return Err(ParserError::not_symbol_parsing_enum(pos.clone()));
            };

            let enumerator: Enumerator;
            let (tok2, pos2) = iter.peek().unwrap();
            if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
            match tok2 {
                Token::BraceRight => {  // '}'
                    enumerator = Enumerator::new(name, value);
                },
                Token::Assign => {      // '='
                    if is_tagged {
                        return Err(ParserError::tagged_enum_cannot_have_value(pos2.clone()));
                    }
                    is_standard = true;

                    iter.next();  // skip '='

                    let const_val = self.parse_constant_expression(iter, defs, labels)?.ok_or(ParserError::enum_should_be_int(pos2.clone()))?;
                    let int_val = const_val.as_u32_value();
                    enumerator = Enumerator::new(name, int_val);
                    value = int_val;

                    let (tok3, pos3) = iter.peek().unwrap();
                    if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }
                    if *tok3 == Token::Comma {
                        iter.next();  // skip ','
                    }
                },
                Token::Comma => {       // ','
                    iter.next();  // skip ','
                    enumerator = Enumerator::new(name, value);
                },
                Token::ParenLeft => {   // '('
                    if is_standard {
                        return Err(ParserError::standard_enum_cannot_be_tagged(pos2.clone()));
                    }
                    is_tagged = true;

                    iter.next();  // skip '('

                    let type_list = self.parse_type_list(iter, defs)?;
                    enumerator = Enumerator::new_tuple(name, type_list);

                    let (tok3, pos3) = iter.peek().unwrap();
                    if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }
                    if *tok3 == Token::Comma {
                        iter.next();  // skip ','
                    }
                },
                Token::BraceLeft => {    // '{'
                    if is_standard {
                        return Err(ParserError::standard_enum_cannot_be_tagged(pos2.clone()));
                    }
                    is_tagged = true;

                    iter.next();  // skip '{'

                    let declaration = self.parse_struct_declaration_list(iter, defs, labels)?;
                    let definition = StructDefinition::try_new(None, Some(declaration), pos2)?;

                    self.parse_expected_token(iter, Token::BraceRight)?;

                    enumerator = Enumerator::new_struct(name, definition);

                    let (tok3, pos3) = iter.peek().unwrap();
                    if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }
                    if *tok3 == Token::Comma {
                        iter.next();  // skip ','
                    }
                },
                _ => {
                    return Err(ParserError::should_be(vec![Token::BraceRight, Token::Assign], tok2, pos2.clone()));
                }
            }

            value += 1;
            list.push(enumerator);
        }

        Ok((list, is_tagged))
    }

    fn parse_type_list(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines) -> Result<Vec<(Rc<Type>, u32)>, ParserError> {
        let mut vec = Vec::new();
        let mut tag_number = 0;

        loop {
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

            if *tok == Token::ParenRight {
                iter.next();  // skip ')'
                break;
            }

            let typ = self.parse_type(iter, defs)?;
            vec.push((typ, tag_number));

            tag_number += 1;
        }

        Ok(vec)
    }

    fn parse_type(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines) -> Result<Rc<Type>, ParserError> {
        let (tok, pos) = self.next_token(iter)?;

        match tok {
            Token::Symbol(name) => {
                if let Some(typ) = defs.get_type(name) {
                    return Ok(Rc::clone(typ));
                }else{
                    return Err(ParserError::no_such_a_type(name, pos.clone()));
                }
            },
            _ => {
                return Err(ParserError::syntax_error(pos.clone()));
            },
        }
    }

    fn parse_declarator(&self, typ: &Rc<Type>, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(Declarator, Option<Initializer>), ParserError> {
        let opt_pointer = self.parse_pointer(iter, defs)?;
        let (direct, initializer) = self.parse_direct_declarator(typ, iter, defs, labels)?;
        Ok((Declarator::new(opt_pointer, direct), initializer))
    }

    #[allow(irrefutable_let_patterns)]
    pub fn parse_pointer(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines) -> Result<Option<Pointer>, ParserError> {
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        if *tok != Token::Mul {
            return Ok(None);
        }

        iter.next();  // skip '*'

        let mut is_const = false;
        let mut is_volatile = false;
        // while let (tok2, _pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(pos.clone()))? {
        while let (tok2, pos2) = iter.peek().unwrap() {
            if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone()))}

            match tok2 {
                Token::Const => {
                    iter.next();  // skip 'const'
                    is_const = true;
                },
                Token::Volatile => {  // skip 'volatile'
                    iter.next();
                    is_volatile = true;
                },
                Token::Mul => {  // '*'
                    break;
                },
                _ => {
                    // if is_const || is_volatile {
                    //     return Err(ParserError::syntax_error(pos2.clone()));
                    // }
                    break;
                },
            }
        }

        let mut pointer = Pointer::new(is_const, is_volatile);

        if self.is_next_token(iter, &Token::Mul)? {
            let next_pointer = self.parse_pointer(iter, defs)?.unwrap();
            pointer.set_next_pointer(next_pointer);
        }

        Ok(Some(pointer))
    }

    fn is_next_token(&self, iter: &mut Peekable<Iter<(Token, Position)>>, tok: &Token) -> Result<bool, ParserError> {
        // let (tok2, _pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok2, pos2) = iter.peek().unwrap();
        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

        Ok(tok == tok2)
    }

    fn parse_type_qualifier(&self, iter: &mut Peekable<Iter<(Token, Position)>>, _defs: &mut Defines) -> Result<Option<TypeQualifier>, ParserError> {
        // let (tok, _pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        match &*tok {
            Token::Const => {
                Ok(Some(TypeQualifier::Const))
            },
            Token::Volatile => {
                Ok(Some(TypeQualifier::Volatile))
            },
            _ => Ok(None),
        }
    }

    fn parse_direct_declarator(&self, typ: &Rc<Type>, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(DirectDeclarator, Option<Initializer>), ParserError> {
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        let decl;
        match tok {
            Token::Symbol(id) => {
                iter.next();
                decl = DirectDeclarator::Symbol(id.to_string(), pos.clone());
            },
            Token::ParenLeft => {
                iter.next();  // skip '('

                let (d, _initializer) = self.parse_declarator(typ, iter, defs, labels)?;
                self.parse_expected_token(iter, Token::ParenRight)?;
                decl = DirectDeclarator::Enclosed(d, pos.clone());
            },
            _ => {
                println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                return Err(ParserError::syntax_error(pos.clone()))
            }
        }

        self.parse_direct_declarator_sub(typ, decl, iter, defs, labels)
    }

    fn parse_direct_declarator_sub(&self, typ: &Rc<Type>, decl: DirectDeclarator, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(DirectDeclarator, Option<Initializer>), ParserError> {
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        let cust_self;
        match tok {
            Token::Mul => {  // '*'
                iter.next();  // skip '*'

                let (tok2, pos2) = iter.peek().unwrap();
                if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
                if *tok2 != Token::_self {
                    println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                    return Err(ParserError::syntax_error(pos2.clone()));
                }

                cust_self = Some(CustSelf::Pointer(Rc::clone(defs.get_self_type(pos)?)));
            },
            _ => {
                cust_self = None;
            },
        }

        let mut result = decl;
        let mut initializer = None;
        loop {
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

            match tok {
                Token::ParenLeft => {  // define function
                    // read parameter type list
                    iter.next();  // skip '('

                    let (next_tok, pos2) = iter.peek().unwrap();
                    if next_tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
                    defs.add_new_function_local();
                    let param_type_list = if *next_tok == Token::ParenRight {
                        iter.next();
                        Params::new_with_self(cust_self.clone(), Vec::new(), false)
                    }else{
                        let param_type_list = self.parse_parameter_type_list(iter, defs, labels)?;
                        self.parse_expected_token(iter, Token::ParenRight)?;  // skip ')'
                        param_type_list
                    };

                    result = DirectDeclarator::FunctionDef(Box::new(result), param_type_list, pos.clone());
                },
                Token::BracketLeft => {  // when '[', define array
                    iter.next();  // skip '['

                    let mut opt_dimension: Vec<Option<usize>> = Vec::new();

                    let opt_const_expr = self.parse_constant_expression(iter, defs, labels)?;
                    if let Some(const_expr) = opt_const_expr {
                        opt_dimension.push(Some(const_expr.to_usize()?));
                    }else{
                        opt_dimension.push(None);
                    }
                    self.parse_expected_token(iter, Token::BracketRight)?;  // skip ']'

                    loop {
                        let (tok2, pos2) = iter.peek().unwrap();
                        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                        if *tok2 != Token::BracketLeft {  // not '['
                            break;
                        }

                        iter.next();  // skip '['

                        let opt_const_expr = self.parse_constant_expression(iter, defs, labels)?;
                        if let Some(const_expr) = opt_const_expr {
                            opt_dimension.push(Some(const_expr.to_usize()?));
                        }else{
                            opt_dimension.push(None);
                        }
                        self.parse_expected_token(iter, Token::BracketRight)?;  // skip ']'
                    }

                    let mut dimension = Vec::new();
                    if let Some((tok3, _pos3)) = iter.peek() {
                        if *tok3 == Token::Assign {
                            iter.next();  // skip '='

                            initializer = Some(self.parse_array_initializer(&typ, &mut opt_dimension, 0, iter, defs, labels)?);

                            for item in opt_dimension {
                                dimension.push(item.unwrap())
                            }

                        }else{
                            for item in opt_dimension {
                                if let Some(const_expr) = item {
                                    dimension.push(const_expr);
                                }else{
                                    return Err(ParserError::array_need_explicit_size_or_initializer(pos.clone()));
                                }
                            }
                        }
                    }

                    result =  DirectDeclarator::ArrayDef(Box::new(result), dimension, pos.clone());
                },
                _ => {
                    break Ok((result, initializer));
                },
            }
        }
    }

    fn parse_constant_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ConstExpr>, ParserError> {
        if let Some(expr) = self.parse_conditional_expression(iter, defs, labels)? {
            Ok(Some(expr.to_const(defs, &iter.peek().unwrap().1)?))
        }else{
            Ok(None)
        }
    }

    fn parse_conditional_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(expr) = self.parse_logical_or_expression(iter, defs, labels)? {
            if let Some((tok, pos)) = iter.peek() {
                if *tok == Token::Question {
                    iter.next();  // skip '?'

                    println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                    let then_expr = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                    self.parse_expected_token(iter, Token::Colon)?;
                    println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                    let else_expr = self.parse_conditional_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                    Ok(Some(ExprAST::TernaryOperator(Box::new(expr), Box::new(then_expr), Box::new(else_expr), pos.clone())))

                }else{
                    Ok(Some(expr))
                }
            }else{
                Ok(Some(expr))
            }

        }else{
            Ok(None)
        }
    }

    fn parse_logical_or_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_logical_and_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::Or => {
                        if let Some(code) = self.parse_logical_or_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_logical_or_expression2(&self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::Or => {
                        let op = tok;
                        iter.next(); // skip '||'

                        if let Some(right) = self.parse_logical_and_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }


    fn parse_logical_and_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_inclusive_or_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::And => {
                        if let Some(code) = self.parse_logical_and_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_logical_and_expression2(&self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::And => {
                        let op = tok;
                        iter.next(); // skip '&&'

                        if let Some(right) = self.parse_inclusive_or_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }


    fn parse_inclusive_or_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_exclusive_or_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::BitOr => {
                        if let Some(code) = self.parse_inclusive_or_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_inclusive_or_expression2(&self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::BitOr => {
                        let op = tok;
                        iter.next(); // skip '|'

                        if let Some(right) = self.parse_exclusive_or_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_exclusive_or_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_and_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::BitXor => {
                        if let Some(code) = self.parse_exclusive_or_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_exclusive_or_expression2(&self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::BitXor => {
                        let op = tok;
                        iter.next(); // skip '^'

                        if let Some(right) = self.parse_and_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }


    fn parse_and_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_equality_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::BitAnd => {
                        if let Some(code) = self.parse_and_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_and_expression2(&self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::BitAnd => {
                        let op = tok;
                        iter.next(); // skip '&'

                        if let Some(right) = self.parse_equality_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_equality_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_relational_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::Equal | Token::NotEqual => {
                        if let Some(code) = self.parse_equality_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_equality_expression2(&self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::Equal | Token::NotEqual => {
                        let op = tok;
                        iter.next(); // skip '==', '!='

                        if let Some(right) = self.parse_relational_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_relational_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_shift_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
                        if let Some(code) = self.parse_relational_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_relational_expression2(&self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
                        let op = tok;
                        iter.next(); // skip '<', '<=', '>', '>='

                        if let Some(right) = self.parse_shift_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_shift_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_additive_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::ShiftLeft | Token::ShiftRight => {
                        if let Some(code) = self.parse_shift_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_shift_expression2(&self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::ShiftLeft | Token::ShiftRight => {
                        let op = tok;
                        iter.next(); // skip '<<' or '>>'

                        if let Some(right) = self.parse_additive_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_additive_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_multiplicative_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::Add | Token::Sub => {
                        if let Some(code) = self.parse_additive_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_additive_expression2(&self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::Add | Token::Sub => {
                        let op = tok;
                        iter.next(); // skip '+' or '-'

                        if let Some(right) = self.parse_multiplicative_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_multiplicative_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_cast_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::Mul | Token::Div | Token::Mod => {
                        if let Some(code) = self.parse_multiplicative_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))
        }else{  // None
            Ok(None)
        }
    }

    fn parse_multiplicative_expression2(&self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::Mul | Token::Div | Token::Mod => {
                        let op = tok;
                        iter.next(); // skip '+' or '-'

                        if let Some(right) = self.parse_cast_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op, pos)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_cast_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let opt_expr = self.parse_unary_expression(iter, defs, labels)?;
        if opt_expr.is_some() {
            Ok(opt_expr)

        }else{
            // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

            if *tok == Token::ParenLeft {
                iter.next();  // skip '('
                self.parse_cast_expression_sub(iter, pos, defs, labels)
            }else{
                Ok(None)
            }
        }
    }

    fn calc_type(&self, typ: &Rc<Type>, opt_abstract_decl: &Option<AbstractDeclarator>) -> Rc<Type> {
        if let Some(abs_decl) = opt_abstract_decl {
            abs_decl.calc_type(typ)
        }else{
            typ.clone()
        }
    }

    fn parse_cast_expression_sub(&self, iter: &mut Peekable<Iter<(Token, Position)>>, pos: &Position, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let (_sq, type_or_variadic, opt_abstract_decl) = self.parse_type_name(iter, defs, labels)?;
        let cast_type = type_or_variadic.get_type().ok_or(ParserError::no_type_defined(None, pos.clone()))?;
        self.parse_expected_token(iter, Token::ParenRight)?;

        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
        let expr = self.parse_cast_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

        let cast_type = self.calc_type(&cast_type, &opt_abstract_decl);

        Ok(Some(ExprAST::Cast(cast_type, Box::new(expr), pos.clone())))
    }

    fn parse_unary_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(ast) = self.parse_postfix_expression(iter, defs, labels)? {
            Ok(Some(ast))

        }else{
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

            match &*tok {
                Token::Inc => {
                    iter.next();  // skip '++'

                    println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
println!("peek: {:?}", iter.peek().unwrap());
                    let expr = self.parse_unary_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;
println!("expr: {:?}", expr);
                    if expr.is_symbol() {
                        let (sym, sym_pos) = expr.get_symbol()?;
                        let inc = ExprAST::PreInc(sym.clone(), sym_pos.clone(), pos.clone());
                        Ok(Some(inc))
                    }else if expr.is_member_access() {
                        let inc = ExprAST::PreIncMemberAccess(Box::new(expr.clone()), pos.clone());
                        Ok(Some(inc))
                    }else{
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        Err(ParserError::syntax_error(pos.clone()))
                    }
               },
                Token::Dec => {
                    iter.next();  // skip '--'

                    println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                    let expr = self.parse_unary_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                    if expr.is_symbol() {
                        let (sym, sym_pos) = expr.get_symbol()?;
                        let dec = ExprAST::PreDec(sym.clone(), sym_pos.clone(), pos.clone());
                        Ok(Some(dec))
    
                    }else if expr.is_member_access() {
                        let inc = ExprAST::PreDecMemberAccess(Box::new(expr.clone()), pos.clone());
                        Ok(Some(inc))
                    }else{
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        Err(ParserError::syntax_error(pos.clone()))
                    }

                },
                Token::Add => {      // '+'
                    iter.next();  // skip '+'
                    self.parse_cast_expression(iter, defs, labels)
                },
                Token::Sub => {      // '-'
                    iter.next();  // skip '-'
                    if let Some(expr) = self.parse_cast_expression(iter, defs, labels)? {
                        Ok(Some(ExprAST::UnaryMinus(Box::new(expr), pos.clone())))
                    }else{
                        Ok(None)
                    }
                },
                Token::BitAnd => {   // '&'. get address
                    iter.next();  // skip '&'
                    if let Some(expr) = self.parse_cast_expression(iter, defs, labels)? {
                        Ok(Some(ExprAST::UnaryGetAddress(Box::new(expr), pos.clone())))
                    }else{
                        Ok(None)
                    }
                },
                Token::Mul => {      // '*'.  "*pointer" pointer access
                    iter.next();  // skip '*'
                    if let Some(expr) = self.parse_cast_expression(iter, defs, labels)? {
                        Ok(Some(ExprAST::UnaryPointerAccess(Box::new(expr), pos.clone())))
                    }else{
                        Ok(None)
                    }
                },
                Token::Tilda => {    // '~'
                    iter.next();  // skip '~'
                    if let Some(expr) = self.parse_cast_expression(iter, defs, labels)? {
                        Ok(Some(ExprAST::UnaryTilda(Box::new(expr), pos.clone())))
                    }else{
                        Ok(None)
                    }
                },
                Token::Not => {      // '!'
                    iter.next();  // skip '!'
                    if let Some(expr) = self.parse_cast_expression(iter, defs, labels)? {
                        Ok(Some(ExprAST::Not(Box::new(expr), pos.clone())))
                    }else{
                        Ok(None)
                    }
                },
                Token::Sizeof => {
                    iter.next();  // skip 'sizeof'
                    if let Some(expr) = self.parse_unary_expression(iter, defs, labels)? {
                        Ok(Some(ExprAST::UnarySizeOfExpr(Box::new(expr), pos.clone())))
                    }else{
                        let (_sq, type_or_variadic, _opt_abstract_decl) = self.parse_type_name(iter, defs, labels)?;
                        let typ = type_or_variadic.get_type().ok_or(ParserError::no_type_defined(None, pos.clone()))?;
                        Ok(Some(ExprAST::UnarySizeOfTypeName(Rc::clone(typ), pos.clone())))
                    }
                },
                Token::ParenRight => {
                    Ok(None)
                },
                _ => {
                    println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                    Err(ParserError::syntax_error(pos.clone()))
                },
            }
        }
    }

    fn parse_postfix_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_primary_expression(iter, defs, labels)? {
            loop {
                if let Some((tok, pos)) = iter.peek() {
                    match tok {
                        Token::BracketLeft => {
                            iter.next();  // skip '['
                            let expr = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::no_expr_while_access_array(pos.clone()))?;
                            self.parse_expected_token(iter, Token::BracketRight)?;  // check ']'
                            match ast {
                                ExprAST::ArrayAccess(box_ast, expr_list, pos) => {
                                    let mut list = expr_list.clone();
                                    list.push(Box::new(expr));

                                    ast = ExprAST::ArrayAccess(box_ast, list, pos.clone());
                                },
                                _ => {
                                    ast = ExprAST::ArrayAccess(Box::new(ast), vec![Box::new(expr)], pos.clone());
                                }
                            }
                        },
                        Token::ParenLeft => {
                            iter.next();  // skip '('
                            let exprs = self.parse_expression(iter, defs, labels)?;
                            self.parse_expected_token(iter, Token::ParenRight)?;
                            let args: Vec<ExprAST> = self.exprs_to_vec(exprs)?;
                            ast = ExprAST::CallFunction(Box::new(ast), args, pos.clone());
                        },
                        Token::Dot => {
                            iter.next();  // skip '.'

                            let (tok2, pos2) = iter.next().unwrap();
                            if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                            match tok2 {
                                Token::Symbol(id) => {
                                    ast = ExprAST::MemberAccess(Box::new(ast), id.clone(), pos.clone());
                                },
                                _ => return Err(ParserError::no_id_after_dot(pos2.clone())),
                            }
                        },
                        Token::MemberSelection => {
                            iter.next();  // skip '->'

                            let (tok2, pos2) = iter.next().unwrap();
                            if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                            match tok2 {
                                Token::Symbol(id) => {
                                    ast = ExprAST::PointerAccess(Box::new(ast), id.clone(), pos.clone());
                                },
                                _ => return Err(ParserError::no_id_after_arrow(pos2.clone())),
                            }

                        },
                        Token::Inc => {
                            iter.next();  // skip '++'

                            if ast.is_symbol() {
                                let (sym, sym_pos) = ast.get_symbol()?;
                                ast = ExprAST::PostInc(sym.clone(), sym_pos.clone(), pos.clone());
                            }else if ast.is_member_access() {
                                ast = ExprAST::PostIncMemberAccess(Box::new(ast.clone()), pos.clone());
                            }else{
                                println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                                return Err(ParserError::syntax_error(pos.clone()));
                            }
                        },
                        Token::Dec => {
                            iter.next();  // skip '--'

                            if ast.is_symbol() {
                                let (sym, sym_pos) = ast.get_symbol()?;
                                ast = ExprAST::PostDec(sym.clone(), sym_pos.clone(), pos.clone());
                            }else if ast.is_member_access() {
                                ast = ExprAST::PostDecMemberAccess(Box::new(ast.clone()), pos.clone());
                            }else{
                                println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                                return Err(ParserError::syntax_error(pos.clone()));
                            }
                        },
    
                        _ => break,
                    }

                }else{
                    break;
                }
        
            }

            Ok(Some(ast))
        }else{  // None
            Ok(None)
        }
    }

    fn exprs_to_vec(&self, exprs: Option<ExprAST>) -> Result<Vec<ExprAST>, ParserError> {
        let mut exprs = exprs;
        let mut result: Vec<ExprAST> = Vec::new();

        while let Some(expr) = exprs {
            match expr {
                ExprAST::BinExpr(BinOp::Comma, left, right, _pos) => {
                    result.push(*left);
                    exprs = Some(*right);
                },
                _ => {
                    result.push(expr);
                    break;
                },
            }
        }

        Ok(result)
    }

    fn try_parse_type(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines) -> bool {
        if let Some((tok, _pos)) = iter.peek() {
            // int, etc   struct enum
            if tok.is_type() {  // int, etc
                true
            }else{
                match tok {
                    Token::Struct => true,
                    Token::Symbol(id) => defs.exists_type(id),
                    _ => false,
                }
            }

        }else{
            false
        }
    }

    fn parse_primary_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some((tok, pos)) = iter.peek() {
            match &*tok {
                Token::Symbol(name) => {
                    iter.next();  // skip symbol

                    let (tok2, _pos2) = iter.peek().unwrap();
                    match tok2 {
                        Token::WColon => {
                            iter.next();  // skip '::'

                            let opt_type = defs.get_type(name);
                            if let Some(typ) = opt_type {
                                let typ = Rc::clone(typ);  // lifetime対策

                                let enum_init = self.parse_after_wcolon(&typ, name, iter, defs, labels)?;
                                match enum_init {
                                    EnumInitializer::Symbol(elem_name) => {
                                        Ok(Some(ExprAST::StructStaticSymbol(name.clone(), elem_name.clone(), pos.clone())))
                                    },
                                    EnumInitializer::Tuple(id, list) => {
                                        unimplemented!()
                                    },
                                    EnumInitializer::Struct(id, struct_literal) => {
                                        let literal = EnumLiteral::Struct(struct_literal);
                                        Ok(Some(ExprAST::EnumLiteral(literal, pos.clone())))
                                    },
                                }

                            }else{
                                return Err(ParserError::no_such_a_type(name, pos.clone()));
                            }

                        },
                        Token::BraceLeft => {  // parse struct literal
                            // check symbol(name) is struct
                            if let Some(cls) = defs.get_struct_type(name) {
                                let struct_literal = self.parse_struct_literal(Rc::clone(cls), name, pos, iter, defs, labels)?;
                                Ok(Some(ExprAST::StructLiteral(struct_literal)))
                            }else if let Some(uni) = defs.get_union_type(name) {
                                self.parse_union_literal(Rc::clone(uni), name, pos, iter, defs, labels)
                            }else{
                                return Err(ParserError::no_such_a_struct(name, pos.clone()));
                            }
                        },
                        _ => {
                            Ok(Some(ExprAST::Symbol(name.clone(), pos.clone())))
                        }
                    }
                },
                Token::_Self => {
                    iter.next();  // skip 'Self'

                    let (tok2, pos2) = iter.peek().unwrap();
                    if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                    self.parse_expected_token(iter, Token::WColon)?;  // check '::'

                    let (tok3, pos3) = iter.next().unwrap();
                    if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }

                    let name = if let Token::Symbol(id) = tok3 {
                        id
                    }else{
                        return Err(ParserError::not_symbol(pos3.clone()));
                    };

                    Ok(Some(ExprAST::SelfStaticSymbol(name.to_string(), pos3.clone())))

                },
                Token::_self => {
                    iter.next();  // skip 'self'
                    Ok(Some(ExprAST::_self(pos.clone())))
                },
                Token::ParenLeft => {
                    iter.next(); // skip '('

                    if self.try_parse_type(iter, defs) {
                        self.parse_cast_expression_sub(iter, pos, defs, labels)

                    }else{
                        let result = self.parse_expression(iter, defs, labels)?;
                        self.parse_expected_token(iter, Token::ParenRight)?;
                        Ok(result)
                    }
                },
                _ => {
                    self.parse_constant(iter, defs)
                },
            }
        }else{
            Ok(None)
        }
    }

    fn parse_after_wcolon(&self, typ: &Rc<Type>, enum_name: &str, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<EnumInitializer, ParserError> {
        let (tok, pos) = iter.next().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        let elem_name = if let Token::Symbol(id) = tok {
            id
        }else{
            return Err(ParserError::not_symbol(pos.clone()));
        };

        let (tok2, pos2) = iter.peek().unwrap();
        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

        match tok2 {
            Token::ParenLeft => {  // '('
                if typ.is_enum() {
                    iter.next();  // skip '('




                    unimplemented!()

                }else{
                    Ok(EnumInitializer::Symbol(elem_name.to_string()))
                }
            },
            Token::BraceLeft => {  // '{'
                if ! typ.is_enum() {
                    return Ok(EnumInitializer::Symbol(elem_name.to_string()));
                }

                let enum_def = typ.get_enum_definition().unwrap();
                let fields = enum_def.get_fields();
                let index_map = enum_def.get_index_map().ok_or(ParserError::not_tagged_enum(pos.clone()))?;
                let index = index_map.get(elem_name).ok_or(ParserError::no_such_a_field(enum_name.to_string(), elem_name.to_string(), pos.clone()))?;
                let elem = &fields[*index];
                let struct_type = elem.get_struct_type().ok_or(ParserError::not_struct_type_enum(enum_name.to_string(), elem_name.to_string(), pos.clone()))?;
                let init = self.parse_struct_literal(Rc::clone(struct_type), enum_name, pos, iter, defs, labels)?;

                Ok(EnumInitializer::Struct(elem_name.to_string(), init))
            },
            _ => {
                Ok(EnumInitializer::Symbol(elem_name.to_string()))
            }
        }
    }

    fn parse_struct_literal(&self, typ: Rc<Type>, name: &str, pos: &Position, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<StructLiteral, ParserError> {
        iter.next();  // skip '{'

        let mut map = HashMap::new();
        let mut all_const = true;
        let mut const_map = HashMap::new();

        'outer: loop {
            let (tok3, pos3) = iter.next().unwrap();
            if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }

            let field_name = match tok3 {
                Token::BraceRight => {
                    break 'outer;
                },
                Token::Symbol(id) => {
                    id
                },
                _ => {
                    return Err(ParserError::not_symbol(pos3.clone()));
                }
            };
            if map.contains_key(field_name) {
                return Err(ParserError::duplicate_field_in_struct_initializer(field_name.to_string(), pos3.clone()));
            }

            self.parse_expected_token(iter, Token::Colon)?;  // skip ':'

            if let Some(expr) = self.parse_expression(iter, defs, labels)? {
                let maybe_const = expr.to_const(defs, pos3);
                if maybe_const.is_err() {
                    all_const = false;
                }else if all_const {
                    const_map.insert(field_name.to_string(), maybe_const.ok().unwrap());
                }

                map.insert(field_name.to_string(), Box::new(expr));
            }else{
                return Err(ParserError::not_expr(iter.peek().unwrap().1.clone()));
            }

            let (tok4, pos4) = iter.peek().unwrap();
            if tok4.is_eof() { return Err(ParserError::illegal_end_of_input(pos4.clone())); }
            match tok4 {
                Token::BraceRight => {
                    break 'outer;
                },
                Token::SemiColon => {
                    iter.next();  // skip ';'
                },
                _ => {
                    return Err(ParserError::syntax_error(pos4.clone()));
                },
            }
        }

        // check fields coount
        if let Some(fields) = typ.get_struct_fields() {
            if fields.len() != map.len() {
                return Err(ParserError::number_of_elements_does_not_match(pos.clone()));
            }

            for field in fields {
                let key = field.get_name().clone().unwrap();
                if ! map.contains_key(&key) {
                    return Err(ParserError::no_such_a_field(name.to_string(), key.to_string(), pos.clone()));
                }
            }

        }else{
            if map.len() != 0 {
                return Err(ParserError::number_of_elements_does_not_match(pos.clone()));
            }
        }

        let struct_init = if all_const {
            StructLiteral::ConstLiteral(typ, const_map, pos.clone())
        }else{
            StructLiteral::NormalLiteral(typ, map, pos.clone())
        };

        Ok(struct_init)
    }

    fn parse_union_literal(&self, typ: Rc<Type>, name: &str, pos: &Position, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        iter.next();  // skip '{'

        let mut hash_set = HashSet::new();
        let mut all_const = true;
        let mut expr_vec = Vec::new();
        let mut const_vec = Vec::new();

        'outer: loop {
            let (tok3, pos3) = iter.next().unwrap();
            if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }

            let field_name = match tok3 {
                Token::BraceRight => {
                    break 'outer;
                },
                Token::Symbol(id) => {
                    id
                },
                _ => {
                    return Err(ParserError::not_symbol(pos3.clone()));
                }
            };
            if hash_set.contains(field_name) {
                return Err(ParserError::duplicate_field_in_struct_initializer(field_name.to_string(), pos3.clone()));
            }

            self.parse_expected_token(iter, Token::Colon)?;  // skip ':'

            if let Some(expr) = self.parse_expression(iter, defs, labels)? {
                let maybe_const = expr.to_const(defs, pos3);
                if maybe_const.is_err() {
                    all_const = false;
                }else if all_const {
                    // const_map.insert(field_name.to_string(), maybe_const.ok().unwrap());
                    const_vec.push((field_name.to_string(), maybe_const.ok().unwrap()));
                }

                hash_set.insert(field_name.to_string());
                expr_vec.push((field_name.to_string(), Box::new(expr)));
            }else{
                return Err(ParserError::not_expr(iter.peek().unwrap().1.clone()));
            }

            let (tok4, pos4) = iter.peek().unwrap();
            if tok4.is_eof() { return Err(ParserError::illegal_end_of_input(pos4.clone())); }
            match tok4 {
                Token::BraceRight => {
                    break 'outer;
                },
                Token::SemiColon => {
                    iter.next();  // skip ';'
                },
                _ => {
                    return Err(ParserError::syntax_error(pos4.clone()));
                },
            }
        }

        // // check fields coount
        // if let Some(fields) = typ.get_union_fields() {
        //     if fields.len() != map.len() {
        //         return Err(ParserError::number_of_elements_does_not_match(pos.clone()));
        //     }

        //     for field in fields {
        //         let key = field.get_name().clone().unwrap();
        //         if ! map.contains_key(&key) {
        //             return Err(ParserError::no_such_a_field(name.to_string(), key.to_string(), pos.clone()));
        //         }
        //     }

        // }else{
        //     if map.len() != 0 {
        //         return Err(ParserError::number_of_elements_does_not_match(pos.clone()));
        //     }
        // }

        let union_init = if all_const {
            ExprAST::UnionConstLiteral(typ, const_vec, pos.clone())
        }else{
            ExprAST::UnionLiteral(typ, expr_vec, pos.clone())
        };

        Ok(Some(union_init))
    }

    fn parse_constant(&self, iter: &mut Peekable<Iter<(Token, Position)>>, _defs: &mut Defines) -> Result<Option<ExprAST>, ParserError> {
        if let Some((tok, pos)) = iter.peek() {
            match &*tok {
                Token::CharLiteral(ch) => {
                    iter.next();
                    Ok(Some(ExprAST::Char(*ch, pos.clone())))
                },
                // Token::ShortLiteral(num) => {
                //     iter.next();
                //     Ok(Some(AST::Short(*num)))
                // },
                Token::IntLiteral(num) => {
                    iter.next();
                    Ok(Some(ExprAST::Int(*num, pos.clone())))
                },
                // Token::LongLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::Long(*num)))
                // },
                // Token::LongLongLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::LongLong(*num)))
                // },
                // Token::UCharLiteral(ch) => {
                //     iter.next();
                //     Ok(Some(ExprAST::UChar(*ch)))
                // },
                // Token::UShortLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::UShort(*num)))
                // },
                // Token::UIntLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::UInt(*num)))
                // },
                // Token::ULongLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::ULong(*num)))
                // },
                // Token::ULongLongLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::ULongLong(*num)))
                // },
                // Token::FloatLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::Float(*num)))
                // },
                Token::DoubleLiteral(num) => {
                    iter.next();
                    Ok(Some(ExprAST::Double(*num, pos.clone())))
                },
                Token::StringLiteral(s) => {
                    iter.next();
                    Ok(Some(ExprAST::StringLiteral(s.clone(), pos.clone())))
                },
                // TODO: enumeration-constant


                _ => Ok(None),
            }
        }else{
            Ok(None)
        }
    }

    pub fn parse_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_assignment_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::Comma => {
                        if let Some(code) = self.parse_expression_sub(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }

            Ok(Some(ast))
        }else{  // None
            Ok(None)
        }
    }

    fn parse_expression_sub<'a>(&'a self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::Comma => {
                        iter.next(); // skip ','

                        if let Some(right) = self.parse_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::Comma, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::Comma, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_assignment_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_logical_or_expression(iter, defs, labels)? {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::Question => {
                        iter.next();  // skip '?'
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        let then_expr = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        self.parse_expected_token(iter, Token::Colon)?;
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        let else_expr = self.parse_conditional_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;
    
                        return Ok(Some(ExprAST::TernaryOperator(Box::new(ast), Box::new(then_expr), Box::new(else_expr), pos.clone())));
                    },
                    Token::Assign | Token::AddAssign | Token::SubAssign | Token::MulAssign | Token::DivAssign | Token::ModAssign 
                     | Token:: ShiftLeftAssign | Token::ShiftRightAssign | Token::BitAndAssign | Token::BitOrAssign | Token::BitXorAssign =>
                    {
                        ast = self.parse_assignment_expression2(iter, ast, defs, labels)?;
 
                    },
                    _ => (),  // do nothing
                }

                Ok(Some(ast))

            }else{
                Ok(Some(ast))
            }

        }else{  // None
            Ok(None)
        }
    }
        
    fn parse_assignment_expression2(&self, iter: &mut Peekable<Iter<(Token, Position)>>, l_value: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<ExprAST, ParserError> {
        let mut result = l_value;
        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::Assign => {
                        iter.next();  // skip '='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::need_expr(pos.clone()))?;

                        result = ExprAST::Assign(Box::new(result), Box::new(r_value), pos.clone());
                    },
                    Token::AddAssign => {
                        iter.next(); // skip '+='
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        result = ExprAST::OpAssign(BinOp::Add, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::SubAssign => {
                        iter.next(); // skip '-='
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        result = ExprAST::OpAssign(BinOp::Sub, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::MulAssign => {
                        iter.next(); // skip '*='
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        result = ExprAST::OpAssign(BinOp::Mul, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::DivAssign => {
                        iter.next(); // skip '/='
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        result = ExprAST::OpAssign(BinOp::Div, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::ModAssign => {
                        iter.next(); // skip '%='
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        result = ExprAST::OpAssign(BinOp::Mod, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::ShiftLeftAssign => {
                        iter.next(); // skip '%='
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        result = ExprAST::OpAssign(BinOp::ShiftLeft, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::ShiftRightAssign => {
                        iter.next(); // skip '%='
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        result = ExprAST::OpAssign(BinOp::ShiftRight, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::BitAndAssign => {
                        iter.next(); // skip '%='
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        result = ExprAST::OpAssign(BinOp::BitAnd, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::BitOrAssign => {
                        iter.next(); // skip '%='
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        result = ExprAST::OpAssign(BinOp::BitOr, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::BitXorAssign => {
                        iter.next(); // skip '%='
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        result = ExprAST::OpAssign(BinOp::BitXor, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },

                    _ => break,
                }
            }else{
                break;
            }
        }
        Ok(result)
    }

    fn parse_parameter_type_list(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Params, ParserError> {
        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        let cust_self;
        if *tok == Token::BitAnd {  // '&'
            iter.next();  // skip '&'

            // let (tok2, pos2) = iter.next().ok_or(ParserError::illegal_end_of_input(pos.clone()))?;
            let (tok2, pos2) = iter.next().unwrap();
            if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
            if *tok2 != Token::_self {
                return Err(ParserError::not_self_after_ref(pos2.clone()));
            }

            cust_self = Some(CustSelf::Ref(Rc::clone(defs.get_self_type(pos)?)));

            // let (tok3, _pos3) = iter.peek().ok_or(ParserError::illegal_end_of_input(pos.clone()))?;
            let (tok3, pos3) = iter.peek().unwrap();
            if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }
            if *tok3 == Token::ParenRight {
                let params = Params::new_with_self(cust_self, Vec::new(), false);
                return Ok(params);
            }

        }else{
            cust_self = None;
        }

        let (param_type_list, mut has_variadic) = self.parse_parameter_list(iter, defs, labels)?;
        if let Some((tok, pos)) = iter.peek() {
            match *tok {
                Token::Comma => {
                    iter.next();  // skip ','

                    let (tok2, pos2) = iter.next().unwrap();
                    if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                    if *tok2 == Token::TripleDot {
                        has_variadic = true;
                    }else{
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        return Err(ParserError::syntax_error(pos.clone()));
                    }
                },
                Token::ParenRight => {

                    // do nothing


                },
                _ => {
                    // do nothing
                }
            }
        }

        let params = Params::new_with_self(cust_self, param_type_list, has_variadic);
        Ok(params)
    }

    fn parse_parameter_list(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(Vec<Param>, bool), ParserError> {
        self.parse_parameter_declaration(iter, defs, labels)
    }

    fn parse_parameter_declaration(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(Vec<Param>, bool), ParserError> {
        let mut list = Vec::new();
        let mut has_variadic = false;
        let ds_or_variadic = self.parse_declaration_specifier(iter, defs, labels)?;

        match ds_or_variadic {
            DeclarationSpecifierOrVariadic::DS(ds) => {
                let (decl, _initializer) = self.parse_declarator(ds.get_type(), iter, defs, &mut None)?;
                list.push(Param::new(ds, decl, defs, &iter.peek().unwrap().1)?);
        
                let (tok, pos) = iter.peek().unwrap();
                if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

                match tok {
                    Token::Comma => {
                        (list, has_variadic) = self.parse_parameter_declaration2(list, iter, defs, labels)?;
                    },
                    Token::ParenRight => (),  // do nothing
                    _ => {
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        return Err(ParserError::syntax_error(pos.clone()));
                    }
                }
            },
            DeclarationSpecifierOrVariadic::Variadic => {
                has_variadic = true;
            }
        }

        Ok((list, has_variadic))
    }

    fn parse_parameter_declaration2(&self, mut list: Vec<Param>, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(Vec<Param>, bool), ParserError> {
        let mut has_variadic = false;
        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::Comma => {
                        iter.next(); // skip ','

                        let ds_or_variadic = self.parse_declaration_specifier(iter, defs, labels)?;
                        match ds_or_variadic {
                            DeclarationSpecifierOrVariadic::DS(ds) => {
                                let (decl2, _initializer2) = self.parse_declarator(ds.get_type(), iter, defs, &mut None)?;
                                list.push(Param::new(ds, decl2, defs, pos)?);
                            },
                            DeclarationSpecifierOrVariadic::Variadic => {
                                has_variadic = true;
                                break;
                            },
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok((list, has_variadic))
    }

    fn parse_type_name(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(SpecifierQualifier, TypeOrVariadic, Option<AbstractDeclarator>), ParserError> {
        let (sq, type_or_variadic, _pos) = self.parse_type_specifier_qualifier(iter, defs, labels)?;
        let opt_abstract_decl = self.parse_abstract_declarator(iter, defs, labels)?;
        Ok((sq, type_or_variadic, opt_abstract_decl))
    }

    fn parse_abstract_declarator(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AbstractDeclarator>, ParserError> {
        let pointer = self.parse_pointer(iter, defs)?;
        let direct_abs_decl = self.parse_direct_abstract_declarator(iter, defs, labels)?;

        if pointer.is_none() && direct_abs_decl.is_none(){
            Ok(None)
        }else{
            Ok(Some(AbstractDeclarator::new(pointer, direct_abs_decl)))
        }
    }

    fn parse_direct_abstract_declarator(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<DirectAbstractDeclarator>, ParserError> {
        // let (tok, _pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        if *tok == Token::ParenLeft {
            iter.next();  // skip '('
            let abs_decl = self.parse_abstract_declarator(iter, defs, labels)?;
            self.parse_expected_token(iter, Token::ParenRight)?;  // skip ')'
            Ok(Some(self.parse_direct_abstract_declarator_sub(abs_decl, iter, defs, labels)?))

        }else{
            Ok(None)
        }
    }

    fn parse_direct_abstract_declarator_sub(&self, abs_decl: Option<AbstractDeclarator>, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<DirectAbstractDeclarator, ParserError> {
        // let (tok, _pos) = iter.next().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.next().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        let mut decl = DirectAbstractDeclarator::new_simple(abs_decl);
        loop {
            match tok {
                Token::ParenLeft => {
                    let param_list = self.parse_parameter_type_list(iter, defs, labels)?;
                    self.parse_expected_token(iter, Token::ParenRight)?;  // skip ')'

                    decl = DirectAbstractDeclarator::new_funcall(
                        decl,
                        param_list
                    );
                },
                Token::BracketLeft => {
                    let const_expr = self.parse_constant_expression(iter, defs, labels)?;
                    self.parse_expected_token(iter, Token::BracketRight)?;

                    decl = DirectAbstractDeclarator::new_array_access(
                        decl,
                        const_expr
                    );
                },
                _ => {
                    break Ok(decl);
                }
            }
        }
    }

    // fn parse_typedef_name(&self, _iter: &mut Peekable<Iter<(Token, Position)>>, _defs: &mut Defines) -> Result<Option<TypeSpecifier>, ParserError> {
    // }

    fn parse_declaration(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>, _pos: &Position) -> Result<Option<AST>, ParserError> {
        let ds = self.parse_declaration_specifier(iter, defs, labels)?;
        let ds = ds.get_declaration_specifier().unwrap();
        let typ = ds.get_type();

        // parse init_declarator
        let mut v = Vec::new();
        let mut cur_pos;
        loop {
            let (decl, opt_initializer) = self.parse_declarator(typ, iter, defs, labels)?;
            let name = decl.get_name();
            if defs.exists_var(&name) {
                return Err(ParserError::already_var_defined(&name, iter.peek().unwrap().1.clone()));
            }

            let (tok, pos) = iter.next().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            cur_pos = pos.clone();

            match tok {
                Token::SemiColon => {
                    let declaration = Declaration::new(decl, opt_initializer);
                    v.push(declaration);
                    break;
                },
                Token::Comma => {
                    let declaration = Declaration::new(decl, opt_initializer);
                    v.push(declaration);
                    continue;
                },
                Token::Assign => {
                    let target_type = decl.make_type(typ);
                    let target_type = Rc::new(target_type);
                    let init_expr = self.parse_initializer(&target_type, iter, defs, labels)?;
                    let declaration = Declaration::new(decl, Some(init_expr));
                    v.push(declaration);

                    let (tok3, pos3) = iter.next().unwrap();
                    if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }

                    match tok3 {
                        Token::SemiColon => {
                            break;
                        },
                        Token::Comma => {
                            continue;
                        },
                        _ => {
                            break;
                        }
                    }
                },
                _ => {
                    println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                    return Err(ParserError::syntax_error(pos.clone()));
                }
            }
        }

        let (ds, declarations) = self.parse_def_var(ds.clone(), v, defs, &iter.peek().unwrap().1)?;
        let ast = AST::DefVar { specifiers: ds, declarations, pos: cur_pos };
        Ok(Some(ast))
    }

    // fn parse_init_declarator(&self, _typ: &Type, _iter: &mut Peekable<Iter<(Token, Position)>>, _defs: &mut Defines, _labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
    // }

    fn parse_initializer(&self, target_type: &Rc<Type>, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Initializer, ParserError> {
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        let init_expr;
        if *tok == Token::BraceLeft {
            iter.next();  // skip '{'
            init_expr = self.parse_struct_initializer_list(target_type, pos, iter, defs, labels)?;
        }else{
            let expr = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::need_expr(pos.clone()))?;
            init_expr = Initializer::Simple(expr, pos.clone());
        }
        Ok(init_expr)
    }

    fn parse_struct_initializer_list(&self, target_type: &Rc<Type>, start_pos: &Position, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Initializer, ParserError> {
        let mut list: Vec<Box<Initializer>> = Vec::new();

        if ! target_type.is_struct() { return Err(ParserError::not_struct_type_when_parsing_struct_initializer(start_pos.clone())) }
        let fields = target_type.get_struct_fields().unwrap();
        let mut index = 0;

        loop {
            let opt_type = fields[index].get_type();
            if opt_type.is_none() { return Err(ParserError::no_type_while_parsing_struct_initializer(start_pos.clone())) }
            let typ = opt_type.unwrap();

            let initializer = self.parse_initializer(typ, iter, defs, labels)?;
            list.push(Box::new(initializer));

            let (tok2, pos2) = iter.next().unwrap();
            if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

            match tok2 {
                Token::BraceRight => {
                    break;
                },
                Token::Comma => {
                    let (tok3, pos3) = iter.peek().unwrap();
                    if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }

                    if *tok3 == Token::BraceRight {
                        iter.next();  // skip '}'
                        break;
                    }
                },
                _ => {
                    return Err(ParserError::need_brace_right_or_comma_when_parsing_initializer_list(pos2.clone()));
                }
            }

            index += 1;
        }

        Ok(Initializer::Struct(list, Rc::clone(target_type), start_pos.clone()))
    }

    fn parse_array_initializer(&self, item_type: &Rc<Type>, dimension: &mut Vec<Option<usize>>, index: usize, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Initializer, ParserError> {
        let (tok, pos) = iter.next().unwrap();  // skip '{'
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok != Token::BraceLeft { return Err(ParserError::not_l_brace_parsing_array_initializer(tok.clone(), pos.clone())) }

        let dim_len = dimension.len();
        let mut list: Vec<Box<Initializer>> = Vec::new();
        loop {
            let initializer;
            if index < dim_len - 1 {
                initializer = self.parse_array_initializer(item_type, dimension, index + 1, iter, defs, labels)?;
            }else{
                initializer = self.parse_initializer(item_type, iter, defs, labels)?;
            }

            list.push(Box::new(initializer));

            let (tok2, pos2) = iter.next().unwrap();
            if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

            match tok2 {
                Token::BraceRight => {
                    break;
                },
                Token::Comma => {
                    let (tok3, pos3) = iter.peek().unwrap();
                    if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }

                    if *tok3 == Token::BraceRight {
                        iter.next();  // skip '}'
                        break;
                    }
                },
                _ => {
                    return Err(ParserError::need_brace_right_or_comma_when_parsing_initializer_list(pos2.clone()));
                }
            }
        }

        let opt_len = dimension[index];
        if opt_len.is_none() {
            dimension[index] = Some(list.len());
        }else{
            let required_len = opt_len.unwrap();
            let real_len = list.len();
            if required_len != real_len {
                return Err(ParserError::array_length_mismatch(required_len, real_len, pos.clone()));
            }
        }

        Ok(Initializer::Array(list, item_type.clone(), pos.clone()))
    }

    fn parse_compound_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Block, ParserError> {
        self.parse_expected_token(iter, Token::BraceLeft)?;
        defs.new_local();

        let mut body = Vec::new();
        loop {
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

            if *tok == Token::BraceRight {
                break;
            }
            if *tok == Token::SemiColon {
                iter.next();  // skip ';'
                continue;
            }

            if let Some(ast) = self.parse_statement(iter, defs, labels)? {
                body.push(ast);
            }else{
                // do nothing
            }
        }

        defs.remove_local();

        self.parse_expected_token(iter, Token::BraceRight)?;

        let block = Block::new_with_block(body);
        Ok(block)
    }

    fn parse_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        if let Some((tok, pos)) = iter.peek() {
            match tok {
                // empty statement
                Token::SemiColon => {
                    iter.next();  // skip ';'
                    Ok(None)
                },
                // compound statement
                Token::BraceLeft => {
                    let block = self.parse_compound_statement(iter, defs, labels)?;
                    Ok(Some(AST::Block(block, pos.clone())))
                },
                // selection statement
                Token::If => {
                    iter.next();  // skip 'if'
                    self.parse_expected_token(iter, Token::ParenLeft)?;  // skip '('

                    println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                    let cond = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                    self.parse_expected_token(iter, Token::ParenRight)?;  // skip ')'

                    println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                    let then = self.parse_statement(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                    if let Some((tok2, pos2)) = iter.peek() {
                        if *tok2 == Token::Else {
                            iter.next();  // skip 'else'

                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            let _else = self.parse_statement(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos2.clone()))?;
                            Ok(Some(AST::If(Box::new(cond), Box::new(then), Some(Box::new(_else)), pos.clone())))
                        }else{
                            Ok(Some(AST::If(Box::new(cond), Box::new(then), None, pos.clone())))
                        }
                    }else{
                        Ok(Some(AST::If(Box::new(cond), Box::new(then), None, pos.clone())))
                    }
                },
                Token::Switch => {
                    iter.next();  // skip 'switch'
                    self.parse_expected_token(iter, Token::ParenLeft)?;
                    let expr = self.parse_expression(iter, defs, labels)?;
                    self.parse_expected_token(iter, Token::ParenRight)?;

                    let stmt = self.parse_statement(iter, defs, labels)?;

                    let cond_expr = match expr {
                        Some(ast) => Some(Box::new(ast)),
                        None => None,
                    };
                    let switch = Switch::new(cond_expr, stmt);

                    Ok(Some(AST::Switch(switch, pos.clone())))
                },
                // iteration statement
                Token::While => {
                    iter.next();  // skip 'while'
                    self.parse_expected_token(iter, Token::ParenLeft)?;  // skip '('

                    let condition = self.parse_expression(iter, defs, labels)?;

                    self.parse_expected_token(iter, Token::ParenRight)?;  // skip ')'

                    let stmt = self.parse_statement(iter, defs, labels)?;

                    Ok(Some(AST::Loop {
                        init_expr: None,
                        pre_condition: if let Some(cond) = condition {
                            Some(Box::new(cond))
                        }else{
                            None
                        },
                        body: if let Some(stmt) = stmt {
                            Some(Box::new(stmt))
                        }else{
                            None
                        },
                        update_expr: None,
                        post_condition: None,
                        pos: pos.clone(),
                    }))
                },
                Token::Do => {
                    iter.next();                                           // skip 'do'

                    let stmt = self.parse_statement(iter, defs, labels)?;

                    self.parse_expected_token(iter, Token::While)?;        // skip 'while'
                    self.parse_expected_token(iter, Token::ParenLeft)?;    // skip '('

                    let cond = self.parse_expression(iter, defs, labels)?;

                    self.parse_expected_token(iter, Token::ParenRight)?;    // skip ')'
                    self.parse_expected_token(iter, Token::SemiColon)?;     // skip ';'

                    Ok(Some(AST::Loop {
                        init_expr: None,
                        pre_condition: None,
                        body: if let Some(stmt) = stmt {
                            Some(Box::new(stmt))
                        }else{
                            None
                        },
                        update_expr: None,
                        post_condition: if let Some(expr) = cond {
                            Some(Box::new(expr))
                        }else{
                            None
                        },
                        pos: pos.clone(),
                    }))
                },
                Token::For => {
                    self.parse_for(iter, defs, &pos, labels)
                },

                // jump statement
                Token::Goto => {
                    iter.next();  // skip 'goto'

                    let (id, pos2) = iter.next().unwrap();
                    if id.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                    if let Token::Symbol(name) = id {
                        Ok(Some(AST::Goto(name.to_string(), pos.clone())))
                    }else{
                        Err(ParserError::no_id_for_goto_statement(pos2.clone()))
                    }
                },
                Token::Continue => {
                    iter.next();  // skip 'continue'
                    self.parse_expected_token(iter, Token::SemiColon)?;  // skip ';'

                    Ok(Some(AST::Continue(pos.clone())))
                },
                Token::Break => {
                    iter.next();  // skip 'break'
                    self.parse_expected_token(iter, Token::SemiColon)?;  // skip ';'

                    Ok(Some(AST::Break(pos.clone())))
                },
                Token::Return => {
                    iter.next();  // skip 'return'
                    // let (t, _pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(pos.clone()))?;
                    let (t, pos2) = iter.peek().unwrap();
                    if t.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                    if *t == Token::SemiColon {
                        iter.next(); // skip ';'
                        Ok(Some(AST::Return(None, pos.clone())))
                    }else{
                        if let Some(ast) = self.parse_expression(iter, defs, labels)? {
                            self.parse_expected_token(iter, Token::SemiColon)?;
                            Ok(Some(AST::Return(Some(Box::new(ast)), pos.clone())))
                        }else{
                            Err(ParserError::illegal_end_of_input(pos.clone()))
                        }
                    }
                },
                // labeled-statement
                Token::Case => {
                    self.parse_case_labeled_statement(iter, defs, labels, pos)
                },
                Token::Default => {
                    self.parse_default_labeled_statement(iter, defs, labels)
                },
                // labeled-statement or expression-statement
                Token::Symbol(id) => {
                    if defs.exists_var(id) {
                        self.parse_expression_statement(iter, defs, labels)
                    }else if defs.exists_type(id) {
                        let decl = self.parse_declaration(iter, defs, labels, pos)?;
                        Ok(decl)
                    }else{
                        iter.next();
                        self.parse_labeled_statement(id, iter, defs, labels, pos)
                    }
                },
                Token::Auto | Token::Register | Token::Static | Token::Extern | Token::Typedef |
                Token::Void | Token::Char | Token::Short | Token::Int | Token::Long | Token::Float |
                Token::Double | Token::Signed | Token::Unsigned | Token::Struct | Token::Enum =>
                {
                    self.parse_declaration(iter, defs, labels, pos)
                },
                _ => {
                    self.parse_expression_statement(iter, defs, labels)
                },
            }

        }else{
            Ok(None)
        }
    }

    fn parse_for(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, pos: &Position, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        iter.next();  // skip 'for'
        self.parse_expected_token(iter, Token::ParenLeft)?; // '('
        defs.new_local();

        let (tok2, pos2) = iter.peek().unwrap();
        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

        let init_expr = if *tok2 == Token::SemiColon {
            None
        }else{
            self.parse_for_init(iter, defs, labels)?
        };

        self.parse_expected_token(iter, Token::SemiColon)?; // ';'

        let (tok3, pos3) = iter.peek().unwrap();
        if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }

        let cond = if *tok3 == Token::SemiColon {
            None
        }else{
            self.parse_expression(iter, defs, labels)?
        };

        self.parse_expected_token(iter, Token::SemiColon)?; // ';'

        let (tok4, pos4) = iter.peek().unwrap();
        if tok4.is_eof() { return Err(ParserError::illegal_end_of_input(pos4.clone())); }

        let step = if *tok4 == Token::ParenRight {
            None
        }else{
            self.parse_expression(iter, defs, labels)?
        };

        self.parse_expected_token(iter, Token::ParenRight)?; // ')'

        let body = self.parse_statement(iter, defs, labels)?;

        defs.remove_local();
        Ok(Some(AST::Loop {
            init_expr: if let Some(expr) = init_expr {
                Some(Box::new(expr))
            }else{
                None
            },
            pre_condition: if let Some(expr) = cond {
                Some(Box::new(expr))
            }else{
                None
            },
            body: if let Some(expr) = body {
                Some(Box::new(expr))
            }else{
                None
            },
            update_expr: if let Some(expr) = step {
                Some(Box::new(expr))
            }else{
                None
            },
            post_condition: None,
            pos: pos.clone(),
        }))
    }

    // only for init-expr in for-statement
    pub fn parse_simple_declaration_or_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        if self.is_type(tok, defs) {
            self.parse_simple_declaration(pos, iter, defs, labels)
        }else{
            if let Some(expr) = self.parse_assignment_expression(iter, defs, labels)? {
                Ok(Some(expr))
            }else{
                Ok(None)
            }

        }
    }

    fn is_type(&self, tok: &Token, defs: &Defines) -> bool {
        if tok.is_type() {
            true
        }else{
            match tok {
                Token::Symbol(name) => defs.is_type(name),
                _ => false,
            }
        }
    }

    pub fn parse_simple_declaration(&self, start_pos: &Position, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let ds = self.parse_declaration_specifier(iter, defs, labels)?;
        let ds = ds.get_declaration_specifier().unwrap();
        let (decl, _opt_initializer) = self.parse_declarator(ds.get_type(), iter, defs, labels)?;

        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        match tok {
            Token::Comma => {
                let declaration = Declaration::new(decl, None);
                let (ds, declarations) = self.parse_def_var(ds.clone(), vec![declaration], defs, pos)?;
                let ast = ExprAST::DefVar { specifiers: ds, declarations: declarations, pos: start_pos.clone() };
                Ok(Some(ast))
            },
            Token::Assign => {
                iter.next();  // skip '='

                let typ = ds.get_type();
                let expr = self.parse_initializer(typ, iter, defs, labels)?;
                let declaration = Declaration::new(decl, Some(expr));
                let (ds, declarations) = self.parse_def_var(ds.clone(), vec![declaration], defs, pos)?;
                let ast = ExprAST::DefVar { specifiers: ds, declarations: declarations, pos: start_pos.clone() };
                Ok(Some(ast))
            },
            _ => {
                println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                Err(ParserError::syntax_error(pos.clone()))
            },
        }
    }

    fn parse_for_init(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_simple_declaration_or_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::Comma => {
                        if let Some(code) = self.parse_for_init_sub(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }

            Ok(Some(ast))
        }else{  // None
            Ok(None)
        }
    }

    fn parse_for_init_sub<'a>(&'a self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::Comma => {
                        iter.next(); // skip ','

                        if let Some(right) = self.parse_for_init(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::Comma, Box::new(left), Box::new(right), ast.get_position().clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::Comma, Box::new(ast.clone()), Box::new(right), ast.get_position().clone()));
                            }
                        }else{
                            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }


    fn parse_labeled_statement(&self, id: &str, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>, pos: &Position) -> Result<Option<AST>, ParserError> {
        self.parse_expected_token(iter, Token::Colon).or(Err(ParserError::undefined_symbol(id, pos.clone())))?;
        let stmt = self.parse_statement(iter, defs, labels)?;

        if let Some(v) = labels {
            v.push(id.to_string());
        }else{
            return Err(ParserError::labeled_statement_without_function(iter.peek().unwrap().1.clone()));
        }

        if let Some(s) = stmt {
            Ok(Some(AST::Labeled(id.to_string(), Some(Box::new(s)), pos.clone())))
        }else{
            Ok(Some(AST::Labeled(id.to_string(), None, pos.clone())))
        }
    }

    fn parse_case_labeled_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>, pos: &Position) -> Result<Option<AST>, ParserError> {
        self.parse_expected_token(iter, Token::Case)?;
        let constant_condition = if let Some(cond) = self.parse_constant_expression(iter, defs, labels)? {
            cond
        }else{
            return Err(ParserError::no_constant_expr_after_case(iter.peek().unwrap().1.clone()));
        };

        self.parse_expected_token(iter, Token::Colon)?;

        let stmt = if let Some(s) = self.parse_statement(iter, defs, labels)? {
            s
        }else{
            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
            return Err(ParserError::syntax_error(iter.peek().unwrap().1.clone()));
        };
        let case = Case::new(constant_condition, Box::new(stmt), pos.clone());
        Ok(Some(AST::Case(case, pos.clone())))
    }

    fn parse_default_labeled_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        let pos = self.parse_expected_token(iter, Token::Default)?;
        self.parse_expected_token(iter, Token::Colon)?;

        let stmt = if let Some(s) = self.parse_statement(iter, defs, labels)? {
            s
        }else{
            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
            return Err(ParserError::syntax_error(iter.peek().unwrap().1.clone()))?;
        };
        Ok(Some(AST::Default(Box::new(stmt), pos.clone())))
    }

    fn parse_expression_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        if let Some((tok, pos)) = iter.peek() {
            match tok {
                Token::SemiColon => {
                    iter.next();
                    Ok(None)
                },
                _ => {
                    let expr = self.parse_expression(iter, defs, labels)?;
                    self.parse_expected_token(iter, Token::SemiColon)?;
                    if let Some(e) = expr {
                        Ok(Some(AST::Expr(Box::new(e), pos.clone())))
                    }else{
                        Ok(None)
                    }
                }
            }

        }else{
            Ok(None)
        }
    }

    // fn parse_selection_statement(&self, _iter: &mut Peekable<Iter<(Token, Position)>>, _defs: &mut Defines, _labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
    // }

    // fn parse_iteration_statement(&self, _iter: &mut Peekable<Iter<(Token, Position)>>, _defs: &mut Defines, _labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
    // }

    // fn parse_jump_statement(&self, _iter: &mut Peekable<Iter<(Token, Position)>>, _defs: &mut Defines, _labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
    // }

    fn parse_expected_token(&self, iter: &mut Peekable<Iter<(Token, Position)>>, expected: Token) -> Result<Position, ParserError> {
        // if let Some((tok, pos)) = iter.next() {
        //     if *tok == expected {
        //         Ok(pos.clone())
        //     }else{
        //         Err(ParserError::without_expected_token(pos.clone(), expected.clone(), tok.clone()))
        //     }
        // }else{
        //     Err(ParserError::illegal_end_of_input(None))
        // }
        let (tok, pos) = iter.next().unwrap();
        if *tok == expected {
            Ok(pos.clone())
        }else{
            Err(ParserError::without_expected_token(expected.clone(), tok.clone(), pos.clone()))
        }
    }

    fn parse_impl(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ToplevelAST>, ParserError> {
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok != Token::Impl {
            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
            return Err(ParserError::syntax_error(pos.clone()));
        }

        iter.next();  // skip 'impl'

        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        let impl_name = match tok {
            Token::Symbol(id) => {
                iter.next();
                id
            },
            _ => {
                return Err(ParserError::not_symbol_while_parsing_impl(pos.clone()));
            },
        };

        let impl_type = defs.get_type(impl_name).ok_or(ParserError::no_such_a_type(impl_name, pos.clone()))?.clone();
        defs.set_self_type(&impl_type)?;

        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        let for_something: Option<String>;
        match tok {
            Token::BraceLeft => {
                for_something = None;
            },
            Token::For => {
                iter.next();  // skip 'for'

                let (tok2, pos2) = iter.peek().unwrap();
                if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
                match tok2 {
                    Token::Symbol(id2) => {
                        for_something = Some(id2.clone());

                        let (tok3, pos3) = iter.peek().unwrap();
                        if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }

                        if *tok3 != Token::BraceLeft {
                            return Err(ParserError::not_brace_left_or_for_while_parsing_impl(tok, pos.clone()));
                        }
                    },
                    _ => {
                        return Err(ParserError::no_id_after_for_while_parsing_impl(tok2, pos2.clone()));
                    }
                }
            },
            _ => {
                return Err(ParserError::not_brace_left_or_for_while_parsing_impl(tok, pos.clone()));
            }
        }

        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok != Token::BraceLeft {
            println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
            return Err(ParserError::syntax_error(pos.clone()));
        }
        iter.next();  // skip '{'

        let decl = self.parse_impl_declaration_list(iter, defs, labels)?;
        self.parse_expected_token(iter, Token::BraceRight)?;  // skip '}'
        let ast_impl = ToplevelAST::new_impl(impl_name, impl_type, for_something, decl, pos);
        Ok(Some(ast_impl))
    }

    pub fn  parse_impl_declaration_list(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Vec<ImplElement>, ParserError> {
        let mut list = Vec::new();
        loop {
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            if *tok == Token::BraceRight {
                // iter.next();  // skip '}'
                break;
            }

            if let Some(toplevel_ast) = self.parse_impl_declaration(iter, defs, labels)? {
                // syntax check
                match toplevel_ast {
                    ToplevelAST::DefineEnum { name: _, fields: _, pos } => {
                        return Err(ParserError::syntax_error(pos.clone()));
                    },
                    ToplevelAST::DefineStruct { name: _, fields: _, pos } => {
                        return Err(ParserError::syntax_error(pos.clone()));
                    },
                    ToplevelAST::DefineUnion { name: _, fields: _, pos } => {
                        return Err(ParserError::syntax_error(pos.clone()));
                    },
                    ToplevelAST::Impl { name: _, typ: _, for_type: _, defines: _, pos } => {
                        return Err(ParserError::syntax_error(pos.clone()));
                    },
                    ToplevelAST::TypeDef(_, _, pos) => {
                        return Err(ParserError::syntax_error(pos.clone()));
                    },
                    ToplevelAST::GlobalDefVar { specifiers, declaration, pos: _ } => {
                        let elem = ImplElement::DefVar {
                            specifiers,
                            declaration,
                        };

                        list.push(elem);
                    },
                    ToplevelAST::FunProto(proto, _pos) => {
                        let proto = FunOrProt::Proto(proto);
                        let elem = ImplElement::FunOrProt(proto);

                        list.push(elem);
                    },
                    ToplevelAST::Function(func, _pos) => {
                        let fun = FunOrProt::Fun(func);
                        let elem = ImplElement::FunOrProt(fun);

                        list.push(elem);
                    },
                }
            }
        }

        Ok(list)
    }

    pub fn  parse_impl_declaration(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ToplevelAST>, ParserError> {
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        let ds = self.parse_declaration_specifier(iter, defs, labels)?;
        let ds = ds.get_declaration_specifier().unwrap();

        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok == Token::SemiColon {
            iter.next();  // skip ';'

            match ds.get_type().as_ref() {
                Type::Struct { name, fields } => {
                    return Ok(Some(ToplevelAST::DefineStruct{name: name.clone(), fields: fields.clone(), pos: pos.clone()}));
                },
                Type::Union { name, fields } => {
                    return Ok(Some(ToplevelAST::DefineUnion{name: name.clone(), fields: fields.clone(), pos: pos.clone()}));
                },
                Type::Enum { name, enum_def } => {
                    return Ok(Some(ToplevelAST::DefineEnum {name: name.clone(), fields: enum_def.clone(), pos: pos.clone()}));
                }
                _ => {
                    println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                    return Err(ParserError::syntax_error(pos.clone()));
                },
            }
        }

        let (decl, opt_initializer) = self.parse_declarator(ds.get_type(), iter, defs, &mut None)?;

        if ds.is_typedef() {
            self.parse_expected_token(iter, Token::SemiColon)?;  // skip ';'

            let typ = ds.get_type();
            let name = decl.get_name();
            defs.set_typedef(&name, typ, pos)?;

            Ok(Some(ToplevelAST::TypeDef(name.to_string(), Rc::clone(typ), pos.clone())))
        }else{
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
    
            self.process_impl_declarator(decl, opt_initializer, ds, tok, pos, iter, defs)
        }
    }

    fn process_impl_declarator(&self, decl: Declarator, opt_initializer: Option<Initializer>, ds: &DeclarationSpecifier, tok: &Token, pos: &Position, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines) ->  Result<Option<ToplevelAST>, ParserError> {
        match decl.get_direct_declarator() {
            DirectDeclarator::Symbol(_id, _pos2) => {
                match tok {
                    Token::SemiColon => {
                        iter.next();  // skip ';'
                        let declaration = Declaration::new(decl, opt_initializer);

                        Ok(Some(self.make_global_def_var(ds.clone(), vec![declaration], defs, pos)?))
                    },
                    Token::Assign => {
                        iter.next();  // skip '='

                        let mut labels = Vec::new();
                        let typ = ds.get_type();
                        let init_expr = self.parse_initializer(typ, iter, defs, &mut Some(&mut labels))?;
                        self.parse_expected_token(iter, Token::SemiColon)?;
                        let declaration = Declaration::new(decl, Some(init_expr));

                        Ok(Some(self.make_global_def_var(ds.clone(), vec![declaration], defs, pos)?))
                    },
                    _ => {
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        Err(ParserError::syntax_error(pos.clone()))
                    },
                }
            },
            DirectDeclarator::Enclosed(decl2, pos2) => {
                self.process_declarator(decl2.clone(), opt_initializer, ds, tok, pos2, iter, defs)
            },
            DirectDeclarator::ArrayDef(_direct_declarator, opt_usize_list, _pos2) => {
                match tok {
                    Token::SemiColon => {
                        iter.next();  // skip ';'

                        let declaration = Declaration::new(decl.clone(), opt_initializer);

                        Ok(Some(self.make_global_def_array(ds.clone(), vec![declaration], opt_usize_list, defs, pos)?))

                    },
                    Token::Assign => {
                        panic!("unreachable")
                    },
                    _ => {
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        Err(ParserError::syntax_error(pos.clone()))
                    },
                }
            },
            DirectDeclarator::FunctionDef(_direct_declarator, params, _pos2) => {
                match tok {
                    Token::SemiColon => {  // pre function definition
                        iter.next();  // skip ';'

                        let proto = ToplevelAST::FunProto(FunProto {
                                specifiers: ds.clone(),
                                declarator: decl.clone(),
                                params: params.clone(),
                            },
                            pos.clone()
                        );

                        defs.remove_function_local();
                        defs.set_function(decl.get_name(), ds.clone(), decl.clone(), params.clone(), pos)?;

                        Ok(Some(proto))
                    },
                    Token::BraceLeft => {  // function
                        let mut labels = Vec::new();
                        let block = self.parse_compound_statement(iter, defs, &mut Some(&mut labels))?;

                        let func = ToplevelAST::Function(Function {
                                specifiers: ds.clone(),
                                declarator: decl.clone(),
                                params: params.clone(),
                                body: block,
                                labels: labels,
                            },
                            pos.clone()
                        );

                        defs.remove_function_local();
                        defs.set_function(decl.get_name(), ds.clone(), decl.clone(), params.clone(), pos)?;

                        Ok(Some(func))
                    },
                    _ => {
                        defs.remove_function_local();
                        println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        Err(ParserError::syntax_error(pos.clone()))
                    },
                }
            },
        }
    }

}
