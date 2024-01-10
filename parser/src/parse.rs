#![allow(dead_code)]

use super::{Position, Token, Tokenizer};
use super::ParserError;
use super::ast::{AST, ExprAST, Block, Param, Params, BinOp, TypeQualifier, DeclarationSpecifier, SpecifierQualifier, Declarator, DirectDeclarator};
use super::ast::{DeclarationSpecifierOrVariadic, Declaration, StructDeclaration, StructDeclarator, AbstractDeclarator, DirectAbstractDeclarator};
use super::ConstExpr;
use super::types::*;
use super::defines::*;
use super::{CustSelf, Function, FunProto, FunOrProto, Switch, Case};

use std::slice::Iter;
use std::iter::Peekable;

#[derive(Debug)]
pub struct Parser;

impl Parser {
    pub fn new() -> Self {
        Parser
    }

    // pub fn parse_from_str(&self, input: &str) -> Result<Vec<AST>, ParserError> {
    //     let tokenizer = Tokenizer::new();
    //     let token_list = Tokenizer::tokenize(input)?;
    //     let mut iter = token_list.iter().peekable();
    //     let parser = Parser::new();
    //     let mut defs = Defines::new();

    //     parser.parse_translation_unit(&mut iter, &mut defs)
    // }

    pub fn parse(input: Vec<(Token, Position)>) -> Result<Vec<AST>, ParserError> {
        let mut iter = input.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();

        parser.parse_translation_unit(&mut iter, &mut defs)
    }

    // fn parse_external_declaration_from_str(src: &str) -> Result<Option<AST>, ParserError> {
    //     let tokenizer = Tokenizer::new();
    //     let token_list = Tokenizer::tokenize(src).unwrap();
    //     let mut iter = token_list.iter().peekable();
    //     let parser = Parser::new();
    //     let mut defs = Defines::new();
    //     let mut v = Vec::new();
    //     parser.parse_external_declaration(&mut iter, &mut defs, &mut Some(&mut v))
    // }

    fn parse_translation_unit(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines) -> Result<Vec<AST>, ParserError> {
        let mut declarations = Vec::new();
        let mut labels = Vec::new();

        while let Some((tok, _pos)) = iter.peek() {
            if tok.is_eof() { break; }
            if let Some(mut decl) = self.parse_external_declaration(iter, defs, &mut Some(&mut labels))? {
                match decl {
                    AST::DefVar {specifiers, declarations: declaration} => {
                        decl = AST::GlobalDefVar{specifiers, declaration};
                    },
                    _ => {
                    },
                }
                declarations.push(decl);
            }
        }

        Ok(declarations)
    }

    fn make_global_def_var(&self, ds: DeclarationSpecifier, declarations: Vec<Declaration>, defs: &mut Defines) -> Result<AST, ParserError> {
        let typ = ds.get_type();

        for declaration in &declarations {
            let decl = declaration.get_declarator();
            let name = decl.get_name();
            let init = if let Some(expr) = declaration.get_init_expr() {
                Some((**expr).clone())
            }else{
                None
            };
            defs.set_var(name, typ, init)?;
        }

        Ok(AST::GlobalDefVar {
            specifiers: ds,
            declaration: declarations,
        })
    }

    fn make_global_def_array(&self, ds: DeclarationSpecifier, declarations: Vec<Declaration>, opt_size_list: &Vec<Option<ConstExpr>>, defs: &mut Defines) -> Result<AST, ParserError> {
        let typ = ds.get_type();

        for declaration in &declarations {
            let decl = declaration.get_declarator();
            let name = decl.get_name();
            let array_type = Type::Array { name: Some(name.to_string()), typ: Box::new(typ.clone()), opt_size_list: opt_size_list.clone() };
            let init = if let Some(expr) = declaration.get_init_expr() {
                Some((**expr).clone())
            }else{
                None
            };
            defs.set_var(name, &array_type, init)?;
        }

        Ok(AST::GlobalDefVar {
            specifiers: ds,
            declaration: declarations,
        })
    }

    fn parse_def_var(&self, ds: DeclarationSpecifier, declarations: Vec<Declaration>, defs: &mut Defines) -> Result<(DeclarationSpecifier, Vec<Declaration>), ParserError> {
        let typ = ds.get_type();

        for declaration in &declarations {
            let decl = declaration.get_declarator();
            let name = decl.get_name();
            let init = if let Some(expr) = declaration.get_init_expr() {
                Some((**expr).clone())
            }else{
                None
            };
            defs.set_var(name, typ, init)?;
        }

        Ok((ds, declarations))
    }

    fn parse_external_declaration(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        // let (tok, _pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        if *tok == Token::Impl {
            return self.parse_impl(iter, defs, labels);
        }

        let ds = self.parse_declaration_specifier(iter, defs, labels)?;
        let ds = ds.get_declaration_specifier().unwrap();

        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok == Token::SemiColon {
            iter.next();  // skip ';'

            match ds.get_type() {
                Type::Struct { name, fields } => {
                    return Ok(Some(AST::DefineStruct{name: name.clone(), fields: fields.clone()}));
                },
                Type::Union { name, fields } => {
                    return Ok(Some(AST::DefineUnion{name: name.clone(), fields: fields.clone()}));
                },
                Type::Enum { name, enum_def } => {
                    return Ok(Some(AST::DefineEnum {name: name.clone(), fields: enum_def.clone()}));
                }
                _ => {
                    return Err(ParserError::syntax_error(Some(pos.clone())));
                },
            }
        }

        let decl = self.parse_declarator(iter, defs, &mut None)?;

        if ds.is_typedef() {
            self.parse_expected_token(iter, Token::SemiColon)?;  // skip ';'

            let typ = ds.get_type();
            let name = decl.get_name();
            defs.set_typedef(&name, typ)?;

            Ok(Some(AST::TypeDef(name.to_string(), typ.clone())))
        }else{
            // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok, pos) = iter.peek().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
    
            match decl.get_direct_declarator() {
                DirectDeclarator::Symbol(_id) => {
                    match tok {
                        Token::SemiColon => {
                            iter.next();  // skip ';'
                            let declaration = Declaration::new(decl, None);
                            Ok(Some(self.make_global_def_var(ds.clone(), vec![declaration], defs)?))
                        },
                        Token::Assign => {
                            iter.next();  // skip '='

                            let mut labels = Vec::new();
                            // let init_expr = self.parse_expression_statement(iter, defs, &mut Some(&mut labels))?.ok_or(ParserError::need_expr(Some(pos.clone())))?;
                            let init_expr = self.parse_expression(iter, defs, &mut Some(&mut labels))?.ok_or(ParserError::need_expr(Some(pos.clone())))?;
                            self.parse_expected_token(iter, Token::SemiColon)?;
                            let declaration = Declaration::new(decl, Some(Box::new(init_expr)));
                            Ok(Some(self.make_global_def_var(ds.clone(), vec![declaration], defs)?))
                        },
                        _ => {
                            Err(ParserError::syntax_error(Some(pos.clone())))
                        },
                    }
                },
                DirectDeclarator::Enclosed(_decl) => {




                    unimplemented!()
                },
                DirectDeclarator::ArrayDef(_direct_declarator, opt_const_expr_list) => {
                    match tok {
                        Token::SemiColon => {
                            iter.next();  // skip ';'

                            let declaration = Declaration::new(decl.clone(), None);
                            Ok(Some(self.make_global_def_array(ds.clone(), vec![declaration], opt_const_expr_list, defs)?))
                        },
                        Token::Assign => {
                            iter.next();  // skip '='

                            let mut labels = Vec::new();
                            // let init_expr = self.parse_expression_statement(iter, defs, &mut Some(&mut labels))?.ok_or(ParserError::need_expr(Some(pos.clone())))?;
                            let init_expr = self.parse_expression(iter, defs, &mut Some(&mut labels))?.ok_or(ParserError::need_expr(Some(pos.clone())))?;
                            self.parse_expected_token(iter, Token::SemiColon)?;
                            let declaration = Declaration::new(decl.clone(), Some(Box::new(init_expr)));
                            Ok(Some(self.make_global_def_array(ds.clone(), vec![declaration], opt_const_expr_list, defs)?))
                        },
                        _ => {
                            // println!("Syntax Error at {}:{}:{}:", file!(), line!(), column!());
                            Err(ParserError::syntax_error(Some(pos.clone())))
                        },
                    }
                },
                DirectDeclarator::FunctionDef(_direct_declarator, params) => {
                    match tok {
                        Token::SemiColon => {  // pre function definition
                            iter.next();  // skip ';'

                            let proto = AST::FunProto(FunProto {
                                specifiers: ds.clone(),
                                declarator: decl.clone(),
                                params: params.clone(),
                            });

                            defs.remove_function_local();
                            defs.set_function(decl.get_name(), ds.clone(), decl.clone(), params.clone())?;

                            Ok(Some(proto))
                        },
                        Token::BraceLeft => {  // function
                            let mut labels = Vec::new();
                            let block = self.parse_compound_statement(iter, defs, &mut Some(&mut labels))?;
 
                            let func = AST::Function(Function {
                                specifiers: ds.clone(),
                                declarator: decl.clone(),
                                params: params.clone(),
                                body: block,
                                labels: labels,
                            });

                            defs.remove_function_local();
                            defs.set_function(decl.get_name(), ds.clone(), decl.clone(), params.clone())?;

                            Ok(Some(func))
                        },
                        _ => {
                            defs.remove_function_local();
                            Err(ParserError::syntax_error(Some(pos.clone())))
                        },
                    }
                },
            }
        }
    }

    fn parse_impl(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok != Token::Impl {
            return Err(ParserError::syntax_error(Some(pos.clone())));
        }

        iter.next();  // skip 'impl'

        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        let impl_name = match tok {
            Token::Symbol(id) => {
                iter.next();
                id
            },
            _ => {
                return Err(ParserError::not_symbol_while_parsing_impl(Some(pos.clone())));
            },
        };
        let impl_type = defs.get_type(impl_name).ok_or(ParserError::no_such_a_type(Some(pos.clone()), impl_name))?.clone();
        defs.set_self_type(&impl_type)?;

        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        let for_something: Option<String>;
        match tok {
            Token::BraceLeft => {
                for_something = None;
            },
            Token::For => {
                iter.next();  // skip 'for'

                // let (tok2, pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
                let (tok2, pos2) = iter.peek().unwrap();
                println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
                match tok2 {
                    Token::Symbol(id2) => {
                        for_something = Some(id2.clone());

                        // let (tok3, _pos3) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
                        let (tok3, pos3) = iter.peek().unwrap();
                        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                        if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }

                        if *tok3 != Token::BraceLeft {
                            return Err(ParserError::not_brace_left_or_for_while_parsing_impl(Some(pos.clone()), tok));
                        }
                    },
                    _ => {
                        return Err(ParserError::no_id_after_for_while_parsing_impl(Some(pos2.clone()), tok2));
                    }
                }
            },
            _ => {
                return Err(ParserError::not_brace_left_or_for_while_parsing_impl(Some(pos.clone()), tok));
            }
        }

        let functions = self.parse_impl_functions(iter, defs, labels)?;
        self.parse_expected_token(iter, Token::BraceRight)?;
        let ast_impl = AST::new_impl(impl_name, impl_type, for_something, functions);
        Ok(Some(ast_impl))
    }

    fn parse_impl_functions(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Vec<FunOrProto>, ParserError> {
        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok != Token::BraceLeft {
            return Err(ParserError::syntax_error(Some(pos.clone())));
        }
        iter.next();  // skip '{'

        let mut list = Vec::new();
        loop {
            // let (tok, _pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok, pos) = iter.peek().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            if *tok == Token::BraceRight {
                iter.next();  // skip '}'
                break;
            }

            let function = self.parse_impl_function(iter, defs, labels)?;
            list.push(function);
        }

        Ok(list)
    }

    fn parse_impl_function(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<FunOrProto, ParserError> {
        defs.add_new_function_local();

        let ds = self.parse_declaration_specifier(iter, defs, labels)?;
        let ds = ds.get_declaration_specifier().unwrap();
        let decl = self.parse_declarator(iter, defs, &mut None)?;

        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        match decl.get_direct_declarator() {
            DirectDeclarator::FunctionDef(_direct_declarator, params) => {
                match tok {
                    Token::SemiColon => {  // pre function definition
                        iter.next();  // skip ';'

                        let proto = FunOrProto::Proto(FunProto {
                            specifiers: ds.clone(),
                            declarator: decl.clone(),
                            params: params.clone(),
                        });

                        defs.remove_function_local();
                        defs.set_function(decl.get_name(), ds.clone(), decl.clone(), params.clone())?;

                        Ok(proto)
                    },
                    Token::BraceLeft => {  // function
                        let mut labels = Vec::new();
                        let block = self.parse_compound_statement(iter, defs, &mut Some(&mut labels))?;

                        let func = FunOrProto::Fun(Function {
                            specifiers: ds.clone(),
                            declarator: decl.clone(),
                            params: params.clone(),
                            body: block,
                            labels: labels,
                        });

                        defs.remove_function_local();
                        defs.set_function(decl.get_name(), ds.clone(), decl.clone(), params.clone())?;

                        Ok(func)
                    },
                    _ => {
                        defs.remove_function_local();
                        Err(ParserError::syntax_error(Some(pos.clone())))
                    },
                }
            },
            _ => {
                defs.remove_function_local();
                return Err(ParserError::not_function_define_in_impl(Some(pos.clone())));
            },
        }
    }

    fn parse_declaration_specifier(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<DeclarationSpecifierOrVariadic, ParserError> {
        let (sq, type_or_variadic) = self.parse_type_specifier_qualifier(iter, defs, labels)?;

        match type_or_variadic {
            TypeOrVariadic::Type(typ) => {
                let ds = DeclarationSpecifier::new(typ, sq);
                Ok(DeclarationSpecifierOrVariadic::DS(ds))
            },
            TypeOrVariadic::Variadic => Ok(DeclarationSpecifierOrVariadic::Variadic),
        }
    }

    fn parse_type_specifier_qualifier(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(SpecifierQualifier, TypeOrVariadic), ParserError> {
        let mut sq = SpecifierQualifier::new();
        let mut opt_signed: Option<(bool, Position)> = None;
        let mut opt_unsigned: Option<(bool, Position)> = None;
        let mut opt_type: Option<(Type, Position)> = None;

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
                        return Ok((sq, TypeOrVariadic::Variadic));
                    },

                    //
                    // Type
                    //
                    Token::Symbol(name) => {
                        if opt_type.is_some() {
                            break;
                        }

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

                        // let (tok2, pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                        let (tok2, pos2) = iter.peek().unwrap();
                        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
                        match tok2 {
                            Token::Symbol(name) => {
                                iter.next();  // skip Symbol

                                // let (tok3, _pos3) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                                let (tok3, pos3) = iter.peek().unwrap();
                                println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                                if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }
                                match tok3 {
                                    Token::BraceLeft => {
                                        iter.next();  // skip '{'

                                        let declaration = self.parse_struct_declaration_list(iter, defs, labels)?;
                                        let definition = StructDefinition::try_new(Some(name.clone()), Some(declaration))?;
                                        let type_struct = Type::struct_from_struct_definition(Some(name.clone()), definition.clone());
                                        defs.set_struct(name, definition)?;

                                        opt_type = Some((
                                            type_struct,
                                            pos.clone()
                                        ));
println!("opt_type: {:?}", opt_type);
                                        self.parse_expected_token(iter, Token::BraceRight)?;
                                    },
                                    _ => {
                                        let type_struct = if let Some(t) = defs.get_type(&name) {
                                            t.clone()
                                        }else{
                                            let definition = StructDefinition::try_new(Some(name.clone()), None)?;
                                            Type::struct_from_struct_definition(Some(name.clone()), definition)
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
                                let definition = StructDefinition::try_new(None, Some(declaration))?;
                                let type_struct = Type::struct_from_struct_definition(None, definition);
                                opt_type = Some((
                                    type_struct,
                                    pos.clone()
                                ));
println!("struct Definition: {:?}", opt_type);
                                self.parse_expected_token(iter, Token::BraceRight)?;
                            },
                            _ => {
                                return Err(ParserError::syntax_error(Some(pos2.clone())));
                            }
                        }
                    },
                    Token::Union => {
                        iter.next();  // skip 'union'

                        // let (tok2, pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                        let (tok2, pos2) = iter.peek().unwrap();
                        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
                        match tok2 {
                            Token::Symbol(name) => {
                                iter.next();  // skip Symbol

                                // let (tok3, _pos3) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                                let (tok3, pos3) = iter.peek().unwrap();
                                println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                                if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }
                                match tok3 {
                                    Token::BraceLeft => {
                                        iter.next();  // skip '{'

                                        let declaration = self.parse_struct_declaration_list(iter, defs, labels)?;
                                        let definition = StructDefinition::try_new(Some(name.clone()), Some(declaration))?;
                                        let type_union = Type::union_from_struct_definition(Some(name.clone()), definition.clone());
                                        defs.set_union(name, definition)?;

                                        opt_type = Some((
                                            type_union,
                                            pos.clone()
                                        ));

                                        self.parse_expected_token(iter, Token::BraceRight)?;
                                    },
                                    _ => {
                                        let type_union = if let Some(t) = defs.get_type(&name) {
                                            t.clone()
                                        }else{
                                            let definition = StructDefinition::try_new(Some(name.clone()), None)?;
                                            Type::union_from_struct_definition(Some(name.clone()), definition)
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
                                let definition = StructDefinition::try_new(None, Some(declaration))?;
                                let type_struct = Type::union_from_struct_definition(None, definition);
                                opt_type = Some((
                                    type_struct,
                                    pos.clone()
                                ));

                                self.parse_expected_token(iter, Token::BraceRight)?;
                            },
                            _ => {
                                return Err(ParserError::syntax_error(Some(pos2.clone())));
                            }
                        }
                    },
                    Token::Enum => {
                        iter.next();  // skip 'enum'

                        // let (tok2, pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                        let (tok2, pos2) = iter.peek().unwrap();
                        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
                        match tok2 {
                            Token::Symbol(name) => {
                                iter.next();  // skip Symbol
                
                                // let (tok3, _pos3) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                                let (tok3, pos3) = iter.peek().unwrap();
                                println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                                if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }
                                match tok3 {
                                    Token::BraceLeft => {
                                        iter.next();  // skip '{'
                
                                        let enum_list = self.parse_enumerator_list(iter, defs, labels)?;
                                        let definition = EnumDefinition::new(Some(name.clone()), Some(enum_list));
                                        let type_struct = Type::enum_from_enum_definition(Some(name.clone()), definition.clone());
                                        defs.set_enum(name, definition)?;
                
                                        opt_type = Some((
                                            type_struct,
                                            pos.clone()
                                        ));
                
                                        self.parse_expected_token(iter, Token::BraceRight)?;
                                    },
                                    _ => {
                                        let type_struct = if let Some(t) = defs.get_type(&name) {
                                            t.clone()
                                        }else{
                                            let definition = EnumDefinition::new(Some(name.clone()), None);
                                            Type::enum_from_enum_definition(Some(name.clone()), definition)
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
                                let enum_list = self.parse_enumerator_list(iter, defs, labels)?;
                                let definition = EnumDefinition::new(None, Some(enum_list));
                                let type_struct = Type::enum_from_enum_definition(None, definition);
                                opt_type = Some((
                                    type_struct,
                                    pos.clone()
                                ));
                
                                self.parse_expected_token(iter, Token::BraceRight)?;
                            },
                            _ => {
                                return Err(ParserError::syntax_error(Some(pos2.clone())));
                            }
                        }
                    },
                    Token::Signed => {
                        iter.next();
                        if let Some((true, pre_pos)) = opt_unsigned {
                            return Err(ParserError::cannot_combine_with_previous_unsigned_declaration_specifier(Some(pos.clone()), pre_pos.clone()));
                        }
                        if let Some((typ, _pos)) = &opt_type {
                            if ! typ.can_sign() {
                                return Err(ParserError::not_number_signed(Some(pos.clone()), &typ));
                            }
                        }
                        opt_signed = Some((true, pos.clone()));
                    },
                    Token::Unsigned => {
                        iter.next();
                        if let Some((true, pre_pos)) = opt_signed {
                            return Err(ParserError::cannot_combine_with_previous_signed_declaration_specifier(Some(pos.clone()), pre_pos.clone()));
                        }
                        if let Some((typ, _pos)) = &opt_type {
                            if ! typ.can_sign() {
                                return Err(ParserError::not_number_unsigned(Some(pos.clone()), &typ));
                            }
                        }
                        opt_unsigned = Some((true, pos.clone()));
                    },
                    Token::Void => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            return Err(ParserError::already_type_defined(Some(pos.clone()), &Type::Void, &pre_type, &pre_pos));
                        }
                        opt_type = Some((Type::Void, pos.clone()));
                    },
                    Token::_Bool => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            return Err(ParserError::already_type_defined(Some(pos.clone()), &Type::Number(NumberType::_Bool), &pre_type, &pre_pos));
                        }
                        opt_type = Some((Type::Number(NumberType::_Bool), pos.clone()));
                    }
                    Token::Char => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            return Err(ParserError::already_type_defined(Some(pos.clone()), &Type::Number(NumberType::Char), &pre_type, &pre_pos));
                        }
                        opt_type = Some((Type::Number(NumberType::Char), pos.clone()));
                    },
                    Token::Short => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            match pre_type {
                                Type::Number(NumberType::Int) => {
                                    opt_type = Some((Type::Number(NumberType::Short), pre_pos.clone()));
                                    continue;
                                },
                                _ => {
                                    return Err(ParserError::already_type_defined(Some(pos.clone()), &Type::Number(NumberType::Short), &pre_type, &pre_pos));
                                },
                            }
                        }
                        opt_type = Some((Type::Number(NumberType::Short), pos.clone()));
                    },
                    Token::Int => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            match pre_type {
                                Type::Number(NumberType::Short) | Type::Number(NumberType::Long) | Type::Number(NumberType::LongLong) => {
                                    continue;
                                },
                                _ => {
                                    return Err(ParserError::already_type_defined(Some(pos.clone()), &Type::Number(NumberType::Int), &pre_type, &pre_pos));
                                },
                            }
                        }
                        opt_type = Some((Type::Number(NumberType::Int), pos.clone()));
                    },
                    Token::Long => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            match pre_type {
                                Type::Number(NumberType::Int) => {
                                    opt_type = Some((Type::Number(NumberType::Long), pre_pos.clone()));
                                    continue;
                                },
                                Type::Number(NumberType::Long) => {
                                    opt_type = Some((Type::Number(NumberType::LongLong), pre_pos.clone()));
                                    continue;
                                },
                                _ => {
                                    return Err(ParserError::already_type_defined(Some(pos.clone()), &Type::Number(NumberType::Long), &pre_type, &pre_pos));
                                },
                            }
                        }
                        opt_type = Some((Type::Number(NumberType::Long), pos.clone()));
                    },
                    Token::Float => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            return Err(ParserError::already_type_defined(Some(pos.clone()), &Type::Number(NumberType::Float), &pre_type, &pre_pos));
                        }
                        opt_type = Some((Type::Number(NumberType::Float), pos.clone()));
                    },
                    Token::Double => {
                        iter.next();
                        if let Some((pre_type, pre_pos)) = &opt_type {
                            return Err(ParserError::already_type_defined(Some(pos.clone()), &Type::Number(NumberType::Double), &pre_type, &pre_pos));
                        }
                        opt_type = Some((Type::Number(NumberType::Double), pos.clone()));
                    },
                    _ => {
                        break;
                    },
                }

            }else{
                break;
            }
        }

        let (typ, _pos) = opt_type.ok_or(ParserError::no_type_defined(None))?;
        let typ = if let Some((true, _)) = opt_unsigned {
            typ.to_unsigned()?
        }else{
            typ
        };

        Ok((sq, TypeOrVariadic::Type(typ)))
    }

    fn parse_struct_declaration_list(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Vec<StructDeclaration>, ParserError> {
        let mut list: Vec<StructDeclaration> = Vec::new();
println!(">>> parse_struct_declaration_list");
        loop {
            // let (tok, _pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok, pos) = iter.peek().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            if *tok == Token::BraceRight {
println!("BREAK with BraceRight");
                break;
            }

            let declaration = self.parse_struct_declaration(iter, defs, labels)?;
            list.push(declaration);
        }
println!("<<< parse_struct_declaration_list. list: {:?}", list);
        Ok(list)
    }

    fn parse_struct_declaration(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<StructDeclaration, ParserError> {
        let mut list: Vec<StructDeclarator> = Vec::new();
println!(">>> parse_struct_declaration");
        let (sq, type_or_variadic) = self.parse_type_specifier_qualifier(iter, defs, labels)?;

        //
        // error check:
        //        check <storage-class-specifier>
        //        check variadic parameter
        //
        if sq.auto || sq.register || sq.static_ || sq.extern_ || sq.typedef || type_or_variadic.is_variadic() {
            return Err(ParserError::syntax_error_while_parsing_struct(None, &sq, &type_or_variadic));
        }

        let typ = type_or_variadic.get_type().unwrap();

        // let (tok, _pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok == Token::BraceRight {
            return Ok(StructDeclaration::new(sq, Some(typ.clone()), list));
        }

        loop {
            let decl = self.parse_struct_declarator(iter, defs, labels)?;
            list.push(decl);

            // let (next_token, _pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            let (next_token, pos) = iter.peek().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", next_token, file!(), line!(), column!());
            if next_token.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            if *next_token != Token::Comma {
                break;
            }else{
                iter.next();  // skip ','
            }
        }

        self.parse_expected_token(iter, Token::SemiColon)?;
println!("<<< parse_struct_declaration. list: {:?}", list);
        Ok(StructDeclaration::new(sq, Some(typ.clone()), list))
    }

    fn parse_struct_declarator(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<StructDeclarator, ParserError> {
println!(">>> parse_struct_declarator");
        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok == Token::Colon {
            iter.next();  // skip ':'

            let const_expr = self.parse_constant_expression(iter, defs, labels)?.ok_or(ParserError::no_constant_expr_parsing_struct_after_colon(Some(pos.clone())))?;
println!("<<<1 parse_struct_declarator");
            Ok(StructDeclarator::new(None, Some(const_expr)))

        }else{
            let decl = self.parse_declarator(iter, defs, labels)?;

            // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok, pos) = iter.peek().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            if *tok == Token::Colon {
                iter.next();  // skip ':'

                let const_expr = self.parse_constant_expression(iter, defs, labels)?.ok_or(ParserError::no_constant_expr_parsing_struct_after_colon(Some(pos.clone())))?;
println!("<<<2 parse_struct_declarator");
                Ok(StructDeclarator::new(Some(decl), Some(const_expr)))

            }else{
println!("<<<3 parse_struct_declarator");
                Ok(StructDeclarator::new(Some(decl), None))
            }
        }
    }

    fn parse_enumerator_list(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Vec<Enumerator>, ParserError> {
        let mut list: Vec<Enumerator> = Vec::new();
        let mut value: u32 = 0;

        loop {
            // let (tok, _pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok, pos) = iter.peek().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            if *tok == Token::BraceRight {
                // iter.next();  // skip '}'
                break;
            }

            // let (next_token, pos) = iter.next().ok_or(ParserError::illegal_end_of_input(None))?;
            let (next_token, pos) = iter.next().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
            if next_token.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
            let name = if let Token::Symbol(id) = next_token {
                id
            }else{
                return Err(ParserError::not_symbol_parsing_enum(Some(pos.clone())));
            };

            let enumerator: Enumerator;
            // let (tok2, pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok2, pos2) = iter.peek().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
            if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
            match tok2 {
                Token::BraceRight => {
                    enumerator = Enumerator::new(name, value);
                },
                Token::Assign => {
                    iter.next();  // skip '='
                    let const_val = self.parse_constant_expression(iter, defs, labels)?.ok_or(ParserError::enum_should_be_int(None))?;
                    let int_val = const_val.as_u32_value();
                    enumerator = Enumerator::new(name, int_val);
                    value = int_val;

                    // let (tok3, _pos3) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
                    let (tok3, pos3) = iter.peek().unwrap();
                    println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                    if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }
                    if *tok3 == Token::Comma {
                        iter.next();  // skip ','
                    }
                },
                Token::Comma => {
                    iter.next();  // skip ','
                    enumerator = Enumerator::new(name, value);
                },
                _ => {
                    return Err(ParserError::should_be(Some(pos2.clone()), vec![Token::BraceRight, Token::Assign], tok2));
                }
            }

            value += 1;
            list.push(enumerator);
        }

        Ok(list)
    }

    fn parse_declarator(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Declarator, ParserError> {
        let opt_pointer = self.parse_pointer(iter, defs)?;
        let direct = self.parse_direct_declarator(iter, defs, labels)?;

        Ok(Declarator::new(opt_pointer, direct))
    }

    #[allow(irrefutable_let_patterns)]
    fn parse_pointer(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines) -> Result<Option<Pointer>, ParserError> {
        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        if *tok != Token::Mul {
            return Ok(None);
        }

        iter.next();  // skip '*'

        let mut is_const = false;
        let mut is_volatile = false;
        // while let (tok2, _pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))? {
        while let (tok2, pos2) = iter.peek().unwrap() {
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
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
                    //     return Err(ParserError::syntax_error(Some(pos2.clone())));
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
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

        Ok(tok == tok2)
    }

    fn parse_type_qualifier(&self, iter: &mut Peekable<Iter<(Token, Position)>>, _defs: &mut Defines) -> Result<Option<TypeQualifier>, ParserError> {
        // let (tok, _pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
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

    fn parse_direct_declarator(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<DirectDeclarator, ParserError> {
        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        let decl;
        match tok {
            Token::Symbol(id) => {
                iter.next();
                decl = DirectDeclarator::Symbol(id.to_string());
            },
            Token::ParenLeft => {
                iter.next();  // skip '('

                let d = self.parse_declarator(iter, defs, labels)?;
                self.parse_expected_token(iter, Token::ParenRight)?;
                decl = DirectDeclarator::Enclosed(d);
            },
            _ => {
                return Err(ParserError::syntax_error(Some(pos.clone())))
            }
        }

        self.parse_direct_declarator_sub(decl, iter, defs, labels)
    }

    fn parse_direct_declarator_sub(&self, decl: DirectDeclarator, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<DirectDeclarator, ParserError> {
        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        let cust_self;
        match tok {
            Token::Mul => {  // '*'
                iter.next();  // skip '*'

                // let (tok2, pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                let (tok2, pos2) = iter.peek().unwrap();
                println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
                if *tok2 != Token::_self {
                    return Err(ParserError::syntax_error(Some(pos2.clone())));
                }

                cust_self = Some(CustSelf::Pointer(defs.get_self_type()?.clone()));
            },
            _ => {
                cust_self = None;
            },
        }

        let mut result = decl;
        loop {
            // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok, pos) = iter.peek().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

            match tok {
                Token::ParenLeft => {  // define function
                    // read parameter type list
                    iter.next();  // skip '('

                    // let (next_tok, _pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                    let (next_tok, pos2) = iter.peek().unwrap();
                    println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
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

                    result = DirectDeclarator::FunctionDef(Box::new(result), param_type_list);
                },
                Token::BracketLeft => {  // define array
                    iter.next();  // skip '['

                    let mut dimension = Vec::new();

                    let opt_const_expr = self.parse_constant_expression(iter, defs, labels)?;
                    dimension.push(opt_const_expr);
                    self.parse_expected_token(iter, Token::BracketRight)?;  // skip ']'

                    loop {
                        // let (tok2, _pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                        let (tok2, pos2) = iter.peek().unwrap();
                        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                        if *tok2 != Token::BracketLeft {
                            break;
                        }

                        iter.next();  // skip '['

                        let opt_const_expr = self.parse_constant_expression(iter, defs, labels)?;
                        dimension.push(opt_const_expr);
                        self.parse_expected_token(iter, Token::BracketRight)?;  // skip ']'
                    }

                    result =  DirectDeclarator::ArrayDef(Box::new(result), dimension);
                },
                _ => {
                    break Ok(result);
                },
            }
        }
    }

    fn parse_constant_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ConstExpr>, ParserError> {
        if let Some(expr) = self.parse_conditional_expression(iter, defs, labels)? {
            Ok(Some(expr.to_const(defs)?))
        }else{
            Ok(None)
        }
    }

    fn parse_conditional_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(expr) = self.parse_logical_or_expression(iter, defs, labels)? {
            if let Some((tok, pos)) = iter.peek() {
                if *tok == Token::Question {
                    iter.next();  // skip '?'
                    let then_expr = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(None))?;

                    self.parse_expected_token(iter, Token::Colon)?;
                    let else_expr = self.parse_conditional_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(None))?;

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
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(pos.clone())));
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
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(pos.clone())));
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
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(pos.clone())));
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
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(pos.clone())));
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
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(pos.clone())));
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
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(pos.clone())));
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
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(pos.clone())));
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
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(pos.clone())));
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
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(pos.clone())));
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
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(left), Box::new(right), pos.clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token(&op)?, Box::new(ast.clone()), Box::new(right), pos.clone()));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(pos.clone())));
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
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

            if *tok == Token::ParenLeft {
                iter.next();  // skip '('
                let (_sq, type_or_variadic, _opt_abstract_decl) = self.parse_type_name(iter, defs, labels)?;
                let cast_type = type_or_variadic.get_type().ok_or(ParserError::no_type_defined(None))?;
                self.parse_expected_token(iter, Token::ParenRight)?;

                let expr = self.parse_cast_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(None))?;

                Ok(Some(ExprAST::Cast(cast_type.clone(), Box::new(expr), pos.clone())))
            }else{
                Ok(None)
            }
        }
    }

    fn parse_unary_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(ast) = self.parse_postfix_expression(iter, defs, labels)? {
            Ok(Some(ast))

        }else{
            // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok, pos) = iter.peek().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

            match &*tok {
                Token::Inc => {
                    iter.next();  // skip '++'

                    let expr = self.parse_unary_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;
                    // let one = ExprAST::Int(1, pos.clone());
                    // let add = ExprAST::BinExpr(BinOp::Add, Box::new(expr.clone()), Box::new(one), pos.clone());
                    // let inc = ExprAST::Assign(Box::new(expr), Box::new(add), pos.clone());

                    if expr.is_symbol() {
                        let (sym, sym_pos) = expr.get_symbol()?;
                        let inc = ExprAST::PreInc(sym.clone(), sym_pos.clone(), pos.clone());
                        Ok(Some(inc))
                    }else if expr.is_member_access() {
                        let inc = ExprAST::PreIncMemberAccess(Box::new(expr.clone()), pos.clone());
                        Ok(Some(inc))
                    }else{
                        Err(ParserError::syntax_error(Some(pos.clone())))
                    }
               },
                Token::Dec => {
                    iter.next();  // skip '--'

                    let expr = self.parse_unary_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;
                    // let one = ExprAST::Int(1, pos.clone());
                    // let add = ExprAST::BinExpr(BinOp::Sub, Box::new(expr.clone()), Box::new(one), pos.clone());
                    // let inc = ExprAST::Assign(Box::new(expr), Box::new(add), pos.clone());

                    if expr.is_symbol() {
                        let (sym, sym_pos) = expr.get_symbol()?;
                        let dec = ExprAST::PreDec(sym.clone(), sym_pos.clone(), pos.clone());
                        Ok(Some(dec))
    
                    }else if expr.is_member_access() {
                        let inc = ExprAST::PreDecMemberAccess(Box::new(expr.clone()), pos.clone());
                        Ok(Some(inc))
                    }else{
                        Err(ParserError::syntax_error(Some(pos.clone())))
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
                Token::Mul => {      // '*'. pointer access
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
                        let type_name = type_or_variadic.get_type().ok_or(ParserError::no_type_defined(None))?;
                        Ok(Some(ExprAST::UnarySizeOfTypeName(type_name.clone(), pos.clone())))
                    }
                },
                Token::ParenRight => {
                    Ok(None)
                },
                _ => {
                    Err(ParserError::syntax_error(Some(pos.clone())))
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
                            let expr = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::no_expr_while_access_array(None))?;
                            self.parse_expected_token(iter, Token::BracketRight)?;
                            ast = ExprAST::ArrayAccess(Box::new(ast), Box::new(expr), pos.clone());
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

                            // let (tok2, pos2) = iter.next().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                            let (tok2, pos2) = iter.next().unwrap();
                            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                            if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                            match tok2 {
                                Token::Symbol(id) => {
                                    ast = ExprAST::MemberAccess(Box::new(ast), id.clone(), pos.clone());
                                },
                                _ => return Err(ParserError::no_id_after_dot(Some(pos2.clone()))),
                            }
                        },
                        Token::MemberSelection => {
                            iter.next();  // skip '->'

                            // let (tok2, pos2) = iter.next().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                            let (tok2, pos2) = iter.next().unwrap();
                            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                            if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                            match tok2 {
                                Token::Symbol(id) => {
                                    ast = ExprAST::PointerAccess(Box::new(ast), id.clone(), pos.clone());
                                },
                                _ => return Err(ParserError::no_id_after_arrow(Some(pos2.clone()))),
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
                                return Err(ParserError::syntax_error(Some(pos.clone())));
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
                                return Err(ParserError::syntax_error(Some(pos.clone())));
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

    fn parse_primary_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some((tok, pos)) = iter.peek() {
            match &*tok {
                Token::Symbol(name) => {
                    iter.next();  // skip symbol
                    Ok(Some(ExprAST::Symbol(name.clone(), pos.clone())))
                },
                Token::_Self => {
                    iter.next();  // skip 'Self'
                    Ok(Some(ExprAST::_Self(pos.clone())))
                },
                Token::_self => {
                    iter.next();  // skip 'self'
                    Ok(Some(ExprAST::_self(pos.clone())))
                },
                Token::ParenLeft => {
                    iter.next(); // skip '('
                    let result = self.parse_expression(iter, defs, labels)?;
                    self.parse_expected_token(iter, Token::ParenRight)?;

                    Ok(result)
                },
                _ => {
                    self.parse_constant(iter, defs)
                },
            }
        }else{
            Ok(None)
        }
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
                            return Err(ParserError::syntax_error(Some(pos.clone())));
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
                        let then_expr = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(None))?;

                        self.parse_expected_token(iter, Token::Colon)?;
                        let else_expr = self.parse_conditional_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(None))?;
    
                        return Ok(Some(ExprAST::TernaryOperator(Box::new(ast), Box::new(then_expr), Box::new(else_expr), pos.clone())));
                    },
                    Token::Assign | Token::AddAssign | Token::SubAssign | Token::MulAssign | Token::DivAssign | Token::ModAssign 
                     | Token:: ShiftLeftAssign | Token::ShiftRightAssign | Token::BitAndAssign | Token::BitOrAssign | Token::BitXorAssign =>
                    {
                        // if let Some(code) = self.parse_assignment_expression2(iter, ast.clone())? {
                        //     ast = code;
                        // }
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
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::need_expr(Some(pos.clone())))?;

                        result = ExprAST::Assign(Box::new(result), Box::new(r_value), pos.clone());
                    },
                    Token::AddAssign => {
                        iter.next(); // skip '+='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;

                        // let add = ExprAST::BinExpr(BinOp::Add, Box::new(result.clone()), Box::new(r_value), pos.clone());
                        // result = ExprAST::Assign(Box::new(result), Box::new(add), pos.clone());
                        result = ExprAST::OpAssign(BinOp::Add, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::SubAssign => {
                        iter.next(); // skip '-='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;

                        // let sub = ExprAST::BinExpr(BinOp::Sub, Box::new(result.clone()), Box::new(r_value), pos.clone());
                        // result = ExprAST::Assign(Box::new(result), Box::new(sub), pos.clone());
                        result = ExprAST::OpAssign(BinOp::Sub, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::MulAssign => {
                        iter.next(); // skip '*='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;

                        // let mul = ExprAST::BinExpr(BinOp::Mul, Box::new(result.clone()), Box::new(r_value), pos.clone());
                        // result = ExprAST::Assign(Box::new(result), Box::new(mul), pos.clone());
                        result = ExprAST::OpAssign(BinOp::Mul, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::DivAssign => {
                        iter.next(); // skip '/='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;

                        // let div = ExprAST::BinExpr(BinOp::Div, Box::new(result.clone()), Box::new(r_value), pos.clone());
                        // result = ExprAST::Assign(Box::new(result), Box::new(div), pos.clone());
                        result = ExprAST::OpAssign(BinOp::Div, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::ModAssign => {
                        iter.next(); // skip '%='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;

                        // let res = ExprAST::BinExpr(BinOp::Mod, Box::new(result.clone()), Box::new(r_value), pos.clone());
                        // result = ExprAST::Assign(Box::new(result), Box::new(res), pos.clone());
                        result = ExprAST::OpAssign(BinOp::Mod, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::ShiftLeftAssign => {
                        iter.next(); // skip '%='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;

                        // let res = ExprAST::BinExpr(BinOp::ShiftLeft, Box::new(result.clone()), Box::new(r_value), pos.clone());
                        // result = ExprAST::Assign(Box::new(result), Box::new(res), pos.clone());
                        result = ExprAST::OpAssign(BinOp::ShiftLeft, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::ShiftRightAssign => {
                        iter.next(); // skip '%='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;

                        // let res = ExprAST::BinExpr(BinOp::ShiftRight, Box::new(result.clone()), Box::new(r_value), pos.clone());
                        // result = ExprAST::Assign(Box::new(result), Box::new(res), pos.clone());
                        result = ExprAST::OpAssign(BinOp::ShiftRight, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::BitAndAssign => {
                        iter.next(); // skip '%='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;

                        // let res = ExprAST::BinExpr(BinOp::BitAnd, Box::new(result.clone()), Box::new(r_value), pos.clone());
                        // result = ExprAST::Assign(Box::new(result), Box::new(res), pos.clone());
                        result = ExprAST::OpAssign(BinOp::BitAnd, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::BitOrAssign => {
                        iter.next(); // skip '%='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;

                        // let res = ExprAST::BinExpr(BinOp::BitOr, Box::new(result.clone()), Box::new(r_value), pos.clone());
                        // result = ExprAST::Assign(Box::new(result), Box::new(res), pos.clone());
                        result = ExprAST::OpAssign(BinOp::BitOr, Box::new(result.clone()), Box::new(r_value), pos.clone());
                    },
                    Token::BitXorAssign => {
                        iter.next(); // skip '%='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;

                        // let res = ExprAST::BinExpr(BinOp::BitXor, Box::new(result.clone()), Box::new(r_value), pos.clone());
                        // result = ExprAST::Assign(Box::new(result), Box::new(res), pos.clone());
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
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        let cust_self;
        if *tok == Token::BitAnd {  // '&'
            iter.next();  // skip '&'

            // let (tok2, pos2) = iter.next().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
            let (tok2, pos2) = iter.next().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
            if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }
            if *tok2 != Token::_self {
                return Err(ParserError::not_self_after_ref(Some(pos2.clone())));
            }

            cust_self = Some(CustSelf::Ref(defs.get_self_type()?.clone()));

            // let (tok3, _pos3) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
            let (tok3, pos3) = iter.peek().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
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

                    // let (tok2, _pos2) = iter.next().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                    let (tok2, pos2) = iter.next().unwrap();
                    println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                    if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                    if *tok2 == Token::TripleDot {
                        has_variadic = true;
                    }else{
                        return Err(ParserError::syntax_error(Some(pos.clone())));
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
                let decl = self.parse_declarator(iter, defs, &mut None)?;
                list.push(Param::new(ds, decl, defs)?);
        
                // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
                let (tok, pos) = iter.peek().unwrap();
                println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

                match tok {
                    Token::Comma => {
                        (list, has_variadic) = self.parse_parameter_declaration2(list, iter, defs, labels)?;
                    },
                    Token::ParenRight => (),  // do nothing
                    _ => {
                        return Err(ParserError::syntax_error(Some(pos.clone())));
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
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::Comma => {
                        iter.next(); // skip ','

                        let ds_or_variadic = self.parse_declaration_specifier(iter, defs, labels)?;
                        match ds_or_variadic {
                            DeclarationSpecifierOrVariadic::DS(ds) => {
                                let decl2 = self.parse_declarator(iter, defs, &mut None)?;
                                list.push(Param::new(ds, decl2, defs)?);
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
        let (sq, type_or_variadic) = self.parse_type_specifier_qualifier(iter, defs, labels)?;
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
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
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
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
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

    fn parse_declaration(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        let ds = self.parse_declaration_specifier(iter, defs, labels)?;
        let ds = ds.get_declaration_specifier().unwrap();

        // parse init_declarator
        let mut v = Vec::new();
        loop {
            let decl = self.parse_declarator(iter, defs, labels)?;
            let name = decl.get_name();

            if defs.exists_var(&name) {
                return Err(ParserError::already_var_defined(None, &name));
            }

            // let (tok, pos) = iter.next().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok, pos) = iter.next().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

            match tok {
                Token::SemiColon => {
                    let declaration = Declaration::new(decl, None);
                    v.push(declaration);
                    break;
                },
                Token::Comma => {
                    let declaration = Declaration::new(decl, None);
                    v.push(declaration);
                    continue;
                },
                Token::Assign => {
                    let init_expr = self.parse_initializer(iter, defs, labels)?;
                    let declaration = Declaration::new(decl, Some(Box::new(init_expr)));
                    v.push(declaration);

                    // let (tok3, _pos3) = iter.next().ok_or(ParserError::illegal_end_of_input(None))?;
                    let (tok3, pos3) = iter.next().unwrap();
                    println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
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
                    return Err(ParserError::syntax_error(Some(pos.clone())));
                }
            }

            // check exists next init_declarator
            // let (tok, pos) = iter.next().ok_or(ParserError::illegal_end_of_input(None))?;
            // match tok {
            //
            //     _ => break;
            // }
        }

        let (ds, declarations) = self.parse_def_var(ds.clone(), v, defs)?;
        let ast = AST::DefVar { specifiers: ds, declarations };
        Ok(Some(ast))
    }

    // fn parse_init_declarator(&self, _typ: &Type, _iter: &mut Peekable<Iter<(Token, Position)>>, _defs: &mut Defines, _labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
    // }

    fn parse_initializer(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<ExprAST, ParserError> {
        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        let init_expr;
        if *tok == Token::BraceLeft {
            iter.next();  // skip '{'
            init_expr = self.parse_initializer_list(pos, iter, defs, labels)?;
        }else{
            init_expr = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::need_expr(Some(pos.clone())))?;
        }
        Ok(init_expr)
    }

    fn parse_initializer_list(&self, start_pos: &Position, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<ExprAST, ParserError> {
        let mut list: Vec<ExprAST> = Vec::new();

        loop {
            let initializer = self.parse_initializer(iter, defs, labels)?;
            list.push(initializer);

            // let (tok2, pos2) = iter.next().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok2, pos2) = iter.next().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok2, file!(), line!(), column!());
            if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

            match tok2 {
                Token::BraceRight => {
                    break;
                },
                Token::Comma => {
                    // let (tok3, _pos3) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
                    let (tok3, pos3) = iter.peek().unwrap();
                    println!("tok: {:?}, illegal end at {}:{}:{}:", tok3, file!(), line!(), column!());
                    if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }

                    if *tok3 == Token::BraceRight {
                        iter.next();  // skip '}'
                        break;
                    }
                },
                _ => {
                    return Err(ParserError::need_brace_right_or_comma_when_parsing_initializer_list(Some(pos2.clone())));
                }
            }
        }

        Ok(ExprAST::InitializerList(list, start_pos.clone()))
    }

    fn parse_compound_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Block, ParserError> {
        self.parse_expected_token(iter, Token::BraceLeft)?;
        defs.new_local();

        let mut body = Vec::new();
        loop {
            // let (tok, _pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            let (tok, pos) = iter.peek().unwrap();
            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
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
                    Ok(Some(AST::Block(block)))
                },
                // selection statement
                Token::If => {
                    iter.next();  // skip 'if'
                    self.parse_expected_token(iter, Token::ParenLeft)?;  // skip '('

                    let cond = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;

                    self.parse_expected_token(iter, Token::ParenRight)?;  // skip ')'

                    let then = self.parse_statement(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;

                    if let Some((tok2, pos2)) = iter.peek() {
                        if *tok2 == Token::Else {
                            iter.next();  // skip 'else'

                            let _else = self.parse_statement(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos2.clone())))?;
                            Ok(Some(AST::If(Box::new(cond), Box::new(then), Some(Box::new(_else)))))
                        }else{
                            Ok(Some(AST::If(Box::new(cond), Box::new(then), None)))
                        }
                    }else{
                        Ok(Some(AST::If(Box::new(cond), Box::new(then), None)))
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

                    Ok(Some(AST::Switch(switch)))
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
                    }))
                },
                Token::For => {
                    self.parse_for(iter, defs, &pos, labels)
                },

                // jump statement
                Token::Goto => {
                    iter.next();  // skip 'goto'
                    // let (id, pos2) = iter.next().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                    let (id, pos2) = iter.next().unwrap();
                    println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                    if id.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                    if let Token::Symbol(name) = id {
                        Ok(Some(AST::Goto(name.to_string())))
                    }else{
                        Err(ParserError::no_id_for_goto_statement(Some(pos2.clone())))
                    }
                },
                Token::Continue => {
                    iter.next();  // skip 'continue'
                    self.parse_expected_token(iter, Token::SemiColon)?;  // skip ';'

                    Ok(Some(AST::Continue))
                },
                Token::Break => {
                    iter.next();  // skip 'break'
                    self.parse_expected_token(iter, Token::SemiColon)?;  // skip ';'

                    Ok(Some(AST::Break))
                },
                Token::Return => {
                    iter.next();  // skip 'return'
                    // let (t, _pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
                    let (t, pos2) = iter.peek().unwrap();
                    println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                    if t.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                    if *t == Token::SemiColon {
                        iter.next(); // skip ';'
                        Ok(Some(AST::Return(None, pos.clone())))
                    }else{
                        if let Some(ast) = self.parse_expression(iter, defs, labels)? {
                            self.parse_expected_token(iter, Token::SemiColon)?;
                            Ok(Some(AST::Return(Some(Box::new(ast)), pos.clone())))
                        }else{
                            println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
                            Err(ParserError::illegal_end_of_input(pos.clone()))
                        }
                    }
                },
                // labeled-statement
                Token::Case => {
                    self.parse_case_labeled_statement(iter, defs, labels)
                },
                Token::Default => {
                    self.parse_default_labeled_statement(iter, defs, labels)
                },
                // labeled-statement or expression-statement
                Token::Symbol(id) => {
                    if defs.exists_var(id) {
                        self.parse_expression_statement(iter, defs, labels)
                    }else if defs.exists_type(id) {
                        let decl = self.parse_declaration(iter, defs, labels)?;
                        Ok(decl)
                    }else{
                        iter.next();
                        self.parse_labeled_statement(id, iter, defs, labels)
                    }
                },
                Token::Auto | Token::Register | Token::Static | Token::Extern | Token::Typedef |
                Token::Void | Token::Char | Token::Short | Token::Int | Token::Long | Token::Float |
                Token::Double | Token::Signed | Token::Unsigned | Token::Struct | Token::Enum =>
                {
                    self.parse_declaration(iter, defs, labels)
                },
                _ => {
                    self.parse_expression_statement(iter, defs, labels)
                },
            }

        }else{
            Ok(None)
        }
    }

    fn parse_for(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, _pos: &Position, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        iter.next();  // skip 'for'
        self.parse_expected_token(iter, Token::ParenLeft)?; // '('
        defs.new_local();

        // let (tok2, _pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
        let (tok2, pos2) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok2, file!(), line!(), column!());
        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

        let init_expr = if *tok2 == Token::SemiColon {
            None
        }else{
            self.parse_for_init(iter, defs, labels)?
        };

        self.parse_expected_token(iter, Token::SemiColon)?; // ';'

        // let (tok2, _pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
        let (tok2, pos2) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok2, file!(), line!(), column!());
        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

        let cond = if *tok2 == Token::SemiColon {
            None
        }else{
            self.parse_expression(iter, defs, labels)?
        };

        self.parse_expected_token(iter, Token::SemiColon)?; // ';'

        // let (tok2, _pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(pos.clone())))?;
        let (tok2, pos2) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok2, file!(), line!(), column!());
        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

        let step = if *tok2 == Token::ParenRight {
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
        }))
    }

    // only for init-expr in for-statement
    pub fn parse_simple_declaration_or_expression(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
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
        let decl = self.parse_declarator(iter, defs, labels)?;
        // let name = decl.get_name();

        // let (tok, pos) = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let (tok, pos) = iter.peek().unwrap();
        println!("tok: {:?}, illegal end at {}:{}:{}:", tok, file!(), line!(), column!());
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

        match tok {
            Token::Comma => {
                let declaration = Declaration::new(decl, None);
                let (ds, declarations) = self.parse_def_var(ds.clone(), vec![declaration], defs)?;
                let ast = ExprAST::DefVar { specifiers: ds, declarations: declarations, pos: start_pos.clone() };
                Ok(Some(ast))
            },
            Token::Assign => {
                iter.next();  // skip '='

                let expr = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(pos.clone())))?;
                let declaration = Declaration::new(decl, Some(Box::new(expr)));
                let (ds, declarations) = self.parse_def_var(ds.clone(), vec![declaration], defs)?;
                let ast = ExprAST::DefVar { specifiers: ds, declarations: declarations, pos: start_pos.clone() };
                Ok(Some(ast))
            },
            _ => {
                Err(ParserError::syntax_error(Some(pos.clone())))
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
                            return Err(ParserError::syntax_error(Some(pos.clone())));
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


    fn parse_labeled_statement(&self, id: &str, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        self.parse_expected_token(iter, Token::Colon)?;
        let stmt = self.parse_statement(iter, defs, labels)?;

        if let Some(v) = labels {
            v.push(id.to_string());
        }else{
            return Err(ParserError::labeled_statement_without_function(None));
        }

        if let Some(s) = stmt {
            Ok(Some(AST::Labeled(id.to_string(), Some(Box::new(s)))))
        }else{
            Ok(Some(AST::Labeled(id.to_string(), None)))
        }
    }

    fn parse_case_labeled_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        self.parse_expected_token(iter, Token::Case)?;
        let constant_condition = self.parse_constant_expression(iter, defs, labels)?.ok_or(ParserError::no_constant_expr_after_case(None))?;

        self.parse_expected_token(iter, Token::Colon)?;

        let stmt = self.parse_statement(iter, defs, labels)?.ok_or(ParserError::syntax_error(None))?;
        let case = Case::new(constant_condition, Box::new(stmt));
        Ok(Some(AST::Case(case)))
    }

    fn parse_default_labeled_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        self.parse_expected_token(iter, Token::Default)?;
        self.parse_expected_token(iter, Token::Colon)?;

        let stmt = self.parse_statement(iter, defs, labels)?.ok_or(ParserError::syntax_error(None))?;
        Ok(Some(AST::Default(Box::new(stmt))))
    }

    fn parse_expression_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        if let Some((tok, _pos)) = iter.peek() {
            match tok {
                Token::SemiColon => {
                    iter.next();
                    Ok(None)
                },
                _ => {
                    let expr = self.parse_expression(iter, defs, labels)?;
                    self.parse_expected_token(iter, Token::SemiColon)?;
                    if let Some(e) = expr {
                        Ok(Some(AST::Expr(Box::new(e))))
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
        //         Err(ParserError::without_expected_token(Some(pos.clone()), expected.clone(), tok.clone()))
        //     }
        // }else{
        //     Err(ParserError::illegal_end_of_input(None))
        // }
        let (tok, pos) = iter.next().unwrap();
println!("expected token: {:?}, real: {:?}", expected, tok);
        if *tok == expected {
            Ok(pos.clone())
        }else{
            Err(ParserError::without_expected_token(Some(pos.clone()), expected.clone(), tok.clone()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_expression_from_str(src: &str) -> Result<Option<ExprAST>, ParserError> {
        let token_list = Tokenizer::tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();
        let mut labels = Vec::new();
        parser.parse_expression(&mut iter, &mut defs, &mut Some(&mut labels))
    }

    fn parse_external_declaration_from_str(src: &str) -> Result<Option<AST>, ParserError> {
        let token_list = Tokenizer::tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();
        let mut labels = Vec::new();
        parser.parse_external_declaration(&mut iter, &mut defs, &mut Some(&mut labels))
    }

    fn parse_translation_unit_from_str(src: &str) -> Result<Vec<AST>, ParserError> {
        let token_list = Tokenizer::tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();
        parser.parse_translation_unit(&mut iter, &mut defs)
    }

    #[test]
    fn parse_constant() {
        let src = "123";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Int(123, Position::new(1, 1))
        );
    }

    #[test]
    fn parse_primary_expression() {
        let src = "(1, 2, 3)";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Comma,
                Box::new(ExprAST::Int(1, Position::new(1, 2))),
                Box::new(ExprAST::BinExpr(
                    BinOp::Comma,
                    Box::new(ExprAST::Int(2, Position::new(1, 5))),
                    Box::new(ExprAST::Int(3, Position::new(1, 8))),
                    Position::new(1, 6)
                )),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_add() {
        let src = "10 + 11";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Add,
                Box::new(ExprAST::Int(10, Position::new(1, 1))),
                Box::new(ExprAST::Int(11, Position::new(1, 6))),
                Position::new(1, 4)
            )
        );
    }

    #[test]
    fn parse_sub() {
        let src = "10 - 11";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Sub,
                Box::new(ExprAST::Int(10, Position::new(1, 1))),
                Box::new(ExprAST::Int(11, Position::new(1, 6))),
                Position::new(1, 4)
            )
        );
    }

    #[test]
    fn parse_mul() {
        let src = "10 * 11";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Mul,
                Box::new(ExprAST::Int(10, Position::new(1, 1))),
                Box::new(ExprAST::Int(11, Position::new(1, 6))),
                Position::new(1, 4)
            )
        );
    }

    #[test]
    fn parse_div() {
        let src = "10 / 11";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Div,
                Box::new(ExprAST::Int(10, Position::new(1, 1))),
                Box::new(ExprAST::Int(11, Position::new(1, 6))),
                Position::new(1, 4)
            )
        );
    }

    #[test]
    fn parse_mod() {
        let src = "10 % 11";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Mod,
                Box::new(ExprAST::Int(10, Position::new(1, 1))),
                Box::new(ExprAST::Int(11, Position::new(1, 6))),
                Position::new(1, 4)
            )
        );
    }

    #[test]
    fn parse_complex_expr() {
        let src = "10 + 11 * 12 - 13";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Sub,
                Box::new(
                    ExprAST::BinExpr(
                        BinOp::Add,
                        Box::new(ExprAST::Int(10, Position::new(1, 1))),
                        Box::new(
                            ExprAST::BinExpr(
                                BinOp::Mul,
                                Box::new(ExprAST::Int(11, Position::new(1, 6))),
                                Box::new(ExprAST::Int(12, Position::new(1, 11))),
                                Position::new(1, 9)
                            )
                        ),
                        Position::new(1, 4)
                    )
                ),
                Box::new(ExprAST::Int(13, Position::new(1, 16))),
                Position::new(1, 14)
            )
        );
    }

    #[test]
    fn parse_unary_plus() {
        let src = "+5";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Int(5, Position::new(1, 2))
        );
    }

    #[test]
    fn parse_unary_minus() {
        let src = "-5";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::UnaryMinus(Box::new(ExprAST::Int(5, Position::new(1, 2))), Position::new(1, 1))
        );
    }

    #[test]
    fn parse_unary_tilda() {
        let src = "~5";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::UnaryTilda(Box::new(ExprAST::Int(5, Position::new(1, 2))), Position::new(1, 1))
        );
    }

    #[test]
    fn parse_unary_not() {
        let src = "! 0";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Not(Box::new(ExprAST::Int(0, Position::new(1, 3))), Position::new(1, 1))
        );
    }

    #[test]
    fn parse_unary_paren() {
        let src = "-(1 + 2 * 3)";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::UnaryMinus(
                Box::new(
                    ExprAST::BinExpr(
                        BinOp::Add,
                        Box::new(ExprAST::Int(1, Position::new(1, 3))),
                        Box::new(
                            ExprAST::BinExpr(
                                BinOp::Mul,
                                Box::new(ExprAST::Int(2, Position::new(1, 7))),
                                Box::new(ExprAST::Int(3, Position::new(1, 11))),
                                Position::new(1, 9)
                            )
                        ),
                        Position::new(1, 5)
                    )
                ),
                Position::new(1, 1)
            )
        );
    }

    #[test]
    fn parse_postfix() {
        let src = "x++";
        let ast = parse_expression_from_str(src).unwrap().unwrap();
        assert_eq!(
            ast,
            ExprAST::PostInc("x".to_string(), Position::new(1, 1), Position::new(1, 2))
        );

        let src = "x--";
        let ast = parse_expression_from_str(src).unwrap().unwrap();
        assert_eq!(
            ast,
            ExprAST::PostDec("x".to_string(), Position::new(1, 1), Position::new(1, 2))
        );
    }

    #[test]
    fn parse_prefix() {
        let src = "++x";
        let ast = parse_expression_from_str(src).unwrap().unwrap();
        assert_eq!(
            ast,
            ExprAST::PreInc("x".to_string(), Position::new(1, 3), Position::new(1, 1))
        );

        let src = "--x";
        let ast = parse_expression_from_str(src).unwrap().unwrap();
        assert_eq!(
            ast,
            ExprAST::PreDec("x".to_string(), Position::new(1, 3), Position::new(1, 1))
        );
    }

    #[test]
    fn parse_add_assign() {
        let src = "x += 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::Add,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_sub_assign() {
        let src = "x -= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::Sub,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_mul_assign() {
        let src = "x *= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::Mul,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_div_assign() {
        let src = "x /= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::Div,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_mod_assign() {
        let src = "x %= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::Mod,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_shift_left_assign() {
        let src = "x <<= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::ShiftLeft,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 7))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_shift_right_assign() {
        let src = "x >>= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::ShiftRight,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 7))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_bit_and_assign() {
        let src = "x &= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::BitAnd,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_bit_or_assign() {
        let src = "x |= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::BitOr,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_bit_xor_assign() {
        let src = "x ^= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::BitXor,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_increment() {
        let src = "x++;";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::PostInc(
                "x".to_string(),
                Position::new(1, 1),
                Position::new(1, 2)
            )            
        );
    }

    #[test]
    fn parse_decrement() {
        let src = "x--;";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::PostDec(
                "x".to_string(),
                Position::new(1, 1),
                Position::new(1, 2)
            )            
        );
    }

    #[test]
    fn parse_type_specifier_qualifier() {
        let src = "int volatile short unsigned * volatile * const foo(";

        let token_list = Tokenizer::tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();
        let mut labels = Vec::new();

        let (sq, type_or_variadic) = parser.parse_type_specifier_qualifier(&mut iter, &mut defs, &mut Some(&mut labels)).unwrap();
        let typ = type_or_variadic.get_type().unwrap();
        let pointer = parser.parse_pointer(&mut iter, &mut defs);
        let pointer = pointer.unwrap().unwrap();

        assert_eq!(typ, &Type::Number(NumberType::UnsignedShort));
        assert_eq!(sq,  SpecifierQualifier {
            auto: false,
            register: false,
            static_: false,
            extern_: false,
            typedef: false,
            const_: false,
            volatile: true,
        });
        assert_eq!(
            pointer.make_type_to(&typ),
            Type::Pointer(
                Pointer::new_with_next_pointer(false, true, Pointer::new(true, false)),
                Box::new(Type::Pointer(
                    Pointer::new(true, false),
                    Box::new(Type::Number(NumberType::UnsignedShort))
                ))
            )
        )
    }

    #[test]
    fn parse_function_prototype() -> Result<(), ParserError> {
        let src = "
            int add(int x, int y);
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            AST::FunProto(FunProto {specifiers, declarator, params}) => {
                let name = declarator.get_name();
                let ret_type = specifiers.get_type();
                assert_eq!(*name, "add".to_string());
                assert_eq!(*ret_type, Type::Number(NumberType::Int));
                assert_eq!(params.len(), 2);

                let sq = SpecifierQualifier::new();
                let mut defs = Defines::new();
                let ds_x = DeclarationSpecifier::new(Type::Number(NumberType::Int), sq.clone());
                let decl_x = Declarator::new(None, DirectDeclarator::Symbol(String::from("x")));
                let ds_y = DeclarationSpecifier::new(Type::Number(NumberType::Int), sq.clone());
                let decl_y = Declarator::new(None, DirectDeclarator::Symbol(String::from("y")));
                assert_eq!(params, Params::from_vec(vec![(ds_x, decl_x), (ds_y, decl_y)], false, &mut defs)?);
            },
            _ => panic!("ast: {:?}", ast),
        }

        Ok(())
    }

    #[test]
    fn parse_function() -> Result<(), ParserError> {
        let src = "
            int add(int x, int y) {
                return x + y;
            }
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            AST::Function(Function {specifiers, declarator, params, body, labels: _}) => {
                let name = declarator.get_name();
                let ret_type = specifiers.get_type();
                assert_eq!(*name, "add".to_string());
                assert_eq!(*ret_type, Type::Number(NumberType::Int));
                assert_eq!(params.len(), 2);

                let sq = SpecifierQualifier::new();
                let mut defs = Defines::new();
                let ds_x = DeclarationSpecifier::new(Type::Number(NumberType::Int), sq.clone());
                let decl_x = Declarator::new(None, DirectDeclarator::Symbol(String::from("x")));
                let ds_y = DeclarationSpecifier::new(Type::Number(NumberType::Int), sq.clone());
                let decl_y = Declarator::new(None, DirectDeclarator::Symbol(String::from("y")));
                assert_eq!(params, Params::from_vec(vec![(ds_x, decl_x), (ds_y, decl_y)], false, &mut defs)?);
                assert_eq!(body,
                    Block {
                        body: vec![
                            AST::Return(Some(Box::new(
                                ExprAST::BinExpr(
                                    BinOp::Add,
                                    Box::new(ExprAST::Symbol(String::from("x"), Position::new(3, 23))),
                                    Box::new(ExprAST::Symbol(String::from("y"), Position::new(3, 27))),
                                    Position::new(3, 25)
                                ))),
                                Position::new(3, 16)
                            )
                        ],
                    }
                );
            },
            _ => panic!("ast: {:?}", ast),
        }

        Ok(())
    }

    #[test]
    fn parse_multiple_declaration() -> Result<(), ParserError> {
        let src = "
            int foo() {
                int x, y = 2, z;
                x = 1;
                z = 3;
                return x + y * z;
            }
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            AST::Function(Function {specifiers, declarator, params, body, labels: _}) => {
                let name = declarator.get_name();
                let ret_type = specifiers.get_type();
                assert_eq!(*name, "foo".to_string());
                assert_eq!(*ret_type, Type::Number(NumberType::Int));
                assert_eq!(params.len(), 0);

                let mut defs = Defines::new();
                assert_eq!(params, Params::from_vec(vec![], false, &mut defs)?);

                let sq = SpecifierQualifier::new();
                let specifier = DeclarationSpecifier::new(Type::Number(NumberType::Int), sq);

                let mut v = Vec::new();
                let declarator = Declarator::new(None, DirectDeclarator::Symbol(String::from("x")));
                let declaration = Declaration::new(declarator, None);
                v.push(declaration);
                let declarator = Declarator::new(None, DirectDeclarator::Symbol(String::from("y")));
                let init_expr = ExprAST::Int(2, Position::new(3, 27));
                let declaration = Declaration::new(declarator, Some(Box::new(init_expr)));
                v.push(declaration);
                let declarator = Declarator::new(None, DirectDeclarator::Symbol(String::from("z")));
                let declaration = Declaration::new(declarator, None);
                v.push(declaration);

                assert_eq!(body,
                    Block {
                        body: vec![
                            AST::DefVar{
                                specifiers: specifier,
                                declarations: v,
                            },
                            AST::Expr(
                                Box::new(ExprAST::Assign(
                                    Box::new(ExprAST::Symbol(String::from("x"), Position::new(4, 16))),
                                    Box::new(ExprAST::Int(1, Position::new(4, 20))),
                                    Position::new(4, 18)
                            ))),
                            AST::Expr(
                                Box::new(ExprAST::Assign(
                                    Box::new(ExprAST::Symbol(String::from("z"), Position::new(5, 16))),
                                    Box::new(ExprAST::Int(3, Position::new(5, 20))),
                                    Position::new(5, 18)
                            ))),
                            AST::Return(Some(Box::new(
                                ExprAST::BinExpr(
                                    BinOp::Add,
                                    Box::new(ExprAST::Symbol(String::from("x"), Position::new(6, 23))),
                                    Box::new(ExprAST::BinExpr(
                                        BinOp::Mul,
                                        Box::new(ExprAST::Symbol(String::from("y"), Position::new(6, 27))),
                                        Box::new(ExprAST::Symbol(String::from("z"), Position::new(6, 31))),
                                        Position::new(6, 29)
                                    )),
                                    Position::new(6, 25)
                                ))),
                                Position::new(6, 16)
                            )
                        ],
                    }
                );
            },
            _ => panic!("ast: {:?}", ast),
        }

        Ok(())
    }

    #[test]
    fn parse_multiple_pointer_declaration() -> Result<(), ParserError> {
        let src = "
            void foo() {
                int x, *y, **z;
            }
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            AST::Function(Function {specifiers, declarator, params, body, labels: _}) => {
                let name = declarator.get_name();
                let ret_type = specifiers.get_type();
                assert_eq!(*name, "foo".to_string());
                assert_eq!(*ret_type, Type::Void);
                assert_eq!(params.len(), 0);

                let mut defs = Defines::new();
                assert_eq!(params, Params::from_vec(vec![], false, &mut defs)?);

                let sq = SpecifierQualifier::new();
                let specifier = DeclarationSpecifier::new(Type::Number(NumberType::Int), sq);

                let mut v = Vec::new();
                let declarator = Declarator::new(None, DirectDeclarator::Symbol(String::from("x")));
                let declaration = Declaration::new(declarator, None);
                v.push(declaration);

                let pointer = Pointer::new(false, false);
                let typ = Type::Number(NumberType::Int);
                assert_eq!(
                    pointer.make_type_to(&typ),
                    Type::Pointer(Pointer::new(false, false), Box::new(typ.clone())));
                let declarator = Declarator::new(Some(pointer.clone()), DirectDeclarator::Symbol(String::from("y")));
                let declaration = Declaration::new(declarator, None);
                v.push(declaration);

                let handle = Pointer::new_with_next_pointer(false, false, pointer);
                assert_eq!(
                    handle.make_type_to(&typ),
                    Type::Pointer(
                        Pointer::new_with_next_pointer(false, false, Pointer::new(false, false)),
                        Box::new(Type::Pointer(
                            Pointer::new(false, false),
                            Box::new(Type::Number(NumberType::Int))
                        ))
                    )
                );
                let declarator = Declarator::new(Some(handle), DirectDeclarator::Symbol(String::from("z")));
                let declaration = Declaration::new(declarator, None);
                v.push(declaration);

                assert_eq!(body,
                    Block {
                        body: vec![
                            AST::DefVar{
                                specifiers: specifier,
                                declarations: v,
                            }
                        ],
                    }
                );
            },
            _ => panic!("ast: {:?}", ast),
        }

        Ok(())
    }

    #[test]
    fn parse_struct() -> Result<(), ParserError> {
        let src = "
            struct date {
                int year, month;
                int day;
            };

            typedef struct date Date;

            void foo() {
                Date date;

                date.year = 2022;
                date.month = 12;
                date.day = 31;

                Date* pointer = &date;
                pointer->year = 2023;
                pointer->month = 1;
                pointer->day = 1;
            }
        ";
        let list = parse_translation_unit_from_str(src).unwrap();

        assert_eq!(list.len(), 3);

        //
        // test first statement
        //
        if let AST::DefineStruct {name: Some(id), fields} = &list[0] {
            assert_eq!(*id, "date".to_string());

            let fields = fields.get_fields().unwrap();

            assert_eq!(fields.len(), 3);

            let sq = SpecifierQualifier::new();
            let field1 = StructField::NormalField {
                name: Some("year".to_string()),
                sq: sq.clone(),
                typ: Type::Number(NumberType::Int)
            };
            assert_eq!(fields[0], field1);
    
            let field2 = StructField::NormalField {
                name: Some("month".to_string()),
                sq: sq.clone(),
                typ: Type::Number(NumberType::Int)
            };
            assert_eq!(fields[1], field2);

            let field3 = StructField::NormalField {
                name: Some("day".to_string()),
                sq: sq.clone(),
                typ: Type::Number(NumberType::Int)
            };
            assert_eq!(fields[2], field3);

        }else{
            panic!();
        }

        //
        // test 2nd statement
        //
        if let AST::TypeDef(name, Type::Struct {name: Some(id), fields}) = &list[1] {
            assert_eq!(*name, "Date".to_string());
            assert_eq!(*id, "date".to_string());
            assert!(fields.has_fields());

        }else{
            panic!("");
        }

        //
        // test 3rd statement
        //
        if let AST::Function(Function {specifiers, declarator, params, body, labels: _}) = &list[2] {
            let name = declarator.get_name();
            let ret_type = specifiers.get_type();
            assert_eq!(*name, "foo".to_string());
            assert_eq!(*ret_type, Type::Void);
            assert_eq!(params.len(), 0);

            let mut defs = Defines::new();
            assert_eq!(*params, Params::from_vec(vec![], false, &mut defs)?);

            let list = &body.body;
            assert_eq!(list.len(), 8);

            //
            // Date date;
            //
            let sq = SpecifierQualifier::new();
            let int_type = Type::Number(NumberType::Int);
            let mut field_list: Vec<StructDeclaration> = Vec::new();

            // member 'year'
            let declarator = StructDeclarator::new(Some(Declarator::new(None, DirectDeclarator::Symbol(String::from("year")))), None);
            let declarator_list = vec![declarator];
            field_list.push(StructDeclaration::new(sq.clone(), Some(int_type.clone()), declarator_list));
            // member 'month'
            let declarator = StructDeclarator::new(Some(Declarator::new(None, DirectDeclarator::Symbol(String::from("month")))), None);
            let declarator_list = vec![declarator];
            field_list.push(StructDeclaration::new(sq.clone(), Some(int_type.clone()), declarator_list));
            // member 'day'
            let declarator = StructDeclarator::new(Some(Declarator::new(None, DirectDeclarator::Symbol(String::from("day")))), None);
            let declarator_list = vec![declarator];
            field_list.push(StructDeclaration::new(sq.clone(), Some(int_type.clone()), declarator_list));

            let struct_definition = StructDefinition::try_new(Some("date".to_string()), Some(field_list))?;
            let type_struct = Type::Struct { name: Some("date".to_string()), fields: struct_definition.clone() };
            let specifier = DeclarationSpecifier::new(type_struct.clone(), sq);

            let mut v = Vec::new();
            let declarator = Declarator::new(None, DirectDeclarator::Symbol(String::from("date")));
            let declaration = Declaration::new(declarator, None);
            v.push(declaration);

            assert_eq!(
                list[0],
                AST::DefVar{
                    specifiers: specifier,
                    declarations: v,
                }
            );
            // date.year = 2022;
            // date.month = 12;
            // date.day = 31;
            assert_eq!(
                list[1],
                AST::Expr(Box::new(
                    ExprAST::Assign(
                        Box::new(ExprAST::MemberAccess(
                            Box::new(ExprAST::Symbol(String::from("date"), Position::new(12, 16))),
                            String::from("year"),
                            Position::new(12, 20)
                        )),
                        Box::new(ExprAST::Int(2022, Position::new(12, 28))),
                        Position::new(12, 26)
                )))
            );
            assert_eq!(
                list[2],
                AST::Expr(Box::new(
                    ExprAST::Assign(
                        Box::new(ExprAST::MemberAccess(
                            Box::new(ExprAST::Symbol(String::from("date"), Position::new(13, 16))),
                            String::from("month"),
                            Position::new(13, 20)
                        )),
                        Box::new(ExprAST::Int(12, Position::new(13, 29))),
                        Position::new(13, 27)
                )))
            );
            assert_eq!(
                list[3],
                AST::Expr(Box::new(
                    ExprAST::Assign(
                        Box::new(ExprAST::MemberAccess(
                            Box::new(ExprAST::Symbol(String::from("date"), Position::new(14, 16))),
                                String::from("day"),
                                Position::new(14, 20)
                        )),
                        Box::new(ExprAST::Int(31, Position::new(14, 27))),
                        Position::new(14, 25)
                )))
            );

            // Date* pointer = &date;
            if let AST::DefVar { specifiers, declarations: declaration } = &list[4] {
                let typ = specifiers.get_type();
                assert_eq!(*typ, type_struct.clone());


                let sq = SpecifierQualifier::new();
                let specifiers2: DeclarationSpecifier = DeclarationSpecifier::new(typ.clone(), sq);
                assert_eq!(*specifiers, specifiers2);

                assert_eq!(declaration.len(), 1);

                let declarator = declaration[0].get_declarator();
                assert_eq!(
                    *declarator,
                    Declarator::new(
                        Some(Pointer::new(false, false)),
                        DirectDeclarator::Symbol(String::from("pointer"))
                    )
                );

                if let Some(expr) = declaration[0].get_init_expr() {
                    assert_eq!(
                        **expr,
                        ExprAST::UnaryGetAddress(
                            Box::new(ExprAST::Symbol(String::from("date"), Position::new(16, 33))),
                            Position::new(16, 32)
                        )
                    );

                }else{
                    panic!()
                }

            }else{
                panic!();
            }

            // pointer->year = 2023;
            // pointer->month = 1;
            // pointer->day = 1;
            assert_eq!(
                list[5],
                AST::Expr(Box::new(
                    ExprAST::Assign(
                        Box::new(ExprAST::PointerAccess(
                            Box::new(ExprAST::Symbol(String::from("pointer"), Position::new(17, 16))),
                            String::from("year"),
                            Position::new(17, 23)
                        )),
                        Box::new(ExprAST::Int(2023, Position::new(17, 32))),
                        Position::new(17, 30)
                )))
            );
            assert_eq!(
                list[6],
                AST::Expr(Box::new(
                    ExprAST::Assign(
                        Box::new(ExprAST::PointerAccess(
                            Box::new(ExprAST::Symbol(String::from("pointer"), Position::new(18, 16))),
                            String::from("month"),
                            Position::new(18, 23)
                        )),
                        Box::new(ExprAST::Int(1, Position::new(18, 33))),
                        Position::new(18, 31)
                )))
            );
            assert_eq!(
                list[7],
                AST::Expr(Box::new(
                    ExprAST::Assign(
                        Box::new(ExprAST::PointerAccess(
                            Box::new(ExprAST::Symbol(String::from("pointer"), Position::new(19, 16))),
                            String::from("day"),
                            Position::new(19, 23)
                        )),
                        Box::new(ExprAST::Int(1, Position::new(19, 31))),
                        Position::new(19, 29)
                )))
            );

        }else{
            panic!();
        }

        Ok(())
    }

    #[test]
    fn parse_define_array() -> Result<(), ParserError> {
        let src = "
            int ary[2][3];
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            AST::GlobalDefVar { specifiers: DeclarationSpecifier {typ, specifier_qualifier}, declaration } => {
                assert_eq!(typ, Type::Number(NumberType::Int));
                assert_eq!(specifier_qualifier, SpecifierQualifier::new());

                assert_eq!(declaration.len(), 1);
                let decl = &declaration[0];

                let init_expr = decl.get_init_expr();
                assert_eq!(*init_expr, None);

                let declarator = decl.get_declarator();
                assert_eq!(*declarator.get_pointer(), None);
                let direct_decl = declarator.get_direct_declarator();
                assert_eq!(direct_decl.get_name(), "ary");

                match direct_decl {
                    DirectDeclarator::ArrayDef(second_direct_decl, size_list) => {
                        assert_eq!(*size_list, vec![Some(ConstExpr::Int(2)), Some(ConstExpr::Int(3))]);
                        assert_eq!(**second_direct_decl, DirectDeclarator::Symbol("ary".to_string()));
                   },
                    _ => panic!("direct_decl: {:?}", direct_decl),
                }

                Ok(())
            },
            _ => panic!("ast: {:?}", ast),
        }
    }
}