use super::{Position, Token};
use super::ParserError;
use super::ast::{AST, ToplevelAST, ExprAST, Block, Param, Params, BinOp, TypeQualifier, DeclarationSpecifier, SpecifierQualifier, Declarator, DirectDeclarator, Initializer};
use super::ast::{DeclarationSpecifierOrVariadic, Declaration, StructDeclaration, StructDeclarator, AbstractDeclarator, DirectAbstractDeclarator, ImplElement};
use super::ast::{StructLiteral, EnumLiteral};
use super::ConstExpr;
use super::types::*;
use super::defines::*;
use super::{CustSelf, Function, FunProto, FunOrProt, Switch, Case};
use super::parse::Parser;

use std::slice::Iter;
use std::iter::Peekable;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

impl Parser {
    pub fn parse_impl(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ToplevelAST>, ParserError> {
        let (tok, pos) = iter.peek().unwrap();
        if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }
        if *tok != Token::Impl {
            // println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
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
            // println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
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
                    // println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
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
                        // println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
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
                        // println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
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
                        // println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                        Err(ParserError::syntax_error(pos.clone()))
                    },
                }
            },
        }
    }
}