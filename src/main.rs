#![feature(trait_alias)]

mod ast;
mod const_eval;
mod fmt;
mod ir;
mod lexer;
mod parser;
mod typecheck;

use const_eval::{eval, VarScope};
use langbox::*;
use lexer::*;
use std::collections::HashMap;
use std::rc::Rc;
use typecheck::*;

type SharedString = Rc<str>;

const TEST_FILE: &str = include_str!("../test.qrz");

fn main() -> std::io::Result<()> {
    let mut file_server = FileServer::new();
    let file = file_server.register_file_memory("<test>", TEST_FILE);

    let lexer = QuartzLexer::new(file, &file_server);
    let tokens: Vec<_> = lexer
        .filter(|t| {
            if let QuartzToken::Comment(_) = &t.kind {
                false
            } else {
                true
            }
        })
        .collect();

    match parser::design().run(TokenStream::new(&tokens)) {
        ParseResult::Match { value: design, .. } => {
            //for (i, item) in design.iter().enumerate() {
            //    if i > 0 {
            //        print!("\n\n");
            //    }
            //    print!("{}", item);
            //}

            let mut errors = Vec::new();

            if let Err(err) = check_for_duplicate_idents(design.iter()) {
                errors.push(err);
            }

            let mut empty_global_scope = Scope::empty();
            for item in design.iter() {
                match item {
                    ast::Item::Const(const_item) => empty_global_scope.add_const(const_item.name()),
                    ast::Item::Func(func_item) => {
                        empty_global_scope.add_func(func_item.name(), func_item.args().len())
                    }
                    _ => {}
                }
            }

            let mut consts = HashMap::new();
            let mut funcs = HashMap::new();
            for item in design.iter() {
                match item {
                    ast::Item::Const(const_item) => {
                        match transform_const_expr(
                            const_item.value(),
                            &mut empty_global_scope,
                            false,
                        ) {
                            Ok(expr) => {
                                if !consts.contains_key(const_item.name().as_ref())
                                    && !funcs.contains_key(const_item.name().as_ref())
                                {
                                    consts.insert(const_item.name().as_string(), expr);
                                }
                            }
                            Err(err) => errors.push(err),
                        }
                    }
                    ast::Item::Func(func_item) => {
                        match transform_const_func(func_item, &mut empty_global_scope) {
                            Ok(func) => {
                                if !consts.contains_key(func_item.name().as_ref())
                                    && !funcs.contains_key(func_item.name().as_ref())
                                {
                                    funcs.insert(func_item.name().as_string(), func);
                                }
                            }
                            Err(err) => errors.push(err),
                        }
                    }
                    _ => {}
                }
            }

            let mut const_values = HashMap::new();
            for (name, expr) in consts.iter() {
                const_values.insert(
                    SharedString::clone(name),
                    eval(expr, &mut VarScope::empty(), &consts, &funcs),
                );
            }

            for (name, value) in const_values.iter() {
                println!("{} = {}", name, value);
            }
        }
        ParseResult::NoMatch => panic!("input did not match"),
        ParseResult::Err(err) => panic!("parse error: {:?}", err),
    }

    Ok(())
}
