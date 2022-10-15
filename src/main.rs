#![feature(trait_alias)]
#![feature(int_log)]

mod ast;
mod const_eval;
mod fmt;
mod ir;
mod lexer;
mod parser;
mod pretty_printing;
mod typecheck;

use const_eval::{eval, VarScope};
use langbox::*;
use lexer::*;
use pretty_printing::write_error;
use std::collections::HashMap;
use std::rc::Rc;
use termcolor::WriteColor;
use typecheck::*;

type SharedString = Rc<str>;

const TEST_FILE: &str = include_str!("../test.qrz");
const TEST_TOP_MOD: &str = "Top";

fn main() -> std::io::Result<()> {
    let stderr = termcolor::StandardStream::stderr(termcolor::ColorChoice::Auto);

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

            if errors.len() > 0 {
                let mut stderr = stderr.lock();

                for (i, err) in errors.into_iter().enumerate() {
                    if i > 0 {
                        use std::io::Write;
                        writeln!(stderr)?;
                    }

                    err.write_colored(&mut stderr, &file_server)?;
                    stderr.reset()?;
                }
            } else {
                let mut const_error = false;
                let mut const_values = HashMap::new();
                for (name, expr) in consts.iter() {
                    match eval(expr, &mut VarScope::empty(), &consts, &funcs) {
                        Ok(value) => {
                            const_values.insert(SharedString::clone(name), value);
                        }
                        Err(err) => {
                            let mut stderr = stderr.lock();
                            err.write_colored(&mut stderr, &file_server)?;
                            const_error = true;
                        }
                    }
                }

                if const_error {
                    return Ok(());
                }

                let mut structs = HashMap::new();
                let mut enums = HashMap::new();
                let mut modules = HashMap::new();
                for item in design.into_iter() {
                    match item {
                        ast::Item::Struct(struct_item) => {
                            structs.insert(struct_item.name().as_string(), struct_item);
                        }
                        ast::Item::Enum(enum_item) => {
                            enums.insert(enum_item.name().as_string(), enum_item);
                        }
                        ast::Item::Module(module_item) => {
                            modules.insert(module_item.name().as_string(), module_item);
                        }
                        _ => {}
                    }
                }

                if let Some(top_module) = modules.get(TEST_TOP_MOD) {
                } else {
                    let mut stderr = stderr.lock();
                    write_error(
                        &format!("top module `{}` not found", TEST_TOP_MOD),
                        &mut stderr,
                    )?;
                }
            }
        }
        ParseResult::NoMatch => {
            let mut stderr = stderr.lock();
            write_error("no valid input code found", &mut stderr)?;
        }
        ParseResult::Err(err) => {
            let mut stderr = stderr.lock();
            err.write_colored(&mut stderr, &file_server)?;
        }
    }

    Ok(())
}
