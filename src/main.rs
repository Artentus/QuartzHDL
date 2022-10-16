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
use pretty_printing::{write_error, WriteColored};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::rc::Rc;
use termcolor::StandardStream;
use typecheck::*;

type SharedString = Rc<str>;

#[doc(hidden)]
fn _write_errors<E: WriteColored>(
    errors: impl IntoIterator<Item = E>,
    stderr: &StandardStream,
    file_server: &FileServer,
) -> std::io::Result<()> {
    let mut stderr = stderr.lock();

    for err in errors.into_iter() {
        err.write_colored(&mut stderr, file_server)?;
    }

    Ok(())
}

#[macro_export]
macro_rules! abort_on_error {
    ($errors:expr, $stderr:expr, $file_server:expr) => {
        if $errors.len() > 0 {
            return $crate::_write_errors($errors, &$stderr, &$file_server);
        }
    };
}

/// QuartzHDL transpiler
#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[command(disable_help_subcommand = true)]
struct Args {
    /// Name of the top module [default: Top]
    #[arg(short, long, value_name = "NAME")]
    top: Option<String>,

    /// Output Verilog file [default: out.v]
    #[arg(short, long, value_name = "FILE")]
    output: Option<PathBuf>,

    /// Input Quartz files
    #[arg(value_name = "INPUT FILES")]
    inputs: Vec<PathBuf>,
}

fn main() -> std::io::Result<()> {
    const DEFAULT_TOP_MODULE_NAME: &str = "Top";
    const DEFAULT_OUTPUT_FILE: &str = "out.v";

    let args = <Args as clap::Parser>::parse();
    let top_module_name = args
        .top
        .as_ref()
        .map(|s| s.as_str())
        .unwrap_or(DEFAULT_TOP_MODULE_NAME);
    let output_file = args
        .output
        .as_ref()
        .map(|p| p.as_path())
        .unwrap_or(DEFAULT_OUTPUT_FILE.as_ref());
    let input_files = args.inputs.as_slice();

    let stderr = StandardStream::stderr(termcolor::ColorChoice::Auto);

    if input_files.len() == 0 {
        let mut stderr = stderr.lock();
        return write_error("no input files specified", &mut stderr);
    }

    let mut file_server = FileServer::new();
    let mut files = HashSet::new();
    for path in input_files.iter() {
        let file = file_server.register_file(path)?;
        files.insert(file);
    }

    let mut tokens = Vec::new();
    for file in files.into_iter() {
        let lexer = QuartzLexer::new(file, &file_server);

        tokens.extend(lexer.filter(|t| {
            if let QuartzToken::Comment(_) = &t.kind {
                false
            } else {
                true
            }
        }));
    }

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

            abort_on_error!(errors, stderr, file_server);

            let mut errors = Vec::new();
            let mut const_values = HashMap::new();
            for (name, expr) in consts.iter() {
                match eval(expr, &mut VarScope::empty(), &consts, &funcs) {
                    Ok(value) => {
                        const_values.insert(SharedString::clone(name), value);
                    }
                    Err(err) => errors.push(err),
                }
            }

            abort_on_error!(errors, stderr, file_server);

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

            if let Some(top_module) = modules.get(top_module_name) {
            } else {
                let mut stderr = stderr.lock();
                write_error(
                    &format!("top module `{}` not found", top_module_name),
                    &mut stderr,
                )?;
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
