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
use std::path::PathBuf;
use std::rc::Rc;
use termcolor::StandardStream;
use typecheck::*;

type SharedString = Rc<str>;
type HashMap<K, V> = std::collections::HashMap<K, V, xxhash_rust::xxh3::Xxh3Builder>;
type HashSet<T> = std::collections::HashSet<T, xxhash_rust::xxh3::Xxh3Builder>;

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

fn tokenize(
    file_server: &mut FileServer,
    input_files: &[PathBuf],
) -> std::io::Result<Vec<Token<QuartzToken>>> {
    let mut files = HashSet::default();
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

    Ok(tokens)
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
    let tokens = tokenize(&mut file_server, input_files)?;

    // TODO: split tokens into ranges containing one item each to accumulate more parse errors.

    match parser::parse(&tokens) {
        ParseResult::Match { value: design, .. } => {
            let mut errors = Vec::new();

            if let Err(err) = check_for_duplicate_items(design.iter()) {
                errors.push(err);
            }

            let mut global_scope = Scope::empty();
            for item in design.iter() {
                match item {
                    ast::Item::Const(const_item) => global_scope.add_const(const_item.name()),
                    ast::Item::Func(func_item) => {
                        global_scope.add_func(func_item.name(), func_item.args().len())
                    }
                    _ => {}
                }
            }

            let mut global_consts = HashMap::default();
            let mut funcs = HashMap::default();
            for item in design.iter() {
                match item {
                    ast::Item::Const(const_item) => {
                        match transform_const_expr(const_item.value(), &mut global_scope, false) {
                            Ok(expr) => {
                                if !global_consts.contains_key(const_item.name().as_ref())
                                    && !funcs.contains_key(const_item.name().as_ref())
                                {
                                    global_consts.insert(const_item.name().as_string(), expr);
                                }
                            }
                            Err(err) => errors.push(err),
                        }
                    }
                    ast::Item::Func(func_item) => {
                        match transform_const_func(func_item, &mut global_scope) {
                            Ok(func) => {
                                if !global_consts.contains_key(func_item.name().as_ref())
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
            let mut global_const_values = HashMap::default();
            for (name, expr) in global_consts.iter() {
                match eval::<_, i64>(expr, &mut VarScope::empty(), &global_consts, None, &funcs) {
                    Ok(value) => {
                        global_const_values.insert(SharedString::clone(name), value);
                    }
                    Err(err) => errors.push(err),
                }
            }

            abort_on_error!(errors, stderr, file_server);

            let type_items = collect_type_items(design);
            if let Some(top_item) = type_items.get(top_module_name) {
                if let ir::TypeItem::Module(top_module) = top_item {
                    if let Some(generic_args) = top_module.generic_args() && (generic_args.args().len() > 0) {
                        let mut stderr = stderr.lock();
                        return write_error("modules with generic arguments cannot be used as top module", &mut stderr);
                    }

                    let result = resolve_types(
                        top_module,
                        &type_items,
                        &global_scope,
                        &global_const_values,
                        &funcs,
                    );

                    match result {
                        Ok(resolved_types) => {
                            // TODO:
                        }
                        Err(err) => {
                            let mut stderr = stderr.lock();
                            err.write_colored(&mut stderr, &file_server)?;
                        }
                    }
                } else {
                    let mut stderr = stderr.lock();
                    write_error(
                        &format!("`{}` is not a module", top_module_name),
                        &mut stderr,
                    )?;
                }
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
