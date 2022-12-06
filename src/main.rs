#![allow(clippy::too_many_arguments)]
#![feature(trait_alias)]
#![feature(int_log)]
#![feature(let_chains)]
#![feature(maybe_uninit_uninit_array)]
#![feature(const_maybe_uninit_uninit_array)]
#![feature(maybe_uninit_slice)]
#![feature(bigint_helper_methods)]
#![feature(try_trait_v2)]

#[macro_use]
mod error;

mod ast;
mod const_eval;
mod fmt;
mod ir;
mod lexer;
mod lowering;
mod parser;
mod pretty_printing;
mod range_collection;
mod scope;
mod small_vec;
mod transpile;
mod type_resolve;
mod typecheck;
mod vir;

use crate::scope::*;
use const_eval::{eval, VarScope};
use langbox::*;
use lexer::*;
use pretty_printing::{write_error, WriteColored};
use std::path::PathBuf;
use std::rc::Rc;
use termcolor::StandardStream;
use type_resolve::*;
use typecheck::*;

type SharedString = Rc<str>;
type HashMap<K, V> = std::collections::HashMap<K, V, xxhash_rust::xxh3::Xxh3Builder>;
type HashSet<T> = std::collections::HashSet<T, xxhash_rust::xxh3::Xxh3Builder>;
pub use small_vec::SmallVec;

trait Clog2 {
    fn clog2(self) -> Self;
}

impl Clog2 for u64 {
    fn clog2(mut self) -> Self {
        if self == 0 {
            return 0;
        }

        self -= 1;
        let mut log = 0;
        while self > 0 {
            self >>= 1;
            log += 1;
        }
        log
    }
}

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

    /// Output SystemVerilog file [default: out.sv]
    #[arg(short, long, value_name = "FILE")]
    output: Option<PathBuf>,

    /// Input Quartz files
    #[arg(value_name = "INPUT FILES")]
    inputs: Vec<PathBuf>,
}

#[repr(transparent)]
struct Tokens(Vec<Token<QuartzToken>>);

impl<'a> IntoIterator for &'a Tokens {
    type Item = &'a [Token<QuartzToken>];
    type IntoIter = TokenSlices<'a>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        TokenSlices {
            tokens: &self.0,
            next: 0,
        }
    }
}

struct TokenSlices<'a> {
    tokens: &'a [Token<QuartzToken>],
    next: usize,
}

impl<'a> Iterator for TokenSlices<'a> {
    type Item = &'a [Token<QuartzToken>];

    fn next(&mut self) -> Option<Self::Item> {
        if self.next == self.tokens.len() {
            return None;
        }

        let start = self.next;

        let mut depth: usize = 0;
        while let Some(token) = self.tokens.get(self.next) {
            self.next += 1;

            if let QuartzToken::Punct(PunctKind::OpenCurl) = &token.kind {
                depth = 1;
                break;
            }
        }

        while depth > 0 {
            if let Some(token) = self.tokens.get(self.next) {
                self.next += 1;

                match &token.kind {
                    QuartzToken::Punct(PunctKind::OpenCurl) => depth += 1,
                    QuartzToken::Punct(PunctKind::CloseCurl) => depth -= 1,
                    _ => {}
                }
            } else {
                break;
            }
        }

        Some(&self.tokens[start..self.next])
    }
}

fn tokenize(file_server: &mut FileServer, input_files: &[PathBuf]) -> std::io::Result<Tokens> {
    let mut files = HashSet::default();
    for path in input_files.iter() {
        let file = file_server.register_file(path)?;
        files.insert(file);
    }

    let mut tokens = Vec::new();
    for file in files.into_iter() {
        let lexer = QuartzLexer::new(file, file_server);

        tokens.extend(lexer.filter(|t| !matches!(&t.kind, QuartzToken::Comment(_))));
    }

    Ok(Tokens(tokens))
}

fn main() -> std::io::Result<()> {
    const DEFAULT_TOP_MODULE_NAME: &str = "Top";
    const DEFAULT_OUTPUT_FILE: &str = "out.sv";

    let args = <Args as clap::Parser>::parse();
    let top_module_name = args.top.as_deref().unwrap_or(DEFAULT_TOP_MODULE_NAME);
    let output_path = args
        .output
        .as_deref()
        .unwrap_or_else(|| DEFAULT_OUTPUT_FILE.as_ref());
    let input_files = args.inputs.as_slice();

    let stderr = StandardStream::stderr(termcolor::ColorChoice::Auto);

    if input_files.is_empty() {
        let mut stderr = stderr.lock();
        return write_error("no input files specified", &mut stderr);
    }

    let mut file_server = FileServer::new();
    let tokens = tokenize(&mut file_server, input_files)?;

    let mut errors = Vec::new();
    let mut design = Vec::new();
    for tokens in tokens.into_iter() {
        match parser::parse(tokens) {
            ParseResult::Match { value, .. } => design.extend(value.into_iter()),
            ParseResult::NoMatch => {}
            ParseResult::Err(err) => errors.push(err.into()),
        }
    }

    abort_on_error!(errors, stderr, file_server);

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
                match transform_const_expr(const_item.value(), &global_scope, false, false) {
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
            ast::Item::Func(func_item) => match transform_const_func(func_item, &global_scope) {
                Ok(func) => {
                    if !global_consts.contains_key(func_item.name().as_ref())
                        && !funcs.contains_key(func_item.name().as_ref())
                    {
                        funcs.insert(func_item.name().as_string(), func);
                    }
                }
                Err(err) => errors.push(err),
            },
            _ => {}
        }
    }

    abort_on_error!(errors, stderr, file_server);

    let mut errors = Vec::new();
    let mut global_const_values = HashMap::default();
    for (name, expr) in global_consts.iter() {
        match eval::<_, i64>(expr, &mut VarScope::empty(), &global_consts, None, &funcs)
            .into_result()
        {
            Ok(value) => {
                global_const_values.insert(SharedString::clone(name), value);
            }
            Err(err) => errors.push(err),
        }
    }

    abort_on_error!(errors, stderr, file_server);

    let type_items = collect_type_items(design);

    let Some(top_item) = type_items.get(top_module_name) else {
        let mut stderr = stderr.lock();
        return write_error(
            &format!("top module `{}` not found", top_module_name),
            &mut stderr,
        );
    };
    let ir::TypeItem::Module(top_module) = top_item else {
        let mut stderr = stderr.lock();
        return write_error(
            &format!("`{}` is not a module", top_module_name),
            &mut stderr,
        );
    };

    if let Some(generic_args) = top_module.generic_args() && !generic_args.args().is_empty() {
        let mut stderr = stderr.lock();
        return write_error("modules with generic arguments cannot be used as top module", &mut stderr);
    }

    let resolve_result = resolve_types(
        top_module,
        &type_items,
        &global_scope,
        &global_const_values,
        &funcs,
    );

    match resolve_result {
        Ok((mut known_types, resolved_types, mut type_order)) => {
            let mut errors = Vec::new();
            let mut checked_modules = Vec::new();
            while let Some(ty_id) = type_order.pop() {
                match &resolved_types.get(&ty_id) {
                    Some(ir::ResolvedTypeItem::Struct(struct_item)) => {
                        if let Err(err) =
                            struct_contains_module(struct_item, &known_types, &resolved_types)
                        {
                            errors.push(err);
                        }
                    }
                    Some(ir::ResolvedTypeItem::Module(module_item)) => {
                        match typecheck_module(
                            module_item,
                            &global_scope,
                            &mut known_types,
                            &resolved_types,
                            &global_const_values,
                            &funcs,
                        ) {
                            Ok(module_item) => checked_modules.push((ty_id, module_item)),
                            Err(err) => errors.push(err),
                        }
                    }
                    _ => {}
                }
            }

            abort_on_error!(errors, stderr, file_server);

            if !type_order.is_empty() {
                let mut stderr = stderr.lock();
                // TODO: find a way to report which types actually form a cycle
                return write_error("cyclic type definitions detected", &mut stderr);
            }

            let mut v_modules = Vec::with_capacity(checked_modules.len());
            for (module_ty, checked_module) in checked_modules.iter() {
                let v_module = lowering::lower(checked_module, &known_types, &resolved_types);
                v_modules.push((*module_ty, v_module));
            }

            if let Some(parent) = output_path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            let mut output_file = std::fs::File::create(output_path)?;
            transpile::transpile(&mut output_file, &known_types, &resolved_types, &v_modules)?;
            output_file.sync_all()?;
        }
        Err(err) => {
            let mut stderr = stderr.lock();
            err.write_colored(&mut stderr, &file_server)?;
        }
    }

    Ok(())
}
