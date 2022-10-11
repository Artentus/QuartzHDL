#![feature(trait_alias)]
#![feature(int_log)]

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
use termcolor::WriteColor;
use typecheck::*;

type SharedString = Rc<str>;

const TEST_FILE: &str = include_str!("../test.qrz");

#[doc(hidden)]
fn _set_style(
    stream: &mut termcolor::StandardStreamLock,
    color: termcolor::Color,
    bold: bool,
    italic: bool,
) -> std::io::Result<()> {
    use termcolor::ColorSpec;

    let mut spec = ColorSpec::new();
    spec.set_fg(Some(color)).set_bold(bold).set_italic(italic);

    stream.set_color(&spec)
}

#[macro_export]
macro_rules! write_styled {
    ($stream:expr, [$color:expr], $($arg:tt)*) => {{
        use termcolor::Color::*;
        $crate::_set_style($stream, $color, false, false).and_then(|_| {
            use std::io::Write;
            write!($stream, $($arg)*)
        })
    }};

    ($stream:expr, [$color:expr; Bold], $($arg:tt)*) => {{
        use termcolor::Color::*;
        $crate::_set_style($stream, $color, true, false).and_then(|_| {
            use std::io::Write;
            write!($stream, $($arg)*)
        })
    }};

    ($stream:expr, [$color:expr; Italic], $($arg:tt)*) => {{
        use termcolor::Color::*;
        $crate::_set_style($stream, $color, false, true).and_then(|_| {
            use std::io::Write;
            write!($stream, $($arg)*)
        })
    }};
}

#[macro_export]
macro_rules! writeln_styled {
    ($stream:expr, [$color:expr], $($arg:tt)*) => {{
        use termcolor::Color::*;
        $crate::_set_style($stream, $color, false, false).and_then(|_| {
            use std::io::Write;
            writeln!($stream, $($arg)*)
        })
    }};

    ($stream:expr, [$color:expr; Bold], $($arg:tt)*) => {{
        use termcolor::Color::*;
        $crate::_set_style($stream, $color, true, false).and_then(|_| {
            use std::io::Write;
            writeln!($stream, $($arg)*)
        })
    }};

    ($stream:expr, [$color:expr; Italic], $($arg:tt)*) => {{
        use termcolor::Color::*;
        $crate::_set_style($stream, $color, false, true).and_then(|_| {
            use std::io::Write;
            writeln!($stream, $($arg)*)
        })
    }};
}

impl TypecheckError<'_> {
    pub fn write_colored(
        &self,
        stream: &mut termcolor::StandardStreamLock,
        file_server: &langbox::FileServer,
    ) -> std::io::Result<()> {
        use ast::Spanned;
        use std::io::Write;

        write_styled!(stream, [Red; Bold], "Error")?;
        write_styled!(stream, [White; Bold], ": ")?;

        let error_span = match self {
            Self::DuplicateIdent { name } => {
                writeln_styled!(stream, [White; Bold], "`{}` has already been defined", name)?;
                name.span()
            }
            Self::InvalidConstExpr { expr } => {
                writeln_styled!(
                    stream,
                    [White; Bold],
                    "expression is not valid in constant context"
                )?;
                expr.span()
            }
            Self::InvalidConstPattern { pattern } => {
                writeln_styled!(
                    stream,
                    [White; Bold],
                    "pattern is not valid in constant context"
                )?;
                pattern.span()
            }
            Self::NonExhaustiveMatch { match_expr } => {
                writeln_styled!(stream, [White; Bold], "match expression is not exhaustive")?;
                match_expr
                    .match_kw()
                    .span()
                    .join(&match_expr.value().span())
            }
            Self::InvalidConstOp { op } => {
                writeln_styled!(
                    stream,
                    [White; Bold],
                    "`{}` operator is not valid in constant context",
                    op
                )?;
                op.span()
            }
            Self::InvalidConstAssignTarget { target } => {
                writeln_styled!(
                    stream,
                    [White; Bold],
                    "target expression is not valid in constant context"
                )?;
                target.span()
            }
            Self::MissingReturnValue { block } => {
                writeln_styled!(stream, [White; Bold], "block is missing return value")?;
                block.span()
            }
            Self::UnexpectedReturnValue { value } => {
                writeln_styled!(stream, [White; Bold], "return value not expected")?;
                value.span()
            }
            Self::UndefinedIdent { name } => {
                writeln_styled!(stream, [White; Bold], "`{}` is not defined", name)?;
                name.span()
            }
            Self::TargetNotAssignable { name } => {
                writeln_styled!(stream, [White; Bold], "`{}` cannot be assigned to", name)?;
                name.span()
            }
            Self::ValueNotConst { name } => {
                writeln_styled!(stream, [White; Bold], "`{}` is not a constant", name)?;
                name.span()
            }
            Self::InvalidValueIdent { name } => {
                writeln_styled!(stream, [White; Bold], "`{}` is not a value", name)?;
                name.span()
            }
            Self::InvalidFuncIdent { name } => {
                writeln_styled!(stream, [White; Bold], "`{}` is not a function", name)?;
                name.span()
            }
            Self::UnknownType { ty } => todo!(),
            Self::InvalidType { ty } => todo!(),
            Self::MissingElseBlock { if_expr } => todo!(),
            Self::ArgumentCountMismatch {
                call_expr,
                arg_count,
            } => todo!(),
            Self::List(list) => {
                for (i, err) in list.iter().enumerate() {
                    if i > 0 {
                        writeln!(stream)?;
                    }

                    err.write_colored(stream, file_server)?;
                }

                return Ok(());
            }
        };

        let file = file_server.get_file(error_span.file_id()).unwrap();

        let start_line = error_span.start_pos().line();
        let end_line = error_span.end_pos().line();
        let line_count = end_line - start_line + 1;
        let digit_count = ((end_line + 1) * 10).ilog10() as usize;

        write_styled!(
            stream,
            [Cyan; Bold],
            "{:>width$}--> ",
            "",
            width = digit_count
        )?;
        writeln_styled!(
            stream,
            [White],
            "{}:{}:{}",
            file.path().display(),
            start_line + 1,
            error_span.start_pos().column() + 1
        )?;

        writeln_styled!(
            stream,
            [Cyan; Bold],
            "{:>width$} |",
            "",
            width = digit_count
        )?;

        for (i, line) in file
            .text()
            .lines()
            .enumerate()
            .skip(start_line)
            .take(line_count)
        {
            write_styled!(
                stream,
                [Cyan; Bold],
                "{:>width$} | ",
                i + 1,
                width = digit_count
            )?;

            writeln_styled!(stream, [White], "{}", line)?;

            write_styled!(
                stream,
                [Cyan; Bold],
                "{:>width$} | ",
                "",
                width = digit_count
            )?;

            if (i == start_line) && (i == end_line) {
                write_styled!(
                    stream,
                    [White; Bold],
                    "{:>width$}",
                    "",
                    width = error_span.start_pos().column()
                )?;

                writeln_styled!(
                    stream,
                    [Red; Bold],
                    "{:^^width$}",
                    "",
                    width = error_span.end_pos().column() - error_span.start_pos().column()
                )?;
            } else if i == start_line {
                write_styled!(
                    stream,
                    [White; Bold],
                    "{:>width$}",
                    "",
                    width = error_span.start_pos().column()
                )?;

                writeln_styled!(
                    stream,
                    [Red; Bold],
                    "{:^^width$}",
                    "",
                    width = line.chars().count() - error_span.start_pos().column()
                )?;
            } else if i == end_line {
                writeln_styled!(
                    stream,
                    [Red; Bold],
                    "{:^^width$}",
                    "",
                    width = error_span.end_pos().column()
                )?;
            } else {
                writeln_styled!(
                    stream,
                    [Red; Bold],
                    "{:^^width$}",
                    "",
                    width = line.chars().count()
                )?;
            }
        }

        writeln_styled!(
            stream,
            [Cyan; Bold],
            "{:>width$} |",
            "",
            width = digit_count
        )?;

        Ok(())
    }
}

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
        }
        ParseResult::NoMatch => panic!("input did not match"),
        ParseResult::Err(err) => panic!("parse error: {:?}", err),
    }

    Ok(())
}
