use langbox::{FileServer, TextSpan};
use std::borrow::Cow;
use std::io::Result;
use termcolor::{StandardStreamLock, WriteColor};

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

macro_rules! write_styled {
    ($stream:expr, [$color:expr], $($arg:tt)*) => {{
        use termcolor::Color::*;
        $crate::pretty_printing::_set_style($stream, $color, false, false).and_then(|_| {
            use std::io::Write;
            write!($stream, $($arg)*)
        })
    }};

    ($stream:expr, [$color:expr; Bold], $($arg:tt)*) => {{
        use termcolor::Color::*;
        $crate::pretty_printing::_set_style($stream, $color, true, false).and_then(|_| {
            use std::io::Write;
            write!($stream, $($arg)*)
        })
    }};

    ($stream:expr, [$color:expr; Italic], $($arg:tt)*) => {{
        use termcolor::Color::*;
        $crate::pretty_printing::_set_style($stream, $color, false, true).and_then(|_| {
            use std::io::Write;
            write!($stream, $($arg)*)
        })
    }};
}

macro_rules! writeln_styled {
    ($stream:expr, [$color:expr], $($arg:tt)*) => {{
        use termcolor::Color::*;
        $crate::pretty_printing::_set_style($stream, $color, false, false).and_then(|_| {
            use std::io::Write;
            writeln!($stream, $($arg)*)
        })
    }};

    ($stream:expr, [$color:expr; Bold], $($arg:tt)*) => {{
        use termcolor::Color::*;
        $crate::pretty_printing::_set_style($stream, $color, true, false).and_then(|_| {
            use std::io::Write;
            writeln!($stream, $($arg)*)
        })
    }};

    ($stream:expr, [$color:expr; Italic], $($arg:tt)*) => {{
        use termcolor::Color::*;
        $crate::pretty_printing::_set_style($stream, $color, false, true).and_then(|_| {
            use std::io::Write;
            writeln!($stream, $($arg)*)
        })
    }};
}

struct ErrorInfo<'a> {
    msg: Cow<'a, str>,
    span: TextSpan,
}

impl<'a> ErrorInfo<'a> {
    #[inline]
    fn new(msg: impl Into<Cow<'a, str>>, span: TextSpan) -> Self {
        Self {
            msg: msg.into(),
            span,
        }
    }
}

#[doc(hidden)]
fn _write_error(
    info: &ErrorInfo,
    stream: &mut StandardStreamLock,
    file_server: &FileServer,
) -> Result<()> {
    use std::io::Write;

    writeln!(stream)?;
    write_styled!(stream, [Red; Bold], "error")?;
    writeln_styled!(stream, [White; Bold], ": {}", info.msg)?;

    let file = file_server.get_file(info.span.file_id()).unwrap();

    let start_line = info.span.start_pos().line();
    let end_line = info.span.end_pos().line();
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
        info.span.start_pos().column() + 1
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
                width = info.span.start_pos().column()
            )?;

            writeln_styled!(
                stream,
                [Red; Bold],
                "{:^^width$}",
                "",
                width = info.span.end_pos().column() - info.span.start_pos().column()
            )?;
        } else if i == start_line {
            write_styled!(
                stream,
                [White; Bold],
                "{:>width$}",
                "",
                width = info.span.start_pos().column()
            )?;

            writeln_styled!(
                stream,
                [Red; Bold],
                "{:^^width$}",
                "",
                width = line.chars().count() - info.span.start_pos().column()
            )?;
        } else if i == end_line {
            writeln_styled!(
                stream,
                [Red; Bold],
                "{:^^width$}",
                "",
                width = info.span.end_pos().column()
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

    stream.reset()
}

pub trait WriteColored {
    fn write_colored(
        &self,
        stream: &mut termcolor::StandardStreamLock,
        file_server: &langbox::FileServer,
    ) -> Result<()>;
}

impl WriteColored for crate::typecheck::TypecheckError<'_> {
    fn write_colored(
        &self,
        stream: &mut termcolor::StandardStreamLock,
        file_server: &langbox::FileServer,
    ) -> Result<()> {
        use crate::ast::Spanned;
        use std::io::Write;

        let info = match self {
            Self::DuplicateIdent { name } => {
                ErrorInfo::new(format!("`{}` has already been defined", name), name.span())
            }
            Self::InvalidConstExpr { expr } => {
                ErrorInfo::new("expression is not valid in constant context", expr.span())
            }
            Self::InvalidConstPattern { pattern } => {
                ErrorInfo::new("pattern is not valid in constant context", pattern.span())
            }
            Self::NonExhaustiveMatch { match_expr } => ErrorInfo::new(
                "match expression is not exhaustive",
                match_expr
                    .match_kw()
                    .span()
                    .join(&match_expr.value().span()),
            ),
            Self::InvalidConstOp { op } => ErrorInfo::new(
                format!("`{}` operator is not valid in constant context", op),
                op.span(),
            ),
            Self::InvalidConstAssignTarget { target } => ErrorInfo::new(
                "target expression is not valid in constant context",
                target.span(),
            ),
            Self::MissingReturnValue { block } => {
                ErrorInfo::new("block is missing return value", block.span())
            }
            Self::UnexpectedReturnValue { value } => {
                ErrorInfo::new("return value not expected", value.span())
            }
            Self::UndefinedIdent { name } => {
                ErrorInfo::new(format!("`{}` is not defined", name), name.span())
            }
            Self::TargetNotAssignable { name } => {
                ErrorInfo::new(format!("`{}` cannot be assigned to", name), name.span())
            }
            Self::ValueNotConst { name } => {
                ErrorInfo::new(format!("`{}` is not a constant", name), name.span())
            }
            Self::InvalidValueIdent { name } => {
                ErrorInfo::new(format!("`{}` is not a value", name), name.span())
            }
            Self::InvalidFuncIdent { name } => {
                ErrorInfo::new(format!("`{}` is not a function", name), name.span())
            }
            Self::MissingElseBlock { if_expr } => ErrorInfo::new(
                "if-expressions with return value must include an else-branch",
                if_expr.span(),
            ),
            Self::ArgumentCountMismatch {
                call_expr,
                arg_count,
            } => ErrorInfo::new(
                format!(
                    "expected {} arguments to function `{}` but found {}",
                    arg_count,
                    call_expr.func().as_ref(),
                    call_expr.args().len()
                ),
                call_expr.span(),
            ),
            Self::GenericCountMismatch { ty, arg_count } => ErrorInfo::new(
                format!(
                    "expected {} generic arguments to type `{}` but found {}",
                    arg_count,
                    ty.name().as_ref(),
                    ty.generic_args().map(|args| args.args().len()).unwrap_or(0)
                ),
                ty.span(),
            ),
            Self::InvalidEnumBaseType { ty } => ErrorInfo::new(
                format!("`{}` is not a valid base type for an enum", ty),
                ty.span(),
            ),
            Self::InvalidBitWidth { width, arg } => {
                ErrorInfo::new(format!("`{}` is not a valid bit width", width), arg.span())
            }
            Self::UndefinedType { ty } => {
                ErrorInfo::new(format!("`the type {}` is not defined", ty), ty.span())
            }
            Self::ArithmeticError(err) => {
                return err.write_colored(stream, file_server);
            }
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

        _write_error(&info, stream, file_server)
    }
}

impl WriteColored for crate::const_eval::ArithmeticError {
    fn write_colored(
        &self,
        stream: &mut termcolor::StandardStreamLock,
        file_server: &langbox::FileServer,
    ) -> Result<()> {
        let (msg, span) = match self {
            crate::const_eval::ArithmeticError::Overflow { expr_span } => (
                "arithmetic overflow while evaluating expression",
                *expr_span,
            ),
            crate::const_eval::ArithmeticError::DivideByZero { expr_span } => {
                ("divide by zero while evaluating expression", *expr_span)
            }
        };

        let info = ErrorInfo::new(msg, span);
        _write_error(&info, stream, file_server)
    }
}

impl WriteColored for crate::parser::QuartzParserErrror {
    fn write_colored(
        &self,
        stream: &mut termcolor::StandardStreamLock,
        file_server: &langbox::FileServer,
    ) -> Result<()> {
        let info = ErrorInfo::new(self.message.as_ref(), self.span);
        _write_error(&info, stream, file_server)
    }
}

pub fn write_error(msg: &str, stream: &mut StandardStreamLock) -> Result<()> {
    use std::io::Write;

    writeln!(stream)?;
    write_styled!(stream, [Red; Bold], "error")?;
    writeln_styled!(stream, [White; Bold], ": {}", msg)?;

    stream.reset()
}
