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
            Self::IncompatibleType { expr, ty } => ErrorInfo::new(
                format!("operator '{}' is not defined on type `{}`", expr.op(), ty),
                expr.span(),
            ),
            Self::IncompatibleTypes {
                expr,
                lhs_ty,
                rhs_ty,
            } => ErrorInfo::new(
                format!(
                    "operator '{}' is not defined on types `{}` and `{}`",
                    expr.op(),
                    lhs_ty,
                    rhs_ty
                ),
                expr.span(),
            ),
            Self::InvalidCast { value_ty, expr } => ErrorInfo::new(
                format!("cannot cast `{}` to `{}`", value_ty, expr.target_ty()),
                expr.span(),
            ),
            Self::TypeNotConstructible { ty } => ErrorInfo::new(
                format!("the type `{}` cannot be constructed", ty),
                ty.span(),
            ),
            Self::UnknownField { ty, field } => ErrorInfo::new(
                format!("no field named `{}` in struct `{}`", field, ty),
                field.span(),
            ),
            Self::MissingField { ty, field } => ErrorInfo::new(
                format!("missing field `{}` for struct `{}`", field, ty),
                ty.span(),
            ),
            Self::IncompatibleFieldType {
                assign,
                field_ty,
                value_ty,
            } => ErrorInfo::new(
                format!(
                    "field `{}` is of type `{}` but found type `{}`",
                    assign.field(),
                    field_ty,
                    value_ty
                ),
                assign.span(),
            ),
            Self::InvalidPath { path } => ErrorInfo::new("invalid path", path.span()),
            Self::InvalidEnumIdent { name } => {
                ErrorInfo::new(format!("`{}` is not an enum", name), name.span())
            }
            Self::InvalidEnumVariant {
                enum_name,
                variant_name,
            } => ErrorInfo::new(
                format!(
                    "enum `{}` does not have a variant named `{}`",
                    enum_name, variant_name
                ),
                variant_name.span(),
            ),
            Self::UndefinedMember { ty, name } => ErrorInfo::new(
                format!("type `{}` does not have a member named `{}`", ty, name),
                name.span(),
            ),
            Self::InvalidIndexing { indexer, base_ty } => ErrorInfo::new(
                format!("value of type `{}` cannot be indexed", base_ty),
                indexer.span(),
            ),
            Self::InvalidRangeIndexing { indexer, base_ty } => ErrorInfo::new(
                format!("value of type `{}` cannot be range indexed", base_ty),
                indexer.span(),
            ),
            Self::InvalidArrayLength { ty, len } => ErrorInfo::new(
                format!(
                    "`{}` is not a valid array length (must be greater than zero)",
                    len
                ),
                ty.span(),
            ),
            Self::IndexOutOfRange {
                index_expr,
                index,
                len,
            } => ErrorInfo::new(
                format!(
                    "index `{}` is out of range for array of length {}",
                    index, len
                ),
                index_expr.span(),
            ),
            Self::InvalidIndexType {
                expr,
                expected_ty,
                value_ty,
            } => ErrorInfo::new(
                format!(
                    "expected index type `{}` but found type `{}`",
                    expected_ty, value_ty
                ),
                expr.span(),
            ),
            Self::ElseIfTypeMismatch {
                if_ty,
                else_if_ty,
                else_if_block,
            } => ErrorInfo::new(
                format!(
                    "if expression is of type `{}` but this branch is returning type `{}`",
                    if_ty, else_if_ty
                ),
                else_if_block.body().result().unwrap().span(),
            ),
            Self::ElseTypeMismatch {
                if_ty,
                else_ty,
                else_block,
            } => ErrorInfo::new(
                format!(
                    "if expression is of type `{}` but this branch is returning type `{}`",
                    if_ty, else_ty
                ),
                else_block.body().result().unwrap().span(),
            ),
            Self::InvalidConditionType { cond, cond_ty } => ErrorInfo::new(
                format!(
                    "expected expression of type `const int` or of type `bit` but found type `{}`",
                    cond_ty
                ),
                cond.span(),
            ),
            Self::UnsupportedDeclaration { decl } => ErrorInfo::new(
                "declarations are not supported in this context",
                decl.span(),
            ),
            Self::UnsupportedWhileLoop { while_loop } => ErrorInfo::new(
                "while loops are not supported in this context",
                while_loop.span(),
            ),
            Self::UnsupportedForLoop { for_loop } => ErrorInfo::new(
                "for loops are not supported in this context",
                for_loop.span(),
            ),
            Self::InvalidMatchType { value, value_ty } => ErrorInfo::new(
                format!("cannot match on expression of type `{}`", value_ty),
                value.span(),
            ),
            Self::IncompatiblePattern { pattern, value_ty } => ErrorInfo::new(
                format!("pattern is not valid for type `{}`", value_ty),
                pattern.span(),
            ),
            Self::PatternOutOfRange { pattern, value_ty } => ErrorInfo::new(
                format!("value out of range for type `{}`", value_ty),
                pattern.span(),
            ),
            Self::MatchBranchTypeMismatch {
                match_ty,
                branch_ty,
                branch,
            } => {
                use crate::ast::MatchBody;
                ErrorInfo::new(
                    format!(
                        "match expression is of type `{}` but this branch is returning type `{}`",
                        match_ty, branch_ty
                    ),
                    match branch.body() {
                        MatchBody::Expr(body_expr) => body_expr.span(),
                        MatchBody::Block(body) => body.result().unwrap().span(),
                    },
                )
            }
            Self::UnreachablePattern { pattern } => {
                ErrorInfo::new("pattern was already covered", pattern.span())
            }
            Self::InvalidAssignOp { assign } => ErrorInfo::new(
                format!(
                    "assignment with `{}` is not valid in this context",
                    assign.op()
                ),
                assign.op().span(),
            ),
            Self::InvalidSeqAssignSig { assign } => ErrorInfo::new(
                "cannot assign to signal in sequential context",
                assign.target().span().join(&assign.op().span()),
            ),
            Self::InvalidSeqAssignMod { assign } => ErrorInfo::new(
                "cannot assign to signal in sequential context",
                assign.target().span().join(&assign.op().span()),
            ),
            Self::InvalidCombAssignReg { assign } => ErrorInfo::new(
                "cannot assign to register in combinatoric context",
                assign.target().span().join(&assign.op().span()),
            ),
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
