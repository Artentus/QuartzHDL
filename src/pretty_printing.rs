use langbox::{FileServer, TextSpan};
use std::borrow::Cow;
use std::io::Result;
use std::ops::Range;
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
    slice: Option<Range<usize>>,
}

impl<'a> ErrorInfo<'a> {
    #[inline]
    fn new(msg: impl Into<Cow<'a, str>>, span: TextSpan) -> Self {
        Self {
            msg: msg.into(),
            span,
            slice: None,
        }
    }

    #[inline]
    fn new_slice(msg: impl Into<Cow<'a, str>>, span: TextSpan, slice: Range<usize>) -> Self {
        Self {
            msg: msg.into(),
            span,
            slice: Some(slice),
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

    let (mut start_line, mut start_column) = info.span.start_pos().line_column(file_server);

    let mut end_line;
    let mut end_column;
    if let Some(slice) = &info.slice {
        let text = info.span.text(file_server);
        let mut chars = text.char_indices();

        while let Some((i, c)) = chars.next() {
            if i >= slice.start {
                break;
            }

            if c == '\n' {
                start_line += 1;
                start_column = 0;
            } else {
                start_column += 1;
            }
        }

        end_line = start_line;
        end_column = start_column;
        while let Some((i, c)) = chars.next() {
            if c == '\n' {
                end_line += 1;
                end_column = 0;
            } else {
                end_column += 1;
            }

            if i >= slice.end {
                break;
            }
        }
    } else {
        (end_line, end_column) = info.span.end_pos().line_column(file_server);
    }

    let (start_line, start_column) = (start_line as usize, start_column as usize);
    let (end_line, end_column) = (end_line as usize, end_column as usize);

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
        start_column + 1
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
                width = start_column
            )?;

            writeln_styled!(
                stream,
                [Red; Bold],
                "{:^^width$}",
                "",
                width = end_column - start_column
            )?;
        } else if i == start_line {
            write_styled!(
                stream,
                [White; Bold],
                "{:>width$}",
                "",
                width = start_column
            )?;

            writeln_styled!(
                stream,
                [Red; Bold],
                "{:^^width$}",
                "",
                width = line.chars().count() - start_column
            )?;
        } else if i == end_line {
            writeln_styled!(stream, [Red; Bold], "{:^^width$}", "", width = end_column)?;
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

impl WriteColored for crate::error::QuartzError<'_> {
    fn write_colored(
        &self,
        stream: &mut termcolor::StandardStreamLock,
        file_server: &langbox::FileServer,
    ) -> Result<()> {
        use crate::ast::Spanned;
        use std::io::Write;

        let info = match self {
            Self::DuplicateIdent { name } => {
                ErrorInfo::new(format!("`{name}` has already been defined"), name.span())
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
                format!("`{op}` operator is not valid in constant context"),
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
                ErrorInfo::new(format!("`{name}` is not defined"), name.span())
            }
            Self::TargetNotAssignable { name } => {
                ErrorInfo::new(format!("`{name}` cannot be assigned to"), name.span())
            }
            Self::ValueNotConst { name } => {
                ErrorInfo::new(format!("`{name}` is not a constant"), name.span())
            }
            Self::InvalidValueIdent { name } => {
                ErrorInfo::new(format!("`{name}` is not a value"), name.span())
            }
            Self::InvalidFuncIdent { name } => {
                ErrorInfo::new(format!("`{name}` is not a function"), name.span())
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
                format!("`{ty}` is not a valid base type for an enum"),
                ty.span(),
            ),
            Self::InvalidBitWidth { width, arg } => {
                ErrorInfo::new(format!("`{width}` is not a valid bit width"), arg.span())
            }
            Self::UndefinedType { ty } => {
                ErrorInfo::new(format!("`the type {ty}` is not defined"), ty.span())
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
                format!("the type `{ty}` cannot be constructed"),
                ty.span(),
            ),
            Self::UnknownField { ty, field } => ErrorInfo::new(
                format!("no field named `{field}` in struct `{ty}`"),
                field.span(),
            ),
            Self::MissingField { ty, field } => ErrorInfo::new(
                format!("missing field `{field}` for struct `{ty}`"),
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
                ErrorInfo::new(format!("`{name}` is not an enum"), name.span())
            }
            Self::InvalidEnumVariant {
                enum_name,
                variant_name,
            } => ErrorInfo::new(
                format!(
                    "enum `{enum_name}` does not have a variant named `{variant_name}`",
                ),
                variant_name.span(),
            ),
            Self::UndefinedMember { ty, name } => ErrorInfo::new(
                format!("type `{ty}` does not have a member named `{name}`"),
                name.span(),
            ),
            Self::InvalidIndexing { indexer, base_ty } => ErrorInfo::new(
                format!("value of type `{base_ty}` cannot be indexed"),
                indexer.span(),
            ),
            Self::InvalidRangeIndexing { indexer, base_ty } => ErrorInfo::new(
                format!("value of type `{base_ty}` cannot be range indexed"),
                indexer.span(),
            ),
            Self::InvalidArrayLength { ty, len } => ErrorInfo::new(
                format!(
                    "`{len}` is not a valid array length (must be greater than zero)",
                ),
                ty.span(),
            ),
            Self::IndexOutOfRange {
                index_expr,
                index,
                len,
            } => ErrorInfo::new(
                format!(
                    "index `{index}` is out of range for array of length {len}",
                ),
                index_expr.span(),
            ),
            Self::InvalidIndexType {
                expr,
                expected_ty,
                value_ty,
            } => ErrorInfo::new(
                format!(
                    "expected index type `{expected_ty}` but found type `{value_ty}`",
                ),
                expr.span(),
            ),
            Self::ElseIfTypeMismatch {
                if_ty,
                else_if_ty,
                else_if_block,
            } => ErrorInfo::new(
                format!(
                    "if expression is of type `{if_ty}` but this branch is returning type `{else_if_ty}`",
                ),
                else_if_block.body().result().unwrap().span(),
            ),
            Self::ElseTypeMismatch {
                if_ty,
                else_ty,
                else_block,
            } => ErrorInfo::new(
                format!(
                    "if expression is of type `{if_ty}` but this branch is returning type `{else_ty}`",
                ),
                else_block.body().result().unwrap().span(),
            ),
            Self::InvalidConditionType { cond, cond_ty } => ErrorInfo::new(
                format!(
                    "expected expression of type `const int` or of type `bit` but found type `{cond_ty}`",
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
                format!("cannot match on expression of type `{value_ty}`"),
                value.span(),
            ),
            Self::IncompatiblePattern { pattern, value_ty } => ErrorInfo::new(
                format!("pattern is not valid for type `{value_ty}`"),
                pattern.span(),
            ),
            Self::PatternOutOfRange { pattern, value_ty } => ErrorInfo::new(
                format!("value out of range for type `{value_ty}`"),
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
                        "match expression is of type `{match_ty}` but this branch is returning type `{branch_ty}`",
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
            Self::InvalidCombAssignIn { assign } => ErrorInfo::new(
                "cannot assign to input port",
                assign.target().span().join(&assign.op().span()),
            ),
            Self::InvalidCombAssignReg { assign } => ErrorInfo::new(
                "cannot assign to register in combinatoric context",
                assign.target().span().join(&assign.op().span()),
            ),
            Self::InvalidPortKind {
                port_span,
                port_dir,
                port_kind,
            } => {
                use crate::ast::Direction;
                let dir_str = match port_dir {
                    Direction::In => "input",
                    Direction::Out => "output",
                    Direction::InOut => "bi-directional",
                };

                use crate::ast::LogicKind;
                let kind_str = match port_kind {
                    LogicKind::Signal => "signals",
                    LogicKind::Register => "registers",
                    LogicKind::Module => "modules",
                };

                ErrorInfo::new(
                    format!("{dir_str} ports cannot be {kind_str}"),
                    *port_span,
                )
            }
            Self::PortKindMismatch { port_span, port_ty } => ErrorInfo::new(
                format!("type `{port_ty}` is not valid for this port"),
                *port_span,
            ),
            Self::PortModuleType { port_span } => {
                ErrorInfo::new("ports cannot contain module types", *port_span)
            }
            Self::MemberKindMismatch {
                member_span,
                member_ty,
            } => ErrorInfo::new(
                format!("type `{member_ty}` is not valid for this member"),
                *member_span,
            ),
            Self::StructModuleField { field_span } => {
                ErrorInfo::new("structs cannot contain module types", *field_span)
            }
            Self::IncompatibleAssignType {
                assign,
                target_ty,
                value_ty,
            } => ErrorInfo::new(
                format!(
                    "assignment target is of type `{target_ty}` but found type `{value_ty}`",
                ),
                assign.span(),
            ),
            Self::InvalidSensType { sens, sens_ty } => ErrorInfo::new(
                format!(
                    "sensitivities need to be of type `bit` but found type `{sens_ty}`",
                ),
                sens.sig().span(),
            ),
            Self::LoopControlOutsideOfLoop { kw } => {
                ErrorInfo::new(format!("`{kw}` statement outside of loop"), kw.span())
            }
            Self::NonConstLoopControl { kw } => ErrorInfo::new(
                format!("`{kw}` statement is not allowed in this context"),
                kw.span(),
            ),
            Self::LexerError(err) => {
                return err.write_colored(stream, file_server);
            }
            Self::ParseError(err) => {
                return err.write_colored(stream, file_server);
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

impl WriteColored for crate::lexer::QuartzLexerError {
    fn write_colored(
        &self,
        stream: &mut termcolor::StandardStreamLock,
        file_server: &langbox::FileServer,
    ) -> Result<()> {
        let info = match self {
            Self::OpenBlockComment { span } => ErrorInfo::new("open block comment", *span),
            Self::InvalidIdent { ident, span } => {
                ErrorInfo::new(format!("`{}` is not a valid identifier", ident), *span)
            }
            Self::InvalidLiteral { literal, span } => {
                ErrorInfo::new(format!("`{}` is not a valid literal", literal), *span)
            }
            Self::InvalidString {
                span,
                invalid_escape_offsets,
                is_open,
                ..
            } => {
                for offset in invalid_escape_offsets {
                    _write_error(
                        &ErrorInfo::new_slice(
                            "invalid escape sequence",
                            *span,
                            *offset..(*offset + 2),
                        ),
                        stream,
                        file_server,
                    )?;
                }

                if *is_open {
                    _write_error(
                        &ErrorInfo::new("open string literal", *span),
                        stream,
                        file_server,
                    )?;
                }

                return Ok(());
            }
            Self::InvalidChar { char, span } => {
                ErrorInfo::new(format!("invalid character `{}`", char), *span)
            }
        };
        _write_error(&info, stream, file_server)
    }
}

impl WriteColored for crate::parser::QuartzParserError {
    fn write_colored(
        &self,
        stream: &mut termcolor::StandardStreamLock,
        file_server: &langbox::FileServer,
    ) -> Result<()> {
        let info = ErrorInfo::new(self.message.as_ref(), self.span);
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

pub fn write_error(msg: &str, stream: &mut StandardStreamLock) -> Result<()> {
    use std::io::Write;

    writeln!(stream)?;
    write_styled!(stream, [Red; Bold], "error")?;
    writeln_styled!(stream, [White; Bold], ": {}", msg)?;

    stream.reset()
}
