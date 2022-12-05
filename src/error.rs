use crate::ast::*;
use crate::const_eval::*;
use crate::parser::QuartzParserError;
use crate::SharedString;
use langbox::TextSpan;
use std::borrow::Cow;

#[derive(Debug)]
pub enum QuartzError<'a> {
    DuplicateIdent {
        name: Ident,
    },
    InvalidConstExpr {
        expr: &'a Expr,
    },
    NonExhaustiveMatch {
        match_expr: &'a MatchExpr,
    },
    InvalidConstOp {
        op: Punct,
    },
    InvalidConstAssignTarget {
        target: &'a AssignTarget,
    },
    MissingReturnValue {
        block: &'a Block,
    },
    UnexpectedReturnValue {
        value: &'a Expr,
    },
    UndefinedIdent {
        name: Ident,
    },
    TargetNotAssignable {
        name: Ident,
    },
    ValueNotConst {
        name: Ident,
    },
    InvalidValueIdent {
        name: Ident,
    },
    InvalidFuncIdent {
        name: Ident,
    },
    MissingElseBlock {
        if_expr: &'a IfExpr,
    },
    ArgumentCountMismatch {
        call_expr: &'a CallExpr,
        arg_count: usize,
    },
    GenericCountMismatch {
        ty: &'a NamedType,
        arg_count: usize,
    },
    InvalidEnumBaseType {
        ty: &'a Type,
    },
    InvalidBitWidth {
        width: i64,
        arg: &'a GenericTypeArg,
    },
    UndefinedType {
        ty: &'a NamedType,
    },
    IncompatibleType {
        expr: &'a UnaryExpr,
        ty: Cow<'a, str>,
    },
    IncompatibleTypes {
        expr: &'a BinaryExpr,
        lhs_ty: Cow<'a, str>,
        rhs_ty: Cow<'a, str>,
    },
    InvalidCast {
        value_ty: Cow<'a, str>,
        expr: &'a CastExpr,
    },
    TypeNotConstructible {
        ty: &'a NamedType,
    },
    UnknownField {
        ty: &'a NamedType,
        field: &'a Ident,
    },
    MissingField {
        ty: &'a NamedType,
        field: SharedString,
    },
    IncompatibleFieldType {
        assign: &'a FieldAssign,
        field_ty: Cow<'a, str>,
        value_ty: Cow<'a, str>,
    },
    InvalidPath {
        path: &'a Path,
    },
    InvalidEnumIdent {
        name: Ident,
    },
    InvalidEnumVariant {
        enum_name: SharedString,
        variant_name: Ident,
    },
    UndefinedMember {
        ty: Cow<'a, str>,
        name: Ident,
    },
    InvalidIndexing {
        indexer: &'a Indexer,
        base_ty: Cow<'a, str>,
    },
    InvalidRangeIndexing {
        indexer: &'a Indexer,
        base_ty: Cow<'a, str>,
    },
    InvalidArrayLength {
        ty: &'a ArrayType,
        len: i64,
    },
    IndexOutOfRange {
        index_expr: &'a Expr,
        index: i64,
        len: u64,
    },
    InvalidIndexType {
        expr: &'a Expr,
        expected_ty: Cow<'a, str>,
        value_ty: Cow<'a, str>,
    },
    ElseIfTypeMismatch {
        if_ty: Cow<'a, str>,
        else_if_ty: Cow<'a, str>,
        else_if_block: &'a ElseIfBlock,
    },
    ElseTypeMismatch {
        if_ty: Cow<'a, str>,
        else_ty: Cow<'a, str>,
        else_block: &'a ElseBlock,
    },
    InvalidConditionType {
        cond: &'a Expr,
        cond_ty: Cow<'a, str>,
    },
    UnsupportedDeclaration {
        decl: &'a Declaration,
    },
    UnsupportedWhileLoop {
        while_loop: &'a WhileLoop,
    },
    UnsupportedForLoop {
        for_loop: &'a ForLoop,
    },
    InvalidMatchType {
        value: &'a Expr,
        value_ty: Cow<'a, str>,
    },
    IncompatiblePattern {
        pattern: &'a MatchPattern,
        value_ty: Cow<'a, str>,
    },
    PatternOutOfRange {
        pattern: &'a MatchPattern,
        value_ty: Cow<'a, str>,
    },
    MatchBranchTypeMismatch {
        match_ty: Cow<'a, str>,
        branch_ty: Cow<'a, str>,
        branch: &'a MatchBranch,
    },
    UnreachablePattern {
        pattern: &'a MatchPattern,
    },
    InvalidAssignOp {
        assign: &'a Assignment,
    },
    InvalidSeqAssignSig {
        assign: &'a Assignment,
    },
    InvalidSeqAssignMod {
        assign: &'a Assignment,
    },
    InvalidCombAssignReg {
        assign: &'a Assignment,
    },
    InvalidPortKind {
        port_span: TextSpan,
        port_dir: Direction,
        port_kind: LogicKind,
    },
    PortKindMismatch {
        port_span: TextSpan,
        port_ty: Cow<'a, str>,
    },
    PortModuleType {
        port_span: TextSpan,
    },
    MemberKindMismatch {
        member_span: TextSpan,
        member_ty: Cow<'a, str>,
    },
    StructModuleField {
        field_span: TextSpan,
    },
    IncompatibleAssignType {
        assign: &'a Assignment,
        target_ty: Cow<'a, str>,
        value_ty: Cow<'a, str>,
    },
    InvalidSensType {
        sens: &'a Sens,
        sens_ty: Cow<'a, str>,
    },
    ParseError(QuartzParserError),
    ArithmeticError(ArithmeticError),
    List(Vec<QuartzError<'a>>),
}

impl QuartzError<'_> {
    pub fn new_list(list: Vec<Self>) -> Self {
        debug_assert!(!list.is_empty());

        if list.len() == 1 {
            list.into_iter().next().unwrap()
        } else {
            Self::List(list)
        }
    }
}

impl From<QuartzParserError> for QuartzError<'_> {
    #[inline]
    fn from(value: QuartzParserError) -> Self {
        Self::ParseError(value)
    }
}

impl From<ArithmeticError> for QuartzError<'_> {
    #[inline]
    fn from(err: ArithmeticError) -> Self {
        Self::ArithmeticError(err)
    }
}

#[macro_export]
macro_rules! wrap_errors {
    ($value:expr, $errors:expr) => {
        if $errors.len() == 0 {
            Ok($value)
        } else {
            Err($crate::error::QuartzError::new_list($errors))
        }
    };
}

pub type QuartzResult<'a, T> = Result<T, QuartzError<'a>>;
