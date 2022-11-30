use crate::ast::*;
use crate::const_eval::*;
use crate::ir::*;
use crate::range_collection::*;
use crate::{Clog2, HashMap, HashSet, SharedString};
use std::borrow::Cow;
use std::collections::hash_map;
use std::collections::VecDeque;
use topological_sort::TopologicalSort;

#[derive(Debug)]
pub enum TypecheckError<'a> {
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
    ArithmeticError(ArithmeticError),
    List(Vec<TypecheckError<'a>>),
}

impl TypecheckError<'_> {
    fn new_list(list: Vec<Self>) -> Self {
        debug_assert!(!list.is_empty());

        if list.len() == 1 {
            list.into_iter().next().unwrap()
        } else {
            Self::List(list)
        }
    }
}

impl From<ArithmeticError> for TypecheckError<'_> {
    #[inline]
    fn from(err: ArithmeticError) -> Self {
        Self::ArithmeticError(err)
    }
}

macro_rules! wrap_errors {
    ($value:expr, $errors:expr) => {
        if $errors.len() == 0 {
            Ok($value)
        } else {
            Err(TypecheckError::new_list($errors))
        }
    };
}

pub type TypecheckResult<'a, T> = Result<T, TypecheckError<'a>>;

pub struct Scope<'p> {
    parent: Option<&'p Scope<'p>>,
    consts: HashSet<SharedString>,
    funcs: HashMap<SharedString, usize>,
    vars: HashSet<SharedString>,
}

impl<'p> Scope<'p> {
    pub fn empty() -> Self {
        Self {
            parent: None,
            consts: HashSet::default(),
            funcs: HashMap::default(),
            vars: HashSet::default(),
        }
    }

    pub fn new<'pp: 'p>(parent: &'p Scope<'pp>) -> Self {
        Self {
            parent: Some(parent),
            consts: HashSet::default(),
            funcs: HashMap::default(),
            vars: HashSet::default(),
        }
    }

    pub fn add_const(&mut self, name: impl Into<SharedString>) {
        self.consts.insert(name.into());
    }

    pub fn add_func(&mut self, name: impl Into<SharedString>, arg_count: usize) {
        self.funcs.insert(name.into(), arg_count);
    }

    pub fn add_var(&mut self, name: impl Into<SharedString>) {
        self.vars.insert(name.into());
    }

    pub fn contains_const_exclusive(&self, name: impl AsRef<str>) -> bool {
        self.consts.contains(name.as_ref())
            || self
                .parent
                .map_or(false, |parent| parent.contains_const_exclusive(name))
    }

    pub fn contains_func_exclusive(&self, name: impl AsRef<str>) -> Option<usize> {
        self.funcs.get(name.as_ref()).copied().or_else(|| {
            self.parent
                .and_then(|parent| parent.contains_func_exclusive(name))
        })
    }

    pub fn contains_var_exclusive(&self, name: impl AsRef<str>) -> bool {
        self.vars.contains(name.as_ref())
            || self
                .parent
                .map_or(false, |parent| parent.contains_var_exclusive(name))
    }

    pub fn contains_func<'err>(&self, name: &Ident) -> TypecheckResult<'err, usize> {
        if let Some(arg_count) = self.contains_func_exclusive(name) {
            Ok(arg_count)
        } else if self.contains_const_exclusive(name) || self.contains_var_exclusive(name) {
            Err(TypecheckError::InvalidFuncIdent { name: name.clone() })
        } else {
            Err(TypecheckError::UndefinedIdent { name: name.clone() })
        }
    }

    pub fn contains_const<'err>(&self, name: &Ident) -> TypecheckResult<'err, ()> {
        if self.contains_const_exclusive(name) {
            Ok(())
        } else if self.contains_var_exclusive(name) {
            Err(TypecheckError::ValueNotConst { name: name.clone() })
        } else if self.contains_func_exclusive(name).is_some() {
            Err(TypecheckError::InvalidValueIdent { name: name.clone() })
        } else {
            Err(TypecheckError::UndefinedIdent { name: name.clone() })
        }
    }

    pub fn contains_var<'err>(&self, name: &Ident) -> TypecheckResult<'err, ()> {
        if self.contains_const_exclusive(name) || self.contains_var_exclusive(name) {
            Ok(())
        } else if self.contains_func_exclusive(name).is_some() {
            Err(TypecheckError::InvalidValueIdent { name: name.clone() })
        } else {
            Err(TypecheckError::UndefinedIdent { name: name.clone() })
        }
    }

    pub fn contains_mut_var<'err>(&self, name: &Ident) -> TypecheckResult<'err, ()> {
        if self.contains_var_exclusive(name) {
            Ok(())
        } else if self.contains_const_exclusive(name) {
            Err(TypecheckError::TargetNotAssignable { name: name.clone() })
        } else if self.contains_func_exclusive(name).is_some() {
            Err(TypecheckError::InvalidValueIdent { name: name.clone() })
        } else {
            Err(TypecheckError::UndefinedIdent { name: name.clone() })
        }
    }
}

pub fn check_for_duplicate_items<'a>(
    items: impl Iterator<Item = &'a Item>,
) -> TypecheckResult<'static, ()> {
    let mut errors = Vec::new();
    let mut set = HashSet::default();

    // Primitive types
    set.insert("bit".into());
    set.insert("bits".into());

    for item in items {
        if set.contains(item.name().as_ref()) {
            errors.push(TypecheckError::DuplicateIdent {
                name: item.name().clone(),
            })
        } else {
            set.insert(item.name().as_string());
        }
    }

    wrap_errors!((), errors)
}

fn transform_const_bin_expr<'a>(
    expr: &'a BinaryExpr,
    scope: &Scope,
) -> TypecheckResult<'a, ConstBinaryExpr> {
    let lhs = transform_const_expr(expr.lhs(), scope, false);
    let rhs = transform_const_expr(expr.rhs(), scope, false);

    match lhs {
        Ok(lhs) => match rhs {
            Ok(rhs) => Ok(ConstBinaryExpr::new(lhs, rhs, expr.span())),
            Err(err) => Err(err),
        },
        Err(err1) => match rhs {
            Ok(_) => Err(err1),
            Err(err2) => Err(TypecheckError::List([err1, err2].into())),
        },
    }
}

fn transform_const_call_expr<'a>(
    expr: &'a CallExpr,
    scope: &Scope,
) -> TypecheckResult<'a, ConstCallExpr> {
    let mut errors = Vec::new();

    match scope.contains_func(expr.func()) {
        Ok(arg_count) => {
            if arg_count != expr.args().len() {
                errors.push(TypecheckError::ArgumentCountMismatch {
                    call_expr: expr,
                    arg_count,
                })
            }
        }
        Err(err) => errors.push(err),
    }

    let mut args = Vec::with_capacity(expr.args().len());
    for arg in expr.args().iter() {
        match transform_const_expr(arg, scope, false) {
            Ok(arg) => args.push(arg),
            Err(err) => errors.push(err),
        }
    }

    wrap_errors!(
        ConstCallExpr::new(expr.func().clone(), args, expr.span()),
        errors
    )
}

fn transform_const_if_expr<'a>(
    expr: &'a IfExpr,
    scope: &Scope,
    needs_return: bool,
) -> TypecheckResult<'a, ConstIfExpr> {
    let mut errors = Vec::new();

    let condition = transform_const_expr(expr.condition(), scope, false);
    let body = transform_const_block(expr.body(), scope, needs_return);

    let condition = match condition {
        Ok(condition) => Some(condition),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let body = match body {
        Ok(body) => Some(body),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let mut else_if_blocks = Vec::with_capacity(expr.else_if_blocks().len());
    for else_if_block in expr.else_if_blocks().iter() {
        let condition = transform_const_expr(else_if_block.condition(), scope, false);
        let body = transform_const_block(else_if_block.body(), scope, needs_return);

        match condition {
            Ok(condition) => match body {
                Ok(body) => else_if_blocks.push(ConstElseIfBlock::new(condition, body)),
                Err(err2) => errors.push(err2),
            },
            Err(err1) => {
                errors.push(err1);

                if let Err(err2) = body {
                    errors.push(err2);
                }
            }
        }
    }

    let else_block = if let Some(else_block) = expr.else_block() {
        match transform_const_block(else_block.body(), scope, needs_return) {
            Ok(body) => Some(ConstElseBlock::new(body)),
            Err(err) => {
                errors.push(err);
                None
            }
        }
    } else {
        None
    };

    if needs_return && else_block.is_none() {
        errors.push(TypecheckError::MissingElseBlock { if_expr: expr })
    }

    if let Some(condition) = condition && let Some(body) = body {
        wrap_errors!(
            ConstIfExpr::new(condition, body, else_if_blocks, else_block, expr.span()),
            errors
        )
    } else {
        Err(TypecheckError::new_list(errors))
    }
}

fn has_exhaustive_patterns(expr: &MatchExpr) -> TypecheckResult<bool> {
    let mut errors = Vec::new();
    let full_range = InclusiveRange::n_bit(64);
    let mut covered_ranges = RangeCollection::new();

    for branch in expr.branches().iter() {
        for pattern in branch.patterns().iter() {
            match pattern {
                MatchPattern::Literal(literal) => {
                    if covered_ranges.contains(literal.value() as u64) {
                        errors.push(TypecheckError::UnreachablePattern { pattern });
                    } else {
                        covered_ranges.insert(literal.value() as u64);
                    }
                }
                MatchPattern::Range(start, end) => {
                    let range = (start.value() as u64)..(end.value() as u64);
                    if covered_ranges.contains(&range) {
                        errors.push(TypecheckError::UnreachablePattern { pattern });
                    } else {
                        covered_ranges.insert(range);
                    }
                }
                MatchPattern::RangeInclusive(start, end) => {
                    let range = (start.value() as u64)..=(end.value() as u64);
                    if covered_ranges.contains(&range) {
                        errors.push(TypecheckError::UnreachablePattern { pattern });
                    } else {
                        covered_ranges.insert(range);
                    }
                }
                MatchPattern::Path(path) => {
                    if let Some(ident) = path.as_ident() {
                        if ident.as_ref() == "_" {
                            if covered_ranges.contains(full_range) {
                                errors.push(TypecheckError::UnreachablePattern { pattern });
                            } else {
                                covered_ranges.insert(full_range);
                            }
                        } else {
                            // FIXME: get value of the constant to perform the range check on it
                        }
                    } else {
                        // Invalid pattern, cought later
                    }
                }
            }
        }
    }

    wrap_errors!(covered_ranges.contains(full_range), errors)
}

fn transform_const_match_pattern<'a>(
    pattern: &'a MatchPattern,
    scope: &Scope,
) -> TypecheckResult<'a, ConstMatchPattern> {
    match pattern {
        MatchPattern::Literal(l) => Ok(ConstMatchPattern::Literal(*l)),
        MatchPattern::Range(s, e) => Ok(ConstMatchPattern::Range(*s, *e)),
        MatchPattern::RangeInclusive(s, e) => Ok(ConstMatchPattern::RangeInclusive(*s, *e)),
        MatchPattern::Path(p) => {
            if let Some(ident) = p.as_ident() {
                if ident.as_ref() != "_" {
                    scope.contains_const(ident)?;
                }

                Ok(ConstMatchPattern::Ident(ident.clone()))
            } else {
                Err(TypecheckError::IncompatiblePattern {
                    pattern,
                    value_ty: "const int".into(),
                })
            }
        }
    }
}

fn transform_const_match_expr<'a>(
    expr: &'a MatchExpr,
    scope: &Scope,
    needs_return: bool,
) -> TypecheckResult<'a, ConstMatchExpr> {
    let mut errors = Vec::new();

    let value = transform_const_expr(expr.value(), scope, false);
    let value = match value {
        Ok(value) => Some(value),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    match has_exhaustive_patterns(expr) {
        Ok(true) => {}
        Ok(false) => errors.push(TypecheckError::NonExhaustiveMatch { match_expr: expr }),
        Err(err) => errors.push(err),
    }

    let mut branches = Vec::with_capacity(expr.branches().len());
    for branch in expr.branches().iter() {
        let mut patterns = Vec::with_capacity(branch.patterns().len());
        for pattern in branch.patterns().iter() {
            match transform_const_match_pattern(pattern, scope) {
                Ok(pattern) => patterns.push(pattern),
                Err(err) => errors.push(err),
            }
        }

        match branch.body() {
            MatchBody::Expr(expr) => {
                if !needs_return {
                    errors.push(TypecheckError::UnexpectedReturnValue { value: expr });
                }

                match transform_const_expr(expr, scope, false) {
                    Ok(expr) => {
                        branches.push(ConstMatchBranch::new(patterns, ConstMatchBody::Expr(expr)))
                    }
                    Err(err) => errors.push(err),
                }
            }
            MatchBody::Block(block) => match transform_const_block(block, scope, needs_return) {
                Ok(block) => branches.push(ConstMatchBranch::new(
                    patterns,
                    ConstMatchBody::Block(block),
                )),
                Err(err) => errors.push(err),
            },
        }
    }

    if let Some(value) = value {
        wrap_errors!(ConstMatchExpr::new(value, branches, expr.span()), errors)
    } else {
        Err(TypecheckError::new_list(errors))
    }
}

pub fn transform_const_expr<'a>(
    expr: &'a Expr,
    scope: &Scope,
    is_statement: bool,
) -> TypecheckResult<'a, ConstExpr> {
    macro_rules! bin_expr {
        ($expr:expr, $op:ident) => {
            Ok(ConstExpr::$op(transform_const_bin_expr($expr, scope)?))
        };
    }

    match expr {
        Expr::Literal(l) => Ok(ConstExpr::Literal(*l)),
        Expr::Path(p) => {
            if let Some(ident) = p.as_ident() {
                scope.contains_var(ident)?;
                Ok(ConstExpr::Ident(ident.clone()))
            } else {
                Err(TypecheckError::InvalidConstExpr { expr })
            }
        }
        Expr::Paren(expr) => transform_const_expr(expr.inner(), scope, is_statement),
        Expr::Call(expr) => Ok(ConstExpr::Call(transform_const_call_expr(expr, scope)?)),
        Expr::Construct(_) => Err(TypecheckError::InvalidConstExpr { expr }),
        Expr::If(expr) => Ok(ConstExpr::If(transform_const_if_expr(
            expr,
            scope,
            !is_statement,
        )?)),
        Expr::Match(expr) => Ok(ConstExpr::Match(transform_const_match_expr(
            expr,
            scope,
            !is_statement,
        )?)),
        Expr::Block(block) => Ok(ConstExpr::Block(Box::new(transform_const_block(
            block,
            scope,
            !is_statement,
        )?))),
        Expr::Index(_) => Err(TypecheckError::InvalidConstExpr { expr }),
        Expr::MemberAccess(_) => Err(TypecheckError::InvalidConstExpr { expr }),
        Expr::Pos(expr) => transform_const_expr(expr.inner(), scope, false),
        Expr::Neg(expr) => Ok(ConstExpr::Neg(ConstUnaryExpr::new(
            transform_const_expr(expr.inner(), scope, false)?,
            expr.span(),
        ))),
        Expr::Not(expr) => Ok(ConstExpr::Not(ConstUnaryExpr::new(
            transform_const_expr(expr.inner(), scope, false)?,
            expr.span(),
        ))),
        Expr::Cast(_) => Err(TypecheckError::InvalidConstExpr { expr }),
        Expr::Concat(_) => Err(TypecheckError::InvalidConstExpr { expr }),
        Expr::Lt(expr) => bin_expr!(expr, Lt),
        Expr::Lte(expr) => bin_expr!(expr, Lte),
        Expr::Gt(expr) => bin_expr!(expr, Gt),
        Expr::Gte(expr) => bin_expr!(expr, Gte),
        Expr::Slt(expr) => Err(TypecheckError::InvalidConstOp { op: *expr.op() }),
        Expr::Slte(expr) => Err(TypecheckError::InvalidConstOp { op: *expr.op() }),
        Expr::Sgt(expr) => Err(TypecheckError::InvalidConstOp { op: *expr.op() }),
        Expr::Sgte(expr) => Err(TypecheckError::InvalidConstOp { op: *expr.op() }),
        Expr::Eq(expr) => bin_expr!(expr, Eq),
        Expr::Ne(expr) => bin_expr!(expr, Ne),
        Expr::Add(expr) => bin_expr!(expr, Add),
        Expr::Sub(expr) => bin_expr!(expr, Sub),
        Expr::Mul(expr) => bin_expr!(expr, Mul),
        Expr::Div(expr) => bin_expr!(expr, Div),
        Expr::Rem(expr) => bin_expr!(expr, Rem),
        Expr::And(expr) => bin_expr!(expr, And),
        Expr::Xor(expr) => bin_expr!(expr, Xor),
        Expr::Or(expr) => bin_expr!(expr, Or),
        Expr::Shl(expr) => bin_expr!(expr, Shl),
        Expr::Lsr(expr) => bin_expr!(expr, Lsr),
        Expr::Asr(expr) => bin_expr!(expr, Asr),
    }
}

pub fn transform_generic_arg<'a>(
    arg: &'a GenericTypeArg,
    scope: &Scope,
) -> TypecheckResult<'a, ConstExpr> {
    match arg {
        GenericTypeArg::Literal(l) => Ok(ConstExpr::Literal(*l)),
        GenericTypeArg::Ident(i) => {
            scope.contains_var(i)?;
            Ok(ConstExpr::Ident(i.clone()))
        }
        GenericTypeArg::Expr(expr) => transform_const_expr(expr, scope, false),
    }
}

fn transform_const_assignment<'a>(
    assign: &'a Assignment,
    scope: &mut Scope,
) -> TypecheckResult<'a, ConstAssignment> {
    if let Some(target) = assign.target().path().as_ident()
        && assign.target().suffixes().is_empty()
    {
        let err1 = match scope.contains_mut_var(target) {
            Ok(_) => None,
            Err(err1) => Some(err1),
        };

        match transform_const_expr(assign.value(), scope, false) {
            Ok(value) => {
                if let Some(err1) = err1 {
                    Err(err1)
                } else {
                    Ok(ConstAssignment::new(
                        target.clone(),
                        assign.op().kind(),
                        value,
                        assign.span(),
                    ))
                }
            }
            Err(err2) => {
                if let Some(err1) = err1 {
                    Err(TypecheckError::List([err1, err2].into()))
                } else {
                    Err(err2)
                }
            }
        }
    } else {
        Err(TypecheckError::InvalidConstAssignTarget {
            target: assign.target(),
        })
    }
}

fn transform_const_while_loop<'a>(
    while_loop: &'a WhileLoop,
    scope: &Scope,
) -> TypecheckResult<'a, ConstWhileLoop> {
    let condition = transform_const_expr(while_loop.condition(), scope, false);
    let body = transform_const_block(while_loop.body(), scope, false);

    match condition {
        Ok(condition) => match body {
            Ok(body) => Ok(ConstWhileLoop::new(condition, body)),
            Err(err2) => Err(err2),
        },
        Err(err1) => match body {
            Ok(_) => Err(err1),
            Err(err2) => Err(TypecheckError::List([err1, err2].into())),
        },
    }
}

fn transform_const_for_loop<'a>(
    for_loop: &'a ForLoop,
    scope: &Scope,
) -> TypecheckResult<'a, ConstForLoop> {
    let mut errors = Vec::new();

    let (start, end, inclusive) = match for_loop.range() {
        ForLoopRange::Range(start, end) => (start, end, false),
        ForLoopRange::RangeInclusive(start, end) => (start, end, true),
    };

    let start = transform_const_expr(start, scope, false);
    let end = transform_const_expr(end, scope, false);

    let start = match start {
        Ok(start) => Some(start),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let end = match end {
        Ok(end) => Some(end),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let mut inner_scope = Scope::new(scope);
    inner_scope.add_var(for_loop.item_name());
    let body = transform_const_block(for_loop.body(), &inner_scope, false);

    let body = match body {
        Ok(body) => Some(body),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    wrap_errors!(
        ConstForLoop::new(
            for_loop.item_name().clone(),
            if inclusive {
                ConstForLoopRange::RangeInclusive(start.unwrap(), end.unwrap())
            } else {
                ConstForLoopRange::Range(start.unwrap(), end.unwrap())
            },
            body.unwrap()
        ),
        errors
    )
}

fn transform_const_statement<'a>(
    statement: &'a Statement,
    scope: &mut Scope,
) -> TypecheckResult<'a, ConstStatement> {
    match statement {
        Statement::Expr(expr) => Ok(ConstStatement::Expr(transform_const_expr(
            expr, scope, true,
        )?)),
        Statement::Declaration(decl) => {
            scope.add_var(decl.name());

            Ok(ConstStatement::Declaration(ConstDeclaration::new(
                decl.name().clone(),
                transform_const_expr(decl.value(), scope, false)?,
            )))
        }
        Statement::Assignment(assign) => Ok(ConstStatement::Assignment(
            transform_const_assignment(assign, scope)?,
        )),
        Statement::WhileLoop(while_loop) => Ok(ConstStatement::WhileLoop(
            transform_const_while_loop(while_loop, scope)?,
        )),
        Statement::ForLoop(for_loop) => Ok(ConstStatement::ForLoop(transform_const_for_loop(
            for_loop, scope,
        )?)),
    }
}

fn transform_const_block<'a>(
    block: &'a Block,
    parent_scope: &Scope,
    needs_return: bool,
) -> TypecheckResult<'a, ConstBlock> {
    let mut scope = Scope::new(parent_scope);

    let mut errors = Vec::new();
    let mut statements = Vec::with_capacity(block.statements().len());
    for statement in block.statements().iter() {
        match transform_const_statement(statement, &mut scope) {
            Ok(statement) => statements.push(statement),
            Err(err) => errors.push(err),
        }
    }

    let result = if let Some(result) = block.result() {
        if needs_return {
            match transform_const_expr(result, &scope, false) {
                Ok(result) => Some(result),
                Err(err) => {
                    errors.push(err);
                    None
                }
            }
        } else {
            errors.push(TypecheckError::UnexpectedReturnValue { value: result });
            None
        }
    } else {
        if needs_return {
            errors.push(TypecheckError::MissingReturnValue { block });
        }

        None
    };

    wrap_errors!(ConstBlock::new(statements, result, block.span()), errors)
}

pub fn transform_const_func<'a>(func: &'a Func, scope: &Scope) -> TypecheckResult<'a, ConstFunc> {
    let mut inner_scope = Scope::new(scope);
    for arg in func.args().iter() {
        inner_scope.add_var(arg);
    }

    let body = transform_const_block(func.body(), &inner_scope, true)?;
    Ok(ConstFunc::new(func.args().to_vec(), body))
}

pub fn collect_type_items(
    items: impl IntoIterator<Item = Item>,
) -> HashMap<SharedString, TypeItem> {
    let mut type_items = HashMap::default();
    for item in items.into_iter() {
        match item {
            Item::Struct(struct_item) => {
                type_items.insert(
                    struct_item.name().as_string(),
                    TypeItem::Struct(struct_item),
                );
            }
            Item::Enum(enum_item) => {
                type_items.insert(enum_item.name().as_string(), TypeItem::Enum(enum_item));
            }
            Item::Module(module_item) => {
                type_items.insert(
                    module_item.name().as_string(),
                    TypeItem::Module(module_item),
                );
            }
            _ => {}
        }
    }
    type_items
}

struct ResolveArgs<'a> {
    type_items: &'a HashMap<SharedString, TypeItem>,
    global_scope: &'a Scope<'a>,
    global_const_values: &'a HashMap<SharedString, i64>,
    funcs: &'a HashMap<SharedString, ConstFunc>,
}

impl<'a> ResolveArgs<'a> {
    fn to_local(&self, local_const_values: &'a HashMap<SharedString, i64>) -> ResolveLocalArgs<'a> {
        ResolveLocalArgs {
            type_items: self.type_items,
            global_const_values: self.global_const_values,
            local_const_values,
            funcs: self.funcs,
        }
    }
}

struct ResolveLocalArgs<'a> {
    type_items: &'a HashMap<SharedString, TypeItem>,
    global_const_values: &'a HashMap<SharedString, i64>,
    local_const_values: &'a HashMap<SharedString, i64>,
    funcs: &'a HashMap<SharedString, ConstFunc>,
}

struct TypeRegistry<'a> {
    known_types: &'a mut HashMap<TypeId, ResolvedType>,
    type_order: &'a mut TopologicalSort<TypeId>,
    type_queue: &'a mut VecDeque<TypeId>,
}

fn resolve_named_type<'a>(
    named_ty: &'a NamedType,
    type_items: &HashMap<SharedString, TypeItem>,
    scope: &mut Scope,
    global_const_values: &HashMap<SharedString, i64>,
    local_const_values: Option<&HashMap<SharedString, i64>>,
    funcs: &HashMap<SharedString, ConstFunc>,
    registry: &mut TypeRegistry,
) -> TypecheckResult<'a, TypeId> {
    let name = named_ty.name().as_ref();
    let generic_arg_count = named_ty.generic_arg_count();

    let resolved_ty = match name {
        "bit" => {
            if generic_arg_count == 0 {
                ResolvedType::BuiltinBits { width: 1 }
            } else {
                return Err(TypecheckError::GenericCountMismatch {
                    ty: named_ty,
                    arg_count: 0,
                });
            }
        }
        "bits" => {
            if generic_arg_count == 1 {
                let width_arg = &named_ty.generic_args().unwrap().args()[0];
                let width_expr = transform_generic_arg(width_arg, scope)?;

                let width = eval(
                    &width_expr,
                    &mut VarScope::empty(),
                    global_const_values,
                    local_const_values,
                    funcs,
                )?;

                if width <= 0 {
                    return Err(TypecheckError::InvalidBitWidth {
                        width,
                        arg: width_arg,
                    });
                }

                ResolvedType::BuiltinBits {
                    width: width as u64,
                }
            } else {
                return Err(TypecheckError::GenericCountMismatch {
                    ty: named_ty,
                    arg_count: 1,
                });
            }
        }
        _ => {
            if let Some(type_item) = type_items.get(name) {
                match type_item {
                    TypeItem::Struct(struct_item) => {
                        let struct_generic_count = struct_item.generic_arg_count();
                        if generic_arg_count != struct_generic_count {
                            return Err(TypecheckError::GenericCountMismatch {
                                ty: named_ty,
                                arg_count: struct_generic_count,
                            });
                        }
                    }
                    TypeItem::Enum(_) => {
                        if generic_arg_count > 0 {
                            return Err(TypecheckError::GenericCountMismatch {
                                ty: named_ty,
                                arg_count: 0,
                            });
                        }
                    }
                    TypeItem::Module(module_item) => {
                        let module_generic_count = module_item.generic_arg_count();
                        if generic_arg_count != module_generic_count {
                            return Err(TypecheckError::GenericCountMismatch {
                                ty: named_ty,
                                arg_count: module_generic_count,
                            });
                        }
                    }
                }
            } else {
                return Err(TypecheckError::UndefinedType { ty: named_ty });
            }

            let mut generic_vals = Vec::with_capacity(generic_arg_count);
            if let Some(generic_args) = named_ty.generic_args() {
                for arg in generic_args.args().iter() {
                    let arg_expr = transform_generic_arg(arg, scope)?;
                    let arg_val = eval(
                        &arg_expr,
                        &mut VarScope::empty(),
                        global_const_values,
                        local_const_values,
                        funcs,
                    )?;
                    generic_vals.push(arg_val);
                }
            }

            ResolvedType::Named {
                name: named_ty.name().as_string(),
                generic_args: generic_vals.into(),
            }
        }
    };

    let id = TypeId::from_type(&resolved_ty);
    if let hash_map::Entry::Vacant(entry) = registry.known_types.entry(id) {
        entry.insert(resolved_ty);
        registry.type_queue.push_back(id);
    }

    Ok(id)
}

fn resolve_type<'a>(
    ty: &'a Type,
    type_items: &HashMap<SharedString, TypeItem>,
    scope: &mut Scope,
    global_const_values: &HashMap<SharedString, i64>,
    local_const_values: Option<&HashMap<SharedString, i64>>,
    funcs: &HashMap<SharedString, ConstFunc>,
    registry: &mut TypeRegistry,
) -> TypecheckResult<'a, TypeId> {
    match ty {
        Type::Named(named_ty) => resolve_named_type(
            named_ty,
            type_items,
            scope,
            global_const_values,
            local_const_values,
            funcs,
            registry,
        ),
        Type::Array(array_ty) => {
            let item_ty = resolve_type(
                array_ty.item_ty(),
                type_items,
                scope,
                global_const_values,
                local_const_values,
                funcs,
                registry,
            )?;

            let len_expr = transform_const_expr(array_ty.len(), scope, false)?;
            let len = eval(
                &len_expr,
                &mut VarScope::empty(),
                global_const_values,
                local_const_values,
                funcs,
            )?;

            if len <= 0 {
                return Err(TypecheckError::InvalidArrayLength { ty: array_ty, len });
            }

            let resolved_ty = ResolvedType::Array {
                item_ty,
                len: len as u64,
            };
            let id = TypeId::from_type(&resolved_ty);
            if let hash_map::Entry::Vacant(entry) = registry.known_types.entry(id) {
                entry.insert(resolved_ty);
                registry.type_queue.push_back(id);
            }

            registry.type_order.add_dependency(item_ty, id);
            Ok(id)
        }
    }
}

fn check_for_duplicate_fields<'a>(
    generic_args: Option<&'a GenericStructArgs>,
    fields: impl Iterator<Item = &'a Field>,
) -> TypecheckResult<'static, ()> {
    let mut errors = Vec::new();
    let mut set = HashSet::default();

    if let Some(generic_args) = generic_args {
        for arg in generic_args.args().iter() {
            if set.contains(arg.as_ref()) {
                errors.push(TypecheckError::DuplicateIdent { name: arg.clone() })
            } else {
                set.insert(arg.as_string());
            }
        }
    }

    for field in fields {
        if set.contains(field.name().as_ref()) {
            errors.push(TypecheckError::DuplicateIdent {
                name: field.name().clone(),
            })
        } else {
            set.insert(field.name().as_string());
        }
    }

    wrap_errors!((), errors)
}

fn resolve_struct<'a>(
    struct_item: &'a Struct,
    generic_values: &[i64],
    args: &ResolveArgs,
    registry: &mut TypeRegistry,
) -> TypecheckResult<'a, ResolvedStruct> {
    debug_assert_eq!(
        struct_item
            .generic_args()
            .map(|args| args.args().len())
            .unwrap_or(0),
        generic_values.len()
    );

    let this_ty = ResolvedType::Named {
        name: struct_item.name().as_string(),
        generic_args: generic_values.into(),
    };
    let this_id = TypeId::from_type(&this_ty);

    let mut errors = Vec::new();
    if let Err(err) =
        check_for_duplicate_fields(struct_item.generic_args(), struct_item.fields().iter())
    {
        errors.push(err);
    }

    let mut local_consts = HashMap::default();
    let mut scope = Scope::new(args.global_scope);
    if let Some(args) = struct_item.generic_args() {
        for (arg, value) in args.args().iter().zip(generic_values.iter().copied()) {
            if !local_consts.contains_key(arg.as_ref()) {
                local_consts.insert(arg.as_string(), ConstExpr::Value(value));
                scope.add_const(arg);
            }
        }
    }

    let mut local_const_values = HashMap::default();
    for (name, expr) in local_consts.iter() {
        let result = eval(
            expr,
            &mut VarScope::empty(),
            args.global_const_values,
            Some(&local_consts),
            args.funcs,
        );

        match result {
            Ok(value) => {
                local_const_values.insert(SharedString::clone(name), value);
            }
            Err(err) => errors.push(err.into()),
        }
    }

    let mut fields = HashMap::default();
    for field in struct_item.fields().iter() {
        let result = resolve_type(
            field.ty(),
            args.type_items,
            &mut scope,
            args.global_const_values,
            Some(&local_const_values),
            args.funcs,
            registry,
        );

        match result {
            Ok(ty_id) => {
                registry.type_order.add_dependency(ty_id, this_id);
                fields.insert(field.name().as_string(), ty_id);
            }
            Err(err) => errors.push(err),
        }
    }

    wrap_errors!(ResolvedStruct::new(fields), errors)
}

fn resolve_enum<'a>(
    enum_item: &'a Enum,
    args: &ResolveArgs,
    registry: &mut TypeRegistry,
) -> TypecheckResult<'a, ResolvedEnum> {
    let this_ty = ResolvedType::Named {
        name: enum_item.name().as_string(),
        generic_args: [].as_slice().into(),
    };
    let this_id = TypeId::from_type(&this_ty);

    let mut scope = Scope::new(args.global_scope);
    let base_id = resolve_type(
        enum_item.base_ty(),
        args.type_items,
        &mut scope,
        args.global_const_values,
        None,
        args.funcs,
        registry,
    )?;

    let mut errors = Vec::new();
    let base_ty = &registry.known_types[&base_id];
    match base_ty {
        ResolvedType::BuiltinBits { .. } => {}
        _ => {
            errors.push(TypecheckError::InvalidEnumBaseType {
                ty: enum_item.base_ty(),
            });
        }
    }

    registry.type_order.add_dependency(base_id, this_id);

    let mut variants = HashMap::default();
    let mut next_variant_value = 0;
    for (name, expr) in enum_item.variants().iter().map(|v| (v.name(), v.value())) {
        if let Some(expr) = expr {
            match transform_const_expr(expr, &scope, false) {
                Ok(expr) => {
                    let result = eval::<_, i64>(
                        &expr,
                        &mut VarScope::empty(),
                        args.global_const_values,
                        None,
                        args.funcs,
                    );

                    match result {
                        Ok(value) => {
                            variants.insert(name.as_string(), value);
                            next_variant_value = value + 1;
                        }
                        Err(err) => errors.push(TypecheckError::ArithmeticError(err)),
                    }
                }
                Err(err) => errors.push(err),
            }
        } else {
            variants.insert(name.as_string(), next_variant_value);
            next_variant_value += 1;
        }
    }

    wrap_errors!(ResolvedEnum::new(base_id, variants), errors)
}

fn check_for_duplicate_members<'a>(
    generic_args: Option<&'a GenericStructArgs>,
    members: impl Iterator<Item = &'a Member>,
) -> TypecheckResult<'static, ()> {
    let mut errors = Vec::new();
    let mut set = HashSet::default();

    if let Some(generic_args) = generic_args {
        for arg in generic_args.args().iter() {
            if set.contains(arg.as_ref()) {
                errors.push(TypecheckError::DuplicateIdent { name: arg.clone() })
            } else {
                set.insert(arg.as_string());
            }
        }
    }

    for member in members {
        if let Some(name) = member.name() {
            if set.contains(name.as_ref()) {
                errors.push(TypecheckError::DuplicateIdent { name: name.clone() })
            } else {
                set.insert(name.as_string());
            }
        }
    }

    wrap_errors!((), errors)
}

fn resolve_expr<'a>(
    expr: &'a Expr,
    parent_id: TypeId,
    scope: &mut Scope,
    args: &ResolveLocalArgs,
    registry: &mut TypeRegistry,
) -> TypecheckResult<'a, ()> {
    macro_rules! resolve_binary_expr {
        ($expr:expr) => {{
            resolve_expr($expr.lhs(), parent_id, scope, args, registry)?;
            resolve_expr($expr.rhs(), parent_id, scope, args, registry)?;
        }};
    }

    match expr {
        Expr::Construct(expr) => {
            let ty_id = resolve_named_type(
                expr.ty(),
                args.type_items,
                scope,
                args.global_const_values,
                Some(args.local_const_values),
                args.funcs,
                registry,
            )?;

            expr.resolve(ty_id);
            registry.type_order.add_dependency(ty_id, parent_id);
        }
        Expr::Cast(expr) => {
            let ty_id = resolve_type(
                expr.target_ty(),
                args.type_items,
                scope,
                args.global_const_values,
                Some(args.local_const_values),
                args.funcs,
                registry,
            )?;

            expr.resolve(ty_id);
            registry.type_order.add_dependency(ty_id, parent_id);
        }

        Expr::Literal(_) => {}
        Expr::Path(_) => {}
        Expr::MemberAccess(_) => {}

        Expr::Call(expr) => {
            for arg in expr.args().iter() {
                resolve_expr(arg, parent_id, scope, args, registry)?;
            }
        }

        Expr::If(expr) => {
            resolve_expr(expr.condition(), parent_id, scope, args, registry)?;
            resolve_block(expr.body(), parent_id, scope, args, registry)?;

            for else_if_block in expr.else_if_blocks().iter() {
                resolve_expr(else_if_block.condition(), parent_id, scope, args, registry)?;
                resolve_block(else_if_block.body(), parent_id, scope, args, registry)?;
            }

            if let Some(else_block) = expr.else_block() {
                resolve_block(else_block.body(), parent_id, scope, args, registry)?;
            }
        }

        Expr::Match(expr) => {
            resolve_expr(expr.value(), parent_id, scope, args, registry)?;
            for branch in expr.branches().iter() {
                match branch.body() {
                    MatchBody::Expr(expr) => resolve_expr(expr, parent_id, scope, args, registry)?,
                    MatchBody::Block(block) => {
                        resolve_block(block, parent_id, scope, args, registry)?
                    }
                }
            }
        }

        Expr::Index(expr) => {
            resolve_expr(expr.base(), parent_id, scope, args, registry)?;
            match expr.indexer().index() {
                IndexKind::Single(index) => resolve_expr(index, parent_id, scope, args, registry)?,
                IndexKind::Range(range) => {
                    resolve_expr(&range.start, parent_id, scope, args, registry)?;
                    resolve_expr(&range.end, parent_id, scope, args, registry)?;
                }
            }
        }

        Expr::Paren(expr) => resolve_expr(expr.inner(), parent_id, scope, args, registry)?,
        Expr::Block(block) => resolve_block(block, parent_id, scope, args, registry)?,
        Expr::Pos(expr) => resolve_expr(expr.inner(), parent_id, scope, args, registry)?,
        Expr::Neg(expr) => resolve_expr(expr.inner(), parent_id, scope, args, registry)?,
        Expr::Not(expr) => resolve_expr(expr.inner(), parent_id, scope, args, registry)?,
        Expr::Concat(expr) => resolve_binary_expr!(expr),
        Expr::Lt(expr) => resolve_binary_expr!(expr),
        Expr::Lte(expr) => resolve_binary_expr!(expr),
        Expr::Gt(expr) => resolve_binary_expr!(expr),
        Expr::Gte(expr) => resolve_binary_expr!(expr),
        Expr::Slt(expr) => resolve_binary_expr!(expr),
        Expr::Slte(expr) => resolve_binary_expr!(expr),
        Expr::Sgt(expr) => resolve_binary_expr!(expr),
        Expr::Sgte(expr) => resolve_binary_expr!(expr),
        Expr::Eq(expr) => resolve_binary_expr!(expr),
        Expr::Ne(expr) => resolve_binary_expr!(expr),
        Expr::Add(expr) => resolve_binary_expr!(expr),
        Expr::Sub(expr) => resolve_binary_expr!(expr),
        Expr::Mul(expr) => resolve_binary_expr!(expr),
        Expr::Div(expr) => resolve_binary_expr!(expr),
        Expr::Rem(expr) => resolve_binary_expr!(expr),
        Expr::And(expr) => resolve_binary_expr!(expr),
        Expr::Xor(expr) => resolve_binary_expr!(expr),
        Expr::Or(expr) => resolve_binary_expr!(expr),
        Expr::Shl(expr) => resolve_binary_expr!(expr),
        Expr::Lsr(expr) => resolve_binary_expr!(expr),
        Expr::Asr(expr) => resolve_binary_expr!(expr),
    }

    Ok(())
}

fn resolve_statement<'a>(
    statement: &'a Statement,
    parent_id: TypeId,
    scope: &mut Scope,
    args: &ResolveLocalArgs,
    registry: &mut TypeRegistry,
) -> TypecheckResult<'a, ()> {
    match statement {
        Statement::Expr(expr) => resolve_expr(expr, parent_id, scope, args, registry),
        Statement::Declaration(decl) => {
            resolve_expr(decl.value(), parent_id, scope, args, registry)
        }
        Statement::Assignment(assign) => {
            resolve_expr(assign.value(), parent_id, scope, args, registry)
        }
        Statement::WhileLoop(while_loop) => {
            resolve_expr(while_loop.condition(), parent_id, scope, args, registry)?;
            resolve_block(while_loop.body(), parent_id, scope, args, registry)
        }
        Statement::ForLoop(for_loop) => {
            let (start, end) = match for_loop.range() {
                ForLoopRange::Range(start, end) => (start, end),
                ForLoopRange::RangeInclusive(start, end) => (start, end),
            };

            resolve_expr(start, parent_id, scope, args, registry)?;
            resolve_expr(end, parent_id, scope, args, registry)?;
            resolve_block(for_loop.body(), parent_id, scope, args, registry)
        }
    }
}

fn resolve_block<'a>(
    block: &'a Block,
    parent_id: TypeId,
    parent_scope: &Scope,
    args: &ResolveLocalArgs,
    registry: &mut TypeRegistry,
) -> TypecheckResult<'a, ()> {
    let mut scope = Scope::new(parent_scope);

    for statement in block.statements().iter() {
        resolve_statement(statement, parent_id, &mut scope, args, registry)?;
    }

    if let Some(result) = block.result() {
        resolve_expr(result, parent_id, &mut scope, args, registry)?;
    }

    Ok(())
}

fn resolve_module<'a>(
    module_item: &'a Module,
    generic_values: &[i64],
    args: &ResolveArgs,
    registry: &mut TypeRegistry,
) -> TypecheckResult<'a, ResolvedModule> {
    debug_assert_eq!(
        module_item
            .generic_args()
            .map(|args| args.args().len())
            .unwrap_or(0),
        generic_values.len()
    );

    let this_ty = ResolvedType::Named {
        name: module_item.name().as_string(),
        generic_args: generic_values.into(),
    };
    let this_id = TypeId::from_type(&this_ty);

    let mut errors = Vec::new();
    if let Err(err) =
        check_for_duplicate_members(module_item.generic_args(), module_item.members().iter())
    {
        errors.push(err);
    }

    let mut local_consts = HashMap::default();
    let mut scope = Scope::new(args.global_scope);
    if let Some(args) = module_item.generic_args() {
        for (arg, value) in args.args().iter().zip(generic_values.iter().copied()) {
            if !local_consts.contains_key(arg.as_ref()) {
                local_consts.insert(arg.as_string(), ConstExpr::Value(value));
                scope.add_const(arg);
            }
        }
    }

    for member in module_item.members().iter() {
        if let Member::Const(const_member) = member {
            if !local_consts.contains_key(const_member.name().as_ref()) {
                let const_expr = transform_const_expr(const_member.value(), &scope, false)?;
                local_consts.insert(const_member.name().as_string(), const_expr);
                scope.add_const(const_member.name());
            }
        }
    }

    let mut local_const_values = HashMap::default();
    for (name, expr) in local_consts.iter() {
        let result = eval(
            expr,
            &mut VarScope::empty(),
            args.global_const_values,
            Some(&local_consts),
            args.funcs,
        );

        match result {
            Ok(value) => {
                local_const_values.insert(SharedString::clone(name), value);
            }
            Err(err) => errors.push(err.into()),
        }
    }

    let mut ports = HashMap::default();
    for port in module_item.ports().iter() {
        let result = resolve_type(
            port.ty(),
            args.type_items,
            &mut scope,
            args.global_const_values,
            Some(&local_const_values),
            args.funcs,
            registry,
        );

        match result {
            Ok(ty_id) => {
                registry.type_order.add_dependency(ty_id, this_id);
                ports.insert(
                    port.name().as_string(),
                    ResolvedPort::new(port.mode().dir(), port.logic_mode().kind(), ty_id),
                );
            }
            Err(err) => errors.push(err),
        }
    }

    let mut logic_members = HashMap::default();
    let mut proc_members = Vec::new();
    let mut comb_members = Vec::new();
    for member in module_item.members().iter() {
        match member {
            Member::Logic(logic_member) => {
                let result = resolve_type(
                    logic_member.ty(),
                    args.type_items,
                    &mut scope,
                    args.global_const_values,
                    Some(&local_const_values),
                    args.funcs,
                    registry,
                );

                match result {
                    Ok(ty_id) => {
                        registry.type_order.add_dependency(ty_id, this_id);
                        logic_members.insert(
                            logic_member.name().as_string(),
                            ResolvedLogicMember::new(logic_member.mode().kind(), ty_id),
                        );
                    }
                    Err(err) => errors.push(err),
                }
            }
            Member::Proc(proc_member) => {
                let result = resolve_block(
                    proc_member.body(),
                    this_id,
                    &scope,
                    &args.to_local(&local_const_values),
                    registry,
                );

                if let Err(err) = result {
                    errors.push(err);
                } else {
                    proc_members.push(proc_member.clone());
                }
            }
            Member::Comb(comb_member) => {
                let result = resolve_block(
                    comb_member.body(),
                    this_id,
                    &scope,
                    &args.to_local(&local_const_values),
                    registry,
                );

                if let Err(err) = result {
                    errors.push(err);
                } else {
                    comb_members.push(comb_member.clone());
                }
            }
            _ => {}
        }
    }

    wrap_errors!(
        ResolvedModule::new(
            ports,
            local_const_values,
            logic_members,
            proc_members,
            comb_members
        ),
        errors
    )
}

pub type ResolvedTypes = (
    HashMap<TypeId, ResolvedType>,
    HashMap<TypeId, ResolvedTypeItem>,
    TopologicalSort<TypeId>,
);

pub fn resolve_types<'a>(
    top_module: &'a Module,
    type_items: &'a HashMap<SharedString, TypeItem>,
    global_scope: &Scope,
    global_const_values: &HashMap<SharedString, i64>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> TypecheckResult<'a, ResolvedTypes> {
    let mut known_types = HashMap::default();
    let mut type_order = TopologicalSort::new();
    let mut type_queue = VecDeque::new();

    known_types.insert(*CONST_TYPE_ID, ResolvedType::Const);

    let top_ty = ResolvedType::Named {
        name: top_module.name().as_string(),
        generic_args: [].as_slice().into(),
    };
    let top_id = TypeId::from_type(&top_ty);
    known_types.insert(top_id, top_ty);
    type_queue.push_back(top_id);

    let mut errors = Vec::new();
    let mut resolved_type_items = HashMap::default();
    while let Some(ty_id) = type_queue.pop_front() {
        let ty = known_types[&ty_id].clone();

        if let ResolvedType::Named { name, generic_args } = ty {
            if let Some(item) = type_items.get(name.as_ref()) {
                let args = ResolveArgs {
                    type_items,
                    global_scope,
                    global_const_values,
                    funcs,
                };

                let mut registry = TypeRegistry {
                    known_types: &mut known_types,
                    type_order: &mut type_order,
                    type_queue: &mut type_queue,
                };

                match item {
                    TypeItem::Struct(struct_item) => {
                        let result =
                            resolve_struct(struct_item, &generic_args, &args, &mut registry);

                        match result {
                            Ok(resolved_struct) => {
                                resolved_type_items
                                    .insert(ty_id, ResolvedTypeItem::Struct(resolved_struct));
                            }
                            Err(err) => errors.push(err),
                        }
                    }
                    TypeItem::Enum(enum_item) => {
                        let result = resolve_enum(enum_item, &args, &mut registry);

                        match result {
                            Ok(resolved_enum) => {
                                resolved_type_items
                                    .insert(ty_id, ResolvedTypeItem::Enum(resolved_enum));
                            }
                            Err(err) => errors.push(err),
                        }
                    }
                    TypeItem::Module(module_item) => {
                        let result =
                            resolve_module(module_item, &generic_args, &args, &mut registry);

                        match result {
                            Ok(resolved_module) => {
                                resolved_type_items
                                    .insert(ty_id, ResolvedTypeItem::Module(resolved_module));
                            }
                            Err(err) => errors.push(err),
                        }
                    }
                }
            } else {
                unreachable!("invalid resolved type");
            }
        }
    }

    wrap_errors!((known_types, resolved_type_items, type_order), errors)
}

fn resolve_type_late(
    ty: &UnresolvedType,
    known_types: &mut HashMap<TypeId, ResolvedType>,
) -> TypeId {
    match ty {
        UnresolvedType::BuiltinBits { width } => {
            let resolved_ty = ResolvedType::BuiltinBits { width: *width };
            let id = TypeId::from_type(&resolved_ty);

            if let hash_map::Entry::Vacant(entry) = known_types.entry(id) {
                entry.insert(resolved_ty);
            }

            id
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypecheckMode {
    Sequential,
    Combinatoric,
}

struct TypecheckArgs<'a> {
    global_const_values: &'a HashMap<SharedString, i64>,
    local_const_values: &'a HashMap<SharedString, i64>,
    funcs: &'a HashMap<SharedString, ConstFunc>,
}

enum Either<A, B> {
    Const(A),
    Checked(B),
}

impl<A, B> Either<A, B> {
    fn map<A1, B1>(self, fa: impl FnOnce(A) -> A1, fb: impl FnOnce(B) -> B1) -> Either<A1, B1> {
        match self {
            Self::Const(a) => Either::Const(fa(a)),
            Self::Checked(b) => Either::Checked(fb(b)),
        }
    }
}

impl<A, B: Typed> Either<A, B> {
    fn ty_string(&self, known_types: &HashMap<TypeId, ResolvedType>) -> Cow<'static, str> {
        let ty_id = match self {
            Self::Const(_) => *CONST_TYPE_ID,
            Self::Checked(expr) => expr.ty(),
        };

        known_types[&ty_id].to_string(known_types)
    }
}

fn typecheck_unary_expr<'a>(
    expr: &'a UnaryExpr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, Either<ConstUnaryExpr, CheckedUnaryExpr>> {
    let inner = typecheck_expr(
        expr.inner(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    )?;
    match inner {
        Either::Const(inner) => Ok(Either::Const(ConstUnaryExpr::new(inner, expr.span()))),
        Either::Checked(inner) => {
            let inner_id = inner.ty();
            let inner_ty = &known_types[&inner_id];
            match inner_ty {
                ResolvedType::Const => unreachable!("error in constant folding"),
                ResolvedType::BuiltinBits { .. } => {
                    Ok(Either::Checked(CheckedUnaryExpr::new(inner)))
                }
                _ => Err(TypecheckError::IncompatibleType {
                    expr,
                    ty: inner_ty.to_string(known_types),
                }),
            }
        }
    }
}

fn typecheck_binary_expr<'a>(
    expr: &'a BinaryExpr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, Either<ConstBinaryExpr, CheckedBinaryExpr>> {
    let mut errors = Vec::new();

    let lhs = match typecheck_expr(
        expr.lhs(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(lhs) => Some(lhs),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let rhs = match typecheck_expr(
        expr.rhs(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(rhs) => Some(rhs),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    if !errors.is_empty() {
        return Err(TypecheckError::new_list(errors));
    }

    let lhs = lhs.unwrap();
    let rhs = rhs.unwrap();

    let (lhs, rhs) = match (lhs, rhs) {
        (Either::Const(lhs), Either::Const(rhs)) => {
            return Ok(Either::Const(ConstBinaryExpr::new(lhs, rhs, expr.span())));
        }
        (Either::Checked(lhs), Either::Checked(rhs)) => {
            let lhs_id = lhs.ty();
            let rhs_id = rhs.ty();

            if lhs_id == rhs_id {
                match &known_types[&lhs_id] {
                    ResolvedType::Const => unreachable!("error in constant folding"),
                    ResolvedType::BuiltinBits { .. } => {
                        return Ok(Either::Checked(CheckedBinaryExpr::new(lhs, rhs, lhs_id)));
                    }
                    _ => {}
                }
            }

            (Either::Checked(lhs), Either::Checked(rhs))
        }
        (lhs, rhs) => (lhs, rhs),
    };

    Err(TypecheckError::IncompatibleTypes {
        expr,
        lhs_ty: lhs.ty_string(known_types),
        rhs_ty: rhs.ty_string(known_types),
    })
}

fn typecheck_compare_expr<'a>(
    expr: &'a BinaryExpr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, Either<ConstBinaryExpr, CheckedBinaryExpr>> {
    let mut errors = Vec::new();

    let lhs = match typecheck_expr(
        expr.lhs(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(lhs) => Some(lhs),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let rhs = match typecheck_expr(
        expr.rhs(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(rhs) => Some(rhs),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    if !errors.is_empty() {
        return Err(TypecheckError::new_list(errors));
    }

    let lhs = lhs.unwrap();
    let rhs = rhs.unwrap();

    let (lhs, rhs) = match (lhs, rhs) {
        (Either::Const(lhs), Either::Const(rhs)) => {
            return Ok(Either::Const(ConstBinaryExpr::new(lhs, rhs, expr.span())));
        }
        (Either::Checked(lhs), Either::Checked(rhs)) => {
            let lhs_id = lhs.ty();
            let rhs_id = rhs.ty();

            if lhs_id == rhs_id {
                match &known_types[&lhs_id] {
                    ResolvedType::Const => unreachable!("error in constant folding"),
                    ResolvedType::BuiltinBits { .. } => {
                        // Returns a single bit (boolean)
                        let result_ty = UnresolvedType::BuiltinBits { width: 1 };
                        let result_id = resolve_type_late(&result_ty, known_types);
                        return Ok(Either::Checked(CheckedBinaryExpr::new(lhs, rhs, result_id)));
                    }
                    ResolvedType::Named { .. } => {
                        if let ResolvedTypeItem::Enum(_) = &resolved_types[&lhs_id] {
                            // Returns a single bit (boolean)
                            let result_ty = UnresolvedType::BuiltinBits { width: 1 };
                            let result_id = resolve_type_late(&result_ty, known_types);
                            return Ok(Either::Checked(CheckedBinaryExpr::new(
                                lhs, rhs, result_id,
                            )));
                        }
                    }
                    _ => {}
                }
            }

            (Either::Checked(lhs), Either::Checked(rhs))
        }
        (lhs, rhs) => (lhs, rhs),
    };

    Err(TypecheckError::IncompatibleTypes {
        expr,
        lhs_ty: lhs.ty_string(known_types),
        rhs_ty: rhs.ty_string(known_types),
    })
}

fn typecheck_concat_expr<'a>(
    expr: &'a BinaryExpr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedConcatExpr> {
    let mut errors = Vec::new();

    let lhs = match typecheck_expr(
        expr.lhs(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(lhs) => Some(lhs),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let rhs = match typecheck_expr(
        expr.rhs(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(rhs) => Some(rhs),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    if !errors.is_empty() {
        return Err(TypecheckError::new_list(errors));
    }

    let lhs = lhs.unwrap();
    let rhs = rhs.unwrap();

    let (lhs, rhs) = match (lhs, rhs) {
        (Either::Checked(lhs), Either::Checked(rhs)) => {
            let lhs_id = lhs.ty();
            let rhs_id = rhs.ty();

            let lhs_ty = &known_types[&lhs_id];
            let rhs_ty = &known_types[&rhs_id];

            if let (
                ResolvedType::BuiltinBits { width: lhs_width },
                ResolvedType::BuiltinBits { width: rhs_width },
            ) = (lhs_ty, rhs_ty)
            {
                let result_ty = UnresolvedType::BuiltinBits {
                    width: lhs_width + rhs_width,
                };
                let result_id = resolve_type_late(&result_ty, known_types);
                return Ok(CheckedConcatExpr::new(lhs, rhs, result_id));
            }

            (Either::Checked(lhs), Either::Checked(rhs))
        }
        (lhs, rhs) => (lhs, rhs),
    };

    Err(TypecheckError::IncompatibleTypes {
        expr,
        lhs_ty: lhs.ty_string(known_types),
        rhs_ty: rhs.ty_string(known_types),
    })
}

fn typecheck_cast_expr<'a>(
    expr: &'a CastExpr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedCastExpr> {
    let value = typecheck_expr(
        expr.value(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    )?;

    let target_id = expr.resolved_ty().expect("type not properly resolved");
    let target_ty = &known_types[&target_id];

    let value = match target_ty {
        ResolvedType::Const => unreachable!("type cannot be declared"),
        ResolvedType::BuiltinBits { .. } => match value {
            Either::Const(value) => {
                let value = eval(
                    &value,
                    &mut VarScope::empty(),
                    args.global_const_values,
                    Some(args.local_const_values),
                    args.funcs,
                )?;

                return Ok(CheckedCastExpr::new(CheckedExpr::Value(value), target_id));
            }
            Either::Checked(value) => {
                let value_ty = &known_types[&value.ty()];
                match value_ty {
                    ResolvedType::Const => unreachable!("error in constant folding"),
                    ResolvedType::BuiltinBits { .. } => {
                        return Ok(CheckedCastExpr::new(value, target_id));
                    }
                    ResolvedType::Named { .. } => {
                        let value_ty_item = &resolved_types[&value.ty()];
                        if let ResolvedTypeItem::Enum(_) = value_ty_item {
                            // Enums can be converted into any bits<N>, but not the other way around
                            return Ok(CheckedCastExpr::new(value, target_id));
                        }
                    }
                    _ => {}
                }

                Either::Checked(value)
            }
        },
        _ => value,
    };

    Err(TypecheckError::InvalidCast {
        value_ty: value.ty_string(known_types),
        expr,
    })
}

fn merge_expr<'a>(
    expr: Either<ConstExpr, CheckedExpr>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedExpr> {
    match expr {
        Either::Const(expr) => {
            let value = eval(
                &expr,
                &mut VarScope::empty(),
                args.global_const_values,
                Some(args.local_const_values),
                args.funcs,
            )?;
            Ok(CheckedExpr::Value(value))
        }
        Either::Checked(expr) => Ok(expr),
    }
}

fn typecheck_construct_expr<'a>(
    expr: &'a ConstructExpr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedConstructExpr> {
    let id = expr.resolved_ty().expect("type not properly resolved");
    let ty = &known_types[&id];

    match ty {
        ResolvedType::Named { .. } => {
            let ty_item = &resolved_types[&id];
            match ty_item {
                ResolvedTypeItem::Struct(struct_item) => {
                    let mut errors = Vec::new();
                    for field in expr.fields().iter().map(|f| f.field()) {
                        if !struct_item.fields().contains_key(field.as_ref()) {
                            errors.push(TypecheckError::UnknownField {
                                ty: expr.ty(),
                                field,
                            });
                        }
                    }

                    let mut fields = HashMap::default();
                    for (name, &field_id) in struct_item.fields().iter() {
                        let assign = expr
                            .fields()
                            .iter()
                            .find(|f| f.field().as_ref() == name.as_ref());

                        match assign {
                            Some(assign) => {
                                let value = typecheck_expr(
                                    assign.value(),
                                    parent_module,
                                    mode,
                                    scope,
                                    known_types,
                                    resolved_types,
                                    args,
                                );

                                match value {
                                    Ok(value) => {
                                        let value = merge_expr(value, args);
                                        match value {
                                            Ok(value) => {
                                                if value.ty() == field_id {
                                                    fields.insert(SharedString::clone(name), value);
                                                } else {
                                                    let field_ty = &known_types[&field_id];
                                                    let value_ty = &known_types[&value.ty()];
                                                    errors.push(
                                                        TypecheckError::IncompatibleFieldType {
                                                            assign,
                                                            field_ty: field_ty
                                                                .to_string(known_types),
                                                            value_ty: value_ty
                                                                .to_string(known_types),
                                                        },
                                                    );
                                                }
                                            }
                                            Err(err) => errors.push(err),
                                        }
                                    }
                                    Err(err) => errors.push(err),
                                }
                            }
                            None => {
                                errors.push(TypecheckError::MissingField {
                                    ty: expr.ty(),
                                    field: SharedString::clone(name),
                                });
                            }
                        }
                    }

                    wrap_errors!(CheckedConstructExpr::new(id, fields), errors)
                }
                _ => Err(TypecheckError::TypeNotConstructible { ty: expr.ty() }),
            }
        }
        _ => unreachable!("type incorrectly resolved"),
    }
}

fn find_module_member_type<'a>(
    ident: &'a Ident,
    module: &ResolvedModule,
    module_id: Option<TypeId>,
    known_types: &HashMap<TypeId, ResolvedType>,
) -> TypecheckResult<'a, (TypeId, LogicKind)> {
    if let Some(port) = module.ports().get(ident.as_ref()) {
        Ok((port.ty(), port.kind()))
    } else if let Some(member) = module.logic_members().get(ident.as_ref()) {
        Ok((member.ty(), member.kind()))
    } else if let Some(module_id) = module_id {
        Err(TypecheckError::UndefinedMember {
            ty: known_types[&module_id].to_string(known_types),
            name: ident.clone(),
        })
    } else {
        Err(TypecheckError::UndefinedIdent {
            name: ident.clone(),
        })
    }
}

fn find_member_type<'a>(
    ident: &'a Ident,
    parent_id: TypeId,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> TypecheckResult<'a, TypeId> {
    match resolved_types.get(&parent_id) {
        Some(ResolvedTypeItem::Struct(struct_item)) => {
            if let Some(field_ty) = struct_item.fields().get(ident.as_ref()).copied() {
                Ok(field_ty)
            } else {
                Err(TypecheckError::UndefinedMember {
                    ty: known_types[&parent_id].to_string(known_types),
                    name: ident.clone(),
                })
            }
        }
        Some(ResolvedTypeItem::Module(module_item)) => {
            Ok(find_module_member_type(ident, module_item, Some(parent_id), known_types)?.0)
        }
        Some(_) => Err(TypecheckError::UndefinedMember {
            ty: known_types[&parent_id].to_string(known_types),
            name: ident.clone(),
        }),
        None => unreachable!("type incorrectly resolved"),
    }
}

fn is_valid_enum_variant<'a>(
    enum_ty: TypeId,
    enum_name: &'a Ident,
    variant_name: &'a Ident,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> TypecheckResult<'a, ()> {
    match resolved_types.get(&enum_ty) {
        Some(ResolvedTypeItem::Enum(enum_item)) => {
            if enum_item.variants().contains_key(variant_name.as_ref()) {
                Ok(())
            } else {
                Err(TypecheckError::InvalidEnumVariant {
                    enum_name: enum_name.as_string(),
                    variant_name: variant_name.clone(),
                })
            }
        }
        _ => Err(TypecheckError::InvalidEnumIdent {
            name: enum_name.clone(),
        }),
    }
}

fn typecheck_path<'a>(
    path: &'a Path,
    parent_module: &ResolvedModule,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, Either<Ident, CheckedPath>> {
    if let Some(ident) = path.as_ident() {
        if args.global_const_values.contains_key(ident.as_ref())
            || args.local_const_values.contains_key(ident.as_ref())
        {
            Ok(Either::Const(ident.clone()))
        } else {
            let ty = find_module_member_type(ident, parent_module, None, known_types)?.0;
            Ok(Either::Checked(CheckedPath::new(path.clone(), ty)))
        }
    } else {
        // Currently this can only refer to an enum variant.
        if path.tail().len() == 1 {
            let enum_ty = ResolvedType::Named {
                name: path.head().as_string(),
                generic_args: [].as_slice().into(),
            };
            let enum_id = TypeId::from_type(&enum_ty);

            is_valid_enum_variant(enum_id, path.head(), path.tail()[0].ident(), resolved_types)?;
            Ok(Either::Checked(CheckedPath::new(path.clone(), enum_id)))
        } else {
            Err(TypecheckError::InvalidPath { path })
        }
    }
}

fn typecheck_indexer<'a>(
    indexer: &'a Indexer,
    base_id: TypeId,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, (CheckedIndexKind, TypeId)> {
    match indexer.index() {
        IndexKind::Single(index_expr) => {
            let index = typecheck_expr(
                index_expr,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;

            match index {
                Either::Const(index) => {
                    let index = eval(
                        &index,
                        &mut VarScope::empty(),
                        args.global_const_values,
                        Some(args.local_const_values),
                        args.funcs,
                    )?;
                    let checked_indexer = CheckedIndexKind::Single(CheckedExpr::Value(index));

                    let base_ty = &known_types[&base_id];
                    match base_ty {
                        ResolvedType::BuiltinBits { width } => {
                            if (index < 0) || (index >= (*width as i64)) {
                                return Err(TypecheckError::IndexOutOfRange {
                                    index_expr,
                                    index,
                                    len: *width,
                                });
                            }

                            let result_ty = UnresolvedType::BuiltinBits { width: 1 };
                            let result_id = resolve_type_late(&result_ty, known_types);

                            Ok((checked_indexer, result_id))
                        }
                        ResolvedType::Array { item_ty, len } => {
                            if (index < 0) || (index >= (*len as i64)) {
                                return Err(TypecheckError::IndexOutOfRange {
                                    index_expr,
                                    index,
                                    len: *len,
                                });
                            }

                            Ok((checked_indexer, *item_ty))
                        }
                        _ => Err(TypecheckError::InvalidIndexing {
                            indexer,
                            base_ty: base_ty.to_string(known_types),
                        }),
                    }
                }
                Either::Checked(index) => {
                    let base_ty = &known_types[&base_id];
                    let required_index_width = match base_ty {
                        ResolvedType::BuiltinBits { width } => width.clog2(),
                        ResolvedType::Array { len, .. } => len.clog2(),
                        _ => {
                            return Err(TypecheckError::InvalidIndexing {
                                indexer,
                                base_ty: base_ty.to_string(known_types),
                            });
                        }
                    };

                    let required_ty = UnresolvedType::BuiltinBits {
                        width: required_index_width.max(1),
                    };
                    let required_id = resolve_type_late(&required_ty, known_types);
                    let required_ty = &known_types[&required_id];

                    let base_ty = &known_types[&base_id];
                    let index_ty = &known_types[&index.ty()];
                    if let ResolvedType::BuiltinBits { width } = index_ty && (*width == required_index_width) {
                        let indexer = CheckedIndexKind::Single(index);

                        match base_ty {
                            ResolvedType::BuiltinBits { .. } => {
                                let result_ty = UnresolvedType::BuiltinBits { width: 1 };
                                let result_id = resolve_type_late(&result_ty, known_types);
                                Ok((indexer, result_id))
                            }
                            ResolvedType::Array { item_ty, .. } => {
                                Ok((indexer, *item_ty))
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        Err(TypecheckError::InvalidIndexType {
                            expr: index_expr,
                            expected_ty: required_ty.to_string(known_types),
                            value_ty: index_ty.to_string(known_types),
                        })
                    }
                }
            }
        }
        IndexKind::Range(range) => {
            // Index ranges can only be constant
            let start = transform_const_expr(&range.start, scope, false)?;
            let start = eval(
                &start,
                &mut VarScope::empty(),
                args.global_const_values,
                Some(args.local_const_values),
                args.funcs,
            )?;

            let end = transform_const_expr(&range.end, scope, false)?;
            let end = eval(
                &end,
                &mut VarScope::empty(),
                args.global_const_values,
                Some(args.local_const_values),
                args.funcs,
            )?;

            let base_ty = &known_types[&base_id];
            match base_ty {
                ResolvedType::BuiltinBits { width } => {
                    if (start < 0) || (start >= (*width as i64)) {
                        return Err(TypecheckError::IndexOutOfRange {
                            index_expr: &range.start,
                            index: start,
                            len: *width,
                        });
                    }

                    if (end <= start) || (end >= (*width as i64)) {
                        return Err(TypecheckError::IndexOutOfRange {
                            index_expr: &range.end,
                            index: end,
                            len: *width,
                        });
                    }

                    let result_width = end - start;
                    assert!(result_width > 0);
                    let result_width = result_width as u64;

                    let result_ty = UnresolvedType::BuiltinBits {
                        width: result_width,
                    };
                    let result_id = resolve_type_late(&result_ty, known_types);

                    let checked_indexer = CheckedIndexKind::Range(start..end);
                    Ok((checked_indexer, result_id))
                }
                ResolvedType::Array { .. } => Err(TypecheckError::InvalidRangeIndexing {
                    indexer,
                    base_ty: base_ty.to_string(known_types),
                }),
                _ => Err(TypecheckError::InvalidIndexing {
                    indexer,
                    base_ty: base_ty.to_string(known_types),
                }),
            }
        }
    }
}

fn typecheck_index_expr<'a>(
    expr: &'a IndexExpr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedIndexExpr> {
    let base = typecheck_expr(
        expr.base(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    )?;
    let base = merge_expr(base, args)?;

    let (indexer, ty) = typecheck_indexer(
        expr.indexer(),
        base.ty(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    )?;

    Ok(CheckedIndexExpr::new(base, indexer, ty))
}

fn typecheck_member_access_expr<'a>(
    expr: &'a MemberAccessExpr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedMemberAccessExpr> {
    let base = typecheck_expr(
        expr.base(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    )?;
    let base = merge_expr(base, args)?;

    let ty = find_member_type(
        expr.member().member(),
        base.ty(),
        known_types,
        resolved_types,
    )?;

    Ok(CheckedMemberAccessExpr::new(
        base,
        expr.member().member().clone(),
        ty,
    ))
}

fn typecheck_if_expr<'a>(
    expr: &'a IfExpr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedIfExpr> {
    let mut errors = Vec::new();

    let cond = match typecheck_expr(
        expr.condition(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(cond) => match merge_expr(cond, args) {
            Ok(cond) => {
                let cond_ty = &known_types[&cond.ty()];
                match cond_ty {
                    ResolvedType::Const | ResolvedType::BuiltinBits { width: 1 } => Some(cond),
                    _ => {
                        errors.push(TypecheckError::InvalidConditionType {
                            cond: expr.condition(),
                            cond_ty: cond_ty.to_string(known_types),
                        });
                        None
                    }
                }
            }
            Err(err) => {
                errors.push(err);
                None
            }
        },
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let body = match typecheck_expr_block(
        expr.body(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(body) => Some(body),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let mut else_if_blocks = Vec::with_capacity(expr.else_if_blocks().len());
    for else_if_block in expr.else_if_blocks().iter() {
        let cond = match typecheck_expr(
            else_if_block.condition(),
            parent_module,
            mode,
            scope,
            known_types,
            resolved_types,
            args,
        ) {
            Ok(cond) => match merge_expr(cond, args) {
                Ok(cond) => {
                    let cond_ty = &known_types[&cond.ty()];
                    match cond_ty {
                        ResolvedType::Const | ResolvedType::BuiltinBits { width: 1 } => Some(cond),
                        _ => {
                            errors.push(TypecheckError::InvalidConditionType {
                                cond: expr.condition(),
                                cond_ty: cond_ty.to_string(known_types),
                            });
                            None
                        }
                    }
                }
                Err(err) => {
                    errors.push(err);
                    None
                }
            },
            Err(err) => {
                errors.push(err);
                None
            }
        };

        let body = match typecheck_expr_block(
            else_if_block.body(),
            parent_module,
            mode,
            scope,
            known_types,
            resolved_types,
            args,
        ) {
            Ok(body) => Some(body),
            Err(err) => {
                errors.push(err);
                None
            }
        };

        if let Some(cond) = cond && let Some(body) = body {
            else_if_blocks.push(CheckedIfExprElseIfBlock::new(cond, body));
        }
    }

    let else_block = if let Some(else_block) = expr.else_block() {
        match typecheck_expr_block(
            else_block.body(),
            parent_module,
            mode,
            scope,
            known_types,
            resolved_types,
            args,
        ) {
            Ok(body) => Some(CheckedIfExprElseBlock::new(body)),
            Err(err) => {
                errors.push(err);
                None
            }
        }
    } else {
        errors.push(TypecheckError::MissingElseBlock { if_expr: expr });
        None
    };

    if !errors.is_empty() {
        Err(TypecheckError::new_list(errors))
    } else {
        let cond = cond.unwrap();
        let body = body.unwrap();
        let else_block = else_block.unwrap();

        let ty_id = body.ty();
        let ty = &known_types[&ty_id];

        for (i, else_if_block) in else_if_blocks.iter().enumerate() {
            let else_if_ty_id = else_if_block.body().ty();
            if else_if_ty_id != ty_id {
                let else_if_ty = &known_types[&else_if_ty_id];

                errors.push(TypecheckError::ElseIfTypeMismatch {
                    if_ty: ty.to_string(known_types),
                    else_if_ty: else_if_ty.to_string(known_types),
                    else_if_block: &expr.else_if_blocks()[i],
                })
            }
        }

        if else_block.body().ty() != ty_id {
            let else_ty_id = else_block.body().ty();
            if else_ty_id != ty_id {
                let else_ty = &known_types[&else_ty_id];

                errors.push(TypecheckError::ElseTypeMismatch {
                    if_ty: ty.to_string(known_types),
                    else_ty: else_ty.to_string(known_types),
                    else_block: expr.else_block().unwrap(),
                })
            }
        }

        wrap_errors!(
            CheckedIfExpr::new(cond, body, else_if_blocks, else_block, ty_id),
            errors
        )
    }
}

fn typecheck_numeric_match_expr<'a>(
    expr: &'a MatchExpr,
    value_ty: &ResolvedType,
    width: u64,
    scope: &Scope,
    known_types: &HashMap<TypeId, ResolvedType>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, ()> {
    let mut errors = Vec::new();

    let full_range = InclusiveRange::n_bit(width);
    let mut covered_ranges = RangeCollection::new();

    for branch in expr.branches().iter() {
        'inner: for pattern in branch.patterns().iter() {
            match pattern {
                MatchPattern::Literal(l) => {
                    if full_range.contains(l.value() as u64) {
                        if covered_ranges.contains(l.value() as u64) {
                            errors.push(TypecheckError::UnreachablePattern { pattern });
                        } else {
                            covered_ranges.insert(l.value() as u64);
                        }
                    } else {
                        errors.push(TypecheckError::PatternOutOfRange {
                            pattern,
                            value_ty: value_ty.to_string(known_types),
                        });
                    }

                    continue 'inner;
                }
                MatchPattern::Range(start, end) => {
                    let range = (start.value() as u64)..(end.value() as u64);
                    if full_range.contains(&range) {
                        if covered_ranges.contains(&range) {
                            errors.push(TypecheckError::UnreachablePattern { pattern });
                        } else {
                            covered_ranges.insert(range);
                        }
                    } else {
                        errors.push(TypecheckError::PatternOutOfRange {
                            pattern,
                            value_ty: value_ty.to_string(known_types),
                        });
                    }

                    continue 'inner;
                }
                MatchPattern::RangeInclusive(start, end) => {
                    let range = (start.value() as u64)..=(end.value() as u64);
                    if full_range.contains(&range) {
                        if covered_ranges.contains(&range) {
                            errors.push(TypecheckError::UnreachablePattern { pattern });
                        } else {
                            covered_ranges.insert(range);
                        }
                    } else {
                        errors.push(TypecheckError::PatternOutOfRange {
                            pattern,
                            value_ty: value_ty.to_string(known_types),
                        });
                    }

                    continue 'inner;
                }
                MatchPattern::Path(p) => {
                    if let Some(ident) = p.as_ident() {
                        if ident.as_ref() == "_" {
                            if covered_ranges.contains(full_range) {
                                errors.push(TypecheckError::UnreachablePattern { pattern });
                            } else {
                                covered_ranges.insert(full_range);
                            }
                        } else {
                            match scope.contains_const(ident) {
                                Ok(_) => {
                                    let value = *args
                                        .global_const_values
                                        .get(ident.as_ref())
                                        .or_else(|| args.local_const_values.get(ident.as_ref()))
                                        .unwrap()
                                        as u64;

                                    if full_range.contains(value) {
                                        if covered_ranges.contains(value) {
                                            errors.push(TypecheckError::UnreachablePattern {
                                                pattern,
                                            });
                                        } else {
                                            covered_ranges.insert(value);
                                        }
                                    } else {
                                        errors.push(TypecheckError::PatternOutOfRange {
                                            pattern,
                                            value_ty: value_ty.to_string(known_types),
                                        });
                                    }
                                }
                                Err(err) => errors.push(err),
                            }
                        }

                        continue 'inner;
                    }
                }
            }

            errors.push(TypecheckError::IncompatiblePattern {
                pattern,
                value_ty: value_ty.to_string(known_types),
            });
        }
    }

    if !covered_ranges.contains(full_range) {
        errors.push(TypecheckError::NonExhaustiveMatch { match_expr: expr })
    }

    wrap_errors!((), errors)
}

fn typecheck_enum_match_expr<'a>(
    expr: &'a MatchExpr,
    value_ty: &ResolvedType,
    enum_item: &ResolvedEnum,
    known_types: &HashMap<TypeId, ResolvedType>,
) -> TypecheckResult<'a, ()> {
    let ResolvedType::Named { name: enum_name, .. } = value_ty else {
        unreachable!("enums must be named types");
    };
    let mut errors = Vec::new();

    let mut unused_variants = HashSet::default();
    unused_variants.extend(enum_item.variants().keys().cloned());

    for branch in expr.branches().iter() {
        'inner: for pattern in branch.patterns().iter() {
            if let MatchPattern::Path(p) = pattern {
                if let Some(ident) = p.as_ident() && (ident.as_ref() == "_") {
                    unused_variants.clear();
                    continue 'inner;
                } else if (p.head().as_ref() == enum_name.as_ref()) && (p.tail().len() == 1) {
                    let variant_name = p.tail()[0].ident();
                    if enum_item.variants().contains_key(variant_name.as_ref()) {
                        if unused_variants.contains(variant_name.as_ref()) {
                            unused_variants.remove(variant_name.as_ref());
                            continue 'inner;
                        } else {
                            errors.push(TypecheckError::UnreachablePattern { pattern });
                            continue 'inner;
                        }
                    } else {
                        errors.push(TypecheckError::InvalidEnumVariant {
                            enum_name: SharedString::clone(enum_name),
                            variant_name: variant_name.clone()
                        });
                        continue 'inner;
                    }
                }
            }

            errors.push(TypecheckError::IncompatiblePattern {
                pattern,
                value_ty: value_ty.to_string(known_types),
            });
        }
    }

    if !unused_variants.is_empty() {
        errors.push(TypecheckError::NonExhaustiveMatch { match_expr: expr })
    }

    wrap_errors!((), errors)
}

fn typecheck_match_expr<'a>(
    expr: &'a MatchExpr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedMatchExpr> {
    let mut errors = Vec::new();

    let value = match typecheck_expr(
        expr.value(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(value) => match merge_expr(value, args) {
            Ok(value) => Some(value),
            Err(err) => {
                errors.push(err);
                None
            }
        },
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let mut branches = Vec::with_capacity(expr.branches().len());
    let mut ty_id = None;
    for branch in expr.branches().iter() {
        let checked_branch = match branch.body() {
            MatchBody::Expr(body_expr) => {
                match typecheck_expr(
                    body_expr,
                    parent_module,
                    mode,
                    scope,
                    known_types,
                    resolved_types,
                    args,
                ) {
                    Ok(body_expr) => match merge_expr(body_expr, args) {
                        Ok(body_expr) => CheckedMatchExprBranch::new(
                            branch.patterns().to_vec(),
                            CheckedMatchExprBody::Expr(body_expr),
                        ),
                        Err(err) => {
                            errors.push(err);
                            continue;
                        }
                    },
                    Err(err) => {
                        errors.push(err);
                        continue;
                    }
                }
            }
            MatchBody::Block(body) => {
                match typecheck_expr_block(
                    body,
                    parent_module,
                    mode,
                    scope,
                    known_types,
                    resolved_types,
                    args,
                ) {
                    Ok(body) => CheckedMatchExprBranch::new(
                        branch.patterns().to_vec(),
                        CheckedMatchExprBody::Block(body),
                    ),
                    Err(err) => {
                        errors.push(err);
                        continue;
                    }
                }
            }
        };

        let branch_ty_id = checked_branch.body().ty();
        if let Some(ty_id) = ty_id {
            if branch_ty_id != ty_id {
                let ty = &known_types[&ty_id];
                let branch_ty = &known_types[&branch_ty_id];
                errors.push(TypecheckError::MatchBranchTypeMismatch {
                    match_ty: ty.to_string(known_types),
                    branch_ty: branch_ty.to_string(known_types),
                    branch,
                })
            }
        } else {
            ty_id = Some(branch_ty_id);
        }

        branches.push(checked_branch);
    }

    if let Some(value) = &value {
        let value_ty = &known_types[&value.ty()];
        match value_ty {
            ResolvedType::Const => {
                if let Err(err) =
                    typecheck_numeric_match_expr(expr, value_ty, 64, scope, known_types, args)
                {
                    errors.push(err);
                }
            }
            ResolvedType::BuiltinBits { width } => {
                if let Err(err) =
                    typecheck_numeric_match_expr(expr, value_ty, *width, scope, known_types, args)
                {
                    errors.push(err);
                }
            }
            ResolvedType::Named { .. } => {
                let value_ty_item = &resolved_types[&value.ty()];
                match value_ty_item {
                    ResolvedTypeItem::Enum(enum_item) => {
                        if let Err(err) =
                            typecheck_enum_match_expr(expr, value_ty, enum_item, known_types)
                        {
                            errors.push(err);
                        }
                    }
                    _ => {
                        errors.push(TypecheckError::InvalidMatchType {
                            value: expr.value(),
                            value_ty: value_ty.to_string(known_types),
                        });
                    }
                }
            }
            _ => {
                errors.push(TypecheckError::InvalidMatchType {
                    value: expr.value(),
                    value_ty: value_ty.to_string(known_types),
                });
            }
        }
    }

    wrap_errors!(
        CheckedMatchExpr::new(value.unwrap(), branches, ty_id.unwrap()),
        errors
    )
}

fn typecheck_expr<'a>(
    expr: &'a Expr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, Either<ConstExpr, CheckedExpr>> {
    macro_rules! wrap_expr {
        ($inner:expr, $variant:ident) => {
            $inner.map(
                |inner| ConstExpr::$variant(inner),
                |inner| CheckedExpr::$variant(inner),
            )
        };
    }

    macro_rules! unary_expr {
        ($expr:expr, $op:ident) => {{
            let inner = typecheck_unary_expr(
                $expr,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            Ok(wrap_expr!(inner, $op))
        }};
    }

    macro_rules! binary_expr {
        ($expr:expr, $op:ident) => {{
            let inner = typecheck_binary_expr(
                $expr,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            Ok(wrap_expr!(inner, $op))
        }};
    }

    macro_rules! cmp_expr {
        ($expr:expr, $op:ident) => {{
            let inner = typecheck_compare_expr(
                $expr,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            Ok(wrap_expr!(inner, $op))
        }};
    }

    macro_rules! checked_only_cmp_expr {
        ($expr:expr, $op:ident) => {{
            let inner = typecheck_compare_expr(
                $expr,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            match inner {
                Either::Const(_) => Err(TypecheckError::InvalidConstOp { op: *$expr.op() }),
                Either::Checked(inner) => Ok(Either::Checked(CheckedExpr::$op(inner))),
            }
        }};
    }

    match expr {
        Expr::Literal(l) => Ok(Either::Const(ConstExpr::Literal(*l))),
        Expr::Path(path) => {
            let path = typecheck_path(path, parent_module, known_types, resolved_types, args)?;
            Ok(path.map(ConstExpr::Ident, CheckedExpr::Path))
        }
        Expr::If(expr) => {
            let if_expr = typecheck_if_expr(
                expr,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            Ok(Either::Checked(CheckedExpr::If(if_expr)))
        }
        Expr::Match(expr) => {
            let match_expr = typecheck_match_expr(
                expr,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            Ok(Either::Checked(CheckedExpr::Match(match_expr)))
        }
        Expr::Block(block) => {
            let block = typecheck_expr_block(
                block,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            Ok(Either::Checked(CheckedExpr::Block(block)))
        }
        Expr::Index(expr) => {
            let index_expr = typecheck_index_expr(
                expr,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            Ok(Either::Checked(CheckedExpr::Index(index_expr)))
        }
        Expr::MemberAccess(expr) => {
            let member_access_expr = typecheck_member_access_expr(
                expr,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            Ok(Either::Checked(CheckedExpr::MemberAccess(
                member_access_expr,
            )))
        }
        Expr::Call(expr) => {
            let call_expr = transform_const_call_expr(expr, scope)?;
            Ok(Either::Const(ConstExpr::Call(call_expr)))
        }
        Expr::Construct(expr) => {
            let construct_expr = typecheck_construct_expr(
                expr,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            Ok(Either::Checked(CheckedExpr::Construct(construct_expr)))
        }
        Expr::Cast(expr) => {
            let cast_expr = typecheck_cast_expr(
                expr,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            Ok(Either::Checked(CheckedExpr::Cast(cast_expr)))
        }
        Expr::Concat(expr) => {
            let concat_expr = typecheck_concat_expr(
                expr,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            Ok(Either::Checked(CheckedExpr::Concat(concat_expr)))
        }
        Expr::Pos(expr) => typecheck_expr(
            expr.inner(),
            parent_module,
            mode,
            scope,
            known_types,
            resolved_types,
            args,
        ),
        Expr::Paren(expr) => typecheck_expr(
            expr.inner(),
            parent_module,
            mode,
            scope,
            known_types,
            resolved_types,
            args,
        ),
        Expr::Neg(expr) => unary_expr!(expr, Neg),
        Expr::Not(expr) => unary_expr!(expr, Not),
        Expr::Lt(expr) => cmp_expr!(expr, Lt),
        Expr::Lte(expr) => cmp_expr!(expr, Lte),
        Expr::Gt(expr) => cmp_expr!(expr, Gt),
        Expr::Gte(expr) => cmp_expr!(expr, Gte),
        Expr::Slt(expr) => checked_only_cmp_expr!(expr, Slt),
        Expr::Slte(expr) => checked_only_cmp_expr!(expr, Slte),
        Expr::Sgt(expr) => checked_only_cmp_expr!(expr, Sgt),
        Expr::Sgte(expr) => checked_only_cmp_expr!(expr, Sgte),
        Expr::Eq(expr) => cmp_expr!(expr, Eq),
        Expr::Ne(expr) => cmp_expr!(expr, Ne),
        Expr::Add(expr) => binary_expr!(expr, Add),
        Expr::Sub(expr) => binary_expr!(expr, Sub),
        Expr::Mul(expr) => binary_expr!(expr, Mul),
        Expr::Div(expr) => binary_expr!(expr, Div),
        Expr::Rem(expr) => binary_expr!(expr, Rem),
        Expr::And(expr) => binary_expr!(expr, And),
        Expr::Xor(expr) => binary_expr!(expr, Xor),
        Expr::Or(expr) => binary_expr!(expr, Or),
        Expr::Shl(expr) => binary_expr!(expr, Shl),
        Expr::Lsr(expr) => binary_expr!(expr, Lsr),
        Expr::Asr(expr) => binary_expr!(expr, Asr),
    }
}

fn typecheck_if_statement<'a>(
    expr: &'a IfExpr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedIfStatement> {
    let mut errors = Vec::new();

    let cond = match typecheck_expr(
        expr.condition(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(cond) => match merge_expr(cond, args) {
            Ok(cond) => {
                let cond_ty = &known_types[&cond.ty()];
                match cond_ty {
                    ResolvedType::Const | ResolvedType::BuiltinBits { width: 1 } => Some(cond),
                    _ => {
                        errors.push(TypecheckError::InvalidConditionType {
                            cond: expr.condition(),
                            cond_ty: cond_ty.to_string(known_types),
                        });
                        None
                    }
                }
            }
            Err(err) => {
                errors.push(err);
                None
            }
        },
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let body = match typecheck_block(
        expr.body(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(body) => Some(body),
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let mut else_if_blocks = Vec::with_capacity(expr.else_if_blocks().len());
    for else_if_block in expr.else_if_blocks().iter() {
        let cond = match typecheck_expr(
            else_if_block.condition(),
            parent_module,
            mode,
            scope,
            known_types,
            resolved_types,
            args,
        ) {
            Ok(cond) => match merge_expr(cond, args) {
                Ok(cond) => {
                    let cond_ty = &known_types[&cond.ty()];
                    match cond_ty {
                        ResolvedType::Const | ResolvedType::BuiltinBits { width: 1 } => Some(cond),
                        _ => {
                            errors.push(TypecheckError::InvalidConditionType {
                                cond: expr.condition(),
                                cond_ty: cond_ty.to_string(known_types),
                            });
                            None
                        }
                    }
                }
                Err(err) => {
                    errors.push(err);
                    None
                }
            },
            Err(err) => {
                errors.push(err);
                None
            }
        };

        let body = match typecheck_block(
            else_if_block.body(),
            parent_module,
            mode,
            scope,
            known_types,
            resolved_types,
            args,
        ) {
            Ok(body) => Some(body),
            Err(err) => {
                errors.push(err);
                None
            }
        };

        if let Some(cond) = cond && let Some(body) = body {
            else_if_blocks.push(CheckedIfStatementElseIfBlock::new(cond, body));
        }
    }

    let else_block = if let Some(else_block) = expr.else_block() {
        match typecheck_block(
            else_block.body(),
            parent_module,
            mode,
            scope,
            known_types,
            resolved_types,
            args,
        ) {
            Ok(body) => Some(CheckedIfStatementElseBlock::new(body)),
            Err(err) => {
                errors.push(err);
                None
            }
        }
    } else {
        None
    };

    wrap_errors!(
        CheckedIfStatement::new(cond.unwrap(), body.unwrap(), else_if_blocks, else_block),
        errors
    )
}

fn typecheck_match_statement<'a>(
    expr: &'a MatchExpr,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedMatchStatement> {
    let mut errors = Vec::new();

    let value = match typecheck_expr(
        expr.value(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(value) => match merge_expr(value, args) {
            Ok(value) => Some(value),
            Err(err) => {
                errors.push(err);
                None
            }
        },
        Err(err) => {
            errors.push(err);
            None
        }
    };

    let mut branches = Vec::with_capacity(expr.branches().len());
    for branch in expr.branches().iter() {
        match branch.body() {
            MatchBody::Expr(body_expr) => {
                errors.push(TypecheckError::UnexpectedReturnValue { value: body_expr });

                match typecheck_expr(
                    body_expr,
                    parent_module,
                    mode,
                    scope,
                    known_types,
                    resolved_types,
                    args,
                ) {
                    Ok(_) => {}
                    Err(err) => errors.push(err),
                }
            }
            MatchBody::Block(body) => {
                match typecheck_block(
                    body,
                    parent_module,
                    mode,
                    scope,
                    known_types,
                    resolved_types,
                    args,
                ) {
                    Ok(body) => {
                        branches.push(CheckedMatchStatementBranch::new(
                            branch.patterns().to_vec(),
                            body,
                        ));
                    }
                    Err(err) => errors.push(err),
                }
            }
        }
    }

    if !errors.is_empty() {
        Err(TypecheckError::new_list(errors))
    } else {
        let value = value.unwrap();
        let value_ty = &known_types[&value.ty()];
        match value_ty {
            ResolvedType::Const => {
                typecheck_numeric_match_expr(expr, value_ty, 64, scope, known_types, args)?;
            }
            ResolvedType::BuiltinBits { width } => {
                typecheck_numeric_match_expr(expr, value_ty, *width, scope, known_types, args)?;
            }
            ResolvedType::Named { .. } => {
                let value_ty_item = &resolved_types[&value.ty()];
                match value_ty_item {
                    ResolvedTypeItem::Enum(enum_item) => {
                        typecheck_enum_match_expr(expr, value_ty, enum_item, known_types)?;
                    }
                    _ => {
                        return Err(TypecheckError::InvalidMatchType {
                            value: expr.value(),
                            value_ty: value_ty.to_string(known_types),
                        });
                    }
                }
            }
            _ => {
                return Err(TypecheckError::InvalidMatchType {
                    value: expr.value(),
                    value_ty: value_ty.to_string(known_types),
                });
            }
        }

        wrap_errors!(CheckedMatchStatement::new(value, branches), errors)
    }
}

fn typecheck_assignment<'a>(
    assign: &'a Assignment,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedAssignment> {
    let mut errors = Vec::new();

    if assign.op().kind() != AssignKind::Assign {
        errors.push(TypecheckError::InvalidAssignOp { assign });
    }

    if !assign.target().path().is_ident() {
        errors.push(TypecheckError::InvalidPath {
            path: assign.target().path(),
        });
    }

    let base = assign.target().path().head();

    let value = match typecheck_expr(
        assign.value(),
        parent_module,
        mode,
        scope,
        known_types,
        resolved_types,
        args,
    ) {
        Ok(value) => match merge_expr(value, args) {
            Ok(value) => Some(value),
            Err(err) => {
                errors.push(err);
                None
            }
        },
        Err(err) => {
            errors.push(err);
            None
        }
    };

    match find_module_member_type(base, parent_module, None, known_types) {
        Ok((base_ty, kind)) => {
            match (mode, kind) {
                (TypecheckMode::Sequential, LogicKind::Signal) => {
                    errors.push(TypecheckError::InvalidSeqAssignSig { assign })
                }
                (TypecheckMode::Sequential, LogicKind::Module) => {
                    errors.push(TypecheckError::InvalidSeqAssignMod { assign })
                }
                (TypecheckMode::Combinatoric, LogicKind::Register) => {
                    errors.push(TypecheckError::InvalidCombAssignReg { assign })
                }
                _ => { /* valid */ }
            }

            let mut ty = base_ty;
            let mut suffixes = Vec::new();
            for suffix in assign.target().suffixes().iter() {
                match suffix {
                    SuffixOp::Indexer(indexer) => {
                        let checked_indexer;
                        (checked_indexer, ty) = match typecheck_indexer(
                            indexer,
                            ty,
                            parent_module,
                            mode,
                            scope,
                            known_types,
                            resolved_types,
                            args,
                        ) {
                            Ok(val) => val,
                            Err(err) => {
                                errors.push(err);
                                break;
                            }
                        };

                        suffixes.push(CheckedSuffixOp::Indexer {
                            index: checked_indexer,
                            ty,
                        });
                    }
                    SuffixOp::MemberAccess(member) => {
                        ty = match find_member_type(
                            member.member(),
                            ty,
                            known_types,
                            resolved_types,
                        ) {
                            Ok(ty) => ty,
                            Err(err) => {
                                errors.push(err);
                                break;
                            }
                        };

                        suffixes.push(CheckedSuffixOp::MemberAccess {
                            member: member.member().clone(),
                            ty,
                        });
                    }
                }
            }

            let target = CheckedAssignTarget::new(base.clone(), base_ty, suffixes);
            wrap_errors!(CheckedAssignment::new(target, value.unwrap()), errors)
        }
        Err(err) => {
            errors.push(err);
            Err(TypecheckError::new_list(errors))
        }
    }
}

fn typecheck_statement<'a>(
    statement: &'a Statement,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedStatement> {
    match statement {
        Statement::Expr(expr) => match expr {
            Expr::If(if_expr) => {
                let checked_if_statement = typecheck_if_statement(
                    if_expr,
                    parent_module,
                    mode,
                    scope,
                    known_types,
                    resolved_types,
                    args,
                )?;
                Ok(CheckedStatement::If(checked_if_statement))
            }
            Expr::Match(match_expr) => {
                let checked_match_statement = typecheck_match_statement(
                    match_expr,
                    parent_module,
                    mode,
                    scope,
                    known_types,
                    resolved_types,
                    args,
                )?;
                Ok(CheckedStatement::Match(checked_match_statement))
            }
            Expr::Block(block) => {
                let checked_block = typecheck_block(
                    block,
                    parent_module,
                    mode,
                    scope,
                    known_types,
                    resolved_types,
                    args,
                )?;
                Ok(CheckedStatement::Block(checked_block))
            }
            expr => {
                let checked_expr = typecheck_expr(
                    expr,
                    parent_module,
                    mode,
                    scope,
                    known_types,
                    resolved_types,
                    args,
                )?;
                let checked_expr = merge_expr(checked_expr, args)?;
                Ok(CheckedStatement::Expr(checked_expr))
            }
        },
        Statement::Declaration(decl) => Err(TypecheckError::UnsupportedDeclaration { decl }),
        Statement::Assignment(assign) => {
            let checked_assign = typecheck_assignment(
                assign,
                parent_module,
                mode,
                scope,
                known_types,
                resolved_types,
                args,
            )?;
            Ok(CheckedStatement::Assignment(checked_assign))
        }
        Statement::WhileLoop(while_loop) => {
            Err(TypecheckError::UnsupportedWhileLoop { while_loop })
        }
        Statement::ForLoop(for_loop) => Err(TypecheckError::UnsupportedForLoop { for_loop }),
    }
}

fn typecheck_expr_block<'a>(
    block: &'a Block,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    parent_scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedExprBlock> {
    let mut errors = Vec::new();
    let scope = Scope::new(parent_scope);

    let mut statements = Vec::new();
    for statement in block.statements().iter() {
        match typecheck_statement(
            statement,
            parent_module,
            mode,
            &scope,
            known_types,
            resolved_types,
            args,
        ) {
            Ok(statement) => statements.push(statement),
            Err(err) => errors.push(err),
        }
    }

    let result = {
        if let Some(result) = block.result() {
            let result = typecheck_expr(
                result,
                parent_module,
                mode,
                &scope,
                known_types,
                resolved_types,
                args,
            );

            match result {
                Ok(result) => match merge_expr(result, args) {
                    Ok(result) => Some(result),
                    Err(err) => {
                        errors.push(err);
                        None
                    }
                },
                Err(err) => {
                    errors.push(err);
                    None
                }
            }
        } else {
            errors.push(TypecheckError::MissingReturnValue { block });
            None
        }
    };

    wrap_errors!(CheckedExprBlock::new(statements, result.unwrap()), errors)
}

fn typecheck_block<'a>(
    block: &'a Block,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    parent_scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> TypecheckResult<'a, CheckedBlock> {
    let mut errors = Vec::new();
    let scope = Scope::new(parent_scope);

    let mut statements = Vec::new();
    for statement in block.statements().iter() {
        match typecheck_statement(
            statement,
            parent_module,
            mode,
            &scope,
            known_types,
            resolved_types,
            args,
        ) {
            Ok(statement) => statements.push(statement),
            Err(err) => errors.push(err),
        }
    }

    if let Some(result) = block.result() {
        // Check if the result expression can also be considered a statement
        match result {
            Expr::If(result_if_expr) => {
                match typecheck_if_statement(
                    result_if_expr,
                    parent_module,
                    mode,
                    &scope,
                    known_types,
                    resolved_types,
                    args,
                ) {
                    Ok(result_if_statement) => {
                        statements.push(CheckedStatement::If(result_if_statement))
                    }
                    Err(err) => errors.push(err),
                }
            }
            Expr::Match(result_match_expr) => {
                match typecheck_match_statement(
                    result_match_expr,
                    parent_module,
                    mode,
                    &scope,
                    known_types,
                    resolved_types,
                    args,
                ) {
                    Ok(result_match_statement) => {
                        statements.push(CheckedStatement::Match(result_match_statement))
                    }
                    Err(err) => errors.push(err),
                }
            }
            Expr::Block(result_block) => {
                match typecheck_block(
                    result_block,
                    parent_module,
                    mode,
                    &scope,
                    known_types,
                    resolved_types,
                    args,
                ) {
                    Ok(result_block) => statements.push(CheckedStatement::Block(result_block)),
                    Err(err) => errors.push(err),
                }
            }
            _ => {
                if let Err(err) = typecheck_expr(
                    result,
                    parent_module,
                    mode,
                    &scope,
                    known_types,
                    resolved_types,
                    args,
                ) {
                    errors.push(err);
                }

                errors.push(TypecheckError::UnexpectedReturnValue { value: result });
            }
        }
    }

    wrap_errors!(CheckedBlock::new(statements), errors)
}

pub fn typecheck_module<'a>(
    module_item: &'a ResolvedModule,
    global_scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    global_const_values: &HashMap<SharedString, i64>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> TypecheckResult<'a, CheckedModule> {
    // The scope is only for const evaluation, so no ports/logic members belong in it
    let mut module_scope = Scope::new(global_scope);
    for name in module_item.local_consts().keys() {
        module_scope.add_const(SharedString::clone(name))
    }

    let args = TypecheckArgs {
        global_const_values,
        local_const_values: module_item.local_consts(),
        funcs,
    };

    let mut errors = Vec::new();

    for (_, port) in module_item.ports().iter() {
        if let ResolvedType::Array { .. } = &known_types[&port.ty()] {
            // TODO:
        } else if let Some(port_ty) = resolved_types.get(&port.ty()) {
            match (port.kind(), port_ty) {
                (LogicKind::Signal, ResolvedTypeItem::Module(_)) => { /* TODO: */ }
                (LogicKind::Register, ResolvedTypeItem::Module(_)) => { /* TODO: */ }
                (LogicKind::Module, ResolvedTypeItem::Struct(_)) => { /* TODO: */ }
                (LogicKind::Module, ResolvedTypeItem::Enum(_)) => { /* TODO: */ }
                (LogicKind::Module, ResolvedTypeItem::Module(_)) => { /* TODO: */ }
                _ => { /* valid */ }
            }
        }
    }

    for (_, logic_member) in module_item.logic_members().iter() {
        if let Some(member_ty) = resolved_types.get(&logic_member.ty()) {
            match (logic_member.kind(), member_ty) {
                (LogicKind::Signal, ResolvedTypeItem::Module(_)) => { /* TODO: */ }
                (LogicKind::Register, ResolvedTypeItem::Module(_)) => { /* TODO: */ }
                (LogicKind::Module, ResolvedTypeItem::Struct(_)) => { /* TODO: */ }
                (LogicKind::Module, ResolvedTypeItem::Enum(_)) => { /* TODO: */ }
                _ => { /* valid */ }
            }
        }
    }

    let mut proc_members = Vec::with_capacity(module_item.proc_members().len());
    for proc_member in module_item.proc_members().iter() {
        // FIXME: typecheck sensitivities

        match typecheck_block(
            proc_member.body(),
            module_item,
            TypecheckMode::Sequential,
            &module_scope,
            known_types,
            resolved_types,
            &args,
        ) {
            Ok(body) => {
                proc_members.push(CheckedProcMember::new(proc_member.sens().to_vec(), body));
            }
            Err(err) => errors.push(err),
        }
    }

    let mut comb_members = Vec::with_capacity(module_item.comb_members().len());
    for comb_member in module_item.comb_members().iter() {
        match typecheck_block(
            comb_member.body(),
            module_item,
            TypecheckMode::Combinatoric,
            &module_scope,
            known_types,
            resolved_types,
            &args,
        ) {
            Ok(body) => {
                comb_members.push(CheckedCombMember::new(body));
            }
            Err(err) => errors.push(err),
        }
    }

    wrap_errors!(CheckedModule::new(proc_members, comb_members), errors)
}
