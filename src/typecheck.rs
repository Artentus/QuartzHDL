use std::collections::VecDeque;

use crate::ast::*;
use crate::const_eval::{eval, ArithmeticError, VarScope};
use crate::ir::*;
use crate::{HashMap, HashSet, SharedString};
use topological_sort::TopologicalSort;

#[derive(Debug)]
pub enum TypecheckError<'a> {
    DuplicateIdent {
        name: Ident,
    },
    InvalidConstExpr {
        expr: &'a Expr,
    },
    InvalidConstPattern {
        pattern: &'a MatchPattern,
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
    ArithmeticError(ArithmeticError),
    List(Vec<TypecheckError<'a>>),
}

impl TypecheckError<'_> {
    fn new_list(list: Vec<Self>) -> Self {
        debug_assert!(list.len() > 0);

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

    pub fn add_const(&mut self, name: &Ident) {
        self.consts.insert(name.as_string());
    }

    pub fn add_func(&mut self, name: &Ident, arg_count: usize) {
        self.funcs.insert(name.as_string(), arg_count);
    }

    pub fn add_var(&mut self, name: &Ident) {
        self.vars.insert(name.as_string());
    }

    fn contains_const_inner(&self, name: &Ident) -> bool {
        self.consts.contains(name.as_ref())
            || self
                .parent
                .map_or(false, |parent| parent.contains_const_inner(name))
    }

    fn contains_func_inner(&self, name: &Ident) -> Option<usize> {
        self.funcs.get(name.as_ref()).copied().or_else(|| {
            self.parent
                .and_then(|parent| parent.contains_func_inner(name))
        })
    }

    fn contains_var_inner(&self, name: &Ident) -> bool {
        self.vars.contains(name.as_ref())
            || self
                .parent
                .map_or(false, |parent| parent.contains_var_inner(name))
    }

    pub fn contains_func<'err>(&self, name: &Ident) -> TypecheckResult<'err, usize> {
        if let Some(arg_count) = self.contains_func_inner(name) {
            Ok(arg_count)
        } else if self.contains_const_inner(name) || self.contains_var_inner(name) {
            Err(TypecheckError::InvalidFuncIdent { name: name.clone() })
        } else {
            Err(TypecheckError::UndefinedIdent { name: name.clone() })
        }
    }

    pub fn contains_const<'err>(&self, name: &Ident) -> TypecheckResult<'err, ()> {
        if self.contains_const_inner(name) {
            Ok(())
        } else if self.contains_var_inner(name) {
            Err(TypecheckError::ValueNotConst { name: name.clone() })
        } else if self.contains_func_inner(name).is_some() {
            Err(TypecheckError::InvalidValueIdent { name: name.clone() })
        } else {
            Err(TypecheckError::UndefinedIdent { name: name.clone() })
        }
    }

    pub fn contains_var<'err>(&self, name: &Ident) -> TypecheckResult<'err, ()> {
        if self.contains_const_inner(name) || self.contains_var_inner(name) {
            Ok(())
        } else if self.contains_func_inner(name).is_some() {
            Err(TypecheckError::InvalidValueIdent { name: name.clone() })
        } else {
            Err(TypecheckError::UndefinedIdent { name: name.clone() })
        }
    }

    pub fn contains_mut_var<'err>(&self, name: &Ident) -> TypecheckResult<'err, ()> {
        if self.contains_var_inner(name) {
            Ok(())
        } else if self.contains_const_inner(name) {
            Err(TypecheckError::TargetNotAssignable { name: name.clone() })
        } else if self.contains_func_inner(name).is_some() {
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
    scope: &mut Scope,
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

fn transform_const_if_expr<'a>(
    expr: &'a IfExpr,
    scope: &mut Scope,
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

fn has_exhaustive_patterns(expr: &MatchExpr) -> bool {
    for branch in expr.branches().iter() {
        for pattern in branch.patterns().iter() {
            if let MatchPattern::Path(path) = pattern {
                if (path.tail().len() == 0) && (path.head().as_ref() == "_") {
                    return true;
                }
            }
        }
    }

    false
}

fn transform_const_match_pattern<'a>(
    pattern: &'a MatchPattern,
    scope: &mut Scope,
) -> TypecheckResult<'a, ConstMatchPattern> {
    match pattern {
        MatchPattern::Literal(l) => Ok(ConstMatchPattern::Literal(*l)),
        MatchPattern::Path(p) => {
            if p.tail().len() == 0 {
                if p.head().as_ref() != "_" {
                    scope.contains_const(p.head())?;
                }

                Ok(ConstMatchPattern::Ident(p.head().clone()))
            } else {
                Err(TypecheckError::InvalidConstPattern { pattern })
            }
        }
    }
}

fn transform_const_match_expr<'a>(
    expr: &'a MatchExpr,
    scope: &mut Scope,
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

    if !has_exhaustive_patterns(expr) {
        errors.push(TypecheckError::NonExhaustiveMatch { match_expr: expr });
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
    scope: &mut Scope,
    is_statement: bool,
) -> TypecheckResult<'a, ConstExpr> {
    macro_rules! bin_expr {
        ($expr:expr, $op:ident) => {
            Ok(ConstExpr::$op(transform_const_bin_expr($expr, scope)?))
        };
    }

    match expr {
        Expr::Literal(l) => Ok(ConstExpr::Literal(l.clone())),
        Expr::Path(p) => {
            if p.tail().len() == 0 {
                scope.contains_var(p.head())?;
                Ok(ConstExpr::Ident(p.head().clone()))
            } else {
                Err(TypecheckError::InvalidConstExpr { expr })
            }
        }
        Expr::Paren(expr) => transform_const_expr(expr.inner(), scope, is_statement),
        Expr::Call(expr) => {
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
                ConstExpr::Call(ConstCallExpr::new(expr.func().clone(), args, expr.span())),
                errors
            )
        }
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
    scope: &mut Scope,
) -> TypecheckResult<'a, ConstExpr> {
    match arg {
        GenericTypeArg::Literal(l) => Ok(ConstExpr::Literal(l.clone())),
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
    if (assign.target().path().tail().len() == 0) && (assign.target().indexers().len() == 0) {
        let target = assign.target().path().head().clone();

        let err1 = match scope.contains_mut_var(&target) {
            Ok(_) => None,
            Err(err1) => Some(err1),
        };

        match transform_const_expr(assign.value(), scope, false) {
            Ok(value) => {
                if let Some(err1) = err1 {
                    Err(err1)
                } else {
                    Ok(ConstAssignment::new(
                        target,
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
    scope: &mut Scope,
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
    scope: &mut Scope,
) -> TypecheckResult<'a, ConstForLoop> {
    let mut errors = Vec::new();

    let start = transform_const_expr(&for_loop.range().start, scope, false);
    let end = transform_const_expr(&for_loop.range().end, scope, false);

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
    let body = transform_const_block(for_loop.body(), &mut inner_scope, false);

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
            start.unwrap()..end.unwrap(),
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
    parent_scope: &mut Scope,
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
            match transform_const_expr(result, &mut scope, false) {
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

pub fn transform_const_func<'a>(
    func: &'a Func,
    scope: &mut Scope,
) -> TypecheckResult<'a, ConstFunc> {
    let mut inner_scope = Scope::new(scope);
    for arg in func.args().iter() {
        inner_scope.add_var(arg);
    }

    let body = transform_const_block(func.body(), &mut inner_scope, true)?;
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

fn resolve_type<'a>(
    ty: &'a Type,
    type_items: &HashMap<SharedString, TypeItem>,
    scope: &mut Scope,
    global_const_values: &HashMap<SharedString, i64>,
    local_const_values: Option<&HashMap<SharedString, i64>>,
    funcs: &HashMap<SharedString, ConstFunc>,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    type_order: &mut TopologicalSort<TypeId>,
    type_queue: &mut VecDeque<TypeId>,
) -> TypecheckResult<'a, TypeId> {
    match ty {
        Type::Named(named_ty) => {
            let name = named_ty.name().as_ref();
            let generic_arg_count = named_ty
                .generic_args()
                .map(|generic_args| generic_args.args().len())
                .unwrap_or(0);

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
                        let width_expr = transform_generic_arg(
                            &named_ty.generic_args().unwrap().args()[0],
                            scope,
                        )?;

                        let width = eval(
                            &width_expr,
                            &mut VarScope::empty(),
                            global_const_values,
                            local_const_values,
                            funcs,
                        )?;

                        ResolvedType::BuiltinBits { width }
                    } else {
                        return Err(TypecheckError::GenericCountMismatch {
                            ty: named_ty,
                            arg_count: 1,
                        });
                    }
                }
                _ => {
                    // FIXME: check if type is valid

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
            if !known_types.contains_key(&id) {
                known_types.insert(id, resolved_ty);
                type_queue.push_back(id);
            }

            Ok(id)
        }
        Type::Array(array_ty) => {
            let item_ty = resolve_type(
                array_ty.item_ty(),
                type_items,
                scope,
                global_const_values,
                local_const_values,
                funcs,
                known_types,
                type_order,
                type_queue,
            )?;

            let len_expr = transform_const_expr(array_ty.len(), scope, false)?;

            let len = eval(
                &len_expr,
                &mut VarScope::empty(),
                global_const_values,
                local_const_values,
                funcs,
            )?;

            let resolved_ty = ResolvedType::Array { item_ty, len };
            let id = TypeId::from_type(&resolved_ty);
            if !known_types.contains_key(&id) {
                known_types.insert(id, resolved_ty);
                type_queue.push_back(id);
            }

            type_order.add_dependency(item_ty, id);
            Ok(id)
        }
    }
}

fn resolve_module<'a>(
    module_item: &'a Module,
    generic_values: &[i64],
    type_items: &HashMap<SharedString, TypeItem>,
    global_scope: &Scope,
    global_const_values: &HashMap<SharedString, i64>,
    funcs: &HashMap<SharedString, ConstFunc>,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    type_order: &mut TopologicalSort<TypeId>,
    type_queue: &mut VecDeque<TypeId>,
) -> TypecheckResult<'a, ResolvedModule> {
    use crate::const_eval::*;

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
    let mut scope = Scope::new(global_scope);
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
                let const_expr = transform_const_expr(const_member.value(), &mut scope, false)?;
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
            global_const_values,
            Some(&local_consts),
            funcs,
        );

        match result {
            Ok(value) => {
                local_const_values.insert(SharedString::clone(name), value);
            }
            Err(err) => errors.push(err.into()),
        }
    }

    for port in module_item.ports().iter() {
        let result = resolve_type(
            port.ty(),
            type_items,
            &mut scope,
            global_const_values,
            Some(&local_const_values),
            funcs,
            known_types,
            type_order,
            type_queue,
        );

        match result {
            Ok(ty_id) => type_order.add_dependency(ty_id, this_id),
            Err(err) => errors.push(err),
        }
    }

    for member in module_item.members().iter() {
        match member {
            Member::Logic(logic_member) => {
                let result = resolve_type(
                    logic_member.ty(),
                    type_items,
                    &mut scope,
                    global_const_values,
                    Some(&local_const_values),
                    funcs,
                    known_types,
                    type_order,
                    type_queue,
                );

                match result {
                    Ok(ty_id) => type_order.add_dependency(ty_id, this_id),
                    Err(err) => errors.push(err),
                }
            }
            Member::Proc(_) => { /* TODO: */ }
            Member::Comb(_) => { /* TODO: */ }
            _ => {}
        }
    }

    wrap_errors!(
        ResolvedModule::new(local_const_values, Vec::new(), Vec::new()),
        errors
    )
}

pub fn resolve_types<'a>(
    top_module: &'a Module,
    type_items: &'a HashMap<SharedString, TypeItem>,
    global_scope: &Scope,
    global_const_values: &HashMap<SharedString, i64>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> TypecheckResult<'a, ()> {
    let mut known_types = HashMap::default();
    let mut type_order = TopologicalSort::new();
    let mut type_queue = VecDeque::new();

    let top_ty = ResolvedType::Named {
        name: top_module.name().as_string(),
        generic_args: [].as_slice().into(),
    };
    let top_id = TypeId::from_type(&top_ty);
    known_types.insert(top_id, top_ty);
    type_queue.push_back(top_id);

    let mut errors = Vec::new();
    while let Some(ty_id) = type_queue.pop_front() {
        let ty = known_types[&ty_id].clone();

        match ty {
            ResolvedType::Named { name, generic_args } => {
                if let Some(item) = type_items.get(name.as_ref()) {
                    match item {
                        TypeItem::Struct(struct_item) => {
                            // TODO:
                        }
                        TypeItem::Enum(enum_item) => {
                            // TODO:
                        }
                        TypeItem::Module(module_item) => {
                            let result = resolve_module(
                                module_item,
                                &generic_args,
                                type_items,
                                &global_scope,
                                &global_const_values,
                                &funcs,
                                &mut known_types,
                                &mut type_order,
                                &mut type_queue,
                            );

                            match result {
                                Ok(_) => {
                                    // TODO:
                                }
                                Err(err) => errors.push(err),
                            }
                        }
                    }
                } else {
                    unreachable!("invalid resolved type");
                }
            }
            _ => {}
        }
    }

    if errors.len() > 0 {
        return Err(TypecheckError::new_list(errors));
    }

    for (id, ty) in known_types.iter() {
        println!("{}: {}", id, ty.to_string(&known_types));
    }

    Ok(())
}
