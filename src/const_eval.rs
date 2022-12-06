use crate::ast::{AssignKind, Ident, Spanned};
use crate::ir::*;
use crate::{HashMap, SharedString};
use langbox::TextSpan;
use std::convert::Infallible;
use std::ops::{ControlFlow, FromResidual, Try};

pub enum ConstValue<'a, G: Evaluatable, L: Evaluatable> {
    Value(i64),
    GlobalExpr(&'a G),
    LocalExpr(&'a L),
}

pub struct VarScope<'p> {
    parent: Option<&'p mut VarScope<'p>>,
    vars: HashMap<SharedString, i64>,
}

impl<'p> VarScope<'p> {
    pub fn empty() -> Self {
        Self {
            parent: None,
            vars: HashMap::default(),
        }
    }

    pub fn new<'pp: 'p>(parent: &'p mut VarScope<'pp>) -> Self {
        Self {
            parent: Some(unsafe {
                // SAFETY:
                // Here we are breaking the invariance over lifetimes of a mutable reference.
                // This is only safe as long as we do not reassign the reference itself.
                // Since we only use the reference to modify the `vars` HashMap this is fine.
                std::mem::transmute(parent)
            }),
            vars: HashMap::default(),
        }
    }

    pub fn add_var(&mut self, name: &Ident, value: i64) {
        self.vars.insert(name.as_string(), value);
    }

    fn var_inner(&self, name: &Ident) -> Option<i64> {
        self.vars.get(name.as_ref()).copied().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.var_inner(name))
        })
    }

    pub fn var<'a, G: Evaluatable, L: Evaluatable>(
        &self,
        name: &Ident,
        global_consts: &'a HashMap<SharedString, G>,
        local_consts: Option<&'a HashMap<SharedString, L>>,
    ) -> ConstValue<'a, G, L> {
        if let Some(value) = self.var_inner(name) {
            ConstValue::Value(value)
        } else if let Some(expr) = global_consts.get(name.as_ref()) {
            ConstValue::GlobalExpr(expr)
        } else if let Some(expr) =
            local_consts.and_then(|local_consts| local_consts.get(name.as_ref()))
        {
            ConstValue::LocalExpr(expr)
        } else {
            unreachable!("variable not found")
        }
    }

    pub fn var_mut(&mut self, name: &Ident) -> &mut i64 {
        self.vars
            .get_mut(name.as_ref())
            .or_else(|| self.parent.as_mut().map(|parent| parent.var_mut(name)))
            .expect("variable not found")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithmeticError {
    Overflow { expr_span: TextSpan },
    DivideByZero { expr_span: TextSpan },
}

pub enum EvalResult<T> {
    Value(T),
    Continue,
    Break,
    Err(ArithmeticError),
}

impl<T> EvalResult<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> EvalResult<U> {
        match self {
            EvalResult::Value(value) => EvalResult::Value(f(value)),
            EvalResult::Continue => EvalResult::Continue,
            EvalResult::Break => EvalResult::Break,
            EvalResult::Err(err) => EvalResult::Err(err),
        }
    }

    pub fn into_result(self) -> Result<T, ArithmeticError> {
        match self {
            EvalResult::Value(value) => Ok(value),
            EvalResult::Err(err) => Err(err),
            _ => panic!("invalid eval result"),
        }
    }
}

impl<T> FromResidual<EvalResult<Infallible>> for EvalResult<T> {
    fn from_residual(residual: EvalResult<Infallible>) -> Self {
        match residual {
            EvalResult::Value(_) => unreachable!(),
            EvalResult::Continue => EvalResult::Continue,
            EvalResult::Break => EvalResult::Break,
            EvalResult::Err(err) => EvalResult::Err(err),
        }
    }
}

impl<T> Try for EvalResult<T> {
    type Output = T;
    type Residual = EvalResult<Infallible>;

    fn from_output(output: Self::Output) -> Self {
        EvalResult::Value(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            EvalResult::Value(value) => ControlFlow::Continue(value),
            EvalResult::Continue => ControlFlow::Break(EvalResult::Continue),
            EvalResult::Break => ControlFlow::Break(EvalResult::Break),
            EvalResult::Err(err) => ControlFlow::Break(EvalResult::Err(err)),
        }
    }
}

impl<T> From<Result<T, ArithmeticError>> for EvalResult<T> {
    fn from(value: Result<T, ArithmeticError>) -> Self {
        match value {
            Ok(value) => EvalResult::Value(value),
            Err(err) => EvalResult::Err(err),
        }
    }
}

macro_rules! ok {
    ($value:expr) => {
        EvalResult::Value($value)
    };
}

macro_rules! err {
    ($err:expr) => {
        EvalResult::Err($err)
    };
}

#[inline]
fn erroring_add(lhs: i64, rhs: i64, expr_span: TextSpan) -> EvalResult<i64> {
    lhs.checked_add(rhs)
        .ok_or(ArithmeticError::Overflow { expr_span })
        .into()
}

#[inline]
fn erroring_sub(lhs: i64, rhs: i64, expr_span: TextSpan) -> EvalResult<i64> {
    lhs.checked_sub(rhs)
        .ok_or(ArithmeticError::Overflow { expr_span })
        .into()
}

#[inline]
fn erroring_mul(lhs: i64, rhs: i64, expr_span: TextSpan) -> EvalResult<i64> {
    lhs.checked_mul(rhs)
        .ok_or(ArithmeticError::Overflow { expr_span })
        .into()
}

#[inline]
fn erroring_div(lhs: i64, rhs: i64, expr_span: TextSpan) -> EvalResult<i64> {
    if rhs == 0 {
        return err!(ArithmeticError::DivideByZero { expr_span });
    }

    lhs.checked_div(rhs)
        .ok_or(ArithmeticError::Overflow { expr_span })
        .into()
}

#[inline]
fn erroring_rem(lhs: i64, rhs: i64, expr_span: TextSpan) -> EvalResult<i64> {
    if rhs == 0 {
        return err!(ArithmeticError::DivideByZero { expr_span });
    }

    lhs.checked_rem(rhs)
        .ok_or(ArithmeticError::Overflow { expr_span })
        .into()
}

#[inline]
fn erroring_neg(value: i64, expr_span: TextSpan) -> EvalResult<i64> {
    value
        .checked_neg()
        .ok_or(ArithmeticError::Overflow { expr_span })
        .into()
}

pub trait Evaluatable {
    fn eval<G: Evaluatable, L: Evaluatable>(
        &self,
        scope: &mut VarScope,
        global_consts: &HashMap<SharedString, G>,
        local_consts: Option<&HashMap<SharedString, L>>,
        funcs: &HashMap<SharedString, ConstFunc>,
    ) -> EvalResult<i64>;
}

impl Evaluatable for i64 {
    #[inline]
    fn eval<G: Evaluatable, L: Evaluatable>(
        &self,
        _scope: &mut VarScope,
        _global_consts: &HashMap<SharedString, G>,
        _local_consts: Option<&HashMap<SharedString, L>>,
        _funcs: &HashMap<SharedString, ConstFunc>,
    ) -> EvalResult<i64> {
        ok!(*self)
    }
}

impl Evaluatable for ConstExpr {
    #[inline]
    fn eval<G: Evaluatable, L: Evaluatable>(
        &self,
        scope: &mut VarScope,
        global_consts: &HashMap<SharedString, G>,
        local_consts: Option<&HashMap<SharedString, L>>,
        funcs: &HashMap<SharedString, ConstFunc>,
    ) -> EvalResult<i64> {
        eval(self, scope, global_consts, local_consts, funcs)
    }
}

fn pattern_matches<G: Evaluatable, L: Evaluatable>(
    patterns: &[ConstMatchPattern],
    value: i64,
    scope: &mut VarScope,
    global_consts: &HashMap<SharedString, G>,
    local_consts: Option<&HashMap<SharedString, L>>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> EvalResult<bool> {
    for pattern in patterns.iter() {
        match pattern {
            ConstMatchPattern::Literal(l) => {
                if l.value() == value {
                    return ok!(true);
                }
            }
            ConstMatchPattern::Range(start, end) => {
                if (start.value()..end.value()).contains(&value) {
                    return ok!(true);
                }
            }
            ConstMatchPattern::RangeInclusive(start, end) => {
                if (start.value()..=end.value()).contains(&value) {
                    return ok!(true);
                }
            }
            ConstMatchPattern::Ident(ident) => {
                if ident.as_ref() == "_" {
                    return ok!(true);
                }

                if let Some(expr) = global_consts.get(ident.as_ref()) {
                    let pattern_value = expr.eval(scope, global_consts, local_consts, funcs)?;
                    if pattern_value == value {
                        return ok!(true);
                    }
                } else if let Some(expr) =
                    local_consts.and_then(|local_consts| local_consts.get(ident.as_ref()))
                {
                    let pattern_value = expr.eval(scope, global_consts, local_consts, funcs)?;
                    if pattern_value == value {
                        return ok!(true);
                    }
                } else {
                    unreachable!("constant does not exist");
                }
            }
        }
    }

    ok!(false)
}

pub fn eval<G: Evaluatable, L: Evaluatable>(
    expr: &ConstExpr,
    scope: &mut VarScope,
    global_consts: &HashMap<SharedString, G>,
    local_consts: Option<&HashMap<SharedString, L>>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> EvalResult<i64> {
    macro_rules! eval_cmp_expr {
        ($lhs:expr, $rhs:expr, $op:tt) => {{
            let lhs = eval($lhs, scope, global_consts, local_consts, funcs)?;
            let rhs = eval($rhs, scope, global_consts, local_consts, funcs)?;
            ok!((lhs $op rhs) as i64)
        }};
    }

    macro_rules! eval_binary_expr {
        ($lhs:expr, $rhs:expr, $op:tt) => {{
            let lhs = eval($lhs, scope, global_consts, local_consts, funcs)?;
            let rhs = eval($rhs, scope, global_consts, local_consts, funcs)?;
            ok!(lhs $op rhs)
        }};
    }

    macro_rules! eval_binary_fn {
        ($lhs:expr, $rhs:expr, $span:expr, $fn:ident) => {{
            let lhs = eval($lhs, scope, global_consts, local_consts, funcs)?;
            let rhs = eval($rhs, scope, global_consts, local_consts, funcs)?;
            $fn(lhs, rhs, $span)
        }};
    }

    match expr {
        ConstExpr::Value(v) => ok!(*v),
        ConstExpr::Literal(l) => ok!(l.value()),
        ConstExpr::Ident(name) => match scope.var(name, global_consts, local_consts) {
            ConstValue::Value(value) => ok!(value),
            ConstValue::GlobalExpr(expr) => expr.eval(scope, global_consts, local_consts, funcs),
            ConstValue::LocalExpr(expr) => expr.eval(scope, global_consts, local_consts, funcs),
        },
        ConstExpr::Call(expr) => {
            let mut arg_values = Vec::with_capacity(expr.args().len());
            for arg in expr.args().iter() {
                arg_values.push(eval(arg, scope, global_consts, local_consts, funcs)?);
            }

            let func = &funcs[expr.func().as_ref()];

            let mut inner_scope = VarScope::new(scope);
            for (arg, value) in func.args().iter().zip(arg_values.into_iter()) {
                inner_scope.add_var(arg, value);
            }

            eval_expr_block(
                func.body(),
                &mut inner_scope,
                global_consts,
                local_consts,
                funcs,
            )
        }
        ConstExpr::If(expr) => {
            if eval(expr.condition(), scope, global_consts, local_consts, funcs)? != 0 {
                eval_expr_block(expr.body(), scope, global_consts, local_consts, funcs)
            } else {
                for else_if_block in expr.else_if_blocks().iter() {
                    if eval(
                        else_if_block.condition(),
                        scope,
                        global_consts,
                        local_consts,
                        funcs,
                    )? != 0
                    {
                        return eval_expr_block(
                            else_if_block.body(),
                            scope,
                            global_consts,
                            local_consts,
                            funcs,
                        );
                    }
                }

                let else_block = expr.else_block().expect("missing else block");
                eval_expr_block(else_block.body(), scope, global_consts, local_consts, funcs)
            }
        }
        ConstExpr::Match(expr) => {
            let value = eval(expr.value(), scope, global_consts, local_consts, funcs)?;
            for branch in expr.branches().iter() {
                if pattern_matches(
                    branch.patterns(),
                    value,
                    scope,
                    global_consts,
                    local_consts,
                    funcs,
                )? {
                    return match branch.body() {
                        ConstMatchBody::Expr(body) => {
                            eval(body, scope, global_consts, local_consts, funcs)
                        }
                        ConstMatchBody::Block(body) => {
                            eval_expr_block(body, scope, global_consts, local_consts, funcs)
                        }
                    };
                }
            }

            unreachable!("unhandled match case");
        }
        ConstExpr::Block(block) => {
            eval_expr_block(block, scope, global_consts, local_consts, funcs)
        }
        ConstExpr::Neg(expr) => {
            let inner = eval(expr.inner(), scope, global_consts, local_consts, funcs)?;
            erroring_neg(inner, expr.span())
        }
        ConstExpr::Not(expr) => {
            let inner = eval(expr.inner(), scope, global_consts, local_consts, funcs)?;
            ok!(!inner)
        }
        ConstExpr::Lt(expr) => eval_cmp_expr!(expr.lhs(), expr.rhs(), <),
        ConstExpr::Lte(expr) => eval_cmp_expr!(expr.lhs(), expr.rhs(), <=),
        ConstExpr::Gt(expr) => eval_cmp_expr!(expr.lhs(), expr.rhs(), >),
        ConstExpr::Gte(expr) => eval_cmp_expr!(expr.lhs(), expr.rhs(), >=),
        ConstExpr::Eq(expr) => eval_cmp_expr!(expr.lhs(), expr.rhs(), ==),
        ConstExpr::Ne(expr) => eval_cmp_expr!(expr.lhs(), expr.rhs(), !=),
        ConstExpr::Add(expr) => eval_binary_fn!(expr.lhs(), expr.rhs(), expr.span(), erroring_add),
        ConstExpr::Sub(expr) => eval_binary_fn!(expr.lhs(), expr.rhs(), expr.span(), erroring_sub),
        ConstExpr::Mul(expr) => eval_binary_fn!(expr.lhs(), expr.rhs(), expr.span(), erroring_mul),
        ConstExpr::Div(expr) => eval_binary_fn!(expr.lhs(), expr.rhs(), expr.span(), erroring_div),
        ConstExpr::Rem(expr) => eval_binary_fn!(expr.lhs(), expr.rhs(), expr.span(), erroring_rem),
        ConstExpr::And(expr) => eval_binary_expr!(expr.lhs(), expr.rhs(), &),
        ConstExpr::Xor(expr) => eval_binary_expr!(expr.lhs(), expr.rhs(), ^),
        ConstExpr::Or(expr) => eval_binary_expr!(expr.lhs(), expr.rhs(), |),
        ConstExpr::Shl(expr) => eval_binary_expr!(expr.lhs(), expr.rhs(), <<),
        ConstExpr::Lsr(expr) => {
            let lhs = eval(expr.lhs(), scope, global_consts, local_consts, funcs)? as u64;
            let rhs = eval(expr.rhs(), scope, global_consts, local_consts, funcs)? as u64;
            ok!((lhs >> rhs) as i64)
        }
        ConstExpr::Asr(expr) => eval_binary_expr!(expr.lhs(), expr.rhs(), >>),
    }
}

fn eval_statement<G: Evaluatable, L: Evaluatable>(
    statement: &ConstStatement,
    scope: &mut VarScope,
    global_consts: &HashMap<SharedString, G>,
    local_consts: Option<&HashMap<SharedString, L>>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> EvalResult<()> {
    match statement {
        ConstStatement::Expr(expr) => match expr {
            ConstExpr::If(expr) => {
                if eval(expr.condition(), scope, global_consts, local_consts, funcs)? != 0 {
                    eval_statement_block(expr.body(), scope, global_consts, local_consts, funcs)
                } else {
                    for else_if_block in expr.else_if_blocks().iter() {
                        if eval(
                            else_if_block.condition(),
                            scope,
                            global_consts,
                            local_consts,
                            funcs,
                        )? != 0
                        {
                            return eval_statement_block(
                                else_if_block.body(),
                                scope,
                                global_consts,
                                local_consts,
                                funcs,
                            );
                        }
                    }

                    if let Some(else_block) = expr.else_block() {
                        return eval_statement_block(
                            else_block.body(),
                            scope,
                            global_consts,
                            local_consts,
                            funcs,
                        );
                    }

                    ok!(())
                }
            }
            ConstExpr::Match(expr) => {
                let value = eval(expr.value(), scope, global_consts, local_consts, funcs)?;
                for branch in expr.branches().iter() {
                    if pattern_matches(
                        branch.patterns(),
                        value,
                        scope,
                        global_consts,
                        local_consts,
                        funcs,
                    )? {
                        return match branch.body() {
                            ConstMatchBody::Expr(body) => {
                                eval(body, scope, global_consts, local_consts, funcs).map(|_| ())
                            }
                            ConstMatchBody::Block(body) => eval_statement_block(
                                body,
                                scope,
                                global_consts,
                                local_consts,
                                funcs,
                            ),
                        };
                    }
                }

                unreachable!("unhandled match case");
            }
            ConstExpr::Block(block) => {
                eval_statement_block(block, scope, global_consts, local_consts, funcs)
            }
            _ => eval(expr, scope, global_consts, local_consts, funcs).map(|_| ()),
        },
        ConstStatement::Declaration(decl) => {
            let value = eval(decl.value(), scope, global_consts, local_consts, funcs)?;
            scope.add_var(decl.name(), value);
            ok!(())
        }
        ConstStatement::Assignment(assign) => {
            let value = eval(assign.value(), scope, global_consts, local_consts, funcs)?;
            let var = scope.var_mut(assign.target());

            match assign.kind() {
                AssignKind::Assign => *var = value,
                AssignKind::AddAssign => *var = erroring_add(*var, value, assign.span())?,
                AssignKind::SubAssign => *var = erroring_sub(*var, value, assign.span())?,
                AssignKind::MulAssign => *var = erroring_mul(*var, value, assign.span())?,
                AssignKind::DivAssign => *var = erroring_div(*var, value, assign.span())?,
                AssignKind::RemAssign => *var = erroring_rem(*var, value, assign.span())?,
                AssignKind::AndAssign => *var &= value,
                AssignKind::OrAssign => *var |= value,
                AssignKind::XorAssign => *var ^= value,
                AssignKind::ShlAssign => *var <<= value,
                AssignKind::AsrAssign => *var = ((*var as u64) >> (value as u64)) as i64,
                AssignKind::LsrAssign => *var >>= value,
            }

            ok!(())
        }
        ConstStatement::WhileLoop(while_loop) => {
            while eval(
                while_loop.condition(),
                scope,
                global_consts,
                local_consts,
                funcs,
            )? != 0
            {
                match eval_statement_block(
                    while_loop.body(),
                    scope,
                    global_consts,
                    local_consts,
                    funcs,
                ) {
                    EvalResult::Value(_) => {}
                    EvalResult::Continue => continue,
                    EvalResult::Break => break,
                    EvalResult::Err(err) => return err!(err),
                }
            }

            ok!(())
        }
        ConstStatement::ForLoop(for_loop) => {
            let (start, end, inclusive) = match for_loop.range() {
                ConstForLoopRange::Range(start, end) => (start, end, false),
                ConstForLoopRange::RangeInclusive(start, end) => (start, end, true),
            };

            let start = eval(start, scope, global_consts, local_consts, funcs)?;
            let end = eval(end, scope, global_consts, local_consts, funcs)?;

            for loop_index in start..=end {
                if (loop_index < end) || inclusive {
                    let mut inner_scope = VarScope::new(scope);
                    inner_scope.add_var(for_loop.item_name(), loop_index);

                    match eval_statement_block(
                        for_loop.body(),
                        &mut inner_scope,
                        global_consts,
                        local_consts,
                        funcs,
                    ) {
                        EvalResult::Value(_) => {}
                        EvalResult::Continue => continue,
                        EvalResult::Break => break,
                        EvalResult::Err(err) => return err!(err),
                    }
                }
            }

            ok!(())
        }
        ConstStatement::Continue(_) => EvalResult::Continue,
        ConstStatement::Break(_) => EvalResult::Break,
    }
}

fn eval_block<G: Evaluatable, L: Evaluatable>(
    block: &ConstBlock,
    parent_scope: &mut VarScope,
    global_consts: &HashMap<SharedString, G>,
    local_consts: Option<&HashMap<SharedString, L>>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> EvalResult<Option<i64>> {
    let mut scope = VarScope::new(parent_scope);
    for statement in block.statements().iter() {
        eval_statement(statement, &mut scope, global_consts, local_consts, funcs)?;
    }

    if let Some(result) = block.result() {
        ok!(Some(eval(
            result,
            &mut scope,
            global_consts,
            local_consts,
            funcs,
        )?))
    } else {
        ok!(None)
    }
}

fn eval_expr_block<G: Evaluatable, L: Evaluatable>(
    block: &ConstBlock,
    parent_scope: &mut VarScope,
    global_consts: &HashMap<SharedString, G>,
    local_consts: Option<&HashMap<SharedString, L>>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> EvalResult<i64> {
    ok!(
        eval_block(block, parent_scope, global_consts, local_consts, funcs)?
            .expect("block is not an expression")
    )
}

fn eval_statement_block<G: Evaluatable, L: Evaluatable>(
    block: &ConstBlock,
    parent_scope: &mut VarScope,
    global_consts: &HashMap<SharedString, G>,
    local_consts: Option<&HashMap<SharedString, L>>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> EvalResult<()> {
    let result = eval_block(block, parent_scope, global_consts, local_consts, funcs)?;
    assert!(result.is_none(), "block is not a statement");
    ok!(())
}
