use crate::ast::{AssignKind, Ident, Spanned};
use crate::ir::*;
use crate::SharedString;
use langbox::TextSpan;
use std::collections::HashMap;

pub enum ConstValue<'a> {
    Value(i64),
    Expr(&'a ConstExpr),
}

pub struct VarScope<'p> {
    parent: Option<&'p mut VarScope<'p>>,
    vars: HashMap<SharedString, i64>,
}

impl<'p> VarScope<'p> {
    pub fn empty() -> Self {
        Self {
            parent: None,
            vars: HashMap::new(),
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
            vars: HashMap::new(),
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

    pub fn var<'a>(
        &self,
        name: &Ident,
        global_consts: &'a HashMap<SharedString, ConstExpr>,
        local_consts: Option<&'a HashMap<SharedString, ConstExpr>>,
    ) -> ConstValue<'a> {
        if let Some(value) = self.var_inner(name) {
            ConstValue::Value(value)
        } else if let Some(expr) = global_consts.get(name.as_ref()) {
            ConstValue::Expr(expr)
        } else if let Some(expr) =
            local_consts.and_then(|local_consts| local_consts.get(name.as_ref()))
        {
            ConstValue::Expr(expr)
        } else {
            unreachable!("variable not found")
        }
    }

    pub fn var_mut(&mut self, name: &Ident) -> &mut i64 {
        self.vars
            .get_mut(name.as_ref())
            .or_else(|| {
                self.parent
                    .as_mut()
                    .and_then(|parent| Some(parent.var_mut(name)))
            })
            .expect("variable not found")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithmeticError {
    Overflow { expr_span: TextSpan },
    DivideByZero { expr_span: TextSpan },
}

pub type ArithmeticResult<T> = Result<T, ArithmeticError>;

#[inline]
fn erroring_add(lhs: i64, rhs: i64, expr_span: TextSpan) -> ArithmeticResult<i64> {
    lhs.checked_add(rhs)
        .ok_or(ArithmeticError::Overflow { expr_span })
}

#[inline]
fn erroring_sub(lhs: i64, rhs: i64, expr_span: TextSpan) -> ArithmeticResult<i64> {
    lhs.checked_sub(rhs)
        .ok_or(ArithmeticError::Overflow { expr_span })
}

#[inline]
fn erroring_mul(lhs: i64, rhs: i64, expr_span: TextSpan) -> ArithmeticResult<i64> {
    lhs.checked_mul(rhs)
        .ok_or(ArithmeticError::Overflow { expr_span })
}

#[inline]
fn erroring_div(lhs: i64, rhs: i64, expr_span: TextSpan) -> ArithmeticResult<i64> {
    if rhs == 0 {
        return Err(ArithmeticError::DivideByZero { expr_span });
    }

    lhs.checked_div(rhs)
        .ok_or(ArithmeticError::Overflow { expr_span })
}

#[inline]
fn erroring_rem(lhs: i64, rhs: i64, expr_span: TextSpan) -> ArithmeticResult<i64> {
    if rhs == 0 {
        return Err(ArithmeticError::DivideByZero { expr_span });
    }

    lhs.checked_rem(rhs)
        .ok_or(ArithmeticError::Overflow { expr_span })
}

#[inline]
fn erroring_neg(value: i64, expr_span: TextSpan) -> ArithmeticResult<i64> {
    value
        .checked_neg()
        .ok_or(ArithmeticError::Overflow { expr_span })
}

fn pattern_matches(
    patterns: &[ConstMatchPattern],
    value: i64,
    scope: &mut VarScope,
    consts: &HashMap<SharedString, ConstExpr>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> ArithmeticResult<bool> {
    for pattern in patterns.iter() {
        match pattern {
            ConstMatchPattern::Literal(l) => {
                if l.value() == value {
                    return Ok(true);
                }
            }
            ConstMatchPattern::Ident(ident) => {
                if ident.as_ref() == "_" {
                    return Ok(true);
                }

                let pattern_value = eval(&consts[ident.as_ref()], scope, consts, funcs)?;
                if pattern_value == value {
                    return Ok(true);
                }
            }
        }
    }

    Ok(false)
}

pub fn eval(
    expr: &ConstExpr,
    scope: &mut VarScope,
    consts: &HashMap<SharedString, ConstExpr>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> ArithmeticResult<i64> {
    macro_rules! eval_cmp_expr {
        ($lhs:expr, $rhs:expr, $op:tt) => {{
            let lhs = eval($lhs, scope, consts, funcs)?;
            let rhs = eval($rhs, scope, consts, funcs)?;
            Ok((lhs $op rhs) as i64)
        }};
    }

    macro_rules! eval_binary_expr {
        ($lhs:expr, $rhs:expr, $op:tt) => {{
            let lhs = eval($lhs, scope, consts, funcs)?;
            let rhs = eval($rhs, scope, consts, funcs)?;
            Ok(lhs $op rhs)
        }};
    }

    macro_rules! eval_binary_fn {
        ($lhs:expr, $rhs:expr, $span:expr, $fn:ident) => {{
            let lhs = eval($lhs, scope, consts, funcs)?;
            let rhs = eval($rhs, scope, consts, funcs)?;
            $fn(lhs, rhs, $span)
        }};
    }

    match expr {
        ConstExpr::Literal(l) => Ok(l.value()),
        ConstExpr::Ident(name) => match scope.var(name, consts, None) {
            ConstValue::Value(value) => Ok(value),
            ConstValue::Expr(expr) => eval(expr, scope, consts, funcs),
        },
        ConstExpr::Call(expr) => {
            let mut arg_values = Vec::with_capacity(expr.args().len());
            for arg in expr.args().iter() {
                arg_values.push(eval(arg, scope, consts, funcs)?);
            }

            let func = &funcs[expr.func().as_ref()];

            let mut inner_scope = VarScope::new(scope);
            for (arg, value) in func.args().iter().zip(arg_values.into_iter()) {
                inner_scope.add_var(arg, value);
            }

            eval_expr_block(func.body(), &mut inner_scope, consts, funcs)
        }
        ConstExpr::If(expr) => {
            if eval(expr.condition(), scope, consts, funcs)? != 0 {
                eval_expr_block(expr.body(), scope, consts, funcs)
            } else {
                for else_if_block in expr.else_if_blocks().iter() {
                    if eval(else_if_block.condition(), scope, consts, funcs)? != 0 {
                        return eval_expr_block(else_if_block.body(), scope, consts, funcs);
                    }
                }

                let else_block = expr.else_block().expect("missing else block");
                eval_expr_block(else_block.body(), scope, consts, funcs)
            }
        }
        ConstExpr::Match(expr) => {
            let value = eval(expr.value(), scope, consts, funcs)?;
            for branch in expr.branches().iter() {
                if pattern_matches(branch.patterns(), value, scope, consts, funcs)? {
                    return match branch.body() {
                        ConstMatchBody::Expr(body) => eval(body, scope, consts, funcs),
                        ConstMatchBody::Block(body) => eval_expr_block(body, scope, consts, funcs),
                    };
                }
            }

            unreachable!("unhandled match case");
        }
        ConstExpr::Block(block) => eval_expr_block(&block, scope, consts, funcs),
        ConstExpr::Neg(expr) => {
            let inner = eval(expr.inner(), scope, consts, funcs)?;
            erroring_neg(inner, expr.span())
        }
        ConstExpr::Not(expr) => {
            let inner = eval(expr.inner(), scope, consts, funcs)?;
            Ok(!inner)
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
            let lhs = eval(expr.lhs(), scope, consts, funcs)? as u64;
            let rhs = eval(expr.rhs(), scope, consts, funcs)? as u64;
            Ok((lhs >> rhs) as i64)
        }
        ConstExpr::Asr(expr) => eval_binary_expr!(expr.lhs(), expr.rhs(), >>),
    }
}

fn eval_statement(
    statement: &ConstStatement,
    scope: &mut VarScope,
    consts: &HashMap<SharedString, ConstExpr>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> ArithmeticResult<()> {
    match statement {
        ConstStatement::Expr(expr) => match expr {
            ConstExpr::If(expr) => {
                if eval(expr.condition(), scope, consts, funcs)? != 0 {
                    eval_statement_block(expr.body(), scope, consts, funcs)
                } else {
                    for else_if_block in expr.else_if_blocks().iter() {
                        if eval(else_if_block.condition(), scope, consts, funcs)? != 0 {
                            return eval_statement_block(
                                else_if_block.body(),
                                scope,
                                consts,
                                funcs,
                            );
                        }
                    }

                    if let Some(else_block) = expr.else_block() {
                        return eval_statement_block(else_block.body(), scope, consts, funcs);
                    }

                    Ok(())
                }
            }
            ConstExpr::Match(expr) => {
                let value = eval(expr.value(), scope, consts, funcs)?;
                for branch in expr.branches().iter() {
                    if pattern_matches(branch.patterns(), value, scope, consts, funcs)? {
                        return match branch.body() {
                            ConstMatchBody::Expr(body) => {
                                eval(body, scope, consts, funcs).map(|_| ())
                            }
                            ConstMatchBody::Block(body) => {
                                eval_statement_block(body, scope, consts, funcs)
                            }
                        };
                    }
                }

                unreachable!("unhandled match case");
            }
            ConstExpr::Block(block) => eval_statement_block(&block, scope, consts, funcs),
            _ => eval(expr, scope, consts, funcs).map(|_| ()),
        },
        ConstStatement::Declaration(decl) => {
            let value = eval(decl.value(), scope, consts, funcs)?;
            scope.add_var(decl.name(), value);
            Ok(())
        }
        ConstStatement::Assignment(assign) => {
            let value = eval(assign.value(), scope, consts, funcs)?;
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

            Ok(())
        }
        ConstStatement::WhileLoop(while_loop) => {
            while eval(while_loop.condition(), scope, consts, funcs)? != 0 {
                eval_statement_block(while_loop.body(), scope, consts, funcs)?;
            }

            Ok(())
        }
        ConstStatement::ForLoop(for_loop) => {
            let start = eval(&for_loop.range().start, scope, consts, funcs)?;
            let end = eval(&for_loop.range().end, scope, consts, funcs)?;

            for loop_index in start..end {
                let mut inner_scope = VarScope::new(scope);
                inner_scope.add_var(for_loop.item_name(), loop_index);

                eval_statement_block(for_loop.body(), &mut inner_scope, consts, funcs)?;
            }

            Ok(())
        }
    }
}

fn eval_block(
    block: &ConstBlock,
    parent_scope: &mut VarScope,
    consts: &HashMap<SharedString, ConstExpr>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> ArithmeticResult<Option<i64>> {
    let mut scope = VarScope::new(parent_scope);
    for statement in block.statements().iter() {
        eval_statement(statement, &mut scope, consts, funcs);
    }

    if let Some(result) = block.result() {
        Ok(Some(eval(result, &mut scope, consts, funcs)?))
    } else {
        Ok(None)
    }
}

fn eval_expr_block(
    block: &ConstBlock,
    parent_scope: &mut VarScope,
    consts: &HashMap<SharedString, ConstExpr>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> ArithmeticResult<i64> {
    Ok(eval_block(block, parent_scope, consts, funcs)?.expect("block is not an expression"))
}

fn eval_statement_block(
    block: &ConstBlock,
    parent_scope: &mut VarScope,
    consts: &HashMap<SharedString, ConstExpr>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> ArithmeticResult<()> {
    let result = eval_block(block, parent_scope, consts, funcs)?;
    assert!(result.is_none(), "block is not a statement");
    Ok(())
}
