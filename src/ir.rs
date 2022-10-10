#![allow(dead_code)]

use crate::ast::*;
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct ConstCallExpr {
    func: Ident,
    args: Vec<ConstExpr>,
}

impl ConstCallExpr {
    #[inline]
    pub fn new(func: Ident, args: Vec<ConstExpr>) -> Self {
        Self { func, args }
    }

    #[inline]
    pub fn func(&self) -> &Ident {
        &self.func
    }

    #[inline]
    pub fn args(&self) -> &[ConstExpr] {
        &self.args
    }
}

#[derive(Debug, Clone)]
pub struct ConstFieldAssign {
    field: Ident,
    value: ConstExpr,
}

impl ConstFieldAssign {
    #[inline]
    pub fn new(field: Ident, value: ConstExpr) -> Self {
        Self { field, value }
    }

    #[inline]
    pub fn field(&self) -> &Ident {
        &self.field
    }

    #[inline]
    pub fn value(&self) -> &ConstExpr {
        &self.value
    }
}

#[derive(Debug, Clone)]
pub struct ConstElseIfBlock {
    condition: Box<ConstExpr>,
    body: Box<ConstBlock>,
}

impl ConstElseIfBlock {
    #[inline]
    pub fn new(condition: ConstExpr, body: ConstBlock) -> Self {
        Self {
            condition: Box::new(condition),
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn condition(&self) -> &ConstExpr {
        &self.condition
    }

    #[inline]
    pub fn body(&self) -> &ConstBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct ConstElseBlock {
    body: Box<ConstBlock>,
}

impl ConstElseBlock {
    #[inline]
    pub fn new(body: ConstBlock) -> Self {
        Self {
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn body(&self) -> &ConstBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct ConstIfExpr {
    condition: Box<ConstExpr>,
    body: Box<ConstBlock>,
    else_if_blocks: Vec<ConstElseIfBlock>,
    else_block: Option<ConstElseBlock>,
}

impl ConstIfExpr {
    #[inline]
    pub fn new(
        condition: ConstExpr,
        body: ConstBlock,
        else_if_blocks: Vec<ConstElseIfBlock>,
        else_block: Option<ConstElseBlock>,
    ) -> Self {
        Self {
            condition: Box::new(condition),
            body: Box::new(body),
            else_if_blocks,
            else_block,
        }
    }

    #[inline]
    pub fn condition(&self) -> &ConstExpr {
        &self.condition
    }

    #[inline]
    pub fn body(&self) -> &ConstBlock {
        &self.body
    }

    #[inline]
    pub fn else_if_blocks(&self) -> &[ConstElseIfBlock] {
        &self.else_if_blocks
    }

    #[inline]
    pub fn else_block(&self) -> Option<&ConstElseBlock> {
        self.else_block.as_ref()
    }
}

#[derive(Debug, Clone)]
pub enum ConstMatchPattern {
    Literal(Literal),
    Ident(Ident),
}

#[derive(Debug, Clone)]
pub enum ConstMatchBody {
    Expr(ConstExpr),
    Block(ConstBlock),
}

#[derive(Debug, Clone)]
pub struct ConstMatchBranch {
    patterns: Vec<ConstMatchPattern>,
    body: Box<ConstMatchBody>,
}

impl ConstMatchBranch {
    #[inline]
    pub fn new(patterns: Vec<ConstMatchPattern>, body: ConstMatchBody) -> Self {
        Self {
            patterns,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn patterns(&self) -> &[ConstMatchPattern] {
        &self.patterns
    }

    #[inline]
    pub fn body(&self) -> &ConstMatchBody {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct ConstMatchExpr {
    value: Box<ConstExpr>,
    branches: Vec<ConstMatchBranch>,
}

impl ConstMatchExpr {
    #[inline]
    pub fn new(value: ConstExpr, branches: Vec<ConstMatchBranch>) -> Self {
        Self {
            value: Box::new(value),
            branches,
        }
    }

    #[inline]
    pub fn value(&self) -> &ConstExpr {
        &self.value
    }

    #[inline]
    pub fn branches(&self) -> &[ConstMatchBranch] {
        &self.branches
    }
}

#[derive(Debug, Clone)]
pub struct ConstUnaryExpr {
    inner: Box<ConstExpr>,
}

impl ConstUnaryExpr {
    #[inline]
    pub fn new(inner: ConstExpr) -> Self {
        Self {
            inner: Box::new(inner),
        }
    }

    #[inline]
    pub fn inner(&self) -> &ConstExpr {
        &self.inner
    }
}

#[derive(Debug, Clone)]
pub struct ConstBinaryExpr {
    lhs: Box<ConstExpr>,
    rhs: Box<ConstExpr>,
}

impl ConstBinaryExpr {
    #[inline]
    pub fn new(lhs: ConstExpr, rhs: ConstExpr) -> Self {
        Self {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    #[inline]
    pub fn lhs(&self) -> &ConstExpr {
        &self.lhs
    }

    #[inline]
    pub fn rhs(&self) -> &ConstExpr {
        &self.rhs
    }
}

#[derive(Debug, Clone)]
pub enum ConstExpr {
    // Leaf expressions
    Literal(Literal),
    Ident(Ident),
    Call(ConstCallExpr),

    If(ConstIfExpr),
    Match(ConstMatchExpr),
    Block(Box<ConstBlock>),

    // Operators
    Neg(ConstUnaryExpr),
    Not(ConstUnaryExpr),
    Lt(ConstBinaryExpr),
    Lte(ConstBinaryExpr),
    Gt(ConstBinaryExpr),
    Gte(ConstBinaryExpr),
    Eq(ConstBinaryExpr),
    Ne(ConstBinaryExpr),
    Add(ConstBinaryExpr),
    Sub(ConstBinaryExpr),
    Mul(ConstBinaryExpr),
    Div(ConstBinaryExpr),
    Rem(ConstBinaryExpr),
    And(ConstBinaryExpr),
    Xor(ConstBinaryExpr),
    Or(ConstBinaryExpr),
    Shl(ConstBinaryExpr),
    Lsr(ConstBinaryExpr),
    Asr(ConstBinaryExpr),
}

#[derive(Debug, Clone)]
pub struct ConstDeclaration {
    name: Ident,
    value: ConstExpr,
}

impl ConstDeclaration {
    #[inline]
    pub fn new(name: Ident, value: ConstExpr) -> Self {
        Self { name, value }
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn value(&self) -> &ConstExpr {
        &self.value
    }
}

#[derive(Debug, Clone)]
pub struct ConstAssignment {
    target: Ident,
    kind: AssignKind,
    value: ConstExpr,
}

impl ConstAssignment {
    #[inline]
    pub fn new(target: Ident, kind: AssignKind, value: ConstExpr) -> Self {
        Self {
            target,
            kind,
            value,
        }
    }

    #[inline]
    pub fn target(&self) -> &Ident {
        &self.target
    }

    #[inline]
    pub fn kind(&self) -> AssignKind {
        self.kind
    }

    #[inline]
    pub fn value(&self) -> &ConstExpr {
        &self.value
    }
}

#[derive(Debug, Clone)]
pub struct ConstWhileLoop {
    condition: ConstExpr,
    body: Box<ConstBlock>,
}

impl ConstWhileLoop {
    #[inline]
    pub fn new(condition: ConstExpr, body: ConstBlock) -> Self {
        Self {
            condition,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn condition(&self) -> &ConstExpr {
        &self.condition
    }

    #[inline]
    pub fn body(&self) -> &ConstBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct ConstForLoop {
    item_name: Ident,
    range: Range<ConstExpr>,
    body: Box<ConstBlock>,
}

impl ConstForLoop {
    #[inline]
    pub fn new(item_name: Ident, range: Range<ConstExpr>, body: ConstBlock) -> Self {
        Self {
            item_name,
            range,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn item_name(&self) -> &Ident {
        &self.item_name
    }

    #[inline]
    pub fn range(&self) -> &Range<ConstExpr> {
        &self.range
    }

    #[inline]
    pub fn body(&self) -> &ConstBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub enum ConstStatement {
    Expr(ConstExpr),
    Declaration(ConstDeclaration),
    Assignment(ConstAssignment),
    WhileLoop(ConstWhileLoop),
    ForLoop(ConstForLoop),
}

#[derive(Debug, Clone)]
pub struct ConstBlock {
    statements: Vec<ConstStatement>,
    result: Option<ConstExpr>,
}

impl ConstBlock {
    #[inline]
    pub fn new(statements: Vec<ConstStatement>, result: Option<ConstExpr>) -> Self {
        Self { statements, result }
    }

    #[inline]
    pub fn statements(&self) -> &[ConstStatement] {
        &self.statements
    }

    #[inline]
    pub fn result(&self) -> Option<&ConstExpr> {
        self.result.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct ConstFunc {
    args: Vec<Ident>,
    body: Box<ConstBlock>,
}

impl ConstFunc {
    #[inline]
    pub fn new(args: Vec<Ident>, body: ConstBlock) -> Self {
        Self {
            args,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn args(&self) -> &[Ident] {
        &self.args
    }

    #[inline]
    pub fn body(&self) -> &ConstBlock {
        &self.body
    }
}
