#![allow(dead_code)]

use crate::ast::EdgeKind;
use crate::ir::TypeId;
use crate::SharedString;
use std::ops::Range;

#[derive(Debug, Clone, Copy)]
pub struct VLiteral {
    value: i64,
    width: u64,
}

impl VLiteral {
    #[inline]
    pub fn new(value: i64, width: u64) -> Self {
        Self { value, width }
    }

    #[inline]
    pub fn value(&self) -> i64 {
        self.value
    }

    #[inline]
    pub fn width(&self) -> u64 {
        self.width
    }
}

impl std::fmt::Display for VLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}'d{}", self.width, self.value)
    }
}

#[derive(Debug, Clone)]
pub struct VIndexExpr {
    base: Box<VExpr>,
    indexer: Box<VIndexKind>,
}

impl VIndexExpr {
    #[inline]
    pub fn new(base: VExpr, indexer: VIndexKind) -> Self {
        Self {
            base: Box::new(base),
            indexer: Box::new(indexer),
        }
    }

    #[inline]
    pub fn base(&self) -> &VExpr {
        &self.base
    }

    #[inline]
    pub fn indexer(&self) -> &VIndexKind {
        &self.indexer
    }
}

#[derive(Debug, Clone)]
pub struct VMemberAccessExpr {
    base: Box<VExpr>,
    member: SharedString,
}

impl VMemberAccessExpr {
    #[inline]
    pub fn new(base: VExpr, member: SharedString) -> Self {
        Self {
            base: Box::new(base),
            member,
        }
    }

    #[inline]
    pub fn base(&self) -> &VExpr {
        &self.base
    }

    #[inline]
    pub fn member(&self) -> &SharedString {
        &self.member
    }
}

#[derive(Debug, Clone)]
pub struct VUnaryExpr {
    inner: Box<VExpr>,
}

impl VUnaryExpr {
    #[inline]
    pub fn new(inner: VExpr) -> Self {
        Self {
            inner: Box::new(inner),
        }
    }

    #[inline]
    pub fn inner(&self) -> &VExpr {
        &self.inner
    }
}

#[derive(Debug, Clone)]
pub struct VBinaryExpr {
    lhs: Box<VExpr>,
    rhs: Box<VExpr>,
}

impl VBinaryExpr {
    #[inline]
    pub fn new(lhs: VExpr, rhs: VExpr) -> Self {
        Self {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    #[inline]
    pub fn lhs(&self) -> &VExpr {
        &self.lhs
    }

    #[inline]
    pub fn rhs(&self) -> &VExpr {
        &self.rhs
    }
}

#[derive(Debug, Clone)]
pub enum VExpr {
    Value(i64),
    Literal(VLiteral),
    Ident(SharedString),
    Index(VIndexExpr),
    MemberAccess(VMemberAccessExpr),
    Neg(VUnaryExpr),
    Not(VUnaryExpr),
    Concat(VBinaryExpr),
    Lt(VBinaryExpr),
    Lte(VBinaryExpr),
    Gt(VBinaryExpr),
    Gte(VBinaryExpr),
    Slt(VBinaryExpr),
    Slte(VBinaryExpr),
    Sgt(VBinaryExpr),
    Sgte(VBinaryExpr),
    Eq(VBinaryExpr),
    Ne(VBinaryExpr),
    Add(VBinaryExpr),
    Sub(VBinaryExpr),
    Mul(VBinaryExpr),
    Div(VBinaryExpr),
    Rem(VBinaryExpr),
    And(VBinaryExpr),
    Xor(VBinaryExpr),
    Or(VBinaryExpr),
    Shl(VBinaryExpr),
    Lsr(VBinaryExpr),
    Asr(VBinaryExpr),
}

#[derive(Debug, Clone)]
pub struct VBlock {
    statements: Vec<VStatement>,
}

impl VBlock {
    #[inline]
    pub fn new(statements: Vec<VStatement>) -> Self {
        Self { statements }
    }

    #[inline]
    pub fn statements(&self) -> &[VStatement] {
        &self.statements
    }
}

#[derive(Debug, Clone)]
pub struct VIfStatement {
    condition: Box<VExpr>,
    body: Box<VBlock>,
    else_if_blocks: Vec<(VExpr, VBlock)>,
    else_block: Option<VBlock>,
}

impl VIfStatement {
    #[inline]
    pub fn new(
        condition: VExpr,
        body: VBlock,
        else_if_blocks: Vec<(VExpr, VBlock)>,
        else_block: Option<VBlock>,
    ) -> Self {
        Self {
            condition: Box::new(condition),
            body: Box::new(body),
            else_if_blocks,
            else_block,
        }
    }

    #[inline]
    pub fn condition(&self) -> &VExpr {
        &self.condition
    }

    #[inline]
    pub fn body(&self) -> &VBlock {
        &self.body
    }

    #[inline]
    pub fn else_if_blocks(&self) -> &[(VExpr, VBlock)] {
        &self.else_if_blocks
    }

    #[inline]
    pub fn else_block(&self) -> Option<&VBlock> {
        self.else_block.as_ref()
    }
}

#[derive(Debug, Clone)]
pub enum VCasePattern {
    Literal(VLiteral),
    Ident(SharedString),
}

#[derive(Debug, Clone)]
pub struct VCaseBranch {
    patterns: Vec<VCasePattern>,
    body: Box<VBlock>,
}

impl VCaseBranch {
    #[inline]
    pub fn new(patterns: Vec<VCasePattern>, body: VBlock) -> Self {
        Self {
            patterns,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn patterns(&self) -> &[VCasePattern] {
        &self.patterns
    }

    #[inline]
    pub fn body(&self) -> &VBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct VCaseStatement {
    value: Box<VExpr>,
    branches: Vec<VCaseBranch>,
}

impl VCaseStatement {
    #[inline]
    pub fn new(value: VExpr, branches: Vec<VCaseBranch>) -> Self {
        Self {
            value: Box::new(value),
            branches,
        }
    }

    #[inline]
    pub fn value(&self) -> &VExpr {
        &self.value
    }

    #[inline]
    pub fn branches(&self) -> &[VCaseBranch] {
        &self.branches
    }
}

#[derive(Debug, Clone)]
pub enum VIndexKind {
    Single(VExpr),
    Range(Range<i64>),
}

#[derive(Debug, Clone)]
pub enum VSuffixOp {
    Indexer(VIndexKind),
    MemberAccess(SharedString),
}

#[derive(Debug, Clone)]
pub struct VAssignTarget {
    base: SharedString,
    suffixes: Vec<VSuffixOp>,
}

impl VAssignTarget {
    #[inline]
    pub fn new(base: SharedString, suffixes: Vec<VSuffixOp>) -> Self {
        Self { base, suffixes }
    }

    #[inline]
    pub fn base(&self) -> &SharedString {
        &self.base
    }

    #[inline]
    pub fn suffixes(&self) -> &[VSuffixOp] {
        &self.suffixes
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VAssignMode {
    Sequential,
    Combinatoric,
}

#[derive(Debug, Clone)]
pub struct VAssignment {
    target: VAssignTarget,
    mode: VAssignMode,
    value: VExpr,
}

impl VAssignment {
    #[inline]
    pub fn new(target: VAssignTarget, mode: VAssignMode, value: VExpr) -> Self {
        Self {
            target,
            mode,
            value,
        }
    }

    #[inline]
    pub fn target(&self) -> &VAssignTarget {
        &self.target
    }

    #[inline]
    pub fn mode(&self) -> VAssignMode {
        self.mode
    }

    #[inline]
    pub fn value(&self) -> &VExpr {
        &self.value
    }
}

#[derive(Debug, Clone)]
pub enum VStatement {
    Block(VBlock),
    If(VIfStatement),
    Case(VCaseStatement),
    Assignment(VAssignment),
}

#[derive(Debug, Clone)]
pub struct VSens {
    target: VAssignTarget,
    edge: EdgeKind,
}

impl VSens {
    #[inline]
    pub fn new(target: VAssignTarget, edge: EdgeKind) -> Self {
        Self { target, edge }
    }

    #[inline]
    pub fn target(&self) -> &VAssignTarget {
        &self.target
    }

    #[inline]
    pub fn edge(&self) -> EdgeKind {
        self.edge
    }
}

#[derive(Debug, Clone)]
pub struct VFFMember {
    sens: Vec<VSens>,
    body: VBlock,
}

impl VFFMember {
    #[inline]
    pub fn new(sens: Vec<VSens>, body: VBlock) -> Self {
        Self { sens, body }
    }

    #[inline]
    pub fn sens(&self) -> &[VSens] {
        &self.sens
    }

    #[inline]
    pub fn body(&self) -> &VBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct VModule {
    tmp_members: Vec<(SharedString, TypeId)>,
    tmp_statements: VBlock,
    ff_members: Vec<VFFMember>,
    comb_members: Vec<VBlock>,
}

impl VModule {
    #[inline]
    pub fn new(
        tmp_members: Vec<(SharedString, TypeId)>,
        tmp_statements: VBlock,
        ff_members: Vec<VFFMember>,
        comb_members: Vec<VBlock>,
    ) -> Self {
        Self {
            tmp_members,
            tmp_statements,
            ff_members,
            comb_members,
        }
    }

    #[inline]
    pub fn tmp_members(&self) -> &[(SharedString, TypeId)] {
        &self.tmp_members
    }

    #[inline]
    pub fn tmp_statements(&self) -> &VBlock {
        &self.tmp_statements
    }

    #[inline]
    pub fn ff_members(&self) -> &[VFFMember] {
        &self.ff_members
    }

    #[inline]
    pub fn comb_members(&self) -> &[VBlock] {
        &self.comb_members
    }
}
