#![allow(dead_code)]

use crate::ast::*;
use crate::{default_spanned_impl, HashMap, SharedString};
use langbox::TextSpan;
use once_cell::sync::Lazy;
use std::borrow::Cow;
use std::ops::Range;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ConstCallExpr {
    func: Ident,
    args: Vec<ConstExpr>,
    span: TextSpan,
}

impl ConstCallExpr {
    #[inline]
    pub fn new(func: Ident, args: Vec<ConstExpr>, span: TextSpan) -> Self {
        Self { func, args, span }
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

default_spanned_impl!(ConstCallExpr);

#[derive(Debug, Clone)]
pub struct ConstFieldAssign {
    field: Ident,
    value: ConstExpr,
    span: TextSpan,
}

impl ConstFieldAssign {
    #[inline]
    pub fn new(field: Ident, value: ConstExpr, span: TextSpan) -> Self {
        Self { field, value, span }
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

default_spanned_impl!(ConstFieldAssign);

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
    span: TextSpan,
}

impl ConstIfExpr {
    #[inline]
    pub fn new(
        condition: ConstExpr,
        body: ConstBlock,
        else_if_blocks: Vec<ConstElseIfBlock>,
        else_block: Option<ConstElseBlock>,
        span: TextSpan,
    ) -> Self {
        Self {
            condition: Box::new(condition),
            body: Box::new(body),
            else_if_blocks,
            else_block,
            span,
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

default_spanned_impl!(ConstIfExpr);

#[derive(Debug, Clone)]
pub enum ConstMatchPattern {
    Literal(Literal),
    Range(Literal, Literal),
    RangeInclusive(Literal, Literal),
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
    span: TextSpan,
}

impl ConstMatchExpr {
    #[inline]
    pub fn new(value: ConstExpr, branches: Vec<ConstMatchBranch>, span: TextSpan) -> Self {
        Self {
            value: Box::new(value),
            branches,
            span,
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

default_spanned_impl!(ConstMatchExpr);

#[derive(Debug, Clone)]
pub struct ConstUnaryExpr {
    inner: Box<ConstExpr>,
    span: TextSpan,
}

impl ConstUnaryExpr {
    #[inline]
    pub fn new(inner: ConstExpr, span: TextSpan) -> Self {
        Self {
            inner: Box::new(inner),
            span,
        }
    }

    #[inline]
    pub fn inner(&self) -> &ConstExpr {
        &self.inner
    }
}

default_spanned_impl!(ConstUnaryExpr);

#[derive(Debug, Clone)]
pub struct ConstBinaryExpr {
    lhs: Box<ConstExpr>,
    rhs: Box<ConstExpr>,
    span: TextSpan,
}

impl ConstBinaryExpr {
    #[inline]
    pub fn new(lhs: ConstExpr, rhs: ConstExpr, span: TextSpan) -> Self {
        Self {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
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

default_spanned_impl!(ConstBinaryExpr);

#[derive(Debug, Clone)]
pub enum ConstExpr {
    Value(i64), // Value that was inserted by the compiler

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
    span: TextSpan,
}

impl ConstAssignment {
    #[inline]
    pub fn new(target: Ident, kind: AssignKind, value: ConstExpr, span: TextSpan) -> Self {
        Self {
            target,
            kind,
            value,
            span,
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

default_spanned_impl!(ConstAssignment);

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
pub enum ConstForLoopRange {
    Range(ConstExpr, ConstExpr),
    RangeInclusive(ConstExpr, ConstExpr),
}

#[derive(Debug, Clone)]
pub struct ConstForLoop {
    item_name: Ident,
    range: ConstForLoopRange,
    body: Box<ConstBlock>,
}

impl ConstForLoop {
    #[inline]
    pub fn new(item_name: Ident, range: ConstForLoopRange, body: ConstBlock) -> Self {
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
    pub fn range(&self) -> &ConstForLoopRange {
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
    Continue(Keyword),
    Break(Keyword),
}

#[derive(Debug, Clone)]
pub struct ConstBlock {
    statements: Vec<ConstStatement>,
    result: Option<ConstExpr>,
    span: TextSpan,
}

impl ConstBlock {
    #[inline]
    pub fn new(statements: Vec<ConstStatement>, result: Option<ConstExpr>, span: TextSpan) -> Self {
        Self {
            statements,
            result,
            span,
        }
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

default_spanned_impl!(ConstBlock);

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

#[derive(Debug, Clone)]
pub enum TypeItem {
    Struct(Struct),
    Enum(Enum),
    Module(Module),
}

#[derive(Debug, Clone)]
pub enum ResolvedType {
    Const,
    BuiltinBits {
        width: u64,
    },
    Named {
        name: SharedString,
        generic_args: Rc<[i64]>,
    },
    Array {
        item_ty: TypeId,
        len: u64,
    },
}

impl ResolvedType {
    pub fn to_string(&self, known_types: &HashMap<TypeId, ResolvedType>) -> Cow<'static, str> {
        match self {
            Self::Const => "const int".into(),
            Self::BuiltinBits { width } => {
                if *width == 1 {
                    "bit".into()
                } else {
                    format!("bits<{width}>").into()
                }
            }
            Self::Named { name, generic_args } => {
                use std::fmt::Write;

                let mut result = String::new();
                let _ = write!(result, "{name}");

                if generic_args.len() > 0 {
                    let _ = write!(result, "<");
                    for (i, arg) in generic_args.iter().copied().enumerate() {
                        if i == 0 {
                            let _ = write!(result, "{arg}");
                        } else {
                            let _ = write!(result, ", {arg}");
                        }
                    }
                    let _ = write!(result, ">");
                }

                result.into()
            }
            Self::Array { item_ty, len } => {
                let item_ty_str = known_types[item_ty].to_string(known_types);
                format!("[{item_ty_str}; {len}]").into()
            }
        }
    }
}

impl PartialEq for ResolvedType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Const, Self::Const) => true,
            (Self::BuiltinBits { width: lhs_width }, Self::BuiltinBits { width: rhs_width }) => {
                *lhs_width == *rhs_width
            }
            (
                Self::Named {
                    name: lhs_name,
                    generic_args: lhs_args,
                },
                Self::Named {
                    name: rhs_name,
                    generic_args: rhs_args,
                },
            ) => lhs_name.eq(rhs_name) && lhs_args.eq(rhs_args),
            (
                Self::Array {
                    item_ty: lhs_item_ty,
                    len: lhs_len,
                },
                Self::Array {
                    item_ty: rhs_item_ty,
                    len: rhs_len,
                },
            ) => lhs_item_ty.eq(rhs_item_ty) && (*lhs_len == *rhs_len),
            _ => false,
        }
    }
}

impl Eq for ResolvedType {}

impl std::hash::Hash for ResolvedType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use std::hash::Hash;

        Hash::hash(&core::mem::discriminant(self), state);

        match self {
            Self::Const => {}
            Self::BuiltinBits { width } => {
                Hash::hash(width, state);
            }
            Self::Named { name, generic_args } => {
                Hash::hash(name, state);
                Hash::hash(generic_args, state);
            }
            Self::Array { item_ty, len } => {
                Hash::hash(item_ty, state);
                Hash::hash(len, state);
            }
        }
    }
}

// We use a 128 bit hash so we can safely ignore the possibility of a collision.
// E.g. with 10^10 different types the chance of at least one collision is about 10^-18.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u128);

impl TypeId {
    pub fn from_type(ty: &ResolvedType) -> Self {
        use xxhash_rust::xxh3::Xxh3;

        let mut hasher = Xxh3::new();
        std::hash::Hash::hash(ty, &mut hasher);
        Self(hasher.digest128())
    }
}

impl std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:0>32x}", self.0)
    }
}

pub static CONST_TYPE_ID: Lazy<TypeId> = Lazy::new(|| TypeId::from_type(&ResolvedType::Const));

#[derive(Debug, Clone)]
pub struct ResolvedStruct {
    fields: HashMap<SharedString, (TypeId, TextSpan)>,
}

impl ResolvedStruct {
    #[inline]
    pub fn new(fields: HashMap<SharedString, (TypeId, TextSpan)>) -> Self {
        Self { fields }
    }

    #[inline]
    pub fn fields(&self) -> &HashMap<SharedString, (TypeId, TextSpan)> {
        &self.fields
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedEnum {
    base_ty: TypeId,
    variants: HashMap<SharedString, i64>,
}

impl ResolvedEnum {
    #[inline]
    pub fn new(base_ty: TypeId, variants: HashMap<SharedString, i64>) -> Self {
        Self { base_ty, variants }
    }

    #[inline]
    pub fn base_ty(&self) -> TypeId {
        self.base_ty
    }

    #[inline]
    pub fn variants(&self) -> &HashMap<SharedString, i64> {
        &self.variants
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedPort {
    dir: Direction,
    kind: LogicKind,
    span: TextSpan,
    ty: TypeId,
}

impl ResolvedPort {
    #[inline]
    pub fn new(dir: Direction, kind: LogicKind, span: TextSpan, ty: TypeId) -> Self {
        Self {
            dir,
            kind,
            span,
            ty,
        }
    }

    #[inline]
    pub fn dir(&self) -> Direction {
        self.dir
    }

    #[inline]
    pub fn kind(&self) -> LogicKind {
        self.kind
    }

    #[inline]
    pub fn ty(&self) -> TypeId {
        self.ty
    }
}

default_spanned_impl!(ResolvedPort);

#[derive(Debug, Clone)]
pub struct ResolvedLogicMember {
    kind: LogicKind,
    span: TextSpan,
    ty: TypeId,
}

impl ResolvedLogicMember {
    #[inline]
    pub fn new(kind: LogicKind, span: TextSpan, ty: TypeId) -> Self {
        Self { kind, span, ty }
    }

    #[inline]
    pub fn kind(&self) -> LogicKind {
        self.kind
    }

    #[inline]
    pub fn ty(&self) -> TypeId {
        self.ty
    }
}

default_spanned_impl!(ResolvedLogicMember);

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    ports: HashMap<SharedString, ResolvedPort>,
    local_consts: HashMap<SharedString, i64>,
    logic_members: HashMap<SharedString, ResolvedLogicMember>,
    proc_members: Vec<ProcMember>,
    comb_members: Vec<CombMember>,
}

impl ResolvedModule {
    #[inline]
    pub fn new(
        ports: HashMap<SharedString, ResolvedPort>,
        local_consts: HashMap<SharedString, i64>,
        logic_members: HashMap<SharedString, ResolvedLogicMember>,
        proc_members: Vec<ProcMember>,
        comb_members: Vec<CombMember>,
    ) -> Self {
        Self {
            ports,
            local_consts,
            logic_members,
            proc_members,
            comb_members,
        }
    }

    #[inline]
    pub fn ports(&self) -> &HashMap<SharedString, ResolvedPort> {
        &self.ports
    }

    #[inline]
    pub fn local_consts(&self) -> &HashMap<SharedString, i64> {
        &self.local_consts
    }

    #[inline]
    pub fn logic_members(&self) -> &HashMap<SharedString, ResolvedLogicMember> {
        &self.logic_members
    }

    #[inline]
    pub fn proc_members(&self) -> &[ProcMember] {
        &self.proc_members
    }

    #[inline]
    pub fn comb_members(&self) -> &[CombMember] {
        &self.comb_members
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedTypeItem {
    Struct(ResolvedStruct),
    Enum(ResolvedEnum),
    Module(ResolvedModule),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnresolvedType {
    BuiltinBits { width: u64 },
}

pub trait Typed {
    fn ty(&self) -> TypeId;
}

#[derive(Debug, Clone)]
pub struct CheckedFieldAssign {
    field: Ident,
    value: CheckedExpr,
}

impl CheckedFieldAssign {
    #[inline]
    pub fn new(field: Ident, value: CheckedExpr) -> Self {
        Self { field, value }
    }

    #[inline]
    pub fn field(&self) -> &Ident {
        &self.field
    }

    #[inline]
    pub fn value(&self) -> &CheckedExpr {
        &self.value
    }
}

#[derive(Debug, Clone)]
pub struct CheckedConstructExpr {
    ty: TypeId,
    fields: HashMap<SharedString, CheckedExpr>,
}

impl CheckedConstructExpr {
    #[inline]
    pub fn new(ty: TypeId, fields: HashMap<SharedString, CheckedExpr>) -> Self {
        Self { ty, fields }
    }

    #[inline]
    pub fn fields(&self) -> &HashMap<SharedString, CheckedExpr> {
        &self.fields
    }
}

impl Typed for CheckedConstructExpr {
    #[inline]
    fn ty(&self) -> TypeId {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub struct CheckedIfExprElseIfBlock {
    condition: Box<CheckedExpr>,
    body: Box<CheckedExprBlock>,
}

impl CheckedIfExprElseIfBlock {
    #[inline]
    pub fn new(condition: CheckedExpr, body: CheckedExprBlock) -> Self {
        Self {
            condition: Box::new(condition),
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn condition(&self) -> &CheckedExpr {
        &self.condition
    }

    #[inline]
    pub fn body(&self) -> &CheckedExprBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct CheckedIfExprElseBlock {
    body: Box<CheckedExprBlock>,
}

impl CheckedIfExprElseBlock {
    #[inline]
    pub fn new(body: CheckedExprBlock) -> Self {
        Self {
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn body(&self) -> &CheckedExprBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct CheckedIfExpr {
    condition: Box<CheckedExpr>,
    body: Box<CheckedExprBlock>,
    else_if_blocks: Vec<CheckedIfExprElseIfBlock>,
    else_block: CheckedIfExprElseBlock,
    ty: TypeId,
}

impl CheckedIfExpr {
    #[inline]
    pub fn new(
        condition: CheckedExpr,
        body: CheckedExprBlock,
        else_if_blocks: Vec<CheckedIfExprElseIfBlock>,
        else_block: CheckedIfExprElseBlock,
        ty: TypeId,
    ) -> Self {
        Self {
            condition: Box::new(condition),
            body: Box::new(body),
            else_if_blocks,
            else_block,
            ty,
        }
    }

    #[inline]
    pub fn condition(&self) -> &CheckedExpr {
        &self.condition
    }

    #[inline]
    pub fn body(&self) -> &CheckedExprBlock {
        &self.body
    }

    #[inline]
    pub fn else_if_blocks(&self) -> &[CheckedIfExprElseIfBlock] {
        &self.else_if_blocks
    }

    #[inline]
    pub fn else_block(&self) -> &CheckedIfExprElseBlock {
        &self.else_block
    }
}

impl Typed for CheckedIfExpr {
    #[inline]
    fn ty(&self) -> TypeId {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub enum CheckedMatchExprBody {
    Expr(CheckedExpr),
    Block(CheckedExprBlock),
}

impl Typed for CheckedMatchExprBody {
    fn ty(&self) -> TypeId {
        match self {
            Self::Expr(expr) => expr.ty(),
            Self::Block(block) => block.ty(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CheckedMatchExprBranch {
    patterns: Vec<MatchPattern>,
    body: Box<CheckedMatchExprBody>,
}

impl CheckedMatchExprBranch {
    #[inline]
    pub fn new(patterns: Vec<MatchPattern>, body: CheckedMatchExprBody) -> Self {
        Self {
            patterns,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn patterns(&self) -> &[MatchPattern] {
        &self.patterns
    }

    #[inline]
    pub fn body(&self) -> &CheckedMatchExprBody {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct CheckedMatchExpr {
    value: Box<CheckedExpr>,
    branches: Vec<CheckedMatchExprBranch>,
    ty: TypeId,
}

impl CheckedMatchExpr {
    #[inline]
    pub fn new(value: CheckedExpr, branches: Vec<CheckedMatchExprBranch>, ty: TypeId) -> Self {
        Self {
            value: Box::new(value),
            branches,
            ty,
        }
    }

    #[inline]
    pub fn value(&self) -> &CheckedExpr {
        &self.value
    }

    #[inline]
    pub fn branches(&self) -> &[CheckedMatchExprBranch] {
        &self.branches
    }
}

impl Typed for CheckedMatchExpr {
    #[inline]
    fn ty(&self) -> TypeId {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub enum CheckedIndexKind {
    Single(CheckedExpr),
    Range(Range<i64>),
}

#[derive(Debug, Clone)]
pub struct CheckedIndexExpr {
    base: Box<CheckedExpr>,
    indexer: Box<CheckedIndexKind>,
    ty: TypeId,
}

impl CheckedIndexExpr {
    #[inline]
    pub fn new(base: CheckedExpr, indexer: CheckedIndexKind, ty: TypeId) -> Self {
        Self {
            base: Box::new(base),
            indexer: Box::new(indexer),
            ty,
        }
    }

    #[inline]
    pub fn base(&self) -> &CheckedExpr {
        &self.base
    }

    #[inline]
    pub fn indexer(&self) -> &CheckedIndexKind {
        &self.indexer
    }
}

impl Typed for CheckedIndexExpr {
    #[inline]
    fn ty(&self) -> TypeId {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub struct CheckedMemberAccessExpr {
    base: Box<CheckedExpr>,
    member: Ident,
    ty: TypeId,
}

impl CheckedMemberAccessExpr {
    #[inline]
    pub fn new(base: CheckedExpr, member: Ident, ty: TypeId) -> Self {
        Self {
            base: Box::new(base),
            member,
            ty,
        }
    }

    #[inline]
    pub fn base(&self) -> &CheckedExpr {
        &self.base
    }

    #[inline]
    pub fn member(&self) -> &Ident {
        &self.member
    }
}

impl Typed for CheckedMemberAccessExpr {
    #[inline]
    fn ty(&self) -> TypeId {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub struct CheckedUnaryExpr {
    inner: Box<CheckedExpr>,
}

impl CheckedUnaryExpr {
    #[inline]
    pub fn new(inner: CheckedExpr) -> Self {
        Self {
            inner: Box::new(inner),
        }
    }

    #[inline]
    pub fn inner(&self) -> &CheckedExpr {
        &self.inner
    }
}

impl Typed for CheckedUnaryExpr {
    #[inline]
    fn ty(&self) -> TypeId {
        self.inner.ty()
    }
}

#[derive(Debug, Clone)]
pub struct CheckedCastExpr {
    value: Box<CheckedExpr>,
    target_ty: TypeId,
}

impl CheckedCastExpr {
    #[inline]
    pub fn new(value: CheckedExpr, target_ty: TypeId) -> Self {
        Self {
            value: Box::new(value),
            target_ty,
        }
    }

    #[inline]
    pub fn value(&self) -> &CheckedExpr {
        &self.value
    }

    #[inline]
    pub fn target_ty(&self) -> TypeId {
        self.target_ty
    }
}

impl Typed for CheckedCastExpr {
    #[inline]
    fn ty(&self) -> TypeId {
        self.target_ty
    }
}

#[derive(Debug, Clone)]
pub struct CheckedConcatExpr {
    lhs: Box<CheckedExpr>,
    rhs: Box<CheckedExpr>,
    ty: TypeId,
}

impl CheckedConcatExpr {
    #[inline]
    pub fn new(lhs: CheckedExpr, rhs: CheckedExpr, ty: TypeId) -> Self {
        Self {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            ty,
        }
    }

    #[inline]
    pub fn lhs(&self) -> &CheckedExpr {
        &self.lhs
    }

    #[inline]
    pub fn rhs(&self) -> &CheckedExpr {
        &self.rhs
    }
}

impl Typed for CheckedConcatExpr {
    #[inline]
    fn ty(&self) -> TypeId {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub struct CheckedBinaryExpr {
    lhs: Box<CheckedExpr>,
    rhs: Box<CheckedExpr>,
    ty: TypeId,
}

impl CheckedBinaryExpr {
    #[inline]
    pub fn new(lhs: CheckedExpr, rhs: CheckedExpr, ty: TypeId) -> Self {
        Self {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            ty,
        }
    }

    #[inline]
    pub fn lhs(&self) -> &CheckedExpr {
        &self.lhs
    }

    #[inline]
    pub fn rhs(&self) -> &CheckedExpr {
        &self.rhs
    }
}

impl Typed for CheckedBinaryExpr {
    #[inline]
    fn ty(&self) -> TypeId {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub struct CheckedPath {
    path: Path,
    ty: TypeId,
}

impl CheckedPath {
    #[inline]
    pub fn new(path: Path, ty: TypeId) -> Self {
        Self { path, ty }
    }

    #[inline]
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl Typed for CheckedPath {
    #[inline]
    fn ty(&self) -> TypeId {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub struct CheckedExprBlock {
    statements: Vec<CheckedStatement>,
    result: Box<CheckedExpr>,
}

impl CheckedExprBlock {
    #[inline]
    pub fn new(statements: Vec<CheckedStatement>, result: CheckedExpr) -> Self {
        Self {
            statements,
            result: Box::new(result),
        }
    }

    #[inline]
    pub fn statements(&self) -> &[CheckedStatement] {
        &self.statements
    }

    #[inline]
    pub fn result(&self) -> &CheckedExpr {
        &self.result
    }
}

impl Typed for CheckedExprBlock {
    #[inline]
    fn ty(&self) -> TypeId {
        self.result.ty()
    }
}

#[derive(Debug, Clone)]
pub enum CheckedExpr {
    // Leaf expressions
    Value(i64),
    Path(CheckedPath),
    Construct(CheckedConstructExpr),

    If(CheckedIfExpr),
    Match(CheckedMatchExpr),
    Block(CheckedExprBlock),

    // Operators
    Index(CheckedIndexExpr),
    MemberAccess(CheckedMemberAccessExpr),
    Neg(CheckedUnaryExpr),
    Not(CheckedUnaryExpr),
    Cast(CheckedCastExpr),
    Concat(CheckedConcatExpr),
    Lt(CheckedBinaryExpr),
    Lte(CheckedBinaryExpr),
    Gt(CheckedBinaryExpr),
    Gte(CheckedBinaryExpr),
    Slt(CheckedBinaryExpr),
    Slte(CheckedBinaryExpr),
    Sgt(CheckedBinaryExpr),
    Sgte(CheckedBinaryExpr),
    Eq(CheckedBinaryExpr),
    Ne(CheckedBinaryExpr),
    Add(CheckedBinaryExpr),
    Sub(CheckedBinaryExpr),
    Mul(CheckedBinaryExpr),
    Div(CheckedBinaryExpr),
    Rem(CheckedBinaryExpr),
    And(CheckedBinaryExpr),
    Xor(CheckedBinaryExpr),
    Or(CheckedBinaryExpr),
    Shl(CheckedBinaryExpr),
    Lsr(CheckedBinaryExpr),
    Asr(CheckedBinaryExpr),
}

impl Typed for CheckedExpr {
    fn ty(&self) -> TypeId {
        match self {
            Self::Value(_) => *CONST_TYPE_ID,
            Self::Path(path) => path.ty(),
            Self::Construct(expr) => expr.ty(),
            Self::If(expr) => expr.ty(),
            Self::Match(expr) => expr.ty(),
            Self::Block(block) => block.ty(),
            Self::Index(expr) => expr.ty(),
            Self::MemberAccess(expr) => expr.ty(),
            Self::Neg(expr) => expr.ty(),
            Self::Not(expr) => expr.ty(),
            Self::Cast(expr) => expr.ty(),
            Self::Concat(expr) => expr.ty(),
            Self::Lt(expr) => expr.ty(),
            Self::Lte(expr) => expr.ty(),
            Self::Gt(expr) => expr.ty(),
            Self::Gte(expr) => expr.ty(),
            Self::Slt(expr) => expr.ty(),
            Self::Slte(expr) => expr.ty(),
            Self::Sgt(expr) => expr.ty(),
            Self::Sgte(expr) => expr.ty(),
            Self::Eq(expr) => expr.ty(),
            Self::Ne(expr) => expr.ty(),
            Self::Add(expr) => expr.ty(),
            Self::Sub(expr) => expr.ty(),
            Self::Mul(expr) => expr.ty(),
            Self::Div(expr) => expr.ty(),
            Self::Rem(expr) => expr.ty(),
            Self::And(expr) => expr.ty(),
            Self::Xor(expr) => expr.ty(),
            Self::Or(expr) => expr.ty(),
            Self::Shl(expr) => expr.ty(),
            Self::Lsr(expr) => expr.ty(),
            Self::Asr(expr) => expr.ty(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CheckedBlock {
    statements: Vec<CheckedStatement>,
}

impl CheckedBlock {
    #[inline]
    pub fn new(statements: Vec<CheckedStatement>) -> Self {
        Self { statements }
    }

    #[inline]
    pub fn statements(&self) -> &[CheckedStatement] {
        &self.statements
    }
}

#[derive(Debug, Clone)]
pub struct CheckedIfStatementElseIfBlock {
    condition: Box<CheckedExpr>,
    body: Box<CheckedBlock>,
}

impl CheckedIfStatementElseIfBlock {
    #[inline]
    pub fn new(condition: CheckedExpr, body: CheckedBlock) -> Self {
        Self {
            condition: Box::new(condition),
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn condition(&self) -> &CheckedExpr {
        &self.condition
    }

    #[inline]
    pub fn body(&self) -> &CheckedBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct CheckedIfStatementElseBlock {
    body: Box<CheckedBlock>,
}

impl CheckedIfStatementElseBlock {
    #[inline]
    pub fn new(body: CheckedBlock) -> Self {
        Self {
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn body(&self) -> &CheckedBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct CheckedIfStatement {
    condition: Box<CheckedExpr>,
    body: Box<CheckedBlock>,
    else_if_blocks: Vec<CheckedIfStatementElseIfBlock>,
    else_block: Option<CheckedIfStatementElseBlock>,
}

impl CheckedIfStatement {
    #[inline]
    pub fn new(
        condition: CheckedExpr,
        body: CheckedBlock,
        else_if_blocks: Vec<CheckedIfStatementElseIfBlock>,
        else_block: Option<CheckedIfStatementElseBlock>,
    ) -> Self {
        Self {
            condition: Box::new(condition),
            body: Box::new(body),
            else_if_blocks,
            else_block,
        }
    }

    #[inline]
    pub fn condition(&self) -> &CheckedExpr {
        &self.condition
    }

    #[inline]
    pub fn body(&self) -> &CheckedBlock {
        &self.body
    }

    #[inline]
    pub fn else_if_blocks(&self) -> &[CheckedIfStatementElseIfBlock] {
        &self.else_if_blocks
    }

    #[inline]
    pub fn else_block(&self) -> Option<&CheckedIfStatementElseBlock> {
        self.else_block.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct CheckedMatchStatementBranch {
    patterns: Vec<MatchPattern>,
    body: Box<CheckedBlock>,
}

impl CheckedMatchStatementBranch {
    #[inline]
    pub fn new(patterns: Vec<MatchPattern>, body: CheckedBlock) -> Self {
        Self {
            patterns,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn patterns(&self) -> &[MatchPattern] {
        &self.patterns
    }

    #[inline]
    pub fn body(&self) -> &CheckedBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct CheckedMatchStatement {
    value: Box<CheckedExpr>,
    branches: Vec<CheckedMatchStatementBranch>,
}

impl CheckedMatchStatement {
    #[inline]
    pub fn new(value: CheckedExpr, branches: Vec<CheckedMatchStatementBranch>) -> Self {
        Self {
            value: Box::new(value),
            branches,
        }
    }

    #[inline]
    pub fn value(&self) -> &CheckedExpr {
        &self.value
    }

    #[inline]
    pub fn branches(&self) -> &[CheckedMatchStatementBranch] {
        &self.branches
    }
}

#[derive(Debug, Clone)]
pub enum CheckedSuffixOp {
    Indexer { index: CheckedIndexKind, ty: TypeId },
    MemberAccess { member: Ident, ty: TypeId },
}

impl Typed for CheckedSuffixOp {
    fn ty(&self) -> TypeId {
        match self {
            Self::Indexer { ty, .. } => *ty,
            Self::MemberAccess { ty, .. } => *ty,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CheckedAssignTarget {
    base: Ident,
    base_ty: TypeId,
    suffixes: Vec<CheckedSuffixOp>,
}

impl CheckedAssignTarget {
    #[inline]
    pub fn new(base: Ident, base_ty: TypeId, suffixes: Vec<CheckedSuffixOp>) -> Self {
        Self {
            base,
            base_ty,
            suffixes,
        }
    }

    #[inline]
    pub fn base(&self) -> &Ident {
        &self.base
    }

    #[inline]
    pub fn suffixes(&self) -> &[CheckedSuffixOp] {
        &self.suffixes
    }
}

impl Typed for CheckedAssignTarget {
    fn ty(&self) -> TypeId {
        if let Some(last) = self.suffixes.last() {
            last.ty()
        } else {
            self.base_ty
        }
    }
}

#[derive(Debug, Clone)]
pub struct CheckedAssignment {
    target: CheckedAssignTarget,
    value: CheckedExpr,
}

impl CheckedAssignment {
    #[inline]
    pub fn new(target: CheckedAssignTarget, value: CheckedExpr) -> Self {
        Self { target, value }
    }

    #[inline]
    pub fn target(&self) -> &CheckedAssignTarget {
        &self.target
    }

    #[inline]
    pub fn value(&self) -> &CheckedExpr {
        &self.value
    }
}

#[derive(Debug, Clone)]
pub enum CheckedStatement {
    Expr(CheckedExpr),
    Block(CheckedBlock),
    If(CheckedIfStatement),
    Match(CheckedMatchStatement),
    Assignment(CheckedAssignment),
}

#[derive(Debug, Clone)]
pub struct CheckedSens {
    target: CheckedAssignTarget,
    edge: EdgeKind,
}

impl CheckedSens {
    #[inline]
    pub fn new(target: CheckedAssignTarget, edge: EdgeKind) -> Self {
        Self { target, edge }
    }

    #[inline]
    pub fn target(&self) -> &CheckedAssignTarget {
        &self.target
    }

    #[inline]
    pub fn edge(&self) -> EdgeKind {
        self.edge
    }
}

#[derive(Debug, Clone)]
pub struct CheckedProcMember {
    sens: Vec<CheckedSens>,
    body: CheckedBlock,
}

impl CheckedProcMember {
    #[inline]
    pub fn new(sens: Vec<CheckedSens>, body: CheckedBlock) -> Self {
        Self { sens, body }
    }

    #[inline]
    pub fn sens(&self) -> &[CheckedSens] {
        &self.sens
    }

    #[inline]
    pub fn body(&self) -> &CheckedBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct CheckedCombMember {
    body: CheckedBlock,
}

impl CheckedCombMember {
    #[inline]
    pub fn new(body: CheckedBlock) -> Self {
        Self { body }
    }

    #[inline]
    pub fn body(&self) -> &CheckedBlock {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct CheckedModule {
    proc_members: Vec<CheckedProcMember>,
    comb_members: Vec<CheckedCombMember>,
}

impl CheckedModule {
    #[inline]
    pub fn new(proc_members: Vec<CheckedProcMember>, comb_members: Vec<CheckedCombMember>) -> Self {
        Self {
            proc_members,
            comb_members,
        }
    }

    #[inline]
    pub fn proc_members(&self) -> &[CheckedProcMember] {
        &self.proc_members
    }

    #[inline]
    pub fn comb_members(&self) -> &[CheckedCombMember] {
        &self.comb_members
    }
}
