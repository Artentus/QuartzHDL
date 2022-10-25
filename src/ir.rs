#![allow(dead_code)]

use crate::ast::*;
use crate::{default_spanned_impl, HashMap, SharedString};
use langbox::TextSpan;
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedType {
    Const,
    BuiltinBits {
        width: i64,
    },
    Named {
        name: SharedString,
        generic_args: Rc<[i64]>,
    },
    Array {
        item_ty: TypeId,
        len: i64,
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
                    format!("bits<{}>", width).into()
                }
            }
            Self::Named { name, generic_args } => {
                use std::fmt::Write;

                let mut result = String::new();
                let _ = write!(result, "{}", name);

                if generic_args.len() > 0 {
                    let _ = write!(result, "<");
                    for (i, arg) in generic_args.iter().copied().enumerate() {
                        if i == 0 {
                            let _ = write!(result, "{}", arg);
                        } else {
                            let _ = write!(result, ", {}", arg);
                        }
                    }
                    let _ = write!(result, ">");
                }

                result.into()
            }
            Self::Array { item_ty, len } => {
                let item_ty_str = known_types[item_ty].to_string(known_types);
                format!("[{}; {}]", item_ty_str, len).into()
            }
        }
    }
}

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

#[derive(Debug, Clone)]
pub struct ResolvedStruct {
    fields: HashMap<SharedString, TypeId>,
}

impl ResolvedStruct {
    #[inline]
    pub fn new(fields: HashMap<SharedString, TypeId>) -> Self {
        Self { fields }
    }

    #[inline]
    pub fn fields(&self) -> &HashMap<SharedString, TypeId> {
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
    ty: TypeId,
}

impl ResolvedPort {
    #[inline]
    pub fn new(dir: Direction, kind: LogicKind, ty: TypeId) -> Self {
        Self { dir, kind, ty }
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

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    ports: HashMap<SharedString, ResolvedPort>,
    members: Vec<Member>,
}

impl ResolvedModule {
    #[inline]
    pub fn new(ports: HashMap<SharedString, ResolvedPort>, members: Vec<Member>) -> Self {
        Self { ports, members }
    }

    #[inline]
    pub fn ports(&self) -> &HashMap<SharedString, ResolvedPort> {
        &self.ports
    }

    #[inline]
    pub fn members(&self) -> &[Member] {
        &self.members
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedTypeItem {
    Const,
    BuiltinBits { width: i64 },
    Array { item_ty: TypeId, len: i64 },
    Struct(ResolvedStruct),
    Enum(ResolvedEnum),
    Module(ResolvedModule),
}
