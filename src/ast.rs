#![allow(dead_code)]

use crate::fmt::{DisplayScoped, ScopedFormatter};
use crate::ir::TypeId;
use crate::lexer::PunctKind;
use crate::{default_display_impl, SharedString};
use langbox::TextSpan;
use std::cell::Cell;
use std::fmt::Write;
use std::hash::Hash;

pub trait Spanned {
    fn span(&self) -> TextSpan;
}

#[macro_export]
macro_rules! default_spanned_impl {
    ($t:ty) => {
        impl $crate::ast::Spanned for $t {
            #[inline]
            fn span(&self) -> langbox::TextSpan {
                self.span
            }
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KeywordKind {
    Mod,
    Struct,
    Enum,
    Fn,
    InOut,
    In,
    Out,
    Sig,
    Reg,
    Let,
    Const,
    Comb,
    Proc,
    If,
    Else,
    Match,
    While,
    For,
    Rising,
    Falling,
    As,
    Continue,
    Break,
    Extern,
    False,
    True,
}

impl DisplayScoped for KeywordKind {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Mod => "mod",
                Self::Struct => "struct",
                Self::Enum => "enum",
                Self::Fn => "fn",
                Self::InOut => "inout",
                Self::In => "in",
                Self::Out => "out",
                Self::Sig => "sig",
                Self::Reg => "reg",
                Self::Let => "let",
                Self::Const => "const",
                Self::Comb => "comb",
                Self::Proc => "proc",
                Self::If => "if",
                Self::Else => "else",
                Self::Match => "match",
                Self::While => "while",
                Self::For => "for",
                Self::Rising => "rising",
                Self::Falling => "falling",
                Self::As => "as",
                Self::Continue => "continue",
                Self::Break => "break",
                Self::Extern => "extern",
                Self::False => "false",
                Self::True => "true",
            }
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Keyword {
    kind: KeywordKind,
    span: TextSpan,
}

impl Keyword {
    #[inline]
    pub fn new(kind: KeywordKind, span: TextSpan) -> Self {
        Self { kind, span }
    }

    #[inline]
    pub fn kind(&self) -> KeywordKind {
        self.kind
    }
}

default_spanned_impl!(Keyword);

impl DisplayScoped for Keyword {
    #[inline]
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        DisplayScoped::fmt(&self.kind, f)
    }
}

default_display_impl!(Keyword);

#[derive(Debug, Clone)]
pub struct Ident {
    value: SharedString,
    span: TextSpan,
}

impl Ident {
    #[inline]
    pub fn new(value: &SharedString, span: TextSpan) -> Self {
        Self {
            value: SharedString::clone(value),
            span,
        }
    }

    #[inline]
    pub fn as_string(&self) -> SharedString {
        SharedString::clone(&self.value)
    }
}

default_spanned_impl!(Ident);

impl DisplayScoped for Ident {
    #[inline]
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        DisplayScoped::fmt(&self.value, f)
    }
}

default_display_impl!(Ident);

impl AsRef<str> for Ident {
    #[inline]
    fn as_ref(&self) -> &str {
        &self.value
    }
}

impl From<Ident> for SharedString {
    #[inline]
    fn from(value: Ident) -> Self {
        value.as_string()
    }
}

impl From<&Ident> for SharedString {
    #[inline]
    fn from(value: &Ident) -> Self {
        value.as_string()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoolLiteral {
    value: bool,
    span: TextSpan,
}

impl BoolLiteral {
    #[inline]
    pub fn new(value: bool, span: TextSpan) -> Self {
        Self { value, span }
    }

    #[inline]
    pub fn value(&self) -> bool {
        self.value
    }
}

default_spanned_impl!(BoolLiteral);

impl DisplayScoped for BoolLiteral {
    #[inline]
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        DisplayScoped::fmt(&self.value, f)
    }
}

default_display_impl!(BoolLiteral);

impl AsRef<bool> for BoolLiteral {
    #[inline]
    fn as_ref(&self) -> &bool {
        &self.value
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Literal {
    value: i64,
    span: TextSpan,
}

impl Literal {
    #[inline]
    pub fn new(value: i64, span: TextSpan) -> Self {
        Self { value, span }
    }

    #[inline]
    pub fn value(&self) -> i64 {
        self.value
    }
}

default_spanned_impl!(Literal);

impl DisplayScoped for Literal {
    #[inline]
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        DisplayScoped::fmt(&self.value, f)
    }
}

default_display_impl!(Literal);

impl AsRef<i64> for Literal {
    #[inline]
    fn as_ref(&self) -> &i64 {
        &self.value
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Punct {
    kind: PunctKind,
    span: TextSpan,
}

impl Punct {
    #[inline]
    pub fn new(kind: PunctKind, span: TextSpan) -> Self {
        Self { kind, span }
    }

    #[inline]
    pub fn kind(&self) -> PunctKind {
        self.kind
    }
}

default_spanned_impl!(Punct);

impl DisplayScoped for Punct {
    #[inline]
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        DisplayScoped::fmt(&self.kind, f)
    }
}

default_display_impl!(Punct);

#[derive(Debug, Clone)]
pub struct PathSegment {
    sep: Punct,
    ident: Ident,
}

impl PathSegment {
    #[inline]
    pub fn new(sep: Punct, ident: Ident) -> Self {
        Self { sep, ident }
    }

    #[inline]
    pub fn sep(&self) -> &Punct {
        &self.sep
    }

    #[inline]
    pub fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl Spanned for PathSegment {
    fn span(&self) -> TextSpan {
        self.sep.span().join(self.ident.span()).unwrap()
    }
}

impl DisplayScoped for PathSegment {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}{}", self.sep, self.ident)
    }
}

default_display_impl!(PathSegment);

#[derive(Debug, Clone)]
pub struct Path {
    head: Ident,
    tail: Vec<PathSegment>,
}

impl Path {
    #[inline]
    pub fn new(head: Ident, tail: Vec<PathSegment>) -> Self {
        Self { head, tail }
    }

    #[inline]
    pub fn head(&self) -> &Ident {
        &self.head
    }

    #[inline]
    pub fn tail(&self) -> &[PathSegment] {
        &self.tail
    }

    #[inline]
    pub fn is_ident(&self) -> bool {
        self.tail.is_empty()
    }

    #[inline]
    pub fn as_ident(&self) -> Option<&Ident> {
        if self.is_ident() {
            Some(self.head())
        } else {
            None
        }
    }
}

impl Spanned for Path {
    fn span(&self) -> TextSpan {
        if let Some(last) = self.tail.last() {
            self.head.span().join(last.span()).unwrap()
        } else {
            self.head.span()
        }
    }
}

impl DisplayScoped for Path {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}", self.head)?;

        for segment in self.tail.iter() {
            write!(f, "{segment}")?;
        }

        Ok(())
    }
}

default_display_impl!(Path);

#[derive(Debug, Clone)]
pub struct ParenExpr {
    open_paren: Punct,
    inner: Box<Expr>,
    close_paren: Punct,
}

impl ParenExpr {
    #[inline]
    pub fn new(open_paren: Punct, inner: Expr, close_paren: Punct) -> Self {
        Self {
            open_paren,
            inner: Box::new(inner),
            close_paren,
        }
    }

    #[inline]
    pub fn open_paren(&self) -> &Punct {
        &self.open_paren
    }

    #[inline]
    pub fn inner(&self) -> &Expr {
        &self.inner
    }

    #[inline]
    pub fn close_paren(&self) -> &Punct {
        &self.close_paren
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.inner.reset_resolved_types();
    }
}

impl Spanned for ParenExpr {
    fn span(&self) -> TextSpan {
        self.open_paren
            .span()
            .join(self.close_paren.span())
            .unwrap()
    }
}

impl DisplayScoped for ParenExpr {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}{}{}", self.open_paren, self.inner, self.close_paren)
    }
}

default_display_impl!(ParenExpr);

#[derive(Debug, Clone)]
pub struct CallExpr {
    func: Ident,
    open_paren: Punct,
    args: Vec<Expr>,
    close_paren: Punct,
}

impl CallExpr {
    #[inline]
    pub fn new(func: Ident, open_paren: Punct, args: Vec<Expr>, close_paren: Punct) -> Self {
        Self {
            func,
            open_paren,
            args,
            close_paren,
        }
    }

    #[inline]
    pub fn func(&self) -> &Ident {
        &self.func
    }

    #[inline]
    pub fn open_paren(&self) -> &Punct {
        &self.open_paren
    }

    #[inline]
    pub fn args(&self) -> &[Expr] {
        &self.args
    }

    #[inline]
    pub fn close_paren(&self) -> &Punct {
        &self.close_paren
    }

    pub fn reset_resolved_types(&self) {
        for arg in self.args.iter() {
            arg.reset_resolved_types();
        }
    }
}

impl Spanned for CallExpr {
    fn span(&self) -> TextSpan {
        self.open_paren
            .span()
            .join(self.close_paren.span())
            .unwrap()
    }
}

impl DisplayScoped for CallExpr {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}{}", self.func, self.open_paren)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", {arg}")?;
            } else {
                write!(f, "{arg}")?;
            }
        }
        write!(f, "{}", self.close_paren)
    }
}

default_display_impl!(CallExpr);

#[derive(Debug, Clone)]
pub struct FieldAssign {
    field: Ident,
    sep: Punct,
    value: Expr,
}

impl FieldAssign {
    #[inline]
    pub fn new(field: Ident, sep: Punct, value: Expr) -> Self {
        Self { field, sep, value }
    }

    #[inline]
    pub fn field(&self) -> &Ident {
        &self.field
    }

    #[inline]
    pub fn sep(&self) -> &Punct {
        &self.sep
    }

    #[inline]
    pub fn value(&self) -> &Expr {
        &self.value
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.value.reset_resolved_types();
    }
}

impl Spanned for FieldAssign {
    fn span(&self) -> TextSpan {
        self.field.span().join(self.value.span()).unwrap()
    }
}

impl DisplayScoped for FieldAssign {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}{} {}", self.field, self.sep, self.value)
    }
}

default_display_impl!(FieldAssign);

#[derive(Debug, Clone)]
pub struct ConstructExpr {
    ty: Box<NamedType>,
    open_curl: Punct,
    fields: Vec<FieldAssign>,
    close_curl: Punct,
    resolved_ty: Cell<Option<TypeId>>,
}

impl ConstructExpr {
    #[inline]
    pub fn new(
        ty: NamedType,
        open_curl: Punct,
        fields: Vec<FieldAssign>,
        close_curl: Punct,
    ) -> Self {
        Self {
            ty: Box::new(ty),
            open_curl,
            fields,
            close_curl,
            resolved_ty: Cell::new(None),
        }
    }

    #[inline]
    pub fn ty(&self) -> &NamedType {
        &self.ty
    }

    #[inline]
    pub fn open_curl(&self) -> &Punct {
        &self.open_curl
    }

    #[inline]
    pub fn fields(&self) -> &[FieldAssign] {
        &self.fields
    }

    #[inline]
    pub fn close_curl(&self) -> &Punct {
        &self.close_curl
    }

    pub fn resolve(&self, ty: TypeId) {
        assert!(
            self.resolved_ty.get().is_none(),
            "construct expression was resolved more than once"
        );

        self.resolved_ty.set(Some(ty));
    }

    #[inline]
    pub fn resolved_ty(&self) -> Option<TypeId> {
        self.resolved_ty.get()
    }

    pub fn reset_resolved_types(&self) {
        for field in self.fields.iter() {
            field.reset_resolved_types();
        }

        self.resolved_ty.set(None);
    }
}

impl Spanned for ConstructExpr {
    fn span(&self) -> TextSpan {
        self.ty.span().join(self.close_curl.span()).unwrap()
    }
}

impl DisplayScoped for ConstructExpr {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        writeln!(f, "{} {}", self.ty, self.open_curl)?;

        f.enter_scope();
        for field in self.fields.iter() {
            writeln!(f, "{field},")?;
        }
        f.exit_scope();

        write!(f, "{}", self.close_curl)
    }
}

default_display_impl!(ConstructExpr);

#[derive(Debug, Clone)]
pub struct ElseIfBlock {
    else_kw: Keyword,
    if_kw: Keyword,
    condition: Box<Expr>,
    body: Box<Block>,
}

impl ElseIfBlock {
    #[inline]
    pub fn new(else_kw: Keyword, if_kw: Keyword, condition: Expr, body: Block) -> Self {
        Self {
            else_kw,
            if_kw,
            condition: Box::new(condition),
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn else_kw(&self) -> &Keyword {
        &self.else_kw
    }

    #[inline]
    pub fn if_kw(&self) -> &Keyword {
        &self.if_kw
    }

    #[inline]
    pub fn condition(&self) -> &Expr {
        &self.condition
    }

    #[inline]
    pub fn body(&self) -> &Block {
        &self.body
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.condition.reset_resolved_types();
        self.body.reset_resolved_types();
    }
}

impl Spanned for ElseIfBlock {
    fn span(&self) -> TextSpan {
        self.else_kw.span().join(self.body.span()).unwrap()
    }
}

impl DisplayScoped for ElseIfBlock {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {} {}",
            self.else_kw,
            self.if_kw,
            self.condition,
            self.body.as_ref()
        )
    }
}

default_display_impl!(ElseIfBlock);

#[derive(Debug, Clone)]
pub struct ElseBlock {
    else_kw: Keyword,
    body: Box<Block>,
}

impl ElseBlock {
    #[inline]
    pub fn new(else_kw: Keyword, body: Block) -> Self {
        Self {
            else_kw,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn else_kw(&self) -> &Keyword {
        &self.else_kw
    }

    #[inline]
    pub fn body(&self) -> &Block {
        &self.body
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.body.reset_resolved_types();
    }
}

impl Spanned for ElseBlock {
    fn span(&self) -> TextSpan {
        self.else_kw.span().join(self.body.span()).unwrap()
    }
}

impl DisplayScoped for ElseBlock {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{} {}", self.else_kw, self.body.as_ref())
    }
}

default_display_impl!(ElseBlock);

#[derive(Debug, Clone)]
pub struct IfExpr {
    if_kw: Keyword,
    condition: Box<Expr>,
    body: Box<Block>,
    else_if_blocks: Vec<ElseIfBlock>,
    else_block: Option<ElseBlock>,
}

impl IfExpr {
    #[inline]
    pub fn new(
        if_kw: Keyword,
        condition: Expr,
        body: Block,
        else_if_blocks: Vec<ElseIfBlock>,
        else_block: Option<ElseBlock>,
    ) -> Self {
        Self {
            if_kw,
            condition: Box::new(condition),
            body: Box::new(body),
            else_if_blocks,
            else_block,
        }
    }

    #[inline]
    pub fn if_kw(&self) -> &Keyword {
        &self.if_kw
    }

    #[inline]
    pub fn condition(&self) -> &Expr {
        &self.condition
    }

    #[inline]
    pub fn body(&self) -> &Block {
        &self.body
    }

    #[inline]
    pub fn else_if_blocks(&self) -> &[ElseIfBlock] {
        &self.else_if_blocks
    }

    #[inline]
    pub fn else_block(&self) -> Option<&ElseBlock> {
        self.else_block.as_ref()
    }

    pub fn reset_resolved_types(&self) {
        self.condition.reset_resolved_types();
        self.body.reset_resolved_types();

        for else_if_block in self.else_if_blocks.iter() {
            else_if_block.reset_resolved_types();
        }

        if let Some(else_block) = &self.else_block {
            else_block.reset_resolved_types();
        }
    }
}

impl Spanned for IfExpr {
    fn span(&self) -> TextSpan {
        if let Some(else_block) = &self.else_block {
            self.if_kw.span().join(else_block.span()).unwrap()
        } else if let Some(last_block) = self.else_if_blocks.last() {
            self.if_kw.span().join(last_block.span()).unwrap()
        } else {
            self.if_kw.span().join(self.body.span()).unwrap()
        }
    }
}

impl DisplayScoped for IfExpr {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.if_kw,
            self.condition,
            self.body.as_ref()
        )?;

        for else_if_block in self.else_if_blocks.iter() {
            write!(f, " {else_if_block}")?;
        }

        if let Some(else_block) = &self.else_block {
            write!(f, " {else_block}")?;
        }

        Ok(())
    }
}

default_display_impl!(IfExpr);

#[derive(Debug, Clone)]
pub enum MatchPattern {
    Literal(Literal),
    Range(Literal, Literal),
    RangeInclusive(Literal, Literal),
    Path(Path),
}

impl Spanned for MatchPattern {
    fn span(&self) -> TextSpan {
        match self {
            Self::Literal(l) => l.span(),
            Self::Range(s, e) => s.span().join(e.span()).unwrap(),
            Self::RangeInclusive(s, e) => s.span().join(e.span()).unwrap(),
            Self::Path(p) => p.span(),
        }
    }
}

impl DisplayScoped for MatchPattern {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => DisplayScoped::fmt(l, f),
            Self::Range(s, e) => write!(f, "{s}..{e}"),
            Self::RangeInclusive(s, e) => write!(f, "{s}..={e}"),
            Self::Path(p) => DisplayScoped::fmt(p, f),
        }
    }
}

default_display_impl!(MatchPattern);

#[derive(Debug, Clone)]
pub enum MatchBody {
    Expr(Expr),
    Block(Block),
}

impl MatchBody {
    pub fn reset_resolved_types(&self) {
        match self {
            Self::Expr(expr) => expr.reset_resolved_types(),
            Self::Block(block) => block.reset_resolved_types(),
        }
    }
}

impl Spanned for MatchBody {
    fn span(&self) -> TextSpan {
        match self {
            Self::Expr(expr) => expr.span(),
            Self::Block(block) => block.span(),
        }
    }
}

impl DisplayScoped for MatchBody {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Expr(expr) => DisplayScoped::fmt(expr, f),
            Self::Block(block) => DisplayScoped::fmt(block, f),
        }
    }
}

default_display_impl!(MatchBody);

#[derive(Debug, Clone)]
pub struct MatchBranch {
    patterns: Vec<MatchPattern>,
    arrow: Punct,
    body: Box<MatchBody>,
}

impl MatchBranch {
    #[inline]
    pub fn new(patterns: Vec<MatchPattern>, arrow: Punct, body: MatchBody) -> Self {
        Self {
            patterns,
            arrow,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn patterns(&self) -> &[MatchPattern] {
        &self.patterns
    }

    #[inline]
    pub fn arrow(&self) -> &Punct {
        &self.arrow
    }

    #[inline]
    pub fn body(&self) -> &MatchBody {
        &self.body
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.body.reset_resolved_types();
    }
}

impl Spanned for MatchBranch {
    fn span(&self) -> TextSpan {
        self.patterns
            .first()
            .unwrap()
            .span()
            .join(self.body.span())
            .unwrap()
    }
}

impl DisplayScoped for MatchBranch {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        for (i, pattern) in self.patterns.iter().enumerate() {
            if i > 0 {
                write!(f, " | {pattern}")?;
            } else {
                write!(f, "{pattern}")?;
            }
        }

        write!(f, " {} {}", self.arrow, self.body)
    }
}

default_display_impl!(MatchBranch);

#[derive(Debug, Clone)]
pub struct MatchExpr {
    match_kw: Keyword,
    value: Box<Expr>,
    open_curl: Punct,
    branches: Vec<MatchBranch>,
    close_curl: Punct,
}

impl MatchExpr {
    #[inline]
    pub fn new(
        match_kw: Keyword,
        value: Expr,
        open_curl: Punct,
        branches: Vec<MatchBranch>,
        close_curl: Punct,
    ) -> Self {
        Self {
            match_kw,
            value: Box::new(value),
            open_curl,
            branches,
            close_curl,
        }
    }

    #[inline]
    pub fn match_kw(&self) -> &Keyword {
        &self.match_kw
    }

    #[inline]
    pub fn value(&self) -> &Expr {
        &self.value
    }

    #[inline]
    pub fn open_curl(&self) -> &Punct {
        &self.open_curl
    }

    #[inline]
    pub fn branches(&self) -> &[MatchBranch] {
        &self.branches
    }

    #[inline]
    pub fn close_curl(&self) -> &Punct {
        &self.close_curl
    }

    pub fn reset_resolved_types(&self) {
        self.value.reset_resolved_types();

        for branch in self.branches.iter() {
            branch.reset_resolved_types();
        }
    }
}

impl Spanned for MatchExpr {
    fn span(&self) -> TextSpan {
        self.match_kw.span().join(self.close_curl.span()).unwrap()
    }
}

impl DisplayScoped for MatchExpr {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        writeln!(f, "{} {} {}", self.match_kw, self.value, self.open_curl)?;

        f.enter_scope();
        for branch in self.branches.iter() {
            writeln!(f, "{branch},")?;
        }
        f.exit_scope();

        write!(f, "{}", self.close_curl)
    }
}

default_display_impl!(MatchExpr);

#[derive(Debug, Clone)]
pub enum IndexKind {
    Single(Expr),
    Range(Expr, Expr),
    RangeInclusive(Expr, Expr),
}

impl IndexKind {
    pub fn reset_resolved_types(&self) {
        match self {
            Self::Single(expr) => expr.reset_resolved_types(),
            Self::Range(start, end) => {
                start.reset_resolved_types();
                end.reset_resolved_types();
            }
            Self::RangeInclusive(start, end) => {
                start.reset_resolved_types();
                end.reset_resolved_types();
            }
        }
    }
}

impl Spanned for IndexKind {
    fn span(&self) -> TextSpan {
        match self {
            Self::Single(index) => index.span(),
            Self::Range(start, end) => start.span().join(end.span()).unwrap(),
            Self::RangeInclusive(start, end) => start.span().join(end.span()).unwrap(),
        }
    }
}

impl DisplayScoped for IndexKind {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Single(index) => DisplayScoped::fmt(index, f),
            Self::Range(start, end) => write!(f, "{start}..{end}"),
            Self::RangeInclusive(start, end) => write!(f, "{start}..={end}"),
        }
    }
}

default_display_impl!(IndexKind);

#[derive(Debug, Clone)]
pub struct Indexer {
    open_paren: Punct,
    index: Box<IndexKind>,
    close_paren: Punct,
}

impl Indexer {
    #[inline]
    pub fn new(open_paren: Punct, index: IndexKind, close_paren: Punct) -> Self {
        Self {
            open_paren,
            index: Box::new(index),
            close_paren,
        }
    }

    #[inline]
    pub fn open_paren(&self) -> &Punct {
        &self.open_paren
    }

    #[inline]
    pub fn index(&self) -> &IndexKind {
        &self.index
    }

    #[inline]
    pub fn close_paren(&self) -> &Punct {
        &self.close_paren
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.index.reset_resolved_types();
    }
}

impl Spanned for Indexer {
    fn span(&self) -> TextSpan {
        self.open_paren
            .span()
            .join(self.close_paren.span())
            .unwrap()
    }
}

impl DisplayScoped for Indexer {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}{}{}", self.open_paren, self.index, self.close_paren)
    }
}

default_display_impl!(Indexer);

#[derive(Debug, Clone)]
pub struct IndexExpr {
    base: Box<Expr>,
    indexer: Indexer,
}

impl IndexExpr {
    #[inline]
    pub fn new(base: Expr, indexer: Indexer) -> Self {
        Self {
            base: Box::new(base),
            indexer,
        }
    }

    #[inline]
    pub fn base(&self) -> &Expr {
        &self.base
    }

    #[inline]
    pub fn indexer(&self) -> &Indexer {
        &self.indexer
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.base.reset_resolved_types();
        self.indexer.reset_resolved_types();
    }
}

impl Spanned for IndexExpr {
    fn span(&self) -> TextSpan {
        self.base.span().join(self.indexer.span()).unwrap()
    }
}

impl DisplayScoped for IndexExpr {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}{}", self.base, self.indexer)
    }
}

default_display_impl!(IndexExpr);

#[derive(Debug, Clone)]
pub struct MemberAccess {
    op: Punct,
    member: Ident,
}

impl MemberAccess {
    #[inline]
    pub fn new(op: Punct, member: Ident) -> Self {
        Self { op, member }
    }

    #[inline]
    pub fn op(&self) -> &Punct {
        &self.op
    }

    #[inline]
    pub fn member(&self) -> &Ident {
        &self.member
    }
}

impl Spanned for MemberAccess {
    fn span(&self) -> TextSpan {
        self.op.span().join(self.member.span()).unwrap()
    }
}

impl DisplayScoped for MemberAccess {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}{}", self.op, self.member)
    }
}

default_display_impl!(MemberAccess);

#[derive(Debug, Clone)]
pub struct MemberAccessExpr {
    base: Box<Expr>,
    member: MemberAccess,
}

impl MemberAccessExpr {
    #[inline]
    pub fn new(base: Expr, member: MemberAccess) -> Self {
        Self {
            base: Box::new(base),
            member,
        }
    }

    #[inline]
    pub fn base(&self) -> &Expr {
        &self.base
    }

    #[inline]
    pub fn member(&self) -> &MemberAccess {
        &self.member
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.base.reset_resolved_types();
    }
}

impl Spanned for MemberAccessExpr {
    fn span(&self) -> TextSpan {
        self.base.span().join(self.member.span()).unwrap()
    }
}

impl DisplayScoped for MemberAccessExpr {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}{}", self.base, self.member)
    }
}

default_display_impl!(MemberAccessExpr);

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    op: Punct,
    inner: Box<Expr>,
}

impl UnaryExpr {
    #[inline]
    pub fn new(op: Punct, inner: Expr) -> Self {
        Self {
            op,
            inner: Box::new(inner),
        }
    }

    #[inline]
    pub fn op(&self) -> &Punct {
        &self.op
    }

    #[inline]
    pub fn inner(&self) -> &Expr {
        &self.inner
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.inner.reset_resolved_types();
    }
}

impl Spanned for UnaryExpr {
    fn span(&self) -> TextSpan {
        self.op.span().join(self.inner.span()).unwrap()
    }
}

impl DisplayScoped for UnaryExpr {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}{}", self.op, self.inner)
    }
}

default_display_impl!(UnaryExpr);

#[derive(Debug, Clone)]
pub struct CastExpr {
    value: Box<Expr>,
    as_kw: Keyword,
    target_ty: Box<Type>,
    resolved_ty: Cell<Option<TypeId>>,
}

impl CastExpr {
    #[inline]
    pub fn new(value: Expr, as_kw: Keyword, target_ty: Type) -> Self {
        Self {
            value: Box::new(value),
            as_kw,
            target_ty: Box::new(target_ty),
            resolved_ty: Cell::new(None),
        }
    }

    #[inline]
    pub fn value(&self) -> &Expr {
        &self.value
    }

    #[inline]
    pub fn as_kw(&self) -> &Keyword {
        &self.as_kw
    }

    #[inline]
    pub fn target_ty(&self) -> &Type {
        &self.target_ty
    }

    pub fn resolve(&self, ty: TypeId) {
        assert!(
            self.resolved_ty.get().is_none(),
            "construct expression was resolved more than once"
        );

        self.resolved_ty.set(Some(ty));
    }

    #[inline]
    pub fn resolved_ty(&self) -> Option<TypeId> {
        self.resolved_ty.get()
    }

    pub fn reset_resolved_types(&self) {
        self.value.reset_resolved_types();
        self.resolved_ty.set(None);
    }
}

impl Spanned for CastExpr {
    fn span(&self) -> TextSpan {
        self.value.span().join(self.target_ty.span()).unwrap()
    }
}

impl DisplayScoped for CastExpr {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.value, self.as_kw, self.target_ty)
    }
}

default_display_impl!(CastExpr);

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    lhs: Box<Expr>,
    op: Punct,
    rhs: Box<Expr>,
}

impl BinaryExpr {
    #[inline]
    pub fn new(lhs: Expr, op: Punct, rhs: Expr) -> Self {
        Self {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }

    #[inline]
    pub fn lhs(&self) -> &Expr {
        &self.lhs
    }

    #[inline]
    pub fn op(&self) -> &Punct {
        &self.op
    }

    #[inline]
    pub fn rhs(&self) -> &Expr {
        &self.rhs
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.lhs.reset_resolved_types();
        self.rhs.reset_resolved_types();
    }
}

impl Spanned for BinaryExpr {
    fn span(&self) -> TextSpan {
        self.lhs.span().join(self.rhs.span()).unwrap()
    }
}

impl DisplayScoped for BinaryExpr {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.op, self.rhs)
    }
}

default_display_impl!(BinaryExpr);

#[derive(Debug, Clone)]
pub enum Expr {
    // Leaf expressions
    BoolLiteral(BoolLiteral),
    Literal(Literal),
    Path(Path),
    Paren(ParenExpr),
    Call(CallExpr),
    Construct(ConstructExpr),

    If(IfExpr),
    Match(MatchExpr),
    Block(Box<Block>),

    // Operators
    Index(IndexExpr),
    MemberAccess(MemberAccessExpr),
    Pos(UnaryExpr),
    Neg(UnaryExpr),
    Not(UnaryExpr),
    Cast(CastExpr),
    Concat(BinaryExpr),
    Lt(BinaryExpr),
    Lte(BinaryExpr),
    Gt(BinaryExpr),
    Gte(BinaryExpr),
    Slt(BinaryExpr),
    Slte(BinaryExpr),
    Sgt(BinaryExpr),
    Sgte(BinaryExpr),
    Eq(BinaryExpr),
    Ne(BinaryExpr),
    Add(BinaryExpr),
    Sub(BinaryExpr),
    Mul(BinaryExpr),
    Div(BinaryExpr),
    Rem(BinaryExpr),
    And(BinaryExpr),
    Xor(BinaryExpr),
    Or(BinaryExpr),
    Shl(BinaryExpr),
    Lsr(BinaryExpr),
    Asr(BinaryExpr),
}

impl Expr {
    pub fn reset_resolved_types(&self) {
        match self {
            Self::BoolLiteral(_) => {}
            Self::Literal(_) => {}
            Self::Path(_) => {}
            Self::Paren(expr) => expr.reset_resolved_types(),
            Self::Call(expr) => expr.reset_resolved_types(),
            Self::Construct(expr) => expr.reset_resolved_types(),
            Self::If(expr) => expr.reset_resolved_types(),
            Self::Match(expr) => expr.reset_resolved_types(),
            Self::Block(expr) => expr.reset_resolved_types(),
            Self::Index(expr) => expr.reset_resolved_types(),
            Self::MemberAccess(expr) => expr.reset_resolved_types(),
            Self::Pos(expr) => expr.reset_resolved_types(),
            Self::Neg(expr) => expr.reset_resolved_types(),
            Self::Not(expr) => expr.reset_resolved_types(),
            Self::Cast(expr) => expr.reset_resolved_types(),
            Self::Concat(expr) => expr.reset_resolved_types(),
            Self::Lt(expr) => expr.reset_resolved_types(),
            Self::Lte(expr) => expr.reset_resolved_types(),
            Self::Gt(expr) => expr.reset_resolved_types(),
            Self::Gte(expr) => expr.reset_resolved_types(),
            Self::Slt(expr) => expr.reset_resolved_types(),
            Self::Slte(expr) => expr.reset_resolved_types(),
            Self::Sgt(expr) => expr.reset_resolved_types(),
            Self::Sgte(expr) => expr.reset_resolved_types(),
            Self::Eq(expr) => expr.reset_resolved_types(),
            Self::Ne(expr) => expr.reset_resolved_types(),
            Self::Add(expr) => expr.reset_resolved_types(),
            Self::Sub(expr) => expr.reset_resolved_types(),
            Self::Mul(expr) => expr.reset_resolved_types(),
            Self::Div(expr) => expr.reset_resolved_types(),
            Self::Rem(expr) => expr.reset_resolved_types(),
            Self::And(expr) => expr.reset_resolved_types(),
            Self::Xor(expr) => expr.reset_resolved_types(),
            Self::Or(expr) => expr.reset_resolved_types(),
            Self::Shl(expr) => expr.reset_resolved_types(),
            Self::Lsr(expr) => expr.reset_resolved_types(),
            Self::Asr(expr) => expr.reset_resolved_types(),
        }
    }
}

impl Spanned for Expr {
    fn span(&self) -> TextSpan {
        match self {
            Self::BoolLiteral(l) => l.span(),
            Self::Literal(l) => l.span(),
            Self::Path(p) => p.span(),
            Self::Paren(expr) => expr.span(),
            Self::Call(expr) => expr.span(),
            Self::Construct(expr) => expr.span(),

            Self::If(expr) => expr.span(),
            Self::Match(expr) => expr.span(),
            Self::Block(b) => b.span(),

            Self::Index(expr) => expr.span(),
            Self::MemberAccess(expr) => expr.span(),
            Self::Pos(expr) => expr.span(),
            Self::Neg(expr) => expr.span(),
            Self::Not(expr) => expr.span(),
            Self::Cast(expr) => expr.span(),
            Self::Concat(expr) => expr.span(),
            Self::Lt(expr) => expr.span(),
            Self::Lte(expr) => expr.span(),
            Self::Gt(expr) => expr.span(),
            Self::Gte(expr) => expr.span(),
            Self::Slt(expr) => expr.span(),
            Self::Slte(expr) => expr.span(),
            Self::Sgt(expr) => expr.span(),
            Self::Sgte(expr) => expr.span(),
            Self::Eq(expr) => expr.span(),
            Self::Ne(expr) => expr.span(),
            Self::Add(expr) => expr.span(),
            Self::Sub(expr) => expr.span(),
            Self::Mul(expr) => expr.span(),
            Self::Div(expr) => expr.span(),
            Self::Rem(expr) => expr.span(),
            Self::And(expr) => expr.span(),
            Self::Xor(expr) => expr.span(),
            Self::Or(expr) => expr.span(),
            Self::Shl(expr) => expr.span(),
            Self::Lsr(expr) => expr.span(),
            Self::Asr(expr) => expr.span(),
        }
    }
}

impl DisplayScoped for Expr {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::BoolLiteral(l) => DisplayScoped::fmt(l, f),
            Self::Literal(l) => DisplayScoped::fmt(l, f),
            Self::Path(p) => DisplayScoped::fmt(p, f),
            Self::Paren(expr) => DisplayScoped::fmt(expr, f),
            Self::Call(expr) => DisplayScoped::fmt(expr, f),
            Self::Construct(expr) => DisplayScoped::fmt(expr, f),

            Self::If(expr) => DisplayScoped::fmt(expr, f),
            Self::Match(expr) => DisplayScoped::fmt(expr, f),
            Self::Block(b) => DisplayScoped::fmt(b.as_ref(), f),

            Self::Index(expr) => DisplayScoped::fmt(expr, f),
            Self::MemberAccess(expr) => DisplayScoped::fmt(expr, f),
            Self::Pos(expr) => DisplayScoped::fmt(expr, f),
            Self::Neg(expr) => DisplayScoped::fmt(expr, f),
            Self::Not(expr) => DisplayScoped::fmt(expr, f),
            Self::Cast(expr) => DisplayScoped::fmt(expr, f),
            Self::Concat(expr) => DisplayScoped::fmt(expr, f),
            Self::Lt(expr) => DisplayScoped::fmt(expr, f),
            Self::Lte(expr) => DisplayScoped::fmt(expr, f),
            Self::Gt(expr) => DisplayScoped::fmt(expr, f),
            Self::Gte(expr) => DisplayScoped::fmt(expr, f),
            Self::Slt(expr) => DisplayScoped::fmt(expr, f),
            Self::Slte(expr) => DisplayScoped::fmt(expr, f),
            Self::Sgt(expr) => DisplayScoped::fmt(expr, f),
            Self::Sgte(expr) => DisplayScoped::fmt(expr, f),
            Self::Eq(expr) => DisplayScoped::fmt(expr, f),
            Self::Ne(expr) => DisplayScoped::fmt(expr, f),
            Self::Add(expr) => DisplayScoped::fmt(expr, f),
            Self::Sub(expr) => DisplayScoped::fmt(expr, f),
            Self::Mul(expr) => DisplayScoped::fmt(expr, f),
            Self::Div(expr) => DisplayScoped::fmt(expr, f),
            Self::Rem(expr) => DisplayScoped::fmt(expr, f),
            Self::And(expr) => DisplayScoped::fmt(expr, f),
            Self::Xor(expr) => DisplayScoped::fmt(expr, f),
            Self::Or(expr) => DisplayScoped::fmt(expr, f),
            Self::Shl(expr) => DisplayScoped::fmt(expr, f),
            Self::Lsr(expr) => DisplayScoped::fmt(expr, f),
            Self::Asr(expr) => DisplayScoped::fmt(expr, f),
        }
    }
}

default_display_impl!(Expr);

#[derive(Debug, Clone)]
pub struct Declaration {
    let_kw: Keyword,
    name: Ident,
    assign: Punct,
    value: Expr,
}

impl Declaration {
    #[inline]
    pub fn new(let_kw: Keyword, name: Ident, assign: Punct, value: Expr) -> Self {
        Self {
            let_kw,
            name,
            assign,
            value,
        }
    }

    #[inline]
    pub fn let_kw(&self) -> &Keyword {
        &self.let_kw
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn assign(&self) -> &Punct {
        &self.assign
    }

    #[inline]
    pub fn value(&self) -> &Expr {
        &self.value
    }
}

impl Spanned for Declaration {
    fn span(&self) -> TextSpan {
        self.let_kw.span().join(self.value.span()).unwrap()
    }
}

impl DisplayScoped for Declaration {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {} {};",
            self.let_kw, self.name, self.assign, self.value
        )
    }
}

default_display_impl!(Declaration);

#[derive(Debug, Clone)]
pub enum SuffixOp {
    Indexer(Indexer),
    MemberAccess(MemberAccess),
}

impl Spanned for SuffixOp {
    fn span(&self) -> TextSpan {
        match self {
            Self::Indexer(indexer) => indexer.span(),
            Self::MemberAccess(member) => member.span(),
        }
    }
}

impl DisplayScoped for SuffixOp {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Indexer(indexer) => DisplayScoped::fmt(indexer, f),
            Self::MemberAccess(member) => DisplayScoped::fmt(member, f),
        }
    }
}

default_display_impl!(SuffixOp);

#[derive(Debug, Clone)]
pub struct AssignTarget {
    path: Path,
    suffixes: Vec<SuffixOp>,
}

impl AssignTarget {
    #[inline]
    pub fn new(path: Path, suffixes: Vec<SuffixOp>) -> Self {
        Self { path, suffixes }
    }

    #[inline]
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[inline]
    pub fn suffixes(&self) -> &[SuffixOp] {
        &self.suffixes
    }
}

impl Spanned for AssignTarget {
    fn span(&self) -> TextSpan {
        if let Some(last) = self.suffixes.last() {
            self.path.span().join(last.span()).unwrap()
        } else {
            self.path.span()
        }
    }
}

impl DisplayScoped for AssignTarget {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}", self.path)?;

        for suffix in self.suffixes.iter() {
            write!(f, "{suffix}")?;
        }

        Ok(())
    }
}

default_display_impl!(AssignTarget);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssignKind {
    /// `=`
    Assign,
    /// `+=`
    AddAssign,
    /// `-=`
    SubAssign,
    /// `*=`
    MulAssign,
    /// `/=`
    DivAssign,
    /// `%=`
    RemAssign,
    /// `&=`
    AndAssign,
    /// `|=`
    OrAssign,
    /// `^=`
    XorAssign,
    /// `<<=`
    ShlAssign,
    /// `>>>=`
    AsrAssign,
    /// `>>=`
    LsrAssign,
}

impl DisplayScoped for AssignKind {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Assign => "=",
                Self::AddAssign => "+=",
                Self::SubAssign => "-=",
                Self::MulAssign => "*=",
                Self::DivAssign => "/=",
                Self::RemAssign => "%=",
                Self::AndAssign => "&=",
                Self::OrAssign => "|=",
                Self::XorAssign => "^=",
                Self::ShlAssign => "<<=",
                Self::AsrAssign => ">>>=",
                Self::LsrAssign => ">>=",
            }
        )
    }
}

default_display_impl!(AssignKind);

#[derive(Debug, Clone, Copy)]
pub struct AssignOp {
    kind: AssignKind,
    span: TextSpan,
}

impl AssignOp {
    #[inline]
    pub fn new(kind: AssignKind, span: TextSpan) -> Self {
        Self { kind, span }
    }

    #[inline]
    pub fn kind(&self) -> AssignKind {
        self.kind
    }
}

default_spanned_impl!(AssignOp);

impl DisplayScoped for AssignOp {
    #[inline]
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        DisplayScoped::fmt(&self.kind, f)
    }
}

default_display_impl!(AssignOp);

#[derive(Debug, Clone)]
pub struct Assignment {
    target: AssignTarget,
    op: AssignOp,
    value: Expr,
}

impl Assignment {
    #[inline]
    pub fn new(target: AssignTarget, op: AssignOp, value: Expr) -> Self {
        Self { target, op, value }
    }

    #[inline]
    pub fn target(&self) -> &AssignTarget {
        &self.target
    }

    #[inline]
    pub fn op(&self) -> &AssignOp {
        &self.op
    }

    #[inline]
    pub fn value(&self) -> &Expr {
        &self.value
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.value.reset_resolved_types();
    }
}

impl Spanned for Assignment {
    fn span(&self) -> TextSpan {
        self.target.span().join(self.value.span()).unwrap()
    }
}

impl DisplayScoped for Assignment {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{} {} {};", self.target, self.op, self.value)
    }
}

default_display_impl!(Assignment);

#[derive(Debug, Clone)]
pub struct WhileLoop {
    while_kw: Keyword,
    condition: Expr,
    body: Box<Block>,
}

impl WhileLoop {
    #[inline]
    pub fn new(while_kw: Keyword, condition: Expr, body: Block) -> Self {
        Self {
            while_kw,
            condition,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn while_kw(&self) -> &Keyword {
        &self.while_kw
    }

    #[inline]
    pub fn condition(&self) -> &Expr {
        &self.condition
    }

    #[inline]
    pub fn body(&self) -> &Block {
        &self.body
    }
}

impl Spanned for WhileLoop {
    fn span(&self) -> TextSpan {
        self.while_kw.span().join(self.body.span()).unwrap()
    }
}

impl DisplayScoped for WhileLoop {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.while_kw, self.condition, self.body)
    }
}

default_display_impl!(WhileLoop);

#[derive(Debug, Clone)]
pub enum ForLoopRange {
    Range(Expr, Expr),
    RangeInclusive(Expr, Expr),
}

impl DisplayScoped for ForLoopRange {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Range(s, e) => write!(f, "{s}..{e}"),
            Self::RangeInclusive(s, e) => write!(f, "{s}..={e}"),
        }
    }
}

default_display_impl!(ForLoopRange);

#[derive(Debug, Clone)]
pub struct ForLoop {
    for_kw: Keyword,
    item_name: Ident,
    in_kw: Keyword,
    range: ForLoopRange,
    body: Box<Block>,
}

impl ForLoop {
    #[inline]
    pub fn new(
        for_kw: Keyword,
        item_name: Ident,
        in_kw: Keyword,
        range: ForLoopRange,
        body: Block,
    ) -> Self {
        Self {
            for_kw,
            item_name,
            in_kw,
            range,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn for_kw(&self) -> &Keyword {
        &self.for_kw
    }

    #[inline]
    pub fn item_name(&self) -> &Ident {
        &self.item_name
    }

    #[inline]
    pub fn in_kw(&self) -> &Keyword {
        &self.in_kw
    }

    #[inline]
    pub fn range(&self) -> &ForLoopRange {
        &self.range
    }

    #[inline]
    pub fn body(&self) -> &Block {
        &self.body
    }
}

impl Spanned for ForLoop {
    fn span(&self) -> TextSpan {
        self.for_kw.span().join(self.body.span()).unwrap()
    }
}

impl DisplayScoped for ForLoop {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {} {} {}",
            self.for_kw, self.item_name, self.in_kw, self.range, self.body
        )
    }
}

default_display_impl!(ForLoop);

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expr),
    Declaration(Declaration),
    Assignment(Assignment),
    WhileLoop(WhileLoop),
    ForLoop(ForLoop),
    Continue(Keyword),
    Break(Keyword),
}

impl Statement {
    pub fn reset_resolved_types(&self) {
        match self {
            Self::Expr(expr) => expr.reset_resolved_types(),
            Self::Declaration(_) => {}
            Self::Assignment(assign) => assign.reset_resolved_types(),
            Self::WhileLoop(_) => {}
            Self::ForLoop(_) => {}
            Self::Continue(_) => {}
            Self::Break(_) => {}
        }
    }
}

impl Spanned for Statement {
    fn span(&self) -> TextSpan {
        match self {
            Self::Expr(expr) => expr.span(),
            Self::Declaration(decl) => decl.span(),
            Self::Assignment(assignment) => assignment.span(),
            Self::WhileLoop(while_loop) => while_loop.span(),
            Self::ForLoop(for_loop) => for_loop.span(),
            Self::Continue(kw) => kw.span(),
            Self::Break(kw) => kw.span(),
        }
    }
}

impl DisplayScoped for Statement {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Expr(expr) => match expr {
                Expr::If(_) | Expr::Match(_) | Expr::Block(_) => DisplayScoped::fmt(expr, f),
                _ => write!(f, "{expr};"),
            },
            Self::Declaration(decl) => DisplayScoped::fmt(decl, f),
            Self::Assignment(assignment) => DisplayScoped::fmt(assignment, f),
            Self::WhileLoop(while_loop) => DisplayScoped::fmt(while_loop, f),
            Self::ForLoop(for_loop) => DisplayScoped::fmt(for_loop, f),
            Self::Continue(kw) => write!(f, "{kw};"),
            Self::Break(kw) => write!(f, "{kw};"),
        }
    }
}

default_display_impl!(Statement);

#[derive(Debug, Clone)]
pub struct Block {
    open_curl: Punct,
    statements: Vec<Statement>,
    result: Option<Expr>,
    close_curl: Punct,
}

impl Block {
    #[inline]
    pub fn new(
        open_curl: Punct,
        statements: Vec<Statement>,
        result: Option<Expr>,
        close_curl: Punct,
    ) -> Self {
        Self {
            open_curl,
            statements,
            result,
            close_curl,
        }
    }

    #[inline]
    pub fn open_curl(&self) -> &Punct {
        &self.open_curl
    }

    #[inline]
    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }

    #[inline]
    pub fn result(&self) -> Option<&Expr> {
        self.result.as_ref()
    }

    #[inline]
    pub fn close_curl(&self) -> &Punct {
        &self.close_curl
    }

    pub fn reset_resolved_types(&self) {
        for statement in self.statements.iter() {
            statement.reset_resolved_types();
        }

        if let Some(result) = &self.result {
            result.reset_resolved_types();
        }
    }
}

impl Spanned for Block {
    fn span(&self) -> TextSpan {
        self.open_curl.span().join(self.close_curl.span()).unwrap()
    }
}

impl DisplayScoped for Block {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        writeln!(f, "{}", self.open_curl)?;

        f.enter_scope();
        for statement in self.statements.iter() {
            writeln!(f, "{statement}")?;
        }
        if let Some(result) = &self.result {
            writeln!(f, "{result}")?;
        }
        f.exit_scope();

        write!(f, "{}", self.close_curl)
    }
}

default_display_impl!(Block);

#[derive(Debug, Clone)]
pub enum GenericTypeArg {
    Literal(Literal),
    Ident(Ident),
    Expr(Expr),
}

impl Spanned for GenericTypeArg {
    fn span(&self) -> TextSpan {
        match self {
            Self::Literal(l) => l.span(),
            Self::Ident(i) => i.span(),
            Self::Expr(expr) => expr.span(),
        }
    }
}

impl DisplayScoped for GenericTypeArg {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => DisplayScoped::fmt(l, f),
            Self::Ident(i) => DisplayScoped::fmt(i, f),
            Self::Expr(expr) => write!(f, "{{ {expr} }}"),
        }
    }
}

default_display_impl!(GenericTypeArg);

#[derive(Debug, Clone)]
pub struct GenericTypeArgs {
    turbofish: Option<Punct>,
    open_paren: Punct,
    args: Vec<GenericTypeArg>,
    close_paren: Punct,
}

impl GenericTypeArgs {
    #[inline]
    pub fn new(
        turbofish: Option<Punct>,
        open_paren: Punct,
        args: Vec<GenericTypeArg>,
        close_paren: Punct,
    ) -> Self {
        Self {
            turbofish,
            open_paren,
            args,
            close_paren,
        }
    }

    #[inline]
    pub fn turbofish(&self) -> Option<&Punct> {
        self.turbofish.as_ref()
    }

    #[inline]
    pub fn open_paren(&self) -> &Punct {
        &self.open_paren
    }

    #[inline]
    pub fn args(&self) -> &[GenericTypeArg] {
        &self.args
    }

    #[inline]
    pub fn close_paren(&self) -> &Punct {
        &self.close_paren
    }
}

impl Spanned for GenericTypeArgs {
    fn span(&self) -> TextSpan {
        self.open_paren
            .span()
            .join(self.close_paren.span())
            .unwrap()
    }
}

impl DisplayScoped for GenericTypeArgs {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        if let Some(turbofish) = &self.turbofish {
            write!(f, "{turbofish}")?;
        }

        write!(f, "{}", self.open_paren)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i == 0 {
                write!(f, "{arg}")?;
            } else {
                write!(f, ", {arg}")?;
            }
        }
        write!(f, "{}", self.close_paren)
    }
}

default_display_impl!(GenericTypeArgs);

#[derive(Debug, Clone)]
pub struct NamedType {
    name: Ident,
    generic_args: Option<GenericTypeArgs>,
}

impl NamedType {
    #[inline]
    pub fn new(name: Ident, generic_args: Option<GenericTypeArgs>) -> Self {
        Self { name, generic_args }
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn generic_args(&self) -> Option<&GenericTypeArgs> {
        self.generic_args.as_ref()
    }

    pub fn generic_arg_count(&self) -> usize {
        self.generic_args()
            .map(|args| args.args().len())
            .unwrap_or(0)
    }
}

impl Spanned for NamedType {
    fn span(&self) -> TextSpan {
        if let Some(generic_args) = &self.generic_args {
            self.name.span().join(generic_args.span()).unwrap()
        } else {
            self.name.span()
        }
    }
}

impl DisplayScoped for NamedType {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        if let Some(generic_args) = &self.generic_args {
            write!(f, "{}{}", self.name, generic_args)
        } else {
            DisplayScoped::fmt(&self.name, f)
        }
    }
}

default_display_impl!(NamedType);

#[derive(Debug, Clone)]
pub struct ArrayType {
    open_bracket: Punct,
    item_ty: Box<Type>,
    sep: Punct,
    len: Expr,
    close_bracket: Punct,
}

impl ArrayType {
    #[inline]
    pub fn new(
        open_bracket: Punct,
        item_ty: Type,
        sep: Punct,
        len: Expr,
        close_bracket: Punct,
    ) -> Self {
        Self {
            open_bracket,
            item_ty: Box::new(item_ty),
            sep,
            len,
            close_bracket,
        }
    }

    #[inline]
    pub fn open_bracket(&self) -> &Punct {
        &self.open_bracket
    }

    #[inline]
    pub fn item_ty(&self) -> &Type {
        &self.item_ty
    }

    #[inline]
    pub fn sep(&self) -> &Punct {
        &self.sep
    }

    #[inline]
    pub fn len(&self) -> &Expr {
        &self.len
    }

    #[inline]
    pub fn close_bracket(&self) -> &Punct {
        &self.close_bracket
    }
}

impl Spanned for ArrayType {
    fn span(&self) -> TextSpan {
        self.open_bracket
            .span()
            .join(self.close_bracket.span())
            .unwrap()
    }
}

impl DisplayScoped for ArrayType {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{} {}{}",
            self.open_bracket, self.item_ty, self.sep, self.len, self.close_bracket
        )
    }
}

default_display_impl!(ArrayType);

#[derive(Debug, Clone)]
pub enum Type {
    Named(NamedType),
    Array(ArrayType),
}

impl Spanned for Type {
    fn span(&self) -> TextSpan {
        match self {
            Self::Named(ty) => ty.span(),
            Self::Array(ty) => ty.span(),
        }
    }
}

impl DisplayScoped for Type {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Named(ty) => DisplayScoped::fmt(ty, f),
            Self::Array(ty) => DisplayScoped::fmt(ty, f),
        }
    }
}

default_display_impl!(Type);

#[derive(Debug, Clone)]
pub struct Field {
    name: Ident,
    sep: Punct,
    ty: Type,
}

impl Field {
    #[inline]
    pub fn new(name: Ident, sep: Punct, ty: Type) -> Self {
        Self { name, sep, ty }
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn sep(&self) -> &Punct {
        &self.sep
    }

    #[inline]
    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

impl Spanned for Field {
    fn span(&self) -> TextSpan {
        self.name.span().join(self.ty.span()).unwrap()
    }
}

impl DisplayScoped for Field {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}{} {}", self.name, self.sep, self.ty)
    }
}

default_display_impl!(Field);

#[derive(Debug, Clone)]
pub struct GenericStructArgs {
    open_paren: Punct,
    args: Vec<Ident>,
    close_paren: Punct,
}

impl GenericStructArgs {
    #[inline]
    pub fn new(open_paren: Punct, args: Vec<Ident>, close_paren: Punct) -> Self {
        Self {
            open_paren,
            args,
            close_paren,
        }
    }

    #[inline]
    pub fn open_paren(&self) -> &Punct {
        &self.open_paren
    }

    #[inline]
    pub fn args(&self) -> &[Ident] {
        &self.args
    }

    #[inline]
    pub fn close_paren(&self) -> &Punct {
        &self.close_paren
    }
}

impl Spanned for GenericStructArgs {
    fn span(&self) -> TextSpan {
        self.open_paren
            .span()
            .join(self.close_paren.span())
            .unwrap()
    }
}

impl DisplayScoped for GenericStructArgs {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}", self.open_paren)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i == 0 {
                write!(f, "{arg}")?;
            } else {
                write!(f, ", {arg}")?;
            }
        }
        write!(f, "{}", self.close_paren)
    }
}

default_display_impl!(GenericStructArgs);

#[derive(Debug, Clone)]
pub struct Struct {
    struct_kw: Keyword,
    name: Ident,
    generic_args: Option<GenericStructArgs>,
    open_curl: Punct,
    fields: Vec<Field>,
    close_curl: Punct,
}

impl Struct {
    #[inline]
    pub fn new(
        struct_kw: Keyword,
        name: Ident,
        generic_args: Option<GenericStructArgs>,
        open_curl: Punct,
        fields: Vec<Field>,
        close_curl: Punct,
    ) -> Self {
        Self {
            struct_kw,
            name,
            generic_args,
            open_curl,
            fields,
            close_curl,
        }
    }

    #[inline]
    pub fn struct_kw(&self) -> &Keyword {
        &self.struct_kw
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn generic_args(&self) -> Option<&GenericStructArgs> {
        self.generic_args.as_ref()
    }

    pub fn generic_arg_count(&self) -> usize {
        self.generic_args()
            .map(|args| args.args().len())
            .unwrap_or(0)
    }

    #[inline]
    pub fn open_curl(&self) -> &Punct {
        &self.open_curl
    }

    #[inline]
    pub fn fields(&self) -> &[Field] {
        &self.fields
    }

    #[inline]
    pub fn close_curl(&self) -> &Punct {
        &self.close_curl
    }
}

impl Spanned for Struct {
    fn span(&self) -> TextSpan {
        self.struct_kw.span().join(self.close_curl.span()).unwrap()
    }
}

impl DisplayScoped for Struct {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        if let Some(generic_args) = &self.generic_args {
            writeln!(
                f,
                "{} {}{} {}",
                self.struct_kw, self.name, generic_args, self.open_curl
            )?;
        } else {
            writeln!(f, "{} {} {}", self.struct_kw, self.name, self.open_curl)?;
        }

        f.enter_scope();
        for field in self.fields.iter() {
            writeln!(f, "{field},")?;
        }
        f.exit_scope();

        write!(f, "{}", self.close_curl)
    }
}

default_display_impl!(Struct);

#[derive(Debug, Clone)]
pub struct Variant {
    name: Ident,
    value: Option<Expr>,
}

impl Variant {
    #[inline]
    pub fn new(name: Ident, value: Option<Expr>) -> Self {
        Self { name, value }
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn value(&self) -> Option<&Expr> {
        self.value.as_ref()
    }
}

impl Spanned for Variant {
    fn span(&self) -> TextSpan {
        if let Some(value) = &self.value {
            self.name.span().join(value.span()).unwrap()
        } else {
            self.name.span()
        }
    }
}

impl DisplayScoped for Variant {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        if let Some(value) = &self.value {
            write!(f, "{} = {}", self.name, value)
        } else {
            DisplayScoped::fmt(&self.name, f)
        }
    }
}

default_display_impl!(Variant);

#[derive(Debug, Clone)]
pub struct Enum {
    enum_kw: Keyword,
    name: Ident,
    ty_sep: Punct,
    base_ty: Type,
    open_curl: Punct,
    variants: Vec<Variant>,
    close_curl: Punct,
}

impl Enum {
    #[inline]
    pub fn new(
        enum_kw: Keyword,
        name: Ident,
        ty_sep: Punct,
        base_ty: Type,
        open_curl: Punct,
        variants: Vec<Variant>,
        close_curl: Punct,
    ) -> Self {
        Self {
            enum_kw,
            name,
            ty_sep,
            base_ty,
            open_curl,
            variants,
            close_curl,
        }
    }

    #[inline]
    pub fn enum_kw(&self) -> &Keyword {
        &self.enum_kw
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn ty_sep(&self) -> &Punct {
        &self.ty_sep
    }

    #[inline]
    pub fn base_ty(&self) -> &Type {
        &self.base_ty
    }

    #[inline]
    pub fn open_curl(&self) -> &Punct {
        &self.open_curl
    }

    #[inline]
    pub fn variants(&self) -> &[Variant] {
        &self.variants
    }

    #[inline]
    pub fn close_curl(&self) -> &Punct {
        &self.close_curl
    }
}

impl Spanned for Enum {
    fn span(&self) -> TextSpan {
        self.enum_kw.span().join(self.close_curl.span()).unwrap()
    }
}

impl DisplayScoped for Enum {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        writeln!(
            f,
            "{} {}: {} {}",
            self.enum_kw, self.name, self.base_ty, self.open_curl
        )?;

        f.enter_scope();
        for variant in self.variants.iter() {
            writeln!(f, "{variant},")?;
        }
        f.exit_scope();

        write!(f, "{}", self.close_curl)
    }
}

default_display_impl!(Enum);

#[derive(Debug, Clone)]
pub struct AttributeValue {
    open_paren: Punct,
    value: SharedString,
    close_paren: Punct,
}

impl AttributeValue {
    #[inline]
    pub fn new(open_paren: Punct, value: SharedString, close_paren: Punct) -> Self {
        Self {
            open_paren,
            value,
            close_paren,
        }
    }

    #[inline]
    pub fn open_paren(&self) -> &Punct {
        &self.open_paren
    }

    #[inline]
    pub fn value(&self) -> &SharedString {
        &self.value
    }

    #[inline]
    pub fn close_paren(&self) -> &Punct {
        &self.close_paren
    }
}

impl Spanned for AttributeValue {
    fn span(&self) -> TextSpan {
        self.open_paren
            .span()
            .join(self.close_paren.span())
            .unwrap()
    }
}

impl DisplayScoped for AttributeValue {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}{}{}", self.open_paren, self.value, self.close_paren)
    }
}

default_display_impl!(AttributeValue);

#[derive(Debug, Clone)]
pub struct Attribute {
    name: Ident,
    value: Option<AttributeValue>,
}

impl Attribute {
    #[inline]
    pub fn new(name: Ident, value: Option<AttributeValue>) -> Self {
        Self { name, value }
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn value(&self) -> Option<&AttributeValue> {
        self.value.as_ref()
    }
}

impl Spanned for Attribute {
    fn span(&self) -> TextSpan {
        if let Some(value) = &self.value {
            self.name.span().join(value.span()).unwrap()
        } else {
            self.name.span()
        }
    }
}

impl DisplayScoped for Attribute {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        if let Some(value) = &self.value {
            write!(f, "{}{}", self.name, value)
        } else {
            DisplayScoped::fmt(&self.name, f)
        }
    }
}

default_display_impl!(Attribute);

#[derive(Debug, Clone)]
pub struct AttributeList {
    hash: Punct,
    open_bracket: Punct,
    attributes: Vec<Attribute>,
    close_bracket: Punct,
}

impl AttributeList {
    #[inline]
    pub fn new(
        hash: Punct,
        open_bracket: Punct,
        attributes: Vec<Attribute>,
        close_bracket: Punct,
    ) -> Self {
        Self {
            hash,
            open_bracket,
            attributes,
            close_bracket,
        }
    }

    #[inline]
    pub fn hash(&self) -> &Punct {
        &self.hash
    }

    #[inline]
    pub fn open_bracket(&self) -> &Punct {
        &self.open_bracket
    }

    #[inline]
    pub fn attributes(&self) -> &[Attribute] {
        &self.attributes
    }

    #[inline]
    pub fn close_bracket(&self) -> &Punct {
        &self.close_bracket
    }
}

impl Spanned for AttributeList {
    fn span(&self) -> TextSpan {
        self.hash.span().join(self.close_bracket.span()).unwrap()
    }
}

impl DisplayScoped for AttributeList {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{}{}", self.hash, self.open_bracket)?;
        for (i, attribute) in self.attributes.iter().enumerate() {
            if i == 0 {
                write!(f, "{attribute}")?;
            } else {
                write!(f, ", {attribute}")?;
            }
        }
        write!(f, "{}", self.close_bracket)
    }
}

default_display_impl!(AttributeList);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LogicKind {
    Signal,
    Register,
    Module,
}

impl DisplayScoped for LogicKind {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Signal => write!(f, "sig"),
            Self::Register => write!(f, "reg"),
            Self::Module => write!(f, "let"),
        }
    }
}

default_display_impl!(LogicKind);

#[derive(Debug, Clone, Copy)]
pub struct LogicMode {
    kind: LogicKind,
    span: TextSpan,
}

impl LogicMode {
    #[inline]
    pub fn new(kind: LogicKind, span: TextSpan) -> Self {
        Self { kind, span }
    }

    #[inline]
    pub fn kind(&self) -> LogicKind {
        self.kind
    }
}

default_spanned_impl!(LogicMode);

impl DisplayScoped for LogicMode {
    #[inline]
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        DisplayScoped::fmt(&self.kind, f)
    }
}

default_display_impl!(LogicMode);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
    In,
    Out,
    InOut,
}

impl DisplayScoped for Direction {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::In => write!(f, "in"),
            Self::Out => write!(f, "out"),
            Self::InOut => write!(f, "inout"),
        }
    }
}

default_display_impl!(Direction);

#[derive(Debug, Clone, Copy)]
pub struct PortMode {
    dir: Direction,
    span: TextSpan,
}

impl PortMode {
    #[inline]
    pub fn new(dir: Direction, span: TextSpan) -> Self {
        Self { dir, span }
    }

    #[inline]
    pub fn dir(&self) -> Direction {
        self.dir
    }
}

default_spanned_impl!(PortMode);

impl DisplayScoped for PortMode {
    #[inline]
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        DisplayScoped::fmt(&self.dir, f)
    }
}

default_display_impl!(PortMode);

#[derive(Debug, Clone)]
pub struct Port {
    attributes: Vec<AttributeList>,
    mode: PortMode,
    logic_mode: LogicMode,
    name: Ident,
    sep: Punct,
    ty: Type,
}

impl Port {
    #[inline]
    pub fn new(
        attributes: Vec<AttributeList>,
        mode: PortMode,
        logic_mode: LogicMode,
        name: Ident,
        sep: Punct,
        ty: Type,
    ) -> Self {
        Self {
            attributes,
            mode,
            logic_mode,
            name,
            sep,
            ty,
        }
    }

    #[inline]
    pub fn attributes(&self) -> &[AttributeList] {
        self.attributes.as_ref()
    }

    #[inline]
    pub fn mode(&self) -> &PortMode {
        &self.mode
    }

    #[inline]
    pub fn logic_mode(&self) -> &LogicMode {
        &self.logic_mode
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn sep(&self) -> &Punct {
        &self.sep
    }

    #[inline]
    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

impl Spanned for Port {
    fn span(&self) -> TextSpan {
        if let Some(first) = self.attributes.first() {
            first.span().join(self.ty.span()).unwrap()
        } else {
            self.mode.span().join(self.ty.span()).unwrap()
        }
    }
}

impl DisplayScoped for Port {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        for attribute_list in self.attributes.iter() {
            writeln!(f, "{attribute_list}")?;
        }

        write!(
            f,
            "{} {} {}{} {}",
            self.mode, self.logic_mode, self.name, self.sep, self.ty
        )
    }
}

default_display_impl!(Port);

#[derive(Debug, Clone)]
pub struct Const {
    const_kw: Keyword,
    name: Ident,
    assign: Punct,
    value: Expr,
}

impl Const {
    #[inline]
    pub fn new(const_kw: Keyword, name: Ident, assign: Punct, value: Expr) -> Self {
        Self {
            const_kw,
            name,
            assign,
            value,
        }
    }

    #[inline]
    pub fn const_kw(&self) -> &Keyword {
        &self.const_kw
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn assign(&self) -> &Punct {
        &self.assign
    }

    #[inline]
    pub fn value(&self) -> &Expr {
        &self.value
    }
}

impl Spanned for Const {
    fn span(&self) -> TextSpan {
        self.const_kw.span().join(self.value.span()).unwrap()
    }
}

impl DisplayScoped for Const {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {} {};",
            self.const_kw, self.name, self.assign, self.value
        )
    }
}

default_display_impl!(Const);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EdgeKind {
    Rising,
    Falling,
}

impl DisplayScoped for EdgeKind {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Rising => write!(f, "rising"),
            Self::Falling => write!(f, "falling"),
        }
    }
}

default_display_impl!(EdgeKind);

#[derive(Debug, Clone, Copy)]
pub struct Edge {
    kind: EdgeKind,
    span: TextSpan,
}

impl Edge {
    #[inline]
    pub fn new(kind: EdgeKind, span: TextSpan) -> Self {
        Self { kind, span }
    }

    #[inline]
    pub fn kind(&self) -> EdgeKind {
        self.kind
    }
}

default_spanned_impl!(Edge);

impl DisplayScoped for Edge {
    #[inline]
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        DisplayScoped::fmt(&self.kind, f)
    }
}

default_display_impl!(Edge);

#[derive(Debug, Clone)]
pub struct Sens {
    edge: Edge,
    open_paren: Punct,
    sig: AssignTarget,
    close_paren: Punct,
}

impl Sens {
    #[inline]
    pub fn new(edge: Edge, open_paren: Punct, sig: AssignTarget, close_paren: Punct) -> Self {
        Self {
            edge,
            open_paren,
            sig,
            close_paren,
        }
    }

    #[inline]
    pub fn edge(&self) -> &Edge {
        &self.edge
    }

    #[inline]
    pub fn open_paren(&self) -> &Punct {
        &self.open_paren
    }

    #[inline]
    pub fn sig(&self) -> &AssignTarget {
        &self.sig
    }

    #[inline]
    pub fn close_paren(&self) -> &Punct {
        &self.close_paren
    }
}

impl Spanned for Sens {
    fn span(&self) -> TextSpan {
        self.edge.span().join(self.close_paren.span()).unwrap()
    }
}

impl DisplayScoped for Sens {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.edge, self.open_paren, self.sig, self.close_paren
        )
    }
}

default_display_impl!(Sens);

#[derive(Debug, Clone)]
pub struct LogicMember {
    mode: LogicMode,
    name: Ident,
    sep: Punct,
    ty: Type,
}

impl LogicMember {
    #[inline]
    pub fn new(mode: LogicMode, name: Ident, sep: Punct, ty: Type) -> Self {
        Self {
            mode,
            name,
            sep,
            ty,
        }
    }

    #[inline]
    pub fn mode(&self) -> &LogicMode {
        &self.mode
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn sep(&self) -> &Punct {
        &self.sep
    }

    #[inline]
    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

impl Spanned for LogicMember {
    fn span(&self) -> TextSpan {
        self.mode.span().join(self.ty.span()).unwrap()
    }
}

impl DisplayScoped for LogicMember {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{} {}{} {};", self.mode, self.name, self.sep, self.ty)
    }
}

default_display_impl!(LogicMember);

#[derive(Debug, Clone)]
pub struct ProcMember {
    proc_kw: Keyword,
    sens: Vec<Sens>,
    body: Box<Block>,
}

impl ProcMember {
    #[inline]
    pub fn new(proc_kw: Keyword, sens: Vec<Sens>, body: Block) -> Self {
        Self {
            proc_kw,
            sens,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn proc_kw(&self) -> &Keyword {
        &self.proc_kw
    }

    #[inline]
    pub fn sens(&self) -> &[Sens] {
        &self.sens
    }

    #[inline]
    pub fn body(&self) -> &Block {
        &self.body
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.body.reset_resolved_types();
    }
}

impl Spanned for ProcMember {
    fn span(&self) -> TextSpan {
        self.proc_kw.span().join(self.body.span()).unwrap()
    }
}

impl DisplayScoped for ProcMember {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{} ", self.proc_kw)?;

        for (i, sens) in self.sens.iter().enumerate() {
            if i > 0 {
                write!(f, " | {sens}")?;
            } else {
                write!(f, "{sens}")?;
            }
        }

        write!(f, " {}", self.body)
    }
}

default_display_impl!(ProcMember);

#[derive(Debug, Clone)]
pub struct CombMember {
    comb_kw: Keyword,
    body: Box<Block>,
}

impl CombMember {
    #[inline]
    pub fn new(comb_kw: Keyword, body: Block) -> Self {
        Self {
            comb_kw,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn comb_kw(&self) -> &Keyword {
        &self.comb_kw
    }

    #[inline]
    pub fn body(&self) -> &Block {
        &self.body
    }

    #[inline]
    pub fn reset_resolved_types(&self) {
        self.body.reset_resolved_types();
    }
}

impl Spanned for CombMember {
    fn span(&self) -> TextSpan {
        self.comb_kw.span().join(self.body.span()).unwrap()
    }
}

impl DisplayScoped for CombMember {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{} {}", self.comb_kw, self.body)
    }
}

default_display_impl!(CombMember);

#[derive(Debug, Clone)]
pub enum MemberKind {
    Logic(LogicMember),
    Const(Const),
    Proc(ProcMember),
    Comb(CombMember),
}

impl MemberKind {
    pub fn name(&self) -> Option<&Ident> {
        match self {
            Self::Logic(member) => Some(member.name()),
            Self::Const(member) => Some(member.name()),
            Self::Proc(_) => None,
            Self::Comb(_) => None,
        }
    }
}

impl Spanned for MemberKind {
    fn span(&self) -> TextSpan {
        match self {
            Self::Logic(member) => member.span(),
            Self::Const(member) => member.span(),
            Self::Proc(member) => member.span(),
            Self::Comb(member) => member.span(),
        }
    }
}

impl DisplayScoped for MemberKind {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Logic(member) => DisplayScoped::fmt(member, f),
            Self::Const(member) => DisplayScoped::fmt(member, f),
            Self::Proc(member) => DisplayScoped::fmt(member, f),
            Self::Comb(member) => DisplayScoped::fmt(member, f),
        }
    }
}

default_display_impl!(MemberKind);

#[derive(Debug, Clone)]
pub struct Member {
    attributes: Vec<AttributeList>,
    kind: MemberKind,
}

impl Member {
    #[inline]
    pub fn new(attributes: Vec<AttributeList>, kind: MemberKind) -> Self {
        Self { attributes, kind }
    }

    #[inline]
    pub fn attributes(&self) -> &[AttributeList] {
        &self.attributes
    }

    #[inline]
    pub fn kind(&self) -> &MemberKind {
        &self.kind
    }

    #[inline]
    pub fn name(&self) -> Option<&Ident> {
        self.kind.name()
    }
}

impl Spanned for Member {
    fn span(&self) -> TextSpan {
        if let Some(first) = self.attributes.first() {
            first.span().join(self.kind.span()).unwrap()
        } else {
            self.kind.span()
        }
    }
}

impl DisplayScoped for Member {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        for attribute_list in self.attributes().iter() {
            writeln!(f, "{attribute_list}")?;
        }

        write!(f, "{}", self.kind)
    }
}

default_display_impl!(Member);

#[derive(Debug, Clone)]
pub struct Module {
    mod_kw: Keyword,
    name: Ident,
    generic_args: Option<GenericStructArgs>,
    open_paren: Punct,
    ports: Vec<Port>,
    close_paren: Punct,
    open_curl: Punct,
    members: Vec<Member>,
    close_curl: Punct,
}

impl Module {
    #[inline]
    pub fn new(
        mod_kw: Keyword,
        name: Ident,
        generic_args: Option<GenericStructArgs>,
        open_paren: Punct,
        ports: Vec<Port>,
        close_paren: Punct,
        open_curl: Punct,
        members: Vec<Member>,
        close_curl: Punct,
    ) -> Self {
        Self {
            mod_kw,
            name,
            generic_args,
            open_paren,
            ports,
            close_paren,
            open_curl,
            members,
            close_curl,
        }
    }

    #[inline]
    pub fn mod_kw(&self) -> &Keyword {
        &self.mod_kw
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn generic_args(&self) -> Option<&GenericStructArgs> {
        self.generic_args.as_ref()
    }

    pub fn generic_arg_count(&self) -> usize {
        self.generic_args()
            .map(|args| args.args().len())
            .unwrap_or(0)
    }

    #[inline]
    pub fn open_paren(&self) -> &Punct {
        &self.open_paren
    }

    #[inline]
    pub fn ports(&self) -> &[Port] {
        &self.ports
    }

    #[inline]
    pub fn close_paren(&self) -> &Punct {
        &self.close_paren
    }

    #[inline]
    pub fn open_curl(&self) -> &Punct {
        &self.open_curl
    }

    #[inline]
    pub fn members(&self) -> &[Member] {
        &self.members
    }

    #[inline]
    pub fn close_curl(&self) -> &Punct {
        &self.close_curl
    }
}

impl Spanned for Module {
    fn span(&self) -> TextSpan {
        self.mod_kw.span().join(self.close_curl.span()).unwrap()
    }
}

impl DisplayScoped for Module {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        if let Some(generic_args) = &self.generic_args {
            writeln!(
                f,
                "{} {}{} {}",
                self.mod_kw, self.name, generic_args, self.open_paren
            )?;
        } else {
            writeln!(f, "{} {} {}", self.mod_kw, self.name, self.open_paren)?;
        }

        f.enter_scope();
        for port in self.ports.iter() {
            writeln!(f, "{port},")?;
        }
        f.exit_scope();
        writeln!(f, "{} {}", self.close_paren, self.open_curl)?;
        f.enter_scope();
        for member in self.members.iter() {
            writeln!(f, "{member}")?;
        }
        f.exit_scope();
        write!(f, "{}", self.close_curl)
    }
}

default_display_impl!(Module);

#[derive(Debug, Clone)]
pub struct ExternModule {
    extern_kw: Keyword,
    mod_kw: Keyword,
    name: Ident,
    open_paren: Punct,
    ports: Vec<Port>,
    close_paren: Punct,
}

impl ExternModule {
    #[inline]
    pub fn new(
        extern_kw: Keyword,
        mod_kw: Keyword,
        name: Ident,
        open_paren: Punct,
        ports: Vec<Port>,
        close_paren: Punct,
    ) -> Self {
        Self {
            extern_kw,
            mod_kw,
            name,
            open_paren,
            ports,
            close_paren,
        }
    }

    #[inline]
    pub fn extern_kw(&self) -> &Keyword {
        &self.extern_kw
    }

    #[inline]
    pub fn mod_kw(&self) -> &Keyword {
        &self.mod_kw
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn open_paren(&self) -> &Punct {
        &self.open_paren
    }

    #[inline]
    pub fn ports(&self) -> &[Port] {
        &self.ports
    }

    #[inline]
    pub fn close_paren(&self) -> &Punct {
        &self.close_paren
    }
}

impl Spanned for ExternModule {
    fn span(&self) -> TextSpan {
        self.extern_kw.span().join(self.close_paren.span()).unwrap()
    }
}

impl DisplayScoped for ExternModule {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        writeln!(f, "{} {} {}", self.mod_kw, self.name, self.open_paren)?;
        f.enter_scope();
        for port in self.ports.iter() {
            writeln!(f, "{port},")?;
        }
        f.exit_scope();
        write!(f, "{};", self.close_paren)
    }
}

default_display_impl!(ExternModule);

#[derive(Debug, Clone)]
pub struct Func {
    fn_kw: Keyword,
    name: Ident,
    open_paren: Punct,
    args: Vec<Ident>,
    close_paren: Punct,
    body: Box<Block>,
}

impl Func {
    #[inline]
    pub fn new(
        fn_kw: Keyword,
        name: Ident,
        open_paren: Punct,
        args: Vec<Ident>,
        close_paren: Punct,
        body: Block,
    ) -> Self {
        Self {
            fn_kw,
            name,
            open_paren,
            args,
            close_paren,
            body: Box::new(body),
        }
    }

    #[inline]
    pub fn fn_kw(&self) -> &Keyword {
        &self.fn_kw
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn open_paren(&self) -> &Punct {
        &self.open_paren
    }

    #[inline]
    pub fn args(&self) -> &[Ident] {
        &self.args
    }

    #[inline]
    pub fn close_paren(&self) -> &Punct {
        &self.close_paren
    }

    #[inline]
    pub fn body(&self) -> &Block {
        &self.body
    }
}

impl Spanned for Func {
    fn span(&self) -> TextSpan {
        self.fn_kw.span().join(self.body.span()).unwrap()
    }
}

impl DisplayScoped for Func {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        write!(f, "{} {}{}", self.fn_kw, self.name, self.open_paren)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", {arg}")?;
            } else {
                write!(f, "{arg}")?;
            }
        }

        write!(f, "{} {}", self.close_paren, self.body)
    }
}

default_display_impl!(Func);

#[derive(Debug, Clone)]
pub enum ItemKind {
    Struct(Struct),
    Enum(Enum),
    Const(Const),
    Module(Module),
    ExternModule(ExternModule),
    Func(Func),
}

impl ItemKind {
    pub fn name(&self) -> &Ident {
        match self {
            Self::Struct(struct_item) => struct_item.name(),
            Self::Enum(enum_item) => enum_item.name(),
            Self::Const(const_item) => const_item.name(),
            Self::Module(module_item) => module_item.name(),
            Self::ExternModule(module_item) => module_item.name(),
            Self::Func(func_item) => func_item.name(),
        }
    }
}

impl Spanned for ItemKind {
    fn span(&self) -> TextSpan {
        match self {
            Self::Struct(struct_item) => struct_item.span(),
            Self::Enum(enum_item) => enum_item.span(),
            Self::Const(const_item) => const_item.span(),
            Self::Module(module_item) => module_item.span(),
            Self::ExternModule(module_item) => module_item.span(),
            Self::Func(func_item) => func_item.span(),
        }
    }
}

impl DisplayScoped for ItemKind {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Struct(struct_item) => DisplayScoped::fmt(struct_item, f),
            Self::Enum(enum_item) => DisplayScoped::fmt(enum_item, f),
            Self::Const(const_item) => DisplayScoped::fmt(const_item, f),
            Self::Module(module_item) => DisplayScoped::fmt(module_item, f),
            Self::ExternModule(module_item) => DisplayScoped::fmt(module_item, f),
            Self::Func(fn_item) => DisplayScoped::fmt(fn_item, f),
        }
    }
}

default_display_impl!(ItemKind);

#[derive(Debug, Clone)]
pub struct Item {
    pub attributes: Vec<AttributeList>,
    pub kind: ItemKind,
}

impl Item {
    #[inline]
    pub fn new(attributes: Vec<AttributeList>, kind: ItemKind) -> Self {
        Self { attributes, kind }
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        self.kind.name()
    }
}

impl Spanned for Item {
    fn span(&self) -> TextSpan {
        if let Some(first) = self.attributes.first() {
            first.span().join(self.kind.span()).unwrap()
        } else {
            self.kind.span()
        }
    }
}

impl DisplayScoped for Item {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        for attribute_list in self.attributes.iter() {
            writeln!(f, "{attribute_list}")?;
        }

        write!(f, "{}", self.kind)
    }
}

default_display_impl!(Item);
