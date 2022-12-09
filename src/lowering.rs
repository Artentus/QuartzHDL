use crate::ast::Path;
use crate::ir::*;
use crate::vir::*;
use crate::{HashMap, SharedString};

fn add_tmp_member(ty: TypeId, tmp_members: &mut Vec<(SharedString, TypeId)>) -> SharedString {
    let name: SharedString = format!("__tmp_{}", tmp_members.len()).into();
    tmp_members.push((SharedString::clone(&name), ty));
    name
}

fn lower_expr_block(
    block: &CheckedExprBlock,
    target: SharedString,
    mode: VAssignMode,
    tmp_members: &mut Vec<(SharedString, TypeId)>,
    tmp_comb_statements: &mut Vec<VStatement>,
    tmp_proc_statements: &mut Vec<VStatement>,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> (VBlock, Option<VBlock>) {
    let mut statements_comb = Vec::with_capacity(block.statements().len() + 1);
    let mut statements_proc = Vec::with_capacity(block.statements().len());
    for statement in block.statements() {
        if let Some(statement) = lower_statement(
            statement,
            VAssignMode::Combinatoric,
            false,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        ) {
            statements_comb.push(statement);
        }

        if let Some(statement) = lower_statement(
            statement,
            mode,
            true,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        ) {
            statements_proc.push(statement);
        }
    }

    let result = lower_expr(
        block.result(),
        mode,
        tmp_members,
        tmp_comb_statements,
        tmp_proc_statements,
        known_types,
        resolved_types,
    );
    let assign_target = VAssignTarget::new(target, Vec::new());
    statements_comb.push(VStatement::Assignment(VAssignment::new(
        assign_target,
        VAssignMode::Combinatoric,
        result,
    )));

    if statements_proc.is_empty() {
        (VBlock::new(statements_comb), None)
    } else {
        (
            VBlock::new(statements_comb),
            Some(VBlock::new(statements_proc)),
        )
    }
}

fn lower_path(path: &Path) -> String {
    let mut result = String::new();
    result.push_str(path.head().as_ref());
    for segment in path.tail().iter() {
        result.push_str("__");
        result.push_str(segment.ident().as_ref());
    }
    result
}

fn lower_construct_expr(
    construct_expr: &CheckedConstructExpr,
    mode: VAssignMode,
    tmp_members: &mut Vec<(SharedString, TypeId)>,
    tmp_comb_statements: &mut Vec<VStatement>,
    tmp_proc_statements: &mut Vec<VStatement>,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> VExpr {
    let tmp_member = add_tmp_member(construct_expr.ty(), tmp_members);

    let mut statements = Vec::with_capacity(construct_expr.fields().len());
    for (field, value) in construct_expr.fields() {
        let value = lower_expr(
            value,
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        );

        let suffixes = vec![VSuffixOp::MemberAccess(SharedString::clone(field))];
        let target = VAssignTarget::new(SharedString::clone(&tmp_member), suffixes);
        statements.push(VStatement::Assignment(VAssignment::new(
            target,
            VAssignMode::Combinatoric,
            value,
        )));
    }

    let block = VBlock::new(statements);
    tmp_comb_statements.push(VStatement::Block(block));

    VExpr::Ident(tmp_member)
}

fn lower_if_expr(
    if_expr: &CheckedIfExpr,
    mode: VAssignMode,
    tmp_members: &mut Vec<(SharedString, TypeId)>,
    tmp_comb_statements: &mut Vec<VStatement>,
    tmp_proc_statements: &mut Vec<VStatement>,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> VExpr {
    let tmp_member = add_tmp_member(if_expr.ty(), tmp_members);

    let condition = lower_expr(
        if_expr.condition(),
        VAssignMode::Combinatoric,
        tmp_members,
        tmp_comb_statements,
        tmp_proc_statements,
        known_types,
        resolved_types,
    );
    let (body_comb, body_proc) = lower_expr_block(
        if_expr.body(),
        SharedString::clone(&tmp_member),
        mode,
        tmp_members,
        tmp_comb_statements,
        tmp_proc_statements,
        known_types,
        resolved_types,
    );

    let mut else_if_blocks_comb = Vec::with_capacity(if_expr.else_if_blocks().len());
    let mut else_if_blocks_proc = Vec::with_capacity(if_expr.else_if_blocks().len());
    for else_if_block in if_expr.else_if_blocks() {
        let condition = lower_expr(
            else_if_block.condition(),
            VAssignMode::Combinatoric,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        );

        let (body_comb, body_proc) = lower_expr_block(
            else_if_block.body(),
            SharedString::clone(&tmp_member),
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        );

        if let Some(body_proc) = body_proc {
            else_if_blocks_proc.push((condition.clone(), body_proc));
        }
        else_if_blocks_comb.push((condition, body_comb));
    }

    let (else_block_comb, else_block_proc) = lower_expr_block(
        if_expr.else_block().body(),
        SharedString::clone(&tmp_member),
        mode,
        tmp_members,
        tmp_comb_statements,
        tmp_proc_statements,
        known_types,
        resolved_types,
    );

    if let Some(body_proc) = body_proc {
        let if_statement_proc = VIfStatement::new(
            condition.clone(),
            body_proc,
            else_if_blocks_proc,
            else_block_proc,
        );
        tmp_proc_statements.push(VStatement::If(if_statement_proc));
    } else if !else_if_blocks_proc.is_empty() {
        let if_statement_proc = VIfStatement::new(
            condition.clone(),
            VBlock::new(Vec::new()),
            else_if_blocks_proc,
            else_block_proc,
        );
        tmp_proc_statements.push(VStatement::If(if_statement_proc));
    }

    let if_statement_comb = VIfStatement::new(
        condition,
        body_comb,
        else_if_blocks_comb,
        Some(else_block_comb),
    );
    tmp_comb_statements.push(VStatement::If(if_statement_comb));

    VExpr::Ident(tmp_member)
}

fn lower_match_expr(
    match_expr: &CheckedMatchExpr,
    mode: VAssignMode,
    tmp_members: &mut Vec<(SharedString, TypeId)>,
    tmp_comb_statements: &mut Vec<VStatement>,
    tmp_proc_statements: &mut Vec<VStatement>,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> VExpr {
    use crate::ast::MatchPattern;

    let tmp_member = add_tmp_member(match_expr.ty(), tmp_members);

    if let Some(ResolvedTypeItem::Enum(_)) = &resolved_types.get(&match_expr.value().ty()) {
        let value = lower_expr(
            match_expr.value(),
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        );

        let mut branches_comb = Vec::with_capacity(match_expr.branches().len());
        let mut branches_proc = Vec::with_capacity(match_expr.branches().len());
        for branch in match_expr.branches() {
            let (body_comb, body_proc) = match branch.body() {
                CheckedMatchExprBody::Expr(body) => {
                    let result = lower_expr(
                        body,
                        mode,
                        tmp_members,
                        tmp_comb_statements,
                        tmp_proc_statements,
                        known_types,
                        resolved_types,
                    );
                    let assign_target =
                        VAssignTarget::new(SharedString::clone(&tmp_member), Vec::new());

                    let statements = vec![VStatement::Assignment(VAssignment::new(
                        assign_target,
                        mode,
                        result,
                    ))];

                    (VBlock::new(statements), None)
                }
                CheckedMatchExprBody::Block(body) => lower_expr_block(
                    body,
                    SharedString::clone(&tmp_member),
                    mode,
                    tmp_members,
                    tmp_comb_statements,
                    tmp_proc_statements,
                    known_types,
                    resolved_types,
                ),
            };

            let mut patterns = Vec::with_capacity(branch.patterns().len());
            for pattern in branch.patterns() {
                let MatchPattern::Path(path) = pattern else {
                    unreachable!("error in type-checking match expression");
                };

                let pattern = lower_path(path);
                patterns.push(VCasePattern::Ident(pattern.into()))
            }

            if let Some(body_proc) = body_proc {
                branches_proc.push(VCaseBranch::new(patterns.clone(), body_proc));
            }
            branches_comb.push(VCaseBranch::new(patterns, body_comb));
        }

        if !branches_proc.is_empty() {
            let case_statement_proc = VCaseStatement::new(value.clone(), branches_proc);
            tmp_proc_statements.push(VStatement::Case(case_statement_proc));
        }

        let case_statement_comb = VCaseStatement::new(value, branches_comb);
        tmp_comb_statements.push(VStatement::Case(case_statement_comb));

        VExpr::Ident(tmp_member)
    } else {
        let value_name = add_tmp_member(match_expr.value().ty(), tmp_members);

        let value = lower_expr(
            match_expr.value(),
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        );

        let value_target = VAssignTarget::new(SharedString::clone(&value_name), Vec::new());
        let value_assign = VAssignment::new(value_target, VAssignMode::Combinatoric, value);
        tmp_comb_statements.push(VStatement::Assignment(value_assign));

        let mut branch_to_cond_body =
            |branch: &CheckedMatchExprBranch| -> (Option<VExpr>, VBlock, Option<VBlock>) {
                macro_rules! value {
                    () => {
                        VExpr::Ident(SharedString::clone(&value_name))
                    };
                }

                let mut is_always_cond = false;
                let mut cond_terms = Vec::with_capacity(branch.patterns().len());
                for pattern in branch.patterns() {
                    match pattern {
                        MatchPattern::Literal(value) => {
                            let value = VExpr::Value(value.value());
                            let term = VExpr::Eq(VBinaryExpr::new(value!(), value));
                            cond_terms.push(term);
                        }
                        MatchPattern::Range(start, end) => {
                            let start = VExpr::Value(start.value());
                            let end = VExpr::Value(end.value());

                            let lower_bound = VExpr::Gte(VBinaryExpr::new(value!(), start));
                            let upper_bound = VExpr::Lt(VBinaryExpr::new(value!(), end));

                            let term = VExpr::And(VBinaryExpr::new(lower_bound, upper_bound));
                            cond_terms.push(term);
                        }
                        MatchPattern::RangeInclusive(start, end) => {
                            let start = VExpr::Value(start.value());
                            let end = VExpr::Value(end.value());

                            let lower_bound = VExpr::Gte(VBinaryExpr::new(value!(), start));
                            let upper_bound = VExpr::Lte(VBinaryExpr::new(value!(), end));

                            let term = VExpr::And(VBinaryExpr::new(lower_bound, upper_bound));
                            cond_terms.push(term);
                        }
                        MatchPattern::Path(p) => {
                            if let Some(ident) = p.as_ident() && (ident.as_ref() == "_") {
                                is_always_cond = true;
                                break;
                            } else {
                                unreachable!("error in type-checking match expression")
                            }
                        }
                    }
                }

                let cond = if is_always_cond {
                    None
                } else {
                    Some(
                        cond_terms
                            .into_iter()
                            .reduce(|a, b| VExpr::Or(VBinaryExpr::new(a, b)))
                            .expect("error in type-checking match expression"),
                    )
                };

                let (body_comb, body_proc) = match branch.body() {
                    CheckedMatchExprBody::Expr(body) => {
                        let result = lower_expr(
                            body,
                            mode,
                            tmp_members,
                            tmp_comb_statements,
                            tmp_proc_statements,
                            known_types,
                            resolved_types,
                        );
                        let assign_target =
                            VAssignTarget::new(SharedString::clone(&tmp_member), Vec::new());

                        let statements = vec![VStatement::Assignment(VAssignment::new(
                            assign_target,
                            mode,
                            result,
                        ))];

                        (VBlock::new(statements), None)
                    }
                    CheckedMatchExprBody::Block(body) => lower_expr_block(
                        body,
                        SharedString::clone(&tmp_member),
                        mode,
                        tmp_members,
                        tmp_comb_statements,
                        tmp_proc_statements,
                        known_types,
                        resolved_types,
                    ),
                };

                (cond, body_comb, body_proc)
            };

        let Some(first) = match_expr.branches().first() else {
            unreachable!("error in type-checking match expression");
        };

        let (condition, body_comb, body_proc) = branch_to_cond_body(first);
        if let Some(condition) = condition {
            let mut else_if_blocks_comb = Vec::with_capacity(match_expr.branches().len() - 1);
            let mut else_if_blocks_proc = Vec::with_capacity(match_expr.branches().len() - 1);
            let mut else_block_comb = None;
            let mut else_block_proc = None;
            for branch in match_expr.branches().iter().skip(1) {
                let (condition, body_comb, body_proc) = branch_to_cond_body(branch);
                if let Some(condition) = condition {
                    if let Some(body_proc) = body_proc {
                        else_if_blocks_proc.push((condition.clone(), body_proc));
                    }
                    else_if_blocks_comb.push((condition, body_comb));
                } else {
                    if let Some(body_proc) = body_proc {
                        else_block_proc = Some(body_proc);
                    }
                    else_block_comb = Some(body_comb);
                    break;
                }
            }

            if let Some(body_proc) = body_proc {
                let if_statement_proc = VIfStatement::new(
                    condition.clone(),
                    body_proc,
                    else_if_blocks_proc,
                    else_block_proc,
                );
                tmp_proc_statements.push(VStatement::If(if_statement_proc));
            } else if !else_if_blocks_proc.is_empty() {
                let if_statement_proc = VIfStatement::new(
                    condition.clone(),
                    VBlock::new(Vec::new()),
                    else_if_blocks_proc,
                    else_block_proc,
                );
                tmp_proc_statements.push(VStatement::If(if_statement_proc));
            }

            let if_statement_comb =
                VIfStatement::new(condition, body_comb, else_if_blocks_comb, else_block_comb);
            tmp_comb_statements.push(VStatement::If(if_statement_comb));
        } else {
            if let Some(body_proc) = body_proc {
                tmp_proc_statements.push(VStatement::Block(body_proc));
            }
            tmp_comb_statements.push(VStatement::Block(body_comb));
        }

        VExpr::Ident(tmp_member)
    }
}

fn lower_cast_expr(
    cast_expr: &CheckedCastExpr,
    mode: VAssignMode,
    tmp_members: &mut Vec<(SharedString, TypeId)>,
    tmp_comb_statements: &mut Vec<VStatement>,
    tmp_proc_statements: &mut Vec<VStatement>,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> VExpr {
    match (
        &known_types[&cast_expr.value().ty()],
        &known_types[&cast_expr.target_ty()],
    ) {
        (ResolvedType::Const, &ResolvedType::BuiltinBits { width }) => {
            let &CheckedExpr::Value(value) = cast_expr.value() else {
                unreachable!();
            };

            VExpr::Literal(VLiteral::new(value, width))
        }
        (
            &ResolvedType::BuiltinBits { width: value_width },
            &ResolvedType::BuiltinBits {
                width: target_width,
            },
        ) => {
            let value = lower_expr(
                cast_expr.value(),
                mode,
                tmp_members,
                tmp_comb_statements,
                tmp_proc_statements,
                known_types,
                resolved_types,
            );

            if target_width > value_width {
                let padding = VExpr::Literal(VLiteral::new(0, target_width - value_width));
                VExpr::Concat(VBinaryExpr::new(padding, value))
            } else if target_width == value_width {
                value
            } else {
                let indexer = VIndexKind::Range(((target_width - 1) as i64)..0);
                VExpr::Index(VIndexExpr::new(value, indexer))
            }
        }
        (
            ResolvedType::Named { .. },
            &ResolvedType::BuiltinBits {
                width: target_width,
            },
        ) => {
            if let ResolvedTypeItem::Enum(enum_item) = &resolved_types[&cast_expr.value().ty()] {
                let &ResolvedType::BuiltinBits { width: value_width } = &known_types[&enum_item.base_ty()] else {
                    unreachable!();
                };

                let value = lower_expr(
                    cast_expr.value(),
                    mode,
                    tmp_members,
                    tmp_comb_statements,
                    tmp_proc_statements,
                    known_types,
                    resolved_types,
                );

                if target_width > value_width {
                    let padding = VExpr::Literal(VLiteral::new(0, target_width - value_width));
                    VExpr::Concat(VBinaryExpr::new(padding, value))
                } else if target_width == value_width {
                    value
                } else {
                    let indexer = VIndexKind::Range(((target_width - 1) as i64)..0);
                    VExpr::Index(VIndexExpr::new(value, indexer))
                }
            } else {
                unreachable!("error in type-checking cast expression");
            }
        }
        _ => unreachable!("error in type-checking cast expression"),
    }
}

fn lower_expr(
    expr: &CheckedExpr,
    mode: VAssignMode,
    tmp_members: &mut Vec<(SharedString, TypeId)>,
    tmp_comb_statements: &mut Vec<VStatement>,
    tmp_proc_statements: &mut Vec<VStatement>,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> VExpr {
    macro_rules! bin_expr {
        ($bin_expr:expr, $op:ident) => {{
            let lhs = lower_expr(
                $bin_expr.lhs(),
                mode,
                tmp_members,
                tmp_comb_statements,
                tmp_proc_statements,
                known_types,
                resolved_types,
            );
            let rhs = lower_expr(
                $bin_expr.rhs(),
                mode,
                tmp_members,
                tmp_comb_statements,
                tmp_proc_statements,
                known_types,
                resolved_types,
            );
            VExpr::$op(VBinaryExpr::new(lhs, rhs))
        }};
    }

    match expr {
        CheckedExpr::Value(value) => VExpr::Value(*value),
        CheckedExpr::Path(path) => VExpr::Ident(lower_path(path.path()).into()),

        CheckedExpr::Construct(construct_expr) => lower_construct_expr(
            construct_expr,
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        ),
        CheckedExpr::If(if_expr) => lower_if_expr(
            if_expr,
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        ),
        CheckedExpr::Match(match_expr) => lower_match_expr(
            match_expr,
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        ),
        CheckedExpr::Block(block) => {
            let tmp_member = add_tmp_member(block.ty(), tmp_members);

            let (block_comb, block_proc) = lower_expr_block(
                block,
                SharedString::clone(&tmp_member),
                mode,
                tmp_members,
                tmp_comb_statements,
                tmp_proc_statements,
                known_types,
                resolved_types,
            );

            tmp_comb_statements.push(VStatement::Block(block_comb));
            if let Some(block_proc) = block_proc {
                tmp_proc_statements.push(VStatement::Block(block_proc));
            }

            VExpr::Ident(tmp_member)
        }

        CheckedExpr::Index(index_expr) => {
            let base = lower_expr(
                index_expr.base(),
                mode,
                tmp_members,
                tmp_comb_statements,
                tmp_proc_statements,
                known_types,
                resolved_types,
            );

            let indexer = match index_expr.indexer() {
                CheckedIndexKind::Single(index) => VIndexKind::Single(lower_expr(
                    index,
                    mode,
                    tmp_members,
                    tmp_comb_statements,
                    tmp_proc_statements,
                    known_types,
                    resolved_types,
                )),
                CheckedIndexKind::Range(range) => VIndexKind::Range(range.clone()),
            };

            VExpr::Index(VIndexExpr::new(base, indexer))
        }
        CheckedExpr::MemberAccess(member_access) => {
            let base = lower_expr(
                member_access.base(),
                mode,
                tmp_members,
                tmp_comb_statements,
                tmp_proc_statements,
                known_types,
                resolved_types,
            );

            VExpr::MemberAccess(VMemberAccessExpr::new(
                base,
                member_access.member().as_string(),
            ))
        }

        CheckedExpr::Neg(expr) => VExpr::Neg(VUnaryExpr::new(lower_expr(
            expr.inner(),
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        ))),
        CheckedExpr::Not(expr) => VExpr::Not(VUnaryExpr::new(lower_expr(
            expr.inner(),
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        ))),

        CheckedExpr::Cast(cast_expr) => lower_cast_expr(
            cast_expr,
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        ),

        CheckedExpr::Concat(bin_expr) => bin_expr!(bin_expr, Concat),
        CheckedExpr::Lt(bin_expr) => bin_expr!(bin_expr, Lt),
        CheckedExpr::Lte(bin_expr) => bin_expr!(bin_expr, Lte),
        CheckedExpr::Gt(bin_expr) => bin_expr!(bin_expr, Gt),
        CheckedExpr::Gte(bin_expr) => bin_expr!(bin_expr, Gte),
        CheckedExpr::Slt(bin_expr) => bin_expr!(bin_expr, Slt),
        CheckedExpr::Slte(bin_expr) => bin_expr!(bin_expr, Slte),
        CheckedExpr::Sgt(bin_expr) => bin_expr!(bin_expr, Sgt),
        CheckedExpr::Sgte(bin_expr) => bin_expr!(bin_expr, Sgte),
        CheckedExpr::Eq(bin_expr) => bin_expr!(bin_expr, Eq),
        CheckedExpr::Ne(bin_expr) => bin_expr!(bin_expr, Ne),
        CheckedExpr::Add(bin_expr) => bin_expr!(bin_expr, Add),
        CheckedExpr::Sub(bin_expr) => bin_expr!(bin_expr, Sub),
        CheckedExpr::Mul(bin_expr) => bin_expr!(bin_expr, Mul),
        CheckedExpr::Div(bin_expr) => bin_expr!(bin_expr, Div),
        CheckedExpr::Rem(bin_expr) => bin_expr!(bin_expr, Rem),
        CheckedExpr::And(bin_expr) => bin_expr!(bin_expr, And),
        CheckedExpr::Xor(bin_expr) => bin_expr!(bin_expr, Xor),
        CheckedExpr::Or(bin_expr) => bin_expr!(bin_expr, Or),
        CheckedExpr::Shl(bin_expr) => bin_expr!(bin_expr, Shl),
        CheckedExpr::Lsr(bin_expr) => bin_expr!(bin_expr, Lsr),
        CheckedExpr::Asr(bin_expr) => bin_expr!(bin_expr, Asr),
    }
}

fn lower_if_statement(
    if_statement: &CheckedIfStatement,
    mode: VAssignMode,
    emit_assignments: bool,
    tmp_members: &mut Vec<(SharedString, TypeId)>,
    tmp_comb_statements: &mut Vec<VStatement>,
    tmp_proc_statements: &mut Vec<VStatement>,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> VIfStatement {
    let condition = lower_expr(
        if_statement.condition(),
        mode,
        tmp_members,
        tmp_comb_statements,
        tmp_proc_statements,
        known_types,
        resolved_types,
    );
    let body = lower_block(
        if_statement.body(),
        mode,
        emit_assignments,
        tmp_members,
        tmp_comb_statements,
        tmp_proc_statements,
        known_types,
        resolved_types,
    );

    let mut else_if_blocks = Vec::with_capacity(if_statement.else_if_blocks().len());
    for else_if_block in if_statement.else_if_blocks() {
        let condition = lower_expr(
            else_if_block.condition(),
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        );
        let body = lower_block(
            else_if_block.body(),
            mode,
            emit_assignments,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        );
        else_if_blocks.push((condition, body));
    }

    let else_block = if_statement.else_block().map(|else_block| {
        lower_block(
            else_block.body(),
            mode,
            emit_assignments,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        )
    });

    VIfStatement::new(condition, body, else_if_blocks, else_block)
}

enum LoweredMatchStatement {
    If(VIfStatement),
    Case(VCaseStatement),
}

fn lower_match_statement(
    match_statement: &CheckedMatchStatement,
    mode: VAssignMode,
    emit_assignments: bool,
    tmp_members: &mut Vec<(SharedString, TypeId)>,
    tmp_comb_statements: &mut Vec<VStatement>,
    tmp_proc_statements: &mut Vec<VStatement>,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> LoweredMatchStatement {
    use crate::ast::MatchPattern;

    if let Some(ResolvedTypeItem::Enum(_)) = &resolved_types.get(&match_statement.value().ty()) {
        let value = lower_expr(
            match_statement.value(),
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        );

        let mut branches = Vec::with_capacity(match_statement.branches().len());
        for branch in match_statement.branches() {
            let body = lower_block(
                branch.body(),
                mode,
                emit_assignments,
                tmp_members,
                tmp_comb_statements,
                tmp_proc_statements,
                known_types,
                resolved_types,
            );

            let mut patterns = Vec::with_capacity(branch.patterns().len());
            for pattern in branch.patterns() {
                let MatchPattern::Path(path) = pattern else {
                    unreachable!("error in type-checking match expression");
                };

                let pattern = lower_path(path);
                patterns.push(VCasePattern::Ident(pattern.into()))
            }

            branches.push(VCaseBranch::new(patterns, body));
        }

        LoweredMatchStatement::Case(VCaseStatement::new(value, branches))
    } else {
        let value_name = add_tmp_member(match_statement.value().ty(), tmp_members);

        let value = lower_expr(
            match_statement.value(),
            mode,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        );

        let value_target = VAssignTarget::new(SharedString::clone(&value_name), Vec::new());
        let value_assign = VAssignment::new(value_target, VAssignMode::Combinatoric, value);
        tmp_comb_statements.push(VStatement::Assignment(value_assign));

        let mut branch_to_cond_body = |branch: &CheckedMatchStatementBranch| -> (VExpr, VBlock) {
            macro_rules! value {
                () => {
                    VExpr::Ident(SharedString::clone(&value_name))
                };
            }

            let mut cond_terms = Vec::with_capacity(branch.patterns().len());
            for pattern in branch.patterns() {
                match pattern {
                    MatchPattern::Literal(value) => {
                        let value = VExpr::Value(value.value());
                        let term = VExpr::Eq(VBinaryExpr::new(value!(), value));
                        cond_terms.push(term);
                    }
                    MatchPattern::Range(start, end) => {
                        let start = VExpr::Value(start.value());
                        let end = VExpr::Value(end.value());

                        let lower_bound = VExpr::Gte(VBinaryExpr::new(value!(), start));
                        let upper_bound = VExpr::Lt(VBinaryExpr::new(value!(), end));

                        let term = VExpr::And(VBinaryExpr::new(lower_bound, upper_bound));
                        cond_terms.push(term);
                    }
                    MatchPattern::RangeInclusive(start, end) => {
                        let start = VExpr::Value(start.value());
                        let end = VExpr::Value(end.value());

                        let lower_bound = VExpr::Gte(VBinaryExpr::new(value!(), start));
                        let upper_bound = VExpr::Lte(VBinaryExpr::new(value!(), end));

                        let term = VExpr::And(VBinaryExpr::new(lower_bound, upper_bound));
                        cond_terms.push(term);
                    }
                    MatchPattern::Path(_) => {
                        unreachable!("error in type-checking match expression")
                    }
                }
            }

            let cond = cond_terms
                .into_iter()
                .reduce(|a, b| VExpr::Or(VBinaryExpr::new(a, b)))
                .expect("error in type-checking match expression");

            let body = lower_block(
                branch.body(),
                mode,
                emit_assignments,
                tmp_members,
                tmp_comb_statements,
                tmp_proc_statements,
                known_types,
                resolved_types,
            );

            (cond, body)
        };

        let Some(first) = match_statement.branches().first() else {
            unreachable!("error in type-checking match expression");
        };

        let (condition, body) = branch_to_cond_body(first);

        let mut else_if_blocks = Vec::with_capacity(match_statement.branches().len() - 1);
        for branch in match_statement.branches().iter().skip(1) {
            let (condition, body) = branch_to_cond_body(branch);
            else_if_blocks.push((condition, body));
        }

        LoweredMatchStatement::If(VIfStatement::new(condition, body, else_if_blocks, None))
    }
}

fn lower_assign_target(
    assign: &CheckedAssignTarget,
    mode: VAssignMode,
    tmp_members: &mut Vec<(SharedString, TypeId)>,
    tmp_comb_statements: &mut Vec<VStatement>,
    tmp_proc_statements: &mut Vec<VStatement>,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> VAssignTarget {
    let base = assign.base().as_string();

    let mut suffixes = Vec::with_capacity(assign.suffixes().len());
    for suffix in assign.suffixes() {
        let suffix = match suffix {
            CheckedSuffixOp::Indexer { index, .. } => match index {
                CheckedIndexKind::Single(index) => {
                    let index = lower_expr(
                        index,
                        mode,
                        tmp_members,
                        tmp_comb_statements,
                        tmp_proc_statements,
                        known_types,
                        resolved_types,
                    );
                    VSuffixOp::Indexer(VIndexKind::Single(index))
                }
                CheckedIndexKind::Range(range) => {
                    VSuffixOp::Indexer(VIndexKind::Range(range.clone()))
                }
            },
            CheckedSuffixOp::MemberAccess { member, .. } => {
                VSuffixOp::MemberAccess(member.as_string())
            }
        };

        suffixes.push(suffix);
    }

    VAssignTarget::new(base, suffixes)
}

fn lower_assignment(
    assign: &CheckedAssignment,
    mode: VAssignMode,
    tmp_members: &mut Vec<(SharedString, TypeId)>,
    tmp_comb_statements: &mut Vec<VStatement>,
    tmp_proc_statements: &mut Vec<VStatement>,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> VAssignment {
    let target = lower_assign_target(
        assign.target(),
        mode,
        tmp_members,
        tmp_comb_statements,
        tmp_proc_statements,
        known_types,
        resolved_types,
    );
    let value = lower_expr(
        assign.value(),
        mode,
        tmp_members,
        tmp_comb_statements,
        tmp_proc_statements,
        known_types,
        resolved_types,
    );
    VAssignment::new(target, mode, value)
}

fn lower_statement(
    statement: &CheckedStatement,
    mode: VAssignMode,
    emit_assignments: bool,
    tmp_members: &mut Vec<(SharedString, TypeId)>,
    tmp_comb_statements: &mut Vec<VStatement>,
    tmp_proc_statements: &mut Vec<VStatement>,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> Option<VStatement> {
    match statement {
        CheckedStatement::Expr(_) => {
            // FIXME: is it ok to drop this?
            None
        }
        CheckedStatement::Block(body) => {
            let body = lower_block(
                body,
                mode,
                emit_assignments,
                tmp_members,
                tmp_comb_statements,
                tmp_proc_statements,
                known_types,
                resolved_types,
            );
            Some(VStatement::Block(body))
        }
        CheckedStatement::If(if_statement) => {
            let if_statement = lower_if_statement(
                if_statement,
                mode,
                emit_assignments,
                tmp_members,
                tmp_comb_statements,
                tmp_proc_statements,
                known_types,
                resolved_types,
            );
            Some(VStatement::If(if_statement))
        }
        CheckedStatement::Match(match_statement) => {
            match lower_match_statement(
                match_statement,
                mode,
                emit_assignments,
                tmp_members,
                tmp_comb_statements,
                tmp_proc_statements,
                known_types,
                resolved_types,
            ) {
                LoweredMatchStatement::If(if_statement) => Some(VStatement::If(if_statement)),
                LoweredMatchStatement::Case(case_statement) => {
                    Some(VStatement::Case(case_statement))
                }
            }
        }
        CheckedStatement::Assignment(assign) => {
            if emit_assignments {
                let assign = lower_assignment(
                    assign,
                    mode,
                    tmp_members,
                    tmp_comb_statements,
                    tmp_proc_statements,
                    known_types,
                    resolved_types,
                );
                Some(VStatement::Assignment(assign))
            } else {
                None
            }
        }
    }
}

fn lower_block(
    block: &CheckedBlock,
    mode: VAssignMode,
    emit_assignments: bool,
    tmp_members: &mut Vec<(SharedString, TypeId)>,
    tmp_comb_statements: &mut Vec<VStatement>,
    tmp_proc_statements: &mut Vec<VStatement>,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> VBlock {
    let mut statements = Vec::with_capacity(block.statements().len());
    for statement in block.statements() {
        if let Some(statement) = lower_statement(
            statement,
            mode,
            emit_assignments,
            tmp_members,
            tmp_comb_statements,
            tmp_proc_statements,
            known_types,
            resolved_types,
        ) {
            statements.push(statement);
        }
    }
    VBlock::new(statements)
}

pub fn lower(
    module: &CheckedModule,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> VModule {
    let mut tmp_members = Vec::new();
    let mut tmp_comb_statements = Vec::new();
    let mut ff_members = Vec::new();
    let mut comb_members = Vec::new();

    for proc_member in module.proc_members() {
        let mut tmp_proc_statements = Vec::new();

        let mut sens = Vec::with_capacity(proc_member.sens().len());
        for s in proc_member.sens() {
            let target = lower_assign_target(
                s.target(),
                VAssignMode::Combinatoric,
                &mut tmp_members,
                &mut tmp_comb_statements,
                &mut tmp_proc_statements,
                known_types,
                resolved_types,
            );
            sens.push(VSens::new(target, s.edge()));
        }

        let body = lower_block(
            proc_member.body(),
            VAssignMode::Sequential,
            true,
            &mut tmp_members,
            &mut tmp_comb_statements,
            &mut tmp_proc_statements,
            known_types,
            resolved_types,
        );

        if !tmp_proc_statements.is_empty() {
            ff_members.push(VFFMember::new(
                sens.clone(),
                VBlock::new(tmp_proc_statements),
            ));
        }

        ff_members.push(VFFMember::new(sens, body));
    }

    for comb_member in module.comb_members() {
        let mut tmp_proc_statements = Vec::new();

        let body = lower_block(
            comb_member.body(),
            VAssignMode::Combinatoric,
            true,
            &mut tmp_members,
            &mut tmp_comb_statements,
            &mut tmp_proc_statements,
            known_types,
            resolved_types,
        );
        comb_members.push(body);

        if !tmp_proc_statements.is_empty() {
            comb_members.push(VBlock::new(tmp_proc_statements));
        }
    }

    VModule::new(
        tmp_members,
        VBlock::new(tmp_comb_statements),
        ff_members,
        comb_members,
    )
}
