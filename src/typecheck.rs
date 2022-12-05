use crate::ast::*;
use crate::const_eval::*;
use crate::error::*;
use crate::ir::*;
use crate::range_collection::*;
use crate::scope::*;
use crate::type_resolve::*;
use crate::{Clog2, HashMap, HashSet, SharedString};
use std::borrow::Cow;

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
) -> QuartzResult<'a, Either<ConstUnaryExpr, CheckedUnaryExpr>> {
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
                _ => Err(QuartzError::IncompatibleType {
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
) -> QuartzResult<'a, Either<ConstBinaryExpr, CheckedBinaryExpr>> {
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
        return Err(QuartzError::new_list(errors));
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

    Err(QuartzError::IncompatibleTypes {
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
) -> QuartzResult<'a, Either<ConstBinaryExpr, CheckedBinaryExpr>> {
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
        return Err(QuartzError::new_list(errors));
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

    Err(QuartzError::IncompatibleTypes {
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
) -> QuartzResult<'a, CheckedConcatExpr> {
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
        return Err(QuartzError::new_list(errors));
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

    Err(QuartzError::IncompatibleTypes {
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
) -> QuartzResult<'a, CheckedCastExpr> {
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

    Err(QuartzError::InvalidCast {
        value_ty: value.ty_string(known_types),
        expr,
    })
}

fn merge_expr<'a>(
    expr: Either<ConstExpr, CheckedExpr>,
    args: &TypecheckArgs,
) -> QuartzResult<'a, CheckedExpr> {
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
) -> QuartzResult<'a, CheckedConstructExpr> {
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
                            errors.push(QuartzError::UnknownField {
                                ty: expr.ty(),
                                field,
                            });
                        }
                    }

                    let mut fields = HashMap::default();
                    for (name, &(field_id, _)) in struct_item.fields().iter() {
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
                                                        QuartzError::IncompatibleFieldType {
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
                                errors.push(QuartzError::MissingField {
                                    ty: expr.ty(),
                                    field: SharedString::clone(name),
                                });
                            }
                        }
                    }

                    wrap_errors!(CheckedConstructExpr::new(id, fields), errors)
                }
                _ => Err(QuartzError::TypeNotConstructible { ty: expr.ty() }),
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
) -> QuartzResult<'a, (TypeId, LogicKind)> {
    if let Some(port) = module.ports().get(ident.as_ref()) {
        Ok((port.ty(), port.kind()))
    } else if let Some(member) = module.logic_members().get(ident.as_ref()) {
        Ok((member.ty(), member.kind()))
    } else if let Some(module_id) = module_id {
        Err(QuartzError::UndefinedMember {
            ty: known_types[&module_id].to_string(known_types),
            name: ident.clone(),
        })
    } else {
        Err(QuartzError::UndefinedIdent {
            name: ident.clone(),
        })
    }
}

fn find_member_type<'a>(
    ident: &'a Ident,
    parent_id: TypeId,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> QuartzResult<'a, TypeId> {
    match resolved_types.get(&parent_id) {
        Some(ResolvedTypeItem::Struct(struct_item)) => {
            if let Some((field_ty, _)) = struct_item.fields().get(ident.as_ref()).copied() {
                Ok(field_ty)
            } else {
                Err(QuartzError::UndefinedMember {
                    ty: known_types[&parent_id].to_string(known_types),
                    name: ident.clone(),
                })
            }
        }
        Some(ResolvedTypeItem::Module(module_item)) => {
            Ok(find_module_member_type(ident, module_item, Some(parent_id), known_types)?.0)
        }
        Some(_) => Err(QuartzError::UndefinedMember {
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
) -> QuartzResult<'a, ()> {
    match resolved_types.get(&enum_ty) {
        Some(ResolvedTypeItem::Enum(enum_item)) => {
            if enum_item.variants().contains_key(variant_name.as_ref()) {
                Ok(())
            } else {
                Err(QuartzError::InvalidEnumVariant {
                    enum_name: enum_name.as_string(),
                    variant_name: variant_name.clone(),
                })
            }
        }
        _ => Err(QuartzError::InvalidEnumIdent {
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
) -> QuartzResult<'a, Either<Ident, CheckedPath>> {
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
            Err(QuartzError::InvalidPath { path })
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
) -> QuartzResult<'a, (CheckedIndexKind, TypeId)> {
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
                                return Err(QuartzError::IndexOutOfRange {
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
                                return Err(QuartzError::IndexOutOfRange {
                                    index_expr,
                                    index,
                                    len: *len,
                                });
                            }

                            Ok((checked_indexer, *item_ty))
                        }
                        _ => Err(QuartzError::InvalidIndexing {
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
                            return Err(QuartzError::InvalidIndexing {
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
                        Err(QuartzError::InvalidIndexType {
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
                        return Err(QuartzError::IndexOutOfRange {
                            index_expr: &range.start,
                            index: start,
                            len: *width,
                        });
                    }

                    if (end <= start) || (end >= (*width as i64)) {
                        return Err(QuartzError::IndexOutOfRange {
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
                ResolvedType::Array { .. } => Err(QuartzError::InvalidRangeIndexing {
                    indexer,
                    base_ty: base_ty.to_string(known_types),
                }),
                _ => Err(QuartzError::InvalidIndexing {
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
) -> QuartzResult<'a, CheckedIndexExpr> {
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
) -> QuartzResult<'a, CheckedMemberAccessExpr> {
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
) -> QuartzResult<'a, CheckedIfExpr> {
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
                        errors.push(QuartzError::InvalidConditionType {
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
                            errors.push(QuartzError::InvalidConditionType {
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
        errors.push(QuartzError::MissingElseBlock { if_expr: expr });
        None
    };

    if !errors.is_empty() {
        Err(QuartzError::new_list(errors))
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

                errors.push(QuartzError::ElseIfTypeMismatch {
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

                errors.push(QuartzError::ElseTypeMismatch {
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
) -> QuartzResult<'a, ()> {
    let mut errors = Vec::new();

    let full_range = InclusiveRange::n_bit(width);
    let mut covered_ranges = RangeCollection::new();

    for branch in expr.branches().iter() {
        'inner: for pattern in branch.patterns().iter() {
            match pattern {
                MatchPattern::Literal(l) => {
                    if full_range.contains(l.value() as u64) {
                        if covered_ranges.contains(l.value() as u64) {
                            errors.push(QuartzError::UnreachablePattern { pattern });
                        } else {
                            covered_ranges.insert(l.value() as u64);
                        }
                    } else {
                        errors.push(QuartzError::PatternOutOfRange {
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
                            errors.push(QuartzError::UnreachablePattern { pattern });
                        } else {
                            covered_ranges.insert(range);
                        }
                    } else {
                        errors.push(QuartzError::PatternOutOfRange {
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
                            errors.push(QuartzError::UnreachablePattern { pattern });
                        } else {
                            covered_ranges.insert(range);
                        }
                    } else {
                        errors.push(QuartzError::PatternOutOfRange {
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
                                errors.push(QuartzError::UnreachablePattern { pattern });
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
                                            errors
                                                .push(QuartzError::UnreachablePattern { pattern });
                                        } else {
                                            covered_ranges.insert(value);
                                        }
                                    } else {
                                        errors.push(QuartzError::PatternOutOfRange {
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

            errors.push(QuartzError::IncompatiblePattern {
                pattern,
                value_ty: value_ty.to_string(known_types),
            });
        }
    }

    if !covered_ranges.contains(full_range) {
        errors.push(QuartzError::NonExhaustiveMatch { match_expr: expr })
    }

    wrap_errors!((), errors)
}

fn typecheck_enum_match_expr<'a>(
    expr: &'a MatchExpr,
    value_ty: &ResolvedType,
    enum_item: &ResolvedEnum,
    known_types: &HashMap<TypeId, ResolvedType>,
) -> QuartzResult<'a, ()> {
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
                            errors.push(QuartzError::UnreachablePattern { pattern });
                            continue 'inner;
                        }
                    } else {
                        errors.push(QuartzError::InvalidEnumVariant {
                            enum_name: SharedString::clone(enum_name),
                            variant_name: variant_name.clone()
                        });
                        continue 'inner;
                    }
                }
            }

            errors.push(QuartzError::IncompatiblePattern {
                pattern,
                value_ty: value_ty.to_string(known_types),
            });
        }
    }

    if !unused_variants.is_empty() {
        errors.push(QuartzError::NonExhaustiveMatch { match_expr: expr })
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
) -> QuartzResult<'a, CheckedMatchExpr> {
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
                errors.push(QuartzError::MatchBranchTypeMismatch {
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
                        errors.push(QuartzError::InvalidMatchType {
                            value: expr.value(),
                            value_ty: value_ty.to_string(known_types),
                        });
                    }
                }
            }
            _ => {
                errors.push(QuartzError::InvalidMatchType {
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
) -> QuartzResult<'a, Either<ConstExpr, CheckedExpr>> {
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
                Either::Const(_) => Err(QuartzError::InvalidConstOp { op: *$expr.op() }),
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
) -> QuartzResult<'a, CheckedIfStatement> {
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
                        errors.push(QuartzError::InvalidConditionType {
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
                            errors.push(QuartzError::InvalidConditionType {
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
) -> QuartzResult<'a, CheckedMatchStatement> {
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
                errors.push(QuartzError::UnexpectedReturnValue { value: body_expr });

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
        Err(QuartzError::new_list(errors))
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
                        return Err(QuartzError::InvalidMatchType {
                            value: expr.value(),
                            value_ty: value_ty.to_string(known_types),
                        });
                    }
                }
            }
            _ => {
                return Err(QuartzError::InvalidMatchType {
                    value: expr.value(),
                    value_ty: value_ty.to_string(known_types),
                });
            }
        }

        wrap_errors!(CheckedMatchStatement::new(value, branches), errors)
    }
}

fn typecheck_assign_target<'a>(
    assign_target: &'a AssignTarget,
    parent_module: &ResolvedModule,
    mode: TypecheckMode,
    scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    args: &TypecheckArgs,
) -> QuartzResult<'a, CheckedAssignTarget> {
    let mut errors = Vec::new();

    if !assign_target.path().is_ident() {
        errors.push(QuartzError::InvalidPath {
            path: assign_target.path(),
        });
    }

    let base = assign_target.path().head();

    match find_module_member_type(base, parent_module, None, known_types) {
        Ok((base_ty, _)) => {
            let mut ty = base_ty;
            let mut suffixes = Vec::new();
            for suffix in assign_target.suffixes().iter() {
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

            wrap_errors!(
                CheckedAssignTarget::new(base.clone(), base_ty, suffixes),
                errors
            )
        }
        Err(err) => {
            errors.push(err);
            Err(QuartzError::new_list(errors))
        }
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
) -> QuartzResult<'a, CheckedAssignment> {
    let mut errors = Vec::new();

    if assign.op().kind() != AssignKind::Assign {
        errors.push(QuartzError::InvalidAssignOp { assign });
    }

    if !assign.target().path().is_ident() {
        errors.push(QuartzError::InvalidPath {
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
                    errors.push(QuartzError::InvalidSeqAssignSig { assign })
                }
                (TypecheckMode::Sequential, LogicKind::Module) => {
                    errors.push(QuartzError::InvalidSeqAssignMod { assign })
                }
                (TypecheckMode::Combinatoric, LogicKind::Register) => {
                    errors.push(QuartzError::InvalidCombAssignReg { assign })
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

            if let Some(value) = &value {
                if value.ty() != ty {
                    let target_ty = &known_types[&ty];
                    let value_ty = &known_types[&value.ty()];
                    errors.push(QuartzError::IncompatibleAssignType {
                        assign,
                        target_ty: target_ty.to_string(known_types),
                        value_ty: value_ty.to_string(known_types),
                    })
                }
            }

            let target = CheckedAssignTarget::new(base.clone(), base_ty, suffixes);
            wrap_errors!(CheckedAssignment::new(target, value.unwrap()), errors)
        }
        Err(err) => {
            errors.push(err);
            Err(QuartzError::new_list(errors))
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
) -> QuartzResult<'a, CheckedStatement> {
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
        Statement::Declaration(decl) => Err(QuartzError::UnsupportedDeclaration { decl }),
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
        Statement::WhileLoop(while_loop) => Err(QuartzError::UnsupportedWhileLoop { while_loop }),
        Statement::ForLoop(for_loop) => Err(QuartzError::UnsupportedForLoop { for_loop }),
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
) -> QuartzResult<'a, CheckedExprBlock> {
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
            errors.push(QuartzError::MissingReturnValue { block });
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
) -> QuartzResult<'a, CheckedBlock> {
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

                errors.push(QuartzError::UnexpectedReturnValue { value: result });
            }
        }
    }

    wrap_errors!(CheckedBlock::new(statements), errors)
}

fn type_contains_module(
    id: TypeId,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> bool {
    match &known_types[&id] {
        ResolvedType::Const | ResolvedType::BuiltinBits { .. } => false,
        ResolvedType::Named { .. } => match &resolved_types[&id] {
            // Structs are checked beforehand to not contain any modules
            ResolvedTypeItem::Struct(_) | ResolvedTypeItem::Enum(_) => false,
            ResolvedTypeItem::Module(_) => true,
        },
        ResolvedType::Array { item_ty, .. } => {
            type_contains_module(*item_ty, known_types, resolved_types)
        }
    }
}

pub fn struct_contains_module<'a>(
    struct_item: &ResolvedStruct,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
) -> QuartzResult<'a, ()> {
    for (_, &(field_ty, field_span)) in struct_item.fields().iter() {
        if type_contains_module(field_ty, known_types, resolved_types) {
            return Err(QuartzError::StructModuleField { field_span });
        }
    }

    Ok(())
}

pub fn typecheck_module<'a>(
    module_item: &'a ResolvedModule,
    global_scope: &Scope,
    known_types: &mut HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    global_const_values: &HashMap<SharedString, i64>,
    funcs: &HashMap<SharedString, ConstFunc>,
) -> QuartzResult<'a, CheckedModule> {
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

    for (_, port) in module_item.ports() {
        match (port.dir(), port.kind()) {
            (Direction::In, LogicKind::Register)
            | (Direction::In, LogicKind::Module)
            | (Direction::Out, LogicKind::Module)
            | (Direction::InOut, LogicKind::Register)
            | (Direction::InOut, LogicKind::Module) => {
                errors.push(QuartzError::InvalidPortKind {
                    port_span: port.span(),
                    port_dir: port.dir(),
                    port_kind: port.kind(),
                });
            }
            _ => { /* valid */ }
        }

        let ty_is_mod = type_contains_module(port.ty(), known_types, resolved_types);
        match (port.kind(), ty_is_mod) {
            (LogicKind::Signal, true)
            | (LogicKind::Register, true)
            | (LogicKind::Module, false) => errors.push(QuartzError::PortKindMismatch {
                port_span: port.span(),
                port_ty: known_types[&port.ty()].to_string(known_types),
            }),
            (LogicKind::Module, true) => errors.push(QuartzError::PortModuleType {
                port_span: port.span(),
            }),
            _ => { /* valid */ }
        }
    }

    for (_, logic_member) in module_item.logic_members() {
        let ty_is_mod = type_contains_module(logic_member.ty(), known_types, resolved_types);
        match (logic_member.kind(), ty_is_mod) {
            (LogicKind::Signal, true)
            | (LogicKind::Register, true)
            | (LogicKind::Module, false) => errors.push(QuartzError::MemberKindMismatch {
                member_span: logic_member.span(),
                member_ty: known_types[&logic_member.ty()].to_string(known_types),
            }),
            _ => { /* valid */ }
        }
    }

    let mut proc_members = Vec::with_capacity(module_item.proc_members().len());
    for proc_member in module_item.proc_members() {
        let mut sens = Vec::with_capacity(proc_member.sens().len());
        for s in proc_member.sens() {
            match typecheck_assign_target(
                s.sig(),
                module_item,
                TypecheckMode::Combinatoric,
                &module_scope,
                known_types,
                resolved_types,
                &args,
            ) {
                Ok(target) => {
                    let target_ty = &known_types[&target.ty()];
                    if matches!(target_ty, ResolvedType::BuiltinBits { width } if *width == 1) {
                        sens.push(CheckedSens::new(target, s.edge().kind()));
                    } else {
                        errors.push(QuartzError::InvalidSensType {
                            sens: s,
                            sens_ty: target_ty.to_string(known_types),
                        });
                    }
                }
                Err(err) => errors.push(err),
            }
        }

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
                proc_members.push(CheckedProcMember::new(sens, body));
            }
            Err(err) => errors.push(err),
        }
    }

    let mut comb_members = Vec::with_capacity(module_item.comb_members().len());
    for comb_member in module_item.comb_members() {
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
