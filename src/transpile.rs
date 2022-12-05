use crate::ast::*;
use crate::ir::*;
use crate::vir::*;
use crate::HashMap;
use std::borrow::Cow;
use std::io::Write;

#[derive(Clone)]
struct TranspiledTypeName<'a> {
    base: Cow<'a, str>,
    array: Option<Cow<'a, str>>,
}

impl<'a> TranspiledTypeName<'a> {
    fn new(base: impl Into<Cow<'a, str>>) -> Self {
        Self {
            base: base.into(),
            array: None,
        }
    }

    fn append_array(self, new_array: impl Into<Cow<'a, str>>) -> Self {
        if let Some(array) = self.array {
            Self {
                base: self.base,
                array: Some(format!("{}{}", array, new_array.into()).into()),
            }
        } else {
            Self {
                base: self.base,
                array: Some(new_array.into()),
            }
        }
    }

    fn base(&self) -> &str {
        &self.base
    }

    fn array(&self) -> &str {
        if let Some(array) = &self.array {
            &array
        } else {
            ""
        }
    }
}

fn get_transpiled_type_name<'a>(
    id: TypeId,
    known_types: &'a HashMap<TypeId, ResolvedType>,
) -> TranspiledTypeName<'a> {
    match &known_types[&id] {
        ResolvedType::Const => unreachable!("invalid type"),
        ResolvedType::BuiltinBits { width } => {
            if *width == 1 {
                TranspiledTypeName::new("logic")
            } else {
                TranspiledTypeName::new(format!("logic[{}:0]", *width - 1))
            }
        }
        ResolvedType::Named { name, generic_args } => {
            if generic_args.is_empty() {
                TranspiledTypeName::new(name.as_ref())
            } else {
                let mut transpiled_name = String::new();
                transpiled_name.push_str(name.as_ref());
                transpiled_name.push('_');

                for generic_arg in generic_args.iter() {
                    use std::fmt::Write;
                    write!(transpiled_name, "_{}", generic_arg).unwrap();
                }

                TranspiledTypeName::new(transpiled_name)
            }
        }
        ResolvedType::Array { item_ty, len } => {
            let item_name = get_transpiled_type_name(*item_ty, known_types);
            item_name.append_array(format!("[{}:0]", len.saturating_sub(1)))
        }
    }
}

fn get_module_item<'a>(
    id: TypeId,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &'a HashMap<TypeId, ResolvedTypeItem>,
) -> Option<&'a ResolvedModule> {
    match &known_types[&id] {
        ResolvedType::Const => None,
        ResolvedType::BuiltinBits { .. } => None,
        ResolvedType::Named { .. } => match &resolved_types[&id] {
            ResolvedTypeItem::Struct(_) => None,
            ResolvedTypeItem::Enum(_) => None,
            ResolvedTypeItem::Module(module_item) => Some(module_item),
        },
        ResolvedType::Array { item_ty, .. } => {
            get_module_item(*item_ty, known_types, resolved_types)
        }
    }
}

pub fn transpile(
    writer: &mut impl Write,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    v_modules: &[(TypeId, VModule)],
) -> std::io::Result<()> {
    for (ty, item) in resolved_types {
        let item_name = get_transpiled_type_name(*ty, known_types);

        match item {
            ResolvedTypeItem::Struct(struct_item) => {
                writeln!(writer, "typedef struct packed {{")?;
                for (field_name, &(field_ty, _)) in struct_item.fields() {
                    let field_ty_name = get_transpiled_type_name(field_ty, known_types);
                    writeln!(
                        writer,
                        "    {} {}{};",
                        field_ty_name.base(),
                        field_name,
                        field_ty_name.array(),
                    )?;
                }
                writeln!(writer, "}} {};\n", item_name.base())?;
            }
            ResolvedTypeItem::Enum(enum_item) => {
                let base_ty_name = get_transpiled_type_name(enum_item.base_ty(), known_types);
                writeln!(writer, "typedef enum {} {{", base_ty_name.base())?;
                for (i, (variant_name, &variant_value)) in enum_item.variants().iter().enumerate() {
                    if (i as isize) >= ((enum_item.variants().len() as isize) - 1) {
                        writeln!(
                            writer,
                            "    {}__{} = {}",
                            item_name.base(),
                            variant_name,
                            variant_value
                        )?;
                    } else {
                        writeln!(
                            writer,
                            "    {}__{} = {},",
                            item_name.base(),
                            variant_name,
                            variant_value
                        )?;
                    }
                }
                writeln!(writer, "}} {};\n", item_name.base())?;
            }
            ResolvedTypeItem::Module(_) => {}
        }
    }

    for (module_ty, module) in v_modules {
        let ResolvedTypeItem::Module(module_item) = &resolved_types[module_ty] else {
            unreachable!();
        };

        let module_name = get_transpiled_type_name(*module_ty, known_types).base;

        if !module_item.ports().is_empty() {
            writeln!(writer, "typedef struct packed {{")?;
            for (port_name, port) in module_item.ports() {
                let port_ty_name = get_transpiled_type_name(port.ty(), known_types);
                writeln!(
                    writer,
                    "    {} {}{};",
                    port_ty_name.base(),
                    port_name,
                    port_ty_name.array(),
                )?;
            }
            writeln!(writer, "}} {}__Interface;\n", module_name)?;
        }

        write!(writer, "module {} (", module_name)?;

        for (i, (port_name, port)) in module_item.ports().iter().enumerate() {
            if i > 0 {
                writeln!(writer, ",")?;
            } else {
                writeln!(writer)?;
            }

            match port.dir() {
                Direction::In => write!(writer, "    input ")?,
                Direction::Out => write!(writer, "    output ")?,
                Direction::InOut => write!(writer, "    inout ")?,
            }

            let port_type_name = get_transpiled_type_name(port.ty(), known_types);
            write!(
                writer,
                "{} {}{}",
                port_type_name.base(),
                port_name,
                port_type_name.array()
            )?;
        }

        writeln!(writer, "\n);\n")?;

        for (member_name, member) in module_item.logic_members() {
            let member_ty_name = get_transpiled_type_name(member.ty(), known_types);
            if member.kind() == LogicKind::Module {
                let Some(member_module_item) = get_module_item(member.ty(), known_types, resolved_types) else {
                    unreachable!("invalid module type");
                };

                if !member_module_item.ports().is_empty() {
                    writeln!(
                        writer,
                        "{}__Interface {}{};",
                        member_ty_name.base(),
                        member_name,
                        member_ty_name.array(),
                    )?;
                }

                writeln!(
                    writer,
                    "{} {}__instance{} (",
                    member_ty_name.base(),
                    member_name,
                    member_ty_name.array(),
                )?;

                // FIXME: these assignments do not work if we have a module array
                for (i, (port_name, _)) in member_module_item.ports().iter().enumerate() {
                    if (i as isize) >= ((member_module_item.ports().len() as isize) - 1) {
                        writeln!(writer, "    .{}({}.{})", port_name, member_name, port_name)?;
                    } else {
                        writeln!(writer, "    .{}({}.{}),", port_name, member_name, port_name)?;
                    }
                }

                writeln!(writer, ");")?;
            } else {
                writeln!(
                    writer,
                    "{} {}{};",
                    member_ty_name.base(),
                    member_name,
                    member_ty_name.array(),
                )?;
            }
        }
        writeln!(writer)?;

        for (member_name, member_ty) in module.tmp_members() {
            let member_ty_name = get_transpiled_type_name(*member_ty, known_types);
            writeln!(
                writer,
                "{} {}{};",
                member_ty_name.base(),
                member_name,
                member_ty_name.array(),
            )?;
        }

        if !module.tmp_statements().statements().is_empty() {
            writeln!(writer, "always_comb begin")?;
            transpile_block(
                writer,
                module.tmp_statements(),
                1,
                TranspileMode::Combinatoric,
                module_item,
                known_types,
                resolved_types,
                v_modules,
            )?;
            writeln!(writer, "end\n")?;
        }

        for ff_member in module.ff_members() {
            write!(writer, "always_ff @(")?;

            for (i, sens) in ff_member.sens().iter().enumerate() {
                if i > 0 {
                    write!(writer, ", ")?;
                }

                match sens.edge() {
                    EdgeKind::Rising => write!(writer, "posedge ")?,
                    EdgeKind::Falling => write!(writer, "negedge ")?,
                }

                write!(writer, "{}", sens.target().base())?;
                for suffix in sens.target().suffixes() {
                    match suffix {
                        VSuffixOp::Indexer(VIndexKind::Single(index)) => {
                            write!(writer, "[")?;
                            transpile_expr(
                                writer,
                                index,
                                0,
                                TranspileMode::Combinatoric,
                                module_item,
                                known_types,
                                resolved_types,
                                v_modules,
                            )?;
                            write!(writer, "]")?;
                        }
                        VSuffixOp::Indexer(VIndexKind::Range(range)) => {
                            write!(writer, "[{}:{}]", range.end - 1, range.start)?;
                        }
                        VSuffixOp::MemberAccess(member) => write!(writer, ".{}", member)?,
                    }
                }
            }

            writeln!(writer, ") begin")?;

            transpile_block(
                writer,
                ff_member.body(),
                1,
                TranspileMode::Sequential,
                module_item,
                known_types,
                resolved_types,
                v_modules,
            )?;

            writeln!(writer, "end\n")?;
        }

        for comb_member in module.comb_members() {
            writeln!(writer, "always_comb begin")?;

            transpile_block(
                writer,
                comb_member,
                1,
                TranspileMode::Combinatoric,
                module_item,
                known_types,
                resolved_types,
                v_modules,
            )?;

            writeln!(writer, "end\n")?;
        }

        writeln!(writer, "endmodule\n")?;
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TranspileMode {
    Sequential,
    Combinatoric,
}

fn transpile_expr(
    writer: &mut impl Write,
    expr: &VExpr,
    level: usize,
    mode: TranspileMode,
    parent_module: &ResolvedModule,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    v_modules: &[(TypeId, VModule)],
) -> std::io::Result<()> {
    macro_rules! bin_expr {
        ($bin_expr:expr, $op:literal) => {{
            write!(writer, "(")?;
            transpile_expr(
                writer,
                $bin_expr.lhs(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;
            write!(writer, ")")?;
            write!(writer, $op)?;
            write!(writer, "(")?;
            transpile_expr(
                writer,
                $bin_expr.rhs(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;
            write!(writer, ")")?;
        }};
    }

    macro_rules! signed_bin_expr {
        ($bin_expr:expr, $op:literal) => {{
            write!(writer, "$signed(")?;
            transpile_expr(
                writer,
                $bin_expr.lhs(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;
            write!(writer, ")")?;
            write!(writer, $op)?;
            write!(writer, "$signed(")?;
            transpile_expr(
                writer,
                $bin_expr.rhs(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;
            write!(writer, ")")?;
        }};
    }

    match expr {
        VExpr::Value(value) => write!(writer, "{}", value)?,
        VExpr::Literal(literal) => write!(writer, "{}", literal)?,
        VExpr::Ident(ident) => write!(writer, "{}", ident)?,
        VExpr::Index(index_expr) => {
            transpile_expr(
                writer,
                index_expr.base(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;
            write!(writer, "[")?;
            match index_expr.indexer() {
                VIndexKind::Single(index) => transpile_expr(
                    writer,
                    index,
                    level,
                    mode,
                    parent_module,
                    known_types,
                    resolved_types,
                    v_modules,
                )?,
                VIndexKind::Range(range) => write!(writer, "{}:{}", range.end - 1, range.start)?,
            }
            write!(writer, "]")?;
        }
        VExpr::MemberAccess(member_access) => {
            transpile_expr(
                writer,
                member_access.base(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;
            write!(writer, ".")?;
            write!(writer, "{}", member_access.member().as_ref())?;
        }
        VExpr::Neg(expr) => {
            write!(writer, "-(")?;
            transpile_expr(
                writer,
                expr.inner(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;
            write!(writer, ")")?;
        }
        VExpr::Not(expr) => {
            write!(writer, "~(")?;
            transpile_expr(
                writer,
                expr.inner(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;
            write!(writer, ")")?;
        }
        VExpr::Concat(expr) => {
            write!(writer, "{{")?;
            transpile_expr(
                writer,
                expr.lhs(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;
            write!(writer, ", ")?;
            transpile_expr(
                writer,
                expr.rhs(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;
            write!(writer, "}}")?;
        }

        VExpr::Lt(bin_expr) => bin_expr!(bin_expr, " < "),
        VExpr::Lte(bin_expr) => bin_expr!(bin_expr, " <= "),
        VExpr::Gt(bin_expr) => bin_expr!(bin_expr, " > "),
        VExpr::Gte(bin_expr) => bin_expr!(bin_expr, " >= "),
        VExpr::Slt(bin_expr) => signed_bin_expr!(bin_expr, " < "),
        VExpr::Slte(bin_expr) => signed_bin_expr!(bin_expr, " <= "),
        VExpr::Sgt(bin_expr) => signed_bin_expr!(bin_expr, " > "),
        VExpr::Sgte(bin_expr) => signed_bin_expr!(bin_expr, " >= "),
        VExpr::Eq(bin_expr) => bin_expr!(bin_expr, " == "),
        VExpr::Ne(bin_expr) => bin_expr!(bin_expr, " != "),

        VExpr::Add(bin_expr) => bin_expr!(bin_expr, " + "),
        VExpr::Sub(bin_expr) => bin_expr!(bin_expr, " - "),
        VExpr::Mul(bin_expr) => bin_expr!(bin_expr, " * "),
        VExpr::Div(bin_expr) => bin_expr!(bin_expr, " / "),
        VExpr::Rem(bin_expr) => bin_expr!(bin_expr, " % "),
        VExpr::And(bin_expr) => bin_expr!(bin_expr, " & "),
        VExpr::Xor(bin_expr) => bin_expr!(bin_expr, " ^ "),
        VExpr::Or(bin_expr) => bin_expr!(bin_expr, " | "),
        VExpr::Shl(bin_expr) => bin_expr!(bin_expr, " << "),
        VExpr::Lsr(bin_expr) => bin_expr!(bin_expr, " >> "),
        VExpr::Asr(bin_expr) => signed_bin_expr!(bin_expr, " >>> "),
    }

    Ok(())
}

fn transpile_statement(
    writer: &mut impl Write,
    statement: &VStatement,
    level: usize,
    mode: TranspileMode,
    parent_module: &ResolvedModule,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    v_modules: &[(TypeId, VModule)],
) -> std::io::Result<()> {
    let leading_ws = str::repeat("    ", level);

    match statement {
        VStatement::Block(block) => {
            writeln!(writer, "{}begin", leading_ws)?;

            transpile_block(
                writer,
                block,
                level + 1,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;

            writeln!(writer, "{}end", leading_ws)?;
        }
        VStatement::If(if_statement) => {
            write!(writer, "{}if (", leading_ws)?;
            transpile_expr(
                writer,
                if_statement.condition(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;
            writeln!(writer, ") begin")?;
            transpile_block(
                writer,
                if_statement.body(),
                level + 1,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;

            for (condition, body) in if_statement.else_if_blocks().iter() {
                write!(writer, "{}end else if (", leading_ws)?;
                transpile_expr(
                    writer,
                    condition,
                    level,
                    mode,
                    parent_module,
                    known_types,
                    resolved_types,
                    v_modules,
                )?;
                writeln!(writer, ") begin")?;

                transpile_block(
                    writer,
                    body,
                    level + 1,
                    mode,
                    parent_module,
                    known_types,
                    resolved_types,
                    v_modules,
                )?;
            }

            if let Some(else_block) = if_statement.else_block() {
                writeln!(writer, "{}end else begin", leading_ws)?;
                transpile_block(
                    writer,
                    else_block,
                    level + 1,
                    mode,
                    parent_module,
                    known_types,
                    resolved_types,
                    v_modules,
                )?;
            }

            writeln!(writer, "{}end", leading_ws)?;
        }
        VStatement::Case(case_statement) => {
            write!(writer, "{}case (", leading_ws)?;
            transpile_expr(
                writer,
                case_statement.value(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;
            writeln!(writer, ")")?;

            for branch in case_statement.branches().iter() {
                write!(writer, "{}    ", leading_ws)?;

                let is_default = branch.patterns().iter().any(|pattern| {
                    matches!(pattern, VCasePattern::Ident(ident) if ident.as_ref() == "_")
                });
                if is_default {
                    writeln!(writer, "default: begin")?;
                } else {
                    for (i, pattern) in branch.patterns().iter().enumerate() {
                        if i > 0 {
                            write!(writer, ", ")?;
                        }

                        match pattern {
                            VCasePattern::Literal(literal) => write!(writer, "{}", literal)?,
                            VCasePattern::Ident(ident) => write!(writer, "{}", ident)?,
                        }
                    }

                    writeln!(writer, ": begin")?;
                }

                transpile_block(
                    writer,
                    branch.body(),
                    level + 2,
                    mode,
                    parent_module,
                    known_types,
                    resolved_types,
                    v_modules,
                )?;

                writeln!(writer, "{}    end", leading_ws)?;
            }

            writeln!(writer, "{}endcase", leading_ws)?;
        }
        VStatement::Assignment(assign) => {
            write!(writer, "{}{}", leading_ws, assign.target().base())?;
            for suffix in assign.target().suffixes() {
                match suffix {
                    VSuffixOp::Indexer(VIndexKind::Single(index)) => {
                        write!(writer, "[")?;
                        transpile_expr(
                            writer,
                            index,
                            level,
                            mode,
                            parent_module,
                            known_types,
                            resolved_types,
                            v_modules,
                        )?;
                        write!(writer, "]")?;
                    }
                    VSuffixOp::Indexer(VIndexKind::Range(range)) => {
                        write!(writer, "[{}:{}]", range.end - 1, range.start)?;
                    }
                    VSuffixOp::MemberAccess(member) => write!(writer, ".{}", member)?,
                }
            }

            match assign.mode() {
                VAssignMode::Sequential => write!(writer, " <= ")?,
                VAssignMode::Combinatoric => write!(writer, " = ")?,
            }

            transpile_expr(
                writer,
                assign.value(),
                level,
                mode,
                parent_module,
                known_types,
                resolved_types,
                v_modules,
            )?;

            writeln!(writer, ";")?
        }
    }

    Ok(())
}

fn transpile_block(
    writer: &mut impl Write,
    block: &VBlock,
    level: usize,
    mode: TranspileMode,
    parent_module: &ResolvedModule,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    v_modules: &[(TypeId, VModule)],
) -> std::io::Result<()> {
    for statement in block.statements().iter() {
        transpile_statement(
            writer,
            statement,
            level,
            mode,
            parent_module,
            known_types,
            resolved_types,
            v_modules,
        )?;
    }

    Ok(())
}
