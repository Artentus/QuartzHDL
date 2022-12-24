use crate::ast::*;
use crate::ir::*;
use crate::vir::*;
use crate::{HashMap, SharedString};
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
                // In Verilog, array syntax is right associative, so "outer" arrays actually appear on the left.
                array: Some(format!("{}{}", new_array.into(), array).into()),
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
            array
        } else {
            ""
        }
    }
}

fn get_transpiled_type_name(
    id: TypeId,
    known_types: &HashMap<TypeId, ResolvedType>,
) -> TranspiledTypeName {
    use std::fmt::Write;

    match &known_types[&id] {
        ResolvedType::Const => unreachable!("invalid type"),
        &ResolvedType::BuiltinBits { width } => {
            if width == 1 {
                TranspiledTypeName::new("logic")
            } else {
                TranspiledTypeName::new(format!("logic[{}:0]", width - 1))
            }
        }
        &ResolvedType::BuiltinInPort { width } => {
            let mut transpiled_name = String::new();
            transpiled_name.push_str("InPort__");
            write!(transpiled_name, "{}", width).unwrap();

            TranspiledTypeName::new(transpiled_name)
        }
        &ResolvedType::BuiltinOutPort { width } => {
            let mut transpiled_name = String::new();
            transpiled_name.push_str("OutPort__");
            write!(transpiled_name, "{}", width).unwrap();

            TranspiledTypeName::new(transpiled_name)
        }
        &ResolvedType::BuiltinInOutPort { width } => {
            let mut transpiled_name = String::new();
            transpiled_name.push_str("InOutPort__");
            write!(transpiled_name, "{}", width).unwrap();

            TranspiledTypeName::new(transpiled_name)
        }
        ResolvedType::Named { name, generic_args } => {
            if generic_args.is_empty() {
                TranspiledTypeName::new(name.as_ref())
            } else {
                let mut transpiled_name = String::new();
                transpiled_name.push_str(name.as_ref());
                transpiled_name.push('_');

                for generic_arg in generic_args.iter() {
                    write!(transpiled_name, "_{generic_arg}").unwrap();
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

enum ResolvedModuleItem<'a> {
    Module(&'a ResolvedModule),
    ExternModule(&'a ResolvedExternModule),
    Port(&'a ResolvedExternModule, Direction),
    TriPort(&'a ResolvedExternModule, u64),
}

impl ResolvedModuleItem<'_> {
    fn ports(&self) -> &HashMap<SharedString, ResolvedPort> {
        match self {
            ResolvedModuleItem::Module(module) => module.ports(),
            ResolvedModuleItem::ExternModule(module) => module.ports(),
            ResolvedModuleItem::Port(module, _) => module.ports(),
            ResolvedModuleItem::TriPort(module, _) => module.ports(),
        }
    }
}

fn get_module_item<'a>(
    id: TypeId,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &'a HashMap<TypeId, ResolvedTypeItem>,
) -> Option<(ResolvedModuleItem<'a>, Vec<u64>)> {
    match &known_types[&id] {
        ResolvedType::Const => None,
        ResolvedType::BuiltinBits { .. } => None,
        ResolvedType::BuiltinInPort { .. } => {
            let ResolvedTypeItem::ExternModule(module_item) = &resolved_types[&id] else {
                unreachable!("invalid port item");
            };

            Some((
                ResolvedModuleItem::Port(module_item, Direction::In),
                Vec::new(),
            ))
        }
        ResolvedType::BuiltinOutPort { .. } => {
            let ResolvedTypeItem::ExternModule(module_item) = &resolved_types[&id] else {
                unreachable!("invalid port item");
            };

            Some((
                ResolvedModuleItem::Port(module_item, Direction::Out),
                Vec::new(),
            ))
        }
        &ResolvedType::BuiltinInOutPort { width } => {
            let ResolvedTypeItem::ExternModule(module_item) = &resolved_types[&id] else {
                unreachable!("invalid port item");
            };

            Some((ResolvedModuleItem::TriPort(module_item, width), Vec::new()))
        }
        ResolvedType::Named { .. } => match &resolved_types[&id] {
            ResolvedTypeItem::Struct(_) => None,
            ResolvedTypeItem::Enum(_) => None,
            ResolvedTypeItem::Module(module_item) | ResolvedTypeItem::TopModule(module_item) => {
                Some((ResolvedModuleItem::Module(module_item), Vec::new()))
            }
            ResolvedTypeItem::ExternModule(module_item) => {
                Some((ResolvedModuleItem::ExternModule(module_item), Vec::new()))
            }
        },
        &ResolvedType::Array { item_ty, len } => {
            let (module_item, mut array_lengths) =
                get_module_item(item_ty, known_types, resolved_types)?;
            array_lengths.push(len);
            Some((module_item, array_lengths))
        }
    }
}

pub fn transpile(
    writer: &mut impl Write,
    known_types: &HashMap<TypeId, ResolvedType>,
    resolved_types: &HashMap<TypeId, ResolvedTypeItem>,
    v_modules: &[(TypeId, VModule)],
) -> std::io::Result<()> {
    writeln!(writer, "`default_nettype none\n")?;

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
            ResolvedTypeItem::ExternModule(module_item) => {
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
                    writeln!(writer, "}} {}__Interface;\n", item_name.base)?;
                }
            }
            ResolvedTypeItem::Module(_) | ResolvedTypeItem::TopModule(_) => {}
        }
    }

    for (module_ty, module) in v_modules {
        let module_item = match &resolved_types[module_ty] {
            ResolvedTypeItem::Module(module_item) | ResolvedTypeItem::TopModule(module_item) => {
                module_item
            }
            _ => unreachable!(),
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
            writeln!(writer, "}} {module_name}__Interface;\n")?;
        }

        if !module_item.attributes().is_empty() {
            write!(writer, "(* ")?;
            for (i, attribute) in module_item.attributes().iter().enumerate() {
                if i > 0 {
                    write!(writer, ", ")?;
                }

                write!(writer, "{}", attribute.name().as_ref())?;
                if let Some(value) = attribute.value() {
                    write!(writer, "=\"{}\"", value.value().as_ref())?;
                }
            }
            writeln!(writer, " *)")?;
        }

        write!(writer, "module {module_name} (")?;

        for (i, (port_name, port)) in module_item.ports().iter().enumerate() {
            if i > 0 {
                writeln!(writer, ",")?;
            } else {
                writeln!(writer)?;
            }

            if !port.attributes().is_empty() {
                write!(writer, "(* ")?;
                for (i, attribute) in port.attributes().iter().enumerate() {
                    if i > 0 {
                        write!(writer, ", ")?;
                    }

                    write!(writer, "{}", attribute.name().as_ref())?;
                    if let Some(value) = attribute.value() {
                        write!(writer, "=\"{}\"", value.value().as_ref())?;
                    }
                }
                write!(writer, " *) ")?;
            }

            match port.dir() {
                Direction::In => write!(writer, "    input var ")?,
                Direction::Out => write!(writer, "    output var ")?,
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

        let mut has_prev = !module_item.ports().is_empty();
        for (member_name, member) in module_item.logic_members() {
            let member_ty_name = get_transpiled_type_name(member.ty(), known_types);
            if member.kind() == LogicKind::Module {
                match get_module_item(member.ty(), known_types, resolved_types) {
                    Some((ResolvedModuleItem::Port(_, dir), _)) => {
                        if has_prev {
                            writeln!(writer, ",")?;
                        } else {
                            writeln!(writer)?;
                        }
                        has_prev = true;

                        if !member.attributes().is_empty() {
                            write!(writer, "(* ")?;
                            for (i, attribute) in member.attributes().iter().enumerate() {
                                if i > 0 {
                                    write!(writer, ", ")?;
                                }

                                write!(writer, "{}", attribute.name().as_ref())?;
                                if let Some(value) = attribute.value() {
                                    write!(writer, "=\"{}\"", value.value().as_ref())?;
                                }
                            }
                            write!(writer, " *) ")?;
                        }

                        match dir {
                            Direction::In => write!(writer, "    input var ")?,
                            Direction::Out => write!(writer, "    output var ")?,
                        }

                        write!(
                            writer,
                            "{}__Interface {}{}",
                            member_ty_name.base(),
                            member_name,
                            member_ty_name.array()
                        )?;
                    }
                    Some((ResolvedModuleItem::TriPort(_, width), _)) => {
                        if has_prev {
                            writeln!(writer, ",")?;
                        } else {
                            writeln!(writer)?;
                        }
                        has_prev = true;

                        if !member.attributes().is_empty() {
                            write!(writer, "(* ")?;
                            for (i, attribute) in member.attributes().iter().enumerate() {
                                if i > 0 {
                                    write!(writer, ", ")?;
                                }

                                write!(writer, "{}", attribute.name().as_ref())?;
                                if let Some(value) = attribute.value() {
                                    write!(writer, "=\"{}\"", value.value().as_ref())?;
                                }
                            }
                            write!(writer, " *) ")?;
                        }

                        write!(writer, "    inout tri ")?;

                        if width == 1 {
                            write!(writer, "logic ")?;
                        } else {
                            write!(writer, "logic[{}:0] ", width - 1)?;
                        }

                        write!(writer, "{}__port{}", member_name, member_ty_name.array())?;
                    }
                    _ => {}
                }
            }
        }

        writeln!(writer, "\n);\n")?;

        for (member_name, member) in module_item.logic_members() {
            let member_ty_name = get_transpiled_type_name(member.ty(), known_types);
            if member.kind() == LogicKind::Module {
                let Some((member_module_item, array_lengths)) = get_module_item(member.ty(), known_types, resolved_types) else {
                    unreachable!("invalid module type");
                };

                if let ResolvedModuleItem::Port(_, _) = member_module_item {
                    continue;
                }

                if !member_module_item.ports().is_empty() {
                    writeln!(
                        writer,
                        "var {}__Interface {}{} /*verilator split_var*/;",
                        member_ty_name.base(),
                        member_name,
                        member_ty_name.array(),
                    )?;
                }

                if let ResolvedModuleItem::TriPort(_, _) = member_module_item {
                    for (array_level, array_len) in array_lengths.iter().copied().rev().enumerate()
                    {
                        writeln!(
                            writer,
                            "for (genvar i{array_level} = 0; i{array_level} < {array_len}; i{array_level}++) begin",
                        )?;
                    }

                    let mut indexers = String::new();
                    for i in 0..array_lengths.len() {
                        use std::fmt::Write;
                        write!(indexers, "[i{i}]",).unwrap();
                    }

                    writeln!(
                        writer,
                        "assign {member_name}__port{indexers} = {member_name}{indexers}.oe ? {member_name}{indexers}.d_out : 'bz;",
                    )?;

                    writeln!(
                        writer,
                        "assign {member_name}{indexers}.d_in = {member_name}__port{indexers};",
                    )?;

                    for _ in 0..array_lengths.len() {
                        writeln!(writer, "end")?;
                    }

                    continue;
                }

                if !member.attributes().is_empty() {
                    write!(writer, "(* ")?;
                    for (i, attribute) in member.attributes().iter().enumerate() {
                        if i > 0 {
                            write!(writer, ", ")?;
                        }

                        write!(writer, "{}", attribute.name().as_ref())?;
                        if let Some(value) = attribute.value() {
                            write!(writer, "=\"{}\"", value.value().as_ref())?;
                        }
                    }
                    write!(writer, " *) ")?;
                }

                for (array_level, array_len) in array_lengths.iter().copied().rev().enumerate() {
                    writeln!(
                        writer,
                        "for (genvar i{array_level} = 0; i{array_level} < {array_len}; i{array_level}++) begin",
                    )?;
                }

                writeln!(
                    writer,
                    "{} {}__instance (",
                    member_ty_name.base(),
                    member_name,
                )?;

                for (i, (port_name, _)) in member_module_item.ports().iter().enumerate() {
                    write!(writer, "    .{port_name}({member_name}")?;

                    for i in 0..array_lengths.len() {
                        write!(writer, "[i{i}]",)?;
                    }

                    write!(writer, ".{port_name}")?;

                    if (i as isize) >= ((member_module_item.ports().len() as isize) - 1) {
                        writeln!(writer, ")")?;
                    } else {
                        writeln!(writer, "),")?;
                    }
                }

                writeln!(writer, ");")?;

                for _ in 0..array_lengths.len() {
                    writeln!(writer, "end")?;
                }
            } else {
                if !member.attributes().is_empty() {
                    write!(writer, "(* ")?;
                    for (i, attribute) in member.attributes().iter().enumerate() {
                        if i > 0 {
                            write!(writer, ", ")?;
                        }

                        write!(writer, "{}", attribute.name().as_ref())?;
                        if let Some(value) = attribute.value() {
                            write!(writer, "=\"{}\"", value.value().as_ref())?;
                        }
                    }
                    write!(writer, " *) ")?;
                }

                writeln!(
                    writer,
                    "var {} {}{};",
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
                "var {} {}{};",
                member_ty_name.base(),
                member_name,
                member_ty_name.array(),
            )?;
        }

        if !module.tmp_statements().statements().is_empty() {
            writeln!(writer, "always_comb begin")?;
            transpile_block(writer, module.tmp_statements(), 1)?;
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
                            transpile_expr(writer, index)?;
                            write!(writer, "]")?;
                        }
                        VSuffixOp::Indexer(VIndexKind::Range(range)) => {
                            write!(writer, "[{}:{}]", range.end - 1, range.start)?;
                        }
                        VSuffixOp::MemberAccess(member) => write!(writer, ".{member}")?,
                    }
                }
            }

            writeln!(writer, ") begin")?;

            transpile_block(writer, ff_member.body(), 1)?;

            writeln!(writer, "end\n")?;
        }

        for comb_member in module.comb_members() {
            writeln!(writer, "always_comb begin")?;

            transpile_block(writer, comb_member, 1)?;

            writeln!(writer, "end\n")?;
        }

        for assign_member in module.assign_members() {
            write!(writer, "assign ")?;

            write!(writer, "{}", assign_member.target().base())?;
            for suffix in assign_member.target().suffixes() {
                match suffix {
                    VSuffixOp::Indexer(VIndexKind::Single(index)) => {
                        write!(writer, "[")?;
                        transpile_expr(writer, index)?;
                        write!(writer, "]")?;
                    }
                    VSuffixOp::Indexer(VIndexKind::Range(range)) => {
                        write!(writer, "[{}:{}]", range.end - 1, range.start)?;
                    }
                    VSuffixOp::MemberAccess(member) => write!(writer, ".{member}")?,
                }
            }

            write!(writer, " = ")?;
            transpile_expr(writer, assign_member.condition())?;
            write!(writer, " ? ")?;
            transpile_expr(writer, assign_member.value())?;
            writeln!(writer, ": 'bz;")?
        }
        writeln!(writer)?;

        writeln!(writer, "endmodule\n")?;
    }

    Ok(())
}

fn transpile_expr(writer: &mut impl Write, expr: &VExpr) -> std::io::Result<()> {
    macro_rules! bin_expr {
        ($bin_expr:expr, $op:literal) => {{
            write!(writer, "(")?;
            transpile_expr(writer, $bin_expr.lhs())?;
            write!(writer, ")")?;
            write!(writer, $op)?;
            write!(writer, "(")?;
            transpile_expr(writer, $bin_expr.rhs())?;
            write!(writer, ")")?;
        }};
    }

    macro_rules! signed_bin_expr {
        ($bin_expr:expr, $op:literal) => {{
            write!(writer, "$signed(")?;
            transpile_expr(writer, $bin_expr.lhs())?;
            write!(writer, ")")?;
            write!(writer, $op)?;
            write!(writer, "$signed(")?;
            transpile_expr(writer, $bin_expr.rhs())?;
            write!(writer, ")")?;
        }};
    }

    match expr {
        VExpr::Value(value) => write!(writer, "{value}")?,
        VExpr::Literal(literal) => write!(writer, "{literal}")?,
        VExpr::HighZLiteral(width) => write!(writer, "{width}'dZ")?,
        VExpr::Ident(ident) => write!(writer, "{ident}")?,
        VExpr::Index(index_expr) => {
            transpile_expr(writer, index_expr.base())?;
            write!(writer, "[")?;
            match index_expr.indexer() {
                VIndexKind::Single(index) => transpile_expr(writer, index)?,
                VIndexKind::Range(range) => write!(writer, "{}:{}", range.end - 1, range.start)?,
            }
            write!(writer, "]")?;
        }
        VExpr::MemberAccess(member_access) => {
            transpile_expr(writer, member_access.base())?;
            write!(writer, ".")?;
            write!(writer, "{}", member_access.member().as_ref())?;
        }
        VExpr::Neg(expr) => {
            write!(writer, "-(")?;
            transpile_expr(writer, expr.inner())?;
            write!(writer, ")")?;
        }
        VExpr::Not(expr) => {
            write!(writer, "~(")?;
            transpile_expr(writer, expr.inner())?;
            write!(writer, ")")?;
        }
        VExpr::Concat(expr) => {
            write!(writer, "{{")?;
            transpile_expr(writer, expr.lhs())?;
            write!(writer, ", ")?;
            transpile_expr(writer, expr.rhs())?;
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
) -> std::io::Result<()> {
    let leading_ws = str::repeat("    ", level);

    match statement {
        VStatement::Block(block) => {
            writeln!(writer, "{leading_ws}begin")?;

            transpile_block(writer, block, level + 1)?;

            writeln!(writer, "{leading_ws}end")?;
        }
        VStatement::If(if_statement) => {
            write!(writer, "{leading_ws}if (")?;
            transpile_expr(writer, if_statement.condition())?;
            writeln!(writer, ") begin")?;
            transpile_block(writer, if_statement.body(), level + 1)?;

            for (condition, body) in if_statement.else_if_blocks().iter() {
                write!(writer, "{leading_ws}end else if (")?;
                transpile_expr(writer, condition)?;
                writeln!(writer, ") begin")?;

                transpile_block(writer, body, level + 1)?;
            }

            if let Some(else_block) = if_statement.else_block() {
                writeln!(writer, "{leading_ws}end else begin")?;
                transpile_block(writer, else_block, level + 1)?;
            }

            writeln!(writer, "{leading_ws}end")?;
        }
        VStatement::Case(case_statement) => {
            write!(writer, "{leading_ws}case (")?;
            transpile_expr(writer, case_statement.value())?;
            writeln!(writer, ")")?;

            for (i, branch) in case_statement.branches().iter().enumerate() {
                write!(writer, "{leading_ws}    ")?;

                let is_default = branch.patterns().iter().any(|pattern| {
                    matches!(pattern, VCasePattern::Ident(ident) if ident.as_ref() == "_")
                });
                if is_default || (i == (case_statement.branches().len() - 1)) {
                    writeln!(writer, "default: begin")?;
                } else {
                    for (i, pattern) in branch.patterns().iter().enumerate() {
                        if i > 0 {
                            write!(writer, ", ")?;
                        }

                        match pattern {
                            VCasePattern::Literal(literal) => write!(writer, "{literal}")?,
                            VCasePattern::Ident(ident) => write!(writer, "{ident}")?,
                        }
                    }

                    writeln!(writer, ": begin")?;
                }

                transpile_block(writer, branch.body(), level + 2)?;

                writeln!(writer, "{leading_ws}    end")?;

                if is_default {
                    break;
                }
            }

            writeln!(writer, "{leading_ws}endcase")?;
        }
        VStatement::Assignment(assign) => {
            write!(writer, "{}{}", leading_ws, assign.target().base())?;
            for suffix in assign.target().suffixes() {
                match suffix {
                    VSuffixOp::Indexer(VIndexKind::Single(index)) => {
                        write!(writer, "[")?;
                        transpile_expr(writer, index)?;
                        write!(writer, "]")?;
                    }
                    VSuffixOp::Indexer(VIndexKind::Range(range)) => {
                        write!(writer, "[{}:{}]", range.end - 1, range.start)?;
                    }
                    VSuffixOp::MemberAccess(member) => write!(writer, ".{member}")?,
                }
            }

            match assign.mode() {
                VAssignMode::Sequential => write!(writer, " <= ")?,
                VAssignMode::Combinatoric => write!(writer, " = ")?,
            }

            transpile_expr(writer, assign.value())?;

            writeln!(writer, ";")?
        }
    }

    Ok(())
}

fn transpile_block(writer: &mut impl Write, block: &VBlock, level: usize) -> std::io::Result<()> {
    for statement in block.statements().iter() {
        transpile_statement(writer, statement, level)?;
    }

    Ok(())
}
