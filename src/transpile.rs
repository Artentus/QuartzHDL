use crate::ast::*;
use crate::ir::*;
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
    checked_modules: &[(TypeId, CheckedModule)],
) -> std::io::Result<()> {
    for (ty, item) in resolved_types.iter() {
        let item_name = get_transpiled_type_name(*ty, known_types);

        match item {
            ResolvedTypeItem::Struct(struct_item) => {
                writeln!(writer, "typedef struct packed {{")?;
                for (field_name, &(field_ty, _)) in struct_item.fields().iter() {
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

    for (module_ty, module) in checked_modules.iter() {
        let ResolvedTypeItem::Module(module_item) = &resolved_types[module_ty] else {
            unreachable!();
        };

        let module_name = get_transpiled_type_name(*module_ty, known_types).base;
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

        for (member_name, member) in module_item.logic_members().iter() {
            let member_ty_name = get_transpiled_type_name(member.ty(), known_types);
            if member.kind() == LogicKind::Module {
                let Some(member_module_item) = get_module_item(member.ty(), known_types, resolved_types) else {
                    unreachable!("invalid module type");
                };

                for (port_name, port) in member_module_item.ports().iter() {
                    let mut signal_name = String::new();
                    signal_name.push_str(member_name.as_ref());
                    signal_name.push_str("__");
                    signal_name.push_str(port_name.as_ref());

                    let port_ty_name = get_transpiled_type_name(port.ty(), known_types);
                    writeln!(
                        writer,
                        "{} {}{}{};",
                        port_ty_name.base(),
                        signal_name,
                        port_ty_name.array(),
                        member_ty_name.array(),
                    )?;
                }

                writeln!(
                    writer,
                    "{} {}{} (",
                    member_ty_name.base(),
                    member_name,
                    member_ty_name.array(),
                )?;

                // FIXME: these assignments do not work if we have a module array
                for (i, (port_name, _)) in member_module_item.ports().iter().enumerate() {
                    let mut signal_name = String::new();
                    signal_name.push_str(member_name.as_ref());
                    signal_name.push_str("__");
                    signal_name.push_str(port_name.as_ref());

                    if (i as isize) >= ((member_module_item.ports().len() as isize) - 1) {
                        writeln!(writer, "    .{}({})", port_name, signal_name)?;
                    } else {
                        writeln!(writer, "    .{}({}),", port_name, signal_name)?;
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

        for proc_member in module.proc_members().iter() {
            write!(writer, "always_ff @(")?;

            for (i, sens) in proc_member.sens().iter().enumerate() {
                if i > 0 {
                    write!(writer, ", ")?;
                }

                match sens.edge().kind() {
                    EdgeKind::Rising => write!(writer, "posedge ")?,
                    EdgeKind::Falling => write!(writer, "negedge ")?,
                }

                write!(writer, "{}", sens.sig())?;
            }

            writeln!(writer, ") begin")?;

            writeln!(writer, "end\n")?;
        }

        for comb_member in module.comb_members().iter() {
            writeln!(writer, "always_comb begin")?;

            writeln!(writer, "end\n")?;
        }

        writeln!(writer, "endmodule\n")?;
    }

    Ok(())
}
