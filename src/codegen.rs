use crate::ir;
use std::fmt;

#[derive(Clone, Debug)]
pub enum GeneratedCodeItem {
    Section(String),
    Label(String),
    Instruction(String, Vec<String>),
}

#[derive(Clone, Debug)]
pub struct GeneratedCode {
    pub items: Vec<GeneratedCodeItem>,
    is_data_section_started: bool,
}

impl GeneratedCode {
    fn new() -> Self {
        return GeneratedCode {
            items: Vec::new(),
            is_data_section_started: false,
        }
    }

    fn visit(&mut self, ir_item: &ir::IRItem) {
        match ir_item {
            ir::IRItem::VarString(label, s) => {
                if !self.is_data_section_started {
                    self.items.push(GeneratedCodeItem::Section(".data".to_string()));
                    self.is_data_section_started = true;
                }

                self.items.push(GeneratedCodeItem::Label(ir::format_variable_label(label)));
                self.items.push(GeneratedCodeItem::Instruction(".asciz".to_string(), vec![format!("\"{}\"", s)]));
            },
            _ => {},
        }
    }

    pub fn from(ir_context: ir::IRContext) -> Self {
        let mut generated_code = Self::new();

        for ir_item in &ir_context.items {
            generated_code.visit(ir_item);
        }

        return generated_code;
    }
}

impl fmt::Display for GeneratedCodeItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            GeneratedCodeItem::Section(s) =>
                write!(f, "{}", s),
            GeneratedCodeItem::Label(s) =>
                write!(f, "{}:", s),
            GeneratedCodeItem::Instruction(s, v) =>
                write!(
                    f,
                    "{}{} {}",
                    " ".repeat(4),
                    s,
                    v.join(" "),
                ),
        }
    }
}

impl fmt::Display for GeneratedCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.items.iter()
                .map(|i| format!("{}", i))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}