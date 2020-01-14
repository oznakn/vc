use crate::ir;
use std::cmp::max;

#[derive(Clone, Debug)]
pub enum GeneratedCodeItem {
    Section(String),
    Label(String),
    Instruction(String, Vec<String>),
}

impl GeneratedCodeItem {
    fn to_string(&self, n: usize) -> String {
        return match self {
            GeneratedCodeItem::Section(s) =>
                format!("{}", s),
            GeneratedCodeItem::Label(s) =>
                format!("{}:", s),
            GeneratedCodeItem::Instruction(s, v) =>
                format!(
                    "{}{}{}{}",
                    " ".repeat(4),
                    s,
                    " ".repeat(max(0, n - s.len())),
                    v.join(", "),
                ),
        }
    }
}

pub fn convert_to_string(items: &Vec<GeneratedCodeItem>) -> String {
    let mut max_size: usize = 0;
    let mut result = "".to_string();

    for item in items {
        match item {
            GeneratedCodeItem::Instruction(s, _) => {
                if s.len() > max_size {
                    max_size = s.len();
                }
            },
            _ => {},
        }
    }

    max_size += 5;

    for item in items {
        result += &item.to_string(max_size);
        result += "\n";
    }

    return result;
}

#[derive(Clone, Debug)]
pub struct CodeGenerator<'ir> {
    is_data_section_started: bool,
    is_text_section_started: bool,
    current_function: Option<&'ir ir::Function>,
}

impl<'ir> CodeGenerator<'ir> {
    pub fn new() -> Self {
        return CodeGenerator {
            is_data_section_started: false,
            is_text_section_started: false,
            current_function: None,
        }
    }

    fn check_data_section(&mut self, items: &mut Vec<GeneratedCodeItem>) {
        if !self.is_data_section_started {
            items.push(GeneratedCodeItem::Section("\n.data".to_string()));
            self.is_data_section_started = true;
        }
    }

    fn check_text_section(&mut self, items: &mut Vec<GeneratedCodeItem>) {
        if !self.is_text_section_started {
            items.push(GeneratedCodeItem::Section("\n.text".to_string()));
            self.is_text_section_started = true;
        }
    }

    fn visit(&mut self, items: &mut Vec<GeneratedCodeItem>, ir_item: &'ir ir::IRItem) {
        match ir_item {
            ir::IRItem::Var(label, size) => {
                self.check_data_section(items);

                items.push(GeneratedCodeItem::Label(ir::format_variable_label(label)));
                items.push(GeneratedCodeItem::Instruction(".zero".to_string(), vec![format!("{}", size)]));
            },
            ir::IRItem::VarString(label, s) => {
                self.check_data_section(items);

                items.push(GeneratedCodeItem::Label(ir::format_variable_label(label)));
                items.push(GeneratedCodeItem::Instruction(".string".to_string(), vec![format!("\"{}\"", s)])); // .ascii .asciz
            },
            ir::IRItem::Label(label) => {
                self.check_text_section(items);

                items.push(GeneratedCodeItem::Label(label.to_owned()));
            },
            ir::IRItem::Function(label, f) =>  {
                self.check_text_section(items);

                self.current_function = Some(f);

                items.push(GeneratedCodeItem::Label(label.to_owned()));
            },
            _ => {},
        }
    }

    pub fn build(&mut self, ir_context: &'ir ir::IRContext) -> Vec<GeneratedCodeItem> {
        let mut items = Vec::new();

        items.push(GeneratedCodeItem::Section(format!(".global {}", ir::START_LABEL)));

        for ir_item in &ir_context.items {
            self.visit(&mut items, ir_item);
        }

        return items;
    }
}