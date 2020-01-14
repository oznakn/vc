use crate::ir;

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

    pub fn print(&self) {
        for item in &self.items {
            match item {
                GeneratedCodeItem::Section(s) =>
                    println!("{}", s),
                GeneratedCodeItem::Label(s) =>
                    println!("{}{}:", " ".repeat(4) , s),
                GeneratedCodeItem::Instruction(s, v) =>
                    println!(
                        "{}{} {}",
                        " ".repeat(8),
                        s,
                        v.join(" "),
                    ),
            }
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