use std::cmp::max;

use crate::ir;
use crate::ir::{fetch_value_type};
use crate::ast;
use std::collections::{HashMap, VecDeque};

const START_LABEL: &str = "_start";
const ADDRESS_SIZE: u64 = 8;

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
                    " ".repeat(8),
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
    function_stack_offset_map: HashMap<String, (u64, HashMap<ir::ValueStorage, u64>)>,
}

impl<'ir> CodeGenerator<'ir> {
    pub fn new() -> Self {
        return CodeGenerator {
            is_data_section_started: false,
            is_text_section_started: false,
            current_function: None,
            function_stack_offset_map: HashMap::new(),
        }
    }

    fn initialize_stack(&mut self, ir_context: &'ir ir::IRContext) {
        let mut current_function = None;
        let mut local_queue = VecDeque::new();

        let mut function_map = HashMap::new();
        let mut stack_offset_map = HashMap::new();

        for item in &ir_context.items {
            match item {
                ir::IRItem::Function(_, f) =>  {
                    current_function = Some(f);
                },
                ir::IRItem::Local(l, s) => {
                    local_queue.push_back((l, s));
                },
                ir::IRItem::Param(l, s) => {
                    local_queue.push_back((l, s));
                },
                ir::IRItem::Return() => {
                    let mut offset: u64 = 0;

                    while let Some((l, s)) = local_queue.pop_back() {
                        stack_offset_map.insert(l.to_owned(), offset);
                        offset += *s;
                    }

                    offset += ADDRESS_SIZE;

                    if let Some(f) = current_function {
                        function_map.insert(f.name.to_owned(), (offset, stack_offset_map));
                    }
                    stack_offset_map = HashMap::new();
                },
                _ => {},
            }
        }

        self.function_stack_offset_map = function_map;
    }

    fn check_data_section(&mut self, items: &mut Vec<GeneratedCodeItem>) {
        if !self.is_data_section_started {
            items.push(GeneratedCodeItem::Section("\n\n.data".to_string()));
            self.is_data_section_started = true;
        }
    }

    fn check_text_section(&mut self, items: &mut Vec<GeneratedCodeItem>) {
        if !self.is_text_section_started {
            items.push(GeneratedCodeItem::Section("\n\n.text".to_string()));
            self.is_text_section_started = true;
        }
    }

    fn to_asm_repr(&self, s: &'ir ir::ValueStorage) -> String {
        return match s {
            ir::ValueStorage::Local(_) => {
                let stack_offset_map = &self.function_stack_offset_map.get(&self.current_function.unwrap().name).unwrap().1;

                format!("{}(sp)", stack_offset_map.get(s).unwrap())
            },
            _ => {
                format!("{}", s)
            }
        }
    }

    fn load_value_storage(&mut self, ir_context: &'ir ir::IRContext, items: &mut Vec<GeneratedCodeItem>, s: &'ir ir::ValueStorage, register: &str) {
        let value_type = fetch_value_type(ir_context, self.current_function.unwrap(), s);

        match s {
            ir::ValueStorage::Local(_) => {
                let stack_offset_map = &self.function_stack_offset_map.get(&self.current_function.unwrap().name).unwrap().1;

                items.push(GeneratedCodeItem::Instruction("lw".to_string(), vec![register.to_string(), format!("{}(sp)", stack_offset_map.get(s).unwrap())]));
            },
            _ => {
                match value_type {
                    ast::VariableType::String(_) => {
                        items.push(GeneratedCodeItem::Instruction("la".to_string(), vec![register.to_string(), format!("{}", s)]));
                    },
                    _ => {
                        items.push(GeneratedCodeItem::Instruction("lw".to_string(), vec![register.to_string(), format!("{}", s)]));
                    }
                }
            }
        }
    }

    fn store_value_storage(&mut self, ir_context: &'ir ir::IRContext, items: &mut Vec<GeneratedCodeItem>, s: &'ir ir::ValueStorage, register: &str) {
        let value_type = fetch_value_type(ir_context, self.current_function.unwrap(), s);

        match s {
            ir::ValueStorage::Local(_) => {
                let stack_offset_map = &self.function_stack_offset_map.get(&self.current_function.unwrap().name).unwrap().1;

                items.push(GeneratedCodeItem::Instruction("sw".to_string(), vec![register.to_string(), format!("{}(sp)", stack_offset_map.get(s).unwrap())]));
            },
            _ => {
                match value_type {
                    ast::VariableType::String(_) => {},
                    _ => {
                        items.push(GeneratedCodeItem::Instruction("sw".to_string(), vec![register.to_string(), format!("{}", s)]));
                    }
                }
            }
        }
    }

    fn visit(&mut self, items: &mut Vec<GeneratedCodeItem>, ir_context: &'ir ir::IRContext, ir_item: &'ir ir::IRItem) {
        match ir_item {
            ir::IRItem::ConstString(label, s) => {
                self.check_data_section(items);

                items.push(GeneratedCodeItem::Label(format!("{}", label)));
                items.push(GeneratedCodeItem::Instruction(".string".to_string(), vec![format!("\"{}\"", s)])); // .ascii .asciz
            },
            ir::IRItem::ConstInt(label, v) => {
                self.check_data_section(items);

                items.push(GeneratedCodeItem::Label(format!("{}", label)));
                items.push(GeneratedCodeItem::Instruction(".word".to_string(), vec![format!("{}", v)]));
            },
            ir::IRItem::ConstReal(label, v) => {
                self.check_data_section(items);

                items.push(GeneratedCodeItem::Label(format!("{}", label)));
                items.push(GeneratedCodeItem::Instruction(".float".to_string(), vec![format!("{:.4}", v)]));
            },
            ir::IRItem::Start() => {
                self.check_text_section(items);

                items.push(GeneratedCodeItem::Label(START_LABEL.to_owned()));

                items.push(GeneratedCodeItem::Instruction("jal".to_owned(), vec!["ra".to_owned(), ir::MAIN_FUNCTION.to_owned()])); // TODO: change later
                items.push(GeneratedCodeItem::Instruction("addi".to_owned(), vec!["a0".to_owned(), "x0".to_owned(), "0".to_owned()])); // TODO: change later
                items.push(GeneratedCodeItem::Instruction("addi".to_owned(), vec!["a7".to_owned(), "x0".to_owned(), "93".to_owned()]));
                items.push(GeneratedCodeItem::Instruction("ecall".to_owned(), vec![]));
            },
            ir::IRItem::Var(label, size) => {
                self.check_data_section(items);

                items.push(GeneratedCodeItem::Label(format!("{}", label)));
                items.push(GeneratedCodeItem::Instruction(".zero".to_string(), vec![format!("{}", size)]));
            },
            ir::IRItem::Label(label) => {
                self.check_text_section(items);

                items.push(GeneratedCodeItem::Label(label.to_owned()));
            },
            ir::IRItem::Function(label, f) =>  {
                self.check_text_section(items);

                let (offset, _) = &self.function_stack_offset_map.get(&f.name).unwrap();

                self.current_function = Some(f);

                items.push(GeneratedCodeItem::Label(label.to_owned()));
                items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["sp".to_owned(), "sp".to_owned(), format!("-{}", offset)]));
                items.push(GeneratedCodeItem::Instruction("sd".to_string(), vec!["ra".to_owned(), format!("{}(sp)", offset - ADDRESS_SIZE)]));
            },
            ir::IRItem::Return() => {
                let f = self.current_function.unwrap();

                let offset = self.function_stack_offset_map.get(&f.name).unwrap().0;

                items.push(GeneratedCodeItem::Instruction("ld".to_string(), vec!["ra".to_owned(), format!("{}(sp)", offset - ADDRESS_SIZE)]));
                items.push(GeneratedCodeItem::Instruction("ld".to_string(), vec!["a0".to_owned(), self.to_asm_repr(&f.stack_list.get(0).unwrap().0)]));

                items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["sp".to_owned(), "sp".to_owned(), format!("{}", offset)]));
                items.push(GeneratedCodeItem::Instruction("ret".to_string(), vec![]));
            },
            ir::IRItem::Move(s1, s2) => {
                match s2 {
                    ir::ValueStorage::Local(_) => {
                        items.push(GeneratedCodeItem::Instruction("move".to_string(), vec![self.to_asm_repr(s1), self.to_asm_repr(s2)]));
                    },
                    _ => {
                        items.push(GeneratedCodeItem::Instruction("la".to_string(), vec!["s1".to_owned(), format!("{}", s2)]));
                        items.push(GeneratedCodeItem::Instruction("sw".to_string(), vec!["s1".to_string(), self.to_asm_repr(s1)]));
                    }
                }
            },
            ir::IRItem::Print(s) => {
                let f = self.current_function.unwrap();

                let value_type = fetch_value_type(ir_context, f, s);

                items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["a0".to_owned(), "x0".to_owned(), "1".to_owned()]));

                self.load_value_storage(ir_context, items, s, "a1");

                items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["a2".to_owned(), "x0".to_owned(), format!("{}", value_type.size())]));
                items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["a7".to_owned(), "x0".to_owned(), "64".to_owned()]));
                items.push(GeneratedCodeItem::Instruction("ecall".to_owned(), vec![]));
            },
            _ => {},
        }
    }

    pub fn build(&mut self, ir_context: &'ir ir::IRContext) -> Vec<GeneratedCodeItem> {
        let mut items = Vec::new();

        self.initialize_stack(ir_context);

        items.push(GeneratedCodeItem::Section(format!(".global {}", START_LABEL)));

        for ir_item in &ir_context.items {
            self.visit(&mut items, ir_context, ir_item);
        }

        return items;
    }
}