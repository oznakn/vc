use std::collections::{HashMap, VecDeque};
use std::cmp::max;

use crate::ir;
use crate::ir::{fetch_variable_type};
use crate::ast;
use crate::MAIN_FUNCTION;

const START_LABEL: &str = "_start";
const FULL_WIDTH_SIZE: u64 = 8;

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
    is_ro_data_section_started: bool,
    is_data_section_started: bool,
    is_text_section_started: bool,
    current_function: Option<&'ir ir::Function>,
    function_stack_offset_map: HashMap<String, (u64, HashMap<ir::ValueStorage, u64>)>,
    items: Vec<GeneratedCodeItem>,
}

impl<'ir> CodeGenerator<'ir> {
    fn new() -> Self {
        return CodeGenerator {
            is_ro_data_section_started: false,
            is_data_section_started: false,
            is_text_section_started: false,
            current_function: None,
            function_stack_offset_map: HashMap::new(),
            items: Vec::new(),
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

                    offset += FULL_WIDTH_SIZE; // return address

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

    fn check_ro_data_section(&mut self) {
        if !self.is_ro_data_section_started {
            self.items.push(GeneratedCodeItem::Section("\n\n.section .rodata".to_string()));
            self.is_ro_data_section_started = true;
        }
    }

    fn check_data_section(&mut self) {
        if !self.is_data_section_started {
            self.items.push(GeneratedCodeItem::Section("\n\n.section .data".to_string()));
            self.is_data_section_started = true;
        }
    }

    fn check_text_section(&mut self) {
        if !self.is_text_section_started {
            self.items.push(GeneratedCodeItem::Section("\n\n.section .text".to_string()));
            self.is_text_section_started = true;
        }
    }

    #[inline]
    fn get_stack_offset_map(&self) -> &HashMap<ir::ValueStorage, u64> {
        return &self.function_stack_offset_map.get(&self.current_function.unwrap().name).unwrap().1;
    }

    #[inline]
    fn get_jump_address(&self) -> String {
        let offset = &self.function_stack_offset_map.get(&self.current_function.unwrap().name).unwrap().0;

        return format!("{}(sp)", *offset - FULL_WIDTH_SIZE);
    }

    #[inline]
    fn get_return_address(&self) -> String {
        return self.value_storage_to_string(&self.current_function.unwrap().stack_list.get(0).unwrap().0);
    }

    fn value_storage_to_string(&self, s: &'ir ir::ValueStorage) -> String {
        return match s {
            ir::ValueStorage::Const(i) => {
                format!("C{}", i)
            },
            ir::ValueStorage::Var(i) => {
                format!("V{}", i)
            },
            ir::ValueStorage::Local(_) => {
                let stack_offset_map = self.get_stack_offset_map();

                format!("{}(sp)", stack_offset_map.get(s).unwrap())
            },
        }
    }

    fn load_value_storage_address_to_register(&mut self, ir_context: &'ir ir::IRContext, storage: &'ir ir::ValueStorage, register: &str) {
        let variable_type = fetch_variable_type(ir_context, self.current_function.unwrap(), storage);

        let mut instruction_option = None;

        match variable_type {
            ast::VariableType::String(_) => instruction_option = Some("la"),
            _ => {},
        }

        if let Some(instruction) = instruction_option {
            self.items.push(GeneratedCodeItem::Instruction(instruction.to_string(), vec![register.to_string(), self.value_storage_to_string(storage)]));
        }
    }

    fn load_value_storage_to_register(&mut self, ir_context: &'ir ir::IRContext, storage: &'ir ir::ValueStorage, register: &str) {
        let variable_type = fetch_variable_type(ir_context, self.current_function.unwrap(), storage);

        let mut instruction_option = None;

        match variable_type {
            ast::VariableType::Int => instruction_option = Some("lw"),
            ast::VariableType::Real => instruction_option = Some("ld"),
            _ => {},
        }

        if let Some(instruction) = instruction_option {
            self.items.push(GeneratedCodeItem::Instruction(instruction.to_string(), vec![register.to_string(), self.value_storage_to_string(storage)]));
        }
    }

    fn store_register_to_value_storage(&mut self, ir_context: &'ir ir::IRContext, storage: &'ir ir::ValueStorage, register: &str) {
        let variable_type = fetch_variable_type(ir_context, self.current_function.unwrap(), storage);

        let mut instruction_option = None;

        match variable_type {
            ast::VariableType::Int => instruction_option = Some("sw"),
            ast::VariableType::Real => instruction_option = Some("sd"),
            _ => {},
        }

        if let Some(instruction) = instruction_option {
            self.items.push(GeneratedCodeItem::Instruction(instruction.to_string(), vec![register.to_string(), self.value_storage_to_string(storage)]));
        }
    }

    fn visit(&mut self, ir_context: &'ir ir::IRContext, ir_item: &'ir ir::IRItem) {
        match ir_item {
            ir::IRItem::ConstString(storage, s) => {
                self.check_ro_data_section();

                self.items.push(GeneratedCodeItem::Label(format!("{}", self.value_storage_to_string(storage))));
                self.items.push(GeneratedCodeItem::Instruction(".string".to_string(), vec![format!("\"{}\"", s)])); // .ascii .asciz
            },
            ir::IRItem::ConstInt(storage, v) => {
                self.check_ro_data_section();

                self.items.push(GeneratedCodeItem::Label(format!("{}", self.value_storage_to_string(storage))));
                self.items.push(GeneratedCodeItem::Instruction(".word".to_string(), vec![format!("{}", v)]));
            },
            ir::IRItem::ConstReal(storage, v) => {
                self.check_ro_data_section();

                self.items.push(GeneratedCodeItem::Label(format!("{}", self.value_storage_to_string(storage))));
                self.items.push(GeneratedCodeItem::Instruction(".double".to_string(), vec![format!("{:.4}", v)]));
            },
            ir::IRItem::Var(storage, size) => {
                self.check_data_section();

                self.items.push(GeneratedCodeItem::Label(format!("{}", self.value_storage_to_string(storage))));
                self.items.push(GeneratedCodeItem::Instruction(".zero".to_string(), vec![format!("{}", size)]));
            },
            ir::IRItem::Start() => {
                self.check_text_section();

                self.items.push(GeneratedCodeItem::Label(START_LABEL.to_owned()));

                self.items.push(GeneratedCodeItem::Instruction("jal".to_owned(), vec!["ra".to_owned(), MAIN_FUNCTION.to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("lw".to_owned(), vec!["a0".to_owned(), "-4(sp)".to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("addi".to_owned(), vec!["a7".to_owned(), "x0".to_owned(), "93".to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("ecall".to_owned(), vec![]));
            },
            ir::IRItem::Label(label) => {
                self.check_text_section();

                self.items.push(GeneratedCodeItem::Label(label.to_owned()));
            },
            ir::IRItem::Function(label, f) =>  {
                self.check_text_section();

                let (offset, _) = &self.function_stack_offset_map.get(&f.name).unwrap();

                self.current_function = Some(f);

                self.items.push(GeneratedCodeItem::Label(label.to_owned()));
                self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["sp".to_owned(), "sp".to_owned(), format!("-{}", offset)]));
                self.items.push(GeneratedCodeItem::Instruction("sd".to_string(), vec!["ra".to_owned(), self.get_jump_address()]));
            },
            ir::IRItem::Return() => {
                let f = self.current_function.unwrap();

                let offset = self.function_stack_offset_map.get(&f.name).unwrap().0;

                self.items.push(GeneratedCodeItem::Instruction("ld".to_string(), vec!["ra".to_owned(), self.get_jump_address()]));
                self.items.push(GeneratedCodeItem::Instruction("ld".to_string(), vec!["a0".to_owned(), self.get_return_address()]));

                self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["sp".to_owned(), "sp".to_owned(), format!("{}", offset)]));
                self.items.push(GeneratedCodeItem::Instruction("ret".to_string(), vec![]));
            },
            ir::IRItem::Move(s1, s2) => {
                self.load_value_storage_to_register(ir_context, s2, "t0");
                self.store_register_to_value_storage(ir_context, s1, "t0");
            },
            ir::IRItem::Print(s) => {
                let f = self.current_function.unwrap();

                let variable_type = fetch_variable_type(ir_context, f, s);

                match variable_type {
                    ast::VariableType::String(_) => {
                        self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["a0".to_owned(), "x0".to_owned(), "1".to_owned()]));

                        self.load_value_storage_address_to_register(ir_context, s, "a1");

                        self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["a2".to_owned(), "x0".to_owned(), format!("{}", variable_type.size())]));
                        self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["a7".to_owned(), "x0".to_owned(), "64".to_owned()]));
                        self.items.push(GeneratedCodeItem::Instruction("ecall".to_owned(), vec![]));
                    },
                    ast::VariableType::Int => {
                        self.load_value_storage_to_register(ir_context, s, "a1");

                        let formatter_storage = &ir_context.string_map.get("%d").unwrap().0;

                        self.items.push(GeneratedCodeItem::Instruction("lui".to_string(), vec!["a5".to_owned(),
                                                                                               format!("%hi({})", self.value_storage_to_string(formatter_storage))]));

                        self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["a0".to_owned(), "a5".to_owned(),
                                                                                                format!("%lo({})", self.value_storage_to_string(formatter_storage))]));

                        self.items.push(GeneratedCodeItem::Instruction("call".to_owned(), vec!["printf".to_string()]));
                    },
                    _ => {},
                }
            },
            ir::IRItem::Jump(label) => {
                self.items.push(GeneratedCodeItem::Instruction("j".to_string(), vec![format!("{}", label)]));
            },
            ir::IRItem::Bz(label, s) => {
                self.load_value_storage_to_register(ir_context, s, "t0");
                self.items.push(GeneratedCodeItem::Instruction("sext.w".to_string(), vec!["t0".to_owned(), "t0".to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("beqz".to_string(), vec!["t0".to_owned(), format!("{}", label)]));
            },
            ir::IRItem::BinaryOp(storage, op, operand1, operand2) => {
                self.load_value_storage_to_register(ir_context, operand1, "t1");
                self.load_value_storage_to_register(ir_context, operand2, "t2");

                match op {
                    ir::Op::Add => {
                        self.items.push(GeneratedCodeItem::Instruction("add".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                    },
                    ir::Op::Sub => {
                        self.items.push(GeneratedCodeItem::Instruction("sub".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                    },
                    ir::Op::Mul => {
                        self.items.push(GeneratedCodeItem::Instruction("mul".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                    },
                    ir::Op::Div => {
                        self.items.push(GeneratedCodeItem::Instruction("div".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                    },
                    ir::Op::Eq => {
                        self.items.push(GeneratedCodeItem::Instruction("div".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                    },
                    _ => {},
                }

                self.store_register_to_value_storage(ir_context, storage, "t0");
            },
            _ => {},
        }
    }

    pub fn build(ir_context: &'ir ir::IRContext) -> Vec<GeneratedCodeItem> {
        let mut generator = CodeGenerator::new();

        generator.initialize_stack(ir_context);

        generator.items.push(GeneratedCodeItem::Section(format!(".global {}", START_LABEL)));

        for ir_item in &ir_context.items {
            generator.visit(ir_context, ir_item);
        }

        return generator.items;
    }
}