use std::cmp::max;
use std::collections::{HashMap, VecDeque};

use crate::ast;
use crate::ir;
use crate::MAIN_FUNCTION;

type ASMLabel = String;
type MemoryLocation = (usize, IntegerRegister);

enum GeneralRegister {
    X0,
    RA,
    SP,
    GP,
    TP,
}

enum IntegerRegister {
    T0,
    T1,
    T2,

    S0,
    S1,

    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,

    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,

    S10,
    S11,

    T3,
    T4,
    T5,
    T6,
}

enum FloatRegister {
    FT0,
    FT1,
    FT2,
    FT3,
    FT4,
    FT5,
    FT6,

    FS0,
    FS1,

    FA0,
    FA1,
    FA2,
    FA3,
    FA4,
    FA5,
    FA6,
    FA7,

    FS2,
    FS3,
    FS4,
    FS5,
    FS6,
    FS7,
    FS8,
    FS9,
    FS10,
    FS11,

    FT8,
    FT9,
    FT10,
    FT11,
}

enum Instruction {
    Add(IntegerRegister, IntegerRegister, IntegerRegister),
    AddI(IntegerRegister, IntegerRegister, String),
    And(IntegerRegister, IntegerRegister, IntegerRegister),
    Beq(IntegerRegister, IntegerRegister, ASMLabel),
    Bne(IntegerRegister, IntegerRegister, ASMLabel),
    ECall(),
    LoadI(IntegerRegister, MemoryLocation),
    Mul(IntegerRegister, IntegerRegister, IntegerRegister),
    Or(IntegerRegister, IntegerRegister, IntegerRegister),
    StoreI(IntegerRegister, MemoryLocation),
    Sub(IntegerRegister, IntegerRegister, IntegerRegister),
    Ret(),

    FAdd(FloatRegister, FloatRegister, FloatRegister),
    FConvertFFromI(FloatRegister, IntegerRegister), // to, from
    FConvertIFromF(IntegerRegister, FloatRegister), // to, from
    FDiv(FloatRegister, FloatRegister, FloatRegister),
    FEq(IntegerRegister, FloatRegister, FloatRegister),
    FLoad(FloatRegister, MemoryLocation),
    FStore(FloatRegister, MemoryLocation),
}

const START_LABEL: &str = "_start";
const FULL_WIDTH_SIZE: u64 = 8;

const FLOAT_10_LABEL: &str = "__F10";

const PRINT_INTEGER_CODE: &str = "
.print_int:
        mv          t0, a0

        addi        sp, sp, -58
        sd          ra, 50(sp)

        beqz        a0, .print_int__3

        addi        s2, x0, 10
        addi        t1, x0, 0

.print_int__1:
        beqz        t0, .print_int__2

        rem         t2, t0, s2
        add         t3, t1, sp
        sb          t2, 0(t3)
        addi        t1, t1, 1
        div         t0, t0, s2

        j .print_int__1

.print_int__2:
        addi        t1, t1, -1
        add         t3, t1, sp
        lb          t4, 0(t3)
        addi        t4, t4, 48
        sb          t4, 0(t3)
        addi        a0, x0, 1
        mv          a1, t3
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall

        beqz        t1, .print_int__4

        j .print_int__2

.print_int__3:
        addi        t4, x0, 48
        sb          t4, 0(sp)

        addi        a0, x0, 1
        mv          a1, sp
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall

.print_int__4:
        addi        sp, sp, 58
        ret
";

const PRINT_REAL_CODE: &str = "
.print_real:
        fld         fa1, __F10 ,a1
        addi        t1, x0, 6

.print_real__loop:
        fmul.d      fa0, fa0, fa1
        addi        t1,t1,-1
        bnez        t1, .print_real__loop

        fcvt.w.d    a0, fa0
        mv          t0, a0

        addi        sp, sp, -58
        sd          ra, 50(sp)

        beqz        a0, .print_real__3

        addi        s2, x0, 10
        addi        t1, x0, 0

.print_real__1:
        beqz        t0, .print_real__2

        rem         t2, t0, s2
        add         t3, t1, sp
        sb          t2, 0(t3)
        addi        t1, t1, 1
        div         t0, t0, s2

        j .print_real__1

.print_real__2:
        addi        t1, t1, -1
        add         t3, t1, sp

        lb          t4, 0(t3)
        addi        t4, t4, 48
        sb          t4, 0(t3)

        addi        a0, x0, 1
        mv          a1, t3
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall

        addi        t5, x0, 6
        subw        t5, t5, t1
        bnez        t5, .print_real_temp

        addi        t4, x0, 46
        sb          t4, 0(sp)

        addi        a0, x0, 1
        mv          a1, sp
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall

.print_real_temp:
        addi        t5, x0, 2
        subw        t5, t5, t1
        beqz        t5, .print_real__4

        j .print_real__2

.print_real__3:
        addi        t4, x0, 48
        sb          t4, 0(sp)

        addi        a0, x0, 1
        mv          a1, sp
        addi        a2, x0, 1
        addi        a7, x0, 64
        ecall

        addi        t4, x0, 46
        sb          t4, 0(sp)
        mv          a1, sp
        ecall

        addi        t4, x0, 48
        sb          t4, 0(sp)
        mv          a1, sp
        ecall

.print_real__4:
        addi        sp, sp, 58
        ret
";

#[derive(Clone, Debug)]
pub enum GeneratedCodeItem {
    Section(String),
    Label(String),
    Instruction(String, Vec<String>),
    Raw(String),
}

impl GeneratedCodeItem {
    fn to_string(&self, n: usize) -> String {
        return match self {
            GeneratedCodeItem::Section(s) => format!("{}", s),
            GeneratedCodeItem::Label(s) => format!("{}:", s),
            GeneratedCodeItem::Instruction(s, v) => format!("{}{}{}{}", " ".repeat(8), s, " ".repeat(max(0, n - s.len())), v.join(", "),),
            GeneratedCodeItem::Raw(s) => format!("\n{}\n", s),
        };
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
            }
            _ => {}
        }
    }

    max_size += 5;

    for item in items {
        result += &item.to_string(max_size);
        result += "\n";
    }

    result += "\n";

    return result;
}

#[derive(Clone, Debug)]
pub struct CodeGenerator<'input, 'ir> {
    ir_context: &'ir ir::IRContext<'input>,

    is_ro_data_section_started: bool,
    is_data_section_started: bool,
    is_text_section_started: bool,
    current_function: Option<&'ir ir::Function<'input>>,
    function_stack_offset_map: HashMap<String, (u64, HashMap<ir::ValueStorage, u64>)>,
    items: Vec<GeneratedCodeItem>,
}

impl<'input, 'ir> CodeGenerator<'input, 'ir> {
    fn new(ir_context: &'ir ir::IRContext<'input>) -> Self {
        return CodeGenerator {
            ir_context,

            is_ro_data_section_started: false,
            is_data_section_started: false,
            is_text_section_started: false,

            current_function: None,
            function_stack_offset_map: HashMap::new(),
            items: Vec::new(),
        };
    }

    fn fetch_value_type(&self, value_storage: &ir::ValueStorage) -> ast::ValueType {
        match value_storage {
            ir::ValueStorage::Local(_) => self.current_function.unwrap().local_map.get(value_storage).unwrap().to_owned(),
            ir::ValueStorage::Var(_) => self.ir_context.var_map.get(value_storage).unwrap().to_owned(),
            ir::ValueStorage::Const(_) => self.ir_context.const_map.get(value_storage).unwrap().to_owned(),
        }
    }

    fn value_storage_to_size(&self, value_storage: &ir::ValueStorage) -> u64 {
        self.value_type_to_size(&self.fetch_value_type(value_storage))
    }

    fn value_type_to_size(&self, value_type: &ast::ValueType) -> u64 {
        match value_type {
            ast::ValueType::Int => 4,
            ast::ValueType::Real => 8,
            ast::ValueType::String(size) => *size,
            ast::ValueType::Vector(v, size) => self.value_type_to_size(v) * (*size),
        }
    }

    fn initialize_stack(&mut self) {
        let mut current_function = None;
        let mut local_queue = VecDeque::new();

        let mut function_map = HashMap::new();
        let mut stack_offset_map = HashMap::new();

        for item in &self.ir_context.items {
            match item {
                ir::IRItem::Function(label) => {
                    let f: &'ir ir::Function<'input> = self.ir_context.function_map.get(label).unwrap();

                    current_function = Some(f);
                }
                ir::IRItem::Local(value_storage) => {
                    local_queue.push_back(value_storage);
                }
                ir::IRItem::Param(value_storage) => {
                    local_queue.push_back(value_storage);
                }
                ir::IRItem::EndFunction() => {
                    let f = current_function.unwrap();
                    let mut offset: u64 = 0;

                    while let Some(value_storage) = local_queue.pop_back() {
                        stack_offset_map.insert(value_storage.to_owned(), offset);

                        let size = self.value_type_to_size(f.local_map.get(value_storage).unwrap());
                        offset += size;
                    }

                    offset += FULL_WIDTH_SIZE; // return address

                    if let Some(f) = current_function {
                        function_map.insert(f.name.to_owned(), (offset, stack_offset_map));
                    }
                    stack_offset_map = HashMap::new();
                }
                _ => {}
            }
        }

        self.function_stack_offset_map = function_map;
    }

    fn check_ro_data_section(&mut self) {
        if !self.is_ro_data_section_started {
            self.items.push(GeneratedCodeItem::Section("\n\n.section .rodata".to_string()));

            self.items.push(GeneratedCodeItem::Label(FLOAT_10_LABEL.to_owned()));
            self.items.push(GeneratedCodeItem::Instruction(".double".to_string(), vec!["10.0000".to_owned()]));

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
            self.items.push(GeneratedCodeItem::Raw(PRINT_INTEGER_CODE.to_owned()));
            self.items.push(GeneratedCodeItem::Raw(PRINT_REAL_CODE.to_owned()));
            self.is_text_section_started = true;
        }
    }

    #[inline]
    fn get_stack_offset_map(&self) -> &HashMap<ir::ValueStorage, u64> {
        return &self.function_stack_offset_map.get(self.current_function.unwrap().name).unwrap().1;
    }

    #[inline]
    fn get_jump_address(&self) -> String {
        let offset = &self.function_stack_offset_map.get(self.current_function.unwrap().name).unwrap().0;

        return format!("{}(sp)", *offset - FULL_WIDTH_SIZE);
    }

    fn value_storage_to_string_for_label(&self, s: &'ir ir::ValueStorage) -> String {
        return match s {
            ir::ValueStorage::Const(i) => format!("C{}", i),
            ir::ValueStorage::Var(i) => format!("V{}", i),
            _ => format!(""),
        };
    }

    fn value_storage_to_string(&self, s: &'ir ir::ValueStorage, write_data: bool) -> String {
        let value_type = self.fetch_value_type(s);

        return match s {
            ir::ValueStorage::Const(i) => match value_type {
                ast::ValueType::Int => format!("C{}", i),
                ast::ValueType::Real => {
                    if write_data {
                        format!("C{}, a6", i)
                    } else {
                        format!("C{}, a6", i)
                    }
                }
                _ => format!("C{}", i),
            },
            ir::ValueStorage::Var(i) => match value_type {
                ast::ValueType::Int => format!("V{}", i),
                ast::ValueType::Real => {
                    if write_data {
                        format!("V{}, a6", i)
                    } else {
                        format!("V{}, a6", i)
                    }
                }
                _ => format!("V{}", i),
            },
            ir::ValueStorage::Local(_) => {
                let stack_offset_map = self.get_stack_offset_map();

                format!("{}(sp)", stack_offset_map.get(s).unwrap())
            }
        };
    }

    fn load_value_storage_address_to_register(&mut self, storage: &'ir ir::ValueStorage, register: &str) {
        let value_type = self.fetch_value_type(storage);

        let mut instruction_option = None;

        match value_type {
            ast::ValueType::String(_) => instruction_option = Some("la"),
            _ => {}
        }

        if let Some(instruction) = instruction_option {
            self.items.push(GeneratedCodeItem::Instruction(instruction.to_string(), vec![register.to_string(), self.value_storage_to_string(storage, false)]));
        }
    }

    fn load_value_storage_to_register(&mut self, storage: &'ir ir::ValueStorage, register: &str) {
        let value_type = self.fetch_value_type(storage);

        let mut instruction_option = None;

        match value_type {
            ast::ValueType::Int => instruction_option = Some("lw"),
            ast::ValueType::Real => instruction_option = Some("fld"),
            _ => {}
        }

        if let Some(instruction) = instruction_option {
            self.items.push(GeneratedCodeItem::Instruction(instruction.to_string(), vec![register.to_string(), self.value_storage_to_string(storage, false)]));
        }
    }

    fn store_register_to_value_storage(&mut self, storage: &'ir ir::ValueStorage, register: &str) {
        let value_type = self.fetch_value_type(storage);

        let mut instruction_option = None;

        match value_type {
            ast::ValueType::Int => instruction_option = Some("sw"),
            ast::ValueType::Real => instruction_option = Some("fsd"),
            _ => {}
        }

        if let Some(instruction) = instruction_option {
            self.items.push(GeneratedCodeItem::Instruction(instruction.to_string(), vec![register.to_string(), self.value_storage_to_string(storage, true)]));
        }
    }

    fn copy_to_call_function(&mut self, call_function: &'ir ir::Function, from: &'ir ir::ValueStorage, to: &'ir ir::ValueStorage) {
        // PART 1
        let value_type = self.fetch_value_type(from);

        match value_type {
            ast::ValueType::Int => self.items.push(GeneratedCodeItem::Instruction("lw".to_string(), vec!["a0".to_string(), self.value_storage_to_string(from, false)])),

            ast::ValueType::Real => self.items.push(GeneratedCodeItem::Instruction("fld".to_string(), vec!["fa0".to_string(), self.value_storage_to_string(from, false)])),
            _ => {}
        }

        let (offset, stack_offset_map) = self.function_stack_offset_map.get(call_function.name).unwrap();
        let variable_offset = -(*offset as i64) + (*stack_offset_map.get(to).unwrap() as i64);

        match value_type {
            ast::ValueType::Int => self.items.push(GeneratedCodeItem::Instruction("sw".to_string(), vec!["a0".to_string(), format!("{}(sp)", variable_offset)])),

            ast::ValueType::Real => self.items.push(GeneratedCodeItem::Instruction("fsd".to_string(), vec!["fa0".to_string(), format!("{}(sp)", variable_offset)])),
            _ => {}
        }
    }

    fn visit(&mut self, ir_item: &'ir ir::IRItem<'input>) {
        match ir_item {
            ir::IRItem::Const(value_storage, const_value) => {
                self.check_ro_data_section();

                match const_value {
                    ir::ConstValue::Bool(c) => {
                        self.items.push(GeneratedCodeItem::Label(self.value_storage_to_string_for_label(value_storage)));

                        if *c {
                            self.items.push(GeneratedCodeItem::Instruction(".byte".to_string(), vec!["1".to_owned()]));
                        } else {
                            self.items.push(GeneratedCodeItem::Instruction(".byte".to_string(), vec!["0".to_owned()]));
                        }
                    }
                    ir::ConstValue::String(s) => {
                        self.items.push(GeneratedCodeItem::Label(self.value_storage_to_string_for_label(value_storage)));
                        self.items.push(GeneratedCodeItem::Instruction(".string".to_string(), vec![format!("\"{}\"", s)]));
                        // .ascii .asciz
                    }
                    ir::ConstValue::Int(v) => {
                        self.items.push(GeneratedCodeItem::Label(self.value_storage_to_string_for_label(value_storage)));
                        self.items.push(GeneratedCodeItem::Instruction(".word".to_string(), vec![format!("{}", v)]));
                    }
                    ir::ConstValue::Real(v) => {
                        self.items.push(GeneratedCodeItem::Label(self.value_storage_to_string_for_label(value_storage)));
                        self.items.push(GeneratedCodeItem::Instruction(".double".to_string(), vec![format!("{:.4}", v)]));
                    }
                }
            }
            ir::IRItem::Var(value_storage) => {
                self.check_data_section();

                self.items.push(GeneratedCodeItem::Label(self.value_storage_to_string_for_label(value_storage)));
                self.items.push(GeneratedCodeItem::Instruction(".zero".to_string(), vec![format!("{}", self.value_storage_to_size(value_storage))]));
            }
            ir::IRItem::Start() => {
                self.check_text_section();

                self.items.push(GeneratedCodeItem::Label(START_LABEL.to_owned()));

                self.items.push(GeneratedCodeItem::Instruction("jal".to_owned(), vec!["ra".to_owned(), MAIN_FUNCTION.to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("lw".to_owned(), vec!["a0".to_owned(), "-4(sp)".to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("addi".to_owned(), vec!["a7".to_owned(), "x0".to_owned(), "93".to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("ecall".to_owned(), vec![]));
            }
            ir::IRItem::Label(label) => {
                self.check_text_section();

                self.items.push(GeneratedCodeItem::Label(label.to_owned()));
            }
            ir::IRItem::Function(label) => {
                self.check_text_section();

                let function = self.ir_context.function_map.get(label).unwrap();

                let (offset, _) = &self.function_stack_offset_map.get(function.name).unwrap();

                self.current_function = Some(function);

                self.items.push(GeneratedCodeItem::Label(label.to_string()));
                self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["sp".to_owned(), "sp".to_owned(), format!("-{}", offset)]));
                self.items.push(GeneratedCodeItem::Instruction("sd".to_string(), vec!["ra".to_owned(), self.get_jump_address()]));
            }
            ir::IRItem::Return(s) => {
                let f = self.current_function.unwrap();

                let offset = self.function_stack_offset_map.get(f.name).unwrap().0;

                let value_type = self.fetch_value_type(s);

                match value_type {
                    ast::ValueType::Int => {
                        self.load_value_storage_to_register(s, "a0");
                    }
                    ast::ValueType::Real => {
                        self.load_value_storage_to_register(s, "fa0");
                    }
                    _ => {}
                }

                self.items.push(GeneratedCodeItem::Instruction("ld".to_string(), vec!["ra".to_owned(), self.get_jump_address()]));
                self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["sp".to_owned(), "sp".to_owned(), format!("{}", offset)]));
                self.items.push(GeneratedCodeItem::Instruction("ret".to_string(), vec![]));
            }
            ir::IRItem::Move(s1, s2) => {
                let value_type = self.fetch_value_type(s2);

                match value_type {
                    ast::ValueType::Int => {
                        self.load_value_storage_to_register(s2, "t0");
                        self.store_register_to_value_storage(s1, "t0");
                    }
                    ast::ValueType::Real => {
                        self.load_value_storage_to_register(s2, "ft0");
                        self.store_register_to_value_storage(s1, "ft0");
                    }
                    _ => {}
                }
            }
            ir::IRItem::Print(s) => {
                let value_type = self.fetch_value_type(s);

                match value_type {
                    ast::ValueType::String(_) => {
                        self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["a0".to_owned(), "x0".to_owned(), "1".to_owned()]));

                        self.load_value_storage_address_to_register(s, "a1");

                        self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["a2".to_owned(), "x0".to_owned(), format!("{}", self.value_type_to_size(&value_type))]));
                        self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["a7".to_owned(), "x0".to_owned(), "64".to_owned()]));
                        self.items.push(GeneratedCodeItem::Instruction("ecall".to_owned(), vec![]));
                    }
                    ast::ValueType::Int => {
                        self.load_value_storage_to_register(s, "a0");

                        self.items.push(GeneratedCodeItem::Instruction("call".to_owned(), vec![".print_int".to_owned()]));
                    }
                    ast::ValueType::Real => {
                        self.load_value_storage_to_register(s, "fa0");

                        self.items.push(GeneratedCodeItem::Instruction("call".to_owned(), vec![".print_real".to_owned()]));
                    }
                    _ => {}
                }
            }
            ir::IRItem::Jump(label) => {
                self.items.push(GeneratedCodeItem::Instruction("j".to_string(), vec![format!("{}", label)]));
            }
            ir::IRItem::Bz(label, s) => {
                let value_type = self.fetch_value_type(s);

                match value_type {
                    ast::ValueType::Int => {
                        self.load_value_storage_to_register(s, "t0");
                        self.items.push(GeneratedCodeItem::Instruction("beqz".to_string(), vec!["t0".to_owned(), format!("{}", label)]));
                    }
                    ast::ValueType::Real => {
                        self.load_value_storage_to_register(s, "ft0");
                        self.items.push(GeneratedCodeItem::Instruction("fcvt.w.d".to_string(), vec!["t0".to_owned(), "ft0".to_owned()]));
                        self.items.push(GeneratedCodeItem::Instruction("beqz".to_string(), vec!["t0".to_owned(), format!("{}", label)]));
                    }
                    _ => {}
                }
            }
            ir::IRItem::Cast(to, from) => {
                self.load_value_storage_to_register(from, "t0");

                self.items.push(GeneratedCodeItem::Instruction("fcvt.d.w".to_string(), vec!["ft0".to_owned(), "t0".to_owned()]));

                self.store_register_to_value_storage(to, "ft0");
            }
            ir::IRItem::BinaryOp(storage, op, operand1, operand2) => {
                let value_type1 = self.fetch_value_type(operand1);
                let value_type2 = self.fetch_value_type(operand2);

                if value_type1 == ast::ValueType::Int && value_type2 == ast::ValueType::Int {
                    self.load_value_storage_to_register(operand1, "t1");
                    self.load_value_storage_to_register(operand2, "t2");

                    match op {
                        ir::Op::Add => {
                            self.items.push(GeneratedCodeItem::Instruction("addw".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                        }
                        ir::Op::Sub => {
                            self.items.push(GeneratedCodeItem::Instruction("subw".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                        }
                        ir::Op::Mul => {
                            self.items.push(GeneratedCodeItem::Instruction("mulw".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                        }
                        ir::Op::Eq => {
                            self.items.push(GeneratedCodeItem::Instruction("subw".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                            self.items.push(GeneratedCodeItem::Instruction("seqz".to_string(), vec!["t0".to_owned(), "t0".to_owned()]));
                        }
                        ir::Op::NotEq => {
                            self.items.push(GeneratedCodeItem::Instruction("subw".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                            self.items.push(GeneratedCodeItem::Instruction("snez".to_string(), vec!["t0".to_owned(), "t0".to_owned()]));
                        }
                        ir::Op::Greater => {
                            self.items.push(GeneratedCodeItem::Instruction("sgt".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                        }
                        ir::Op::GreaterEq => {
                            self.items.push(GeneratedCodeItem::Instruction("slt".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                            self.items.push(GeneratedCodeItem::Instruction("seqz".to_string(), vec!["t0".to_owned(), "t0".to_owned()]));
                        }
                        ir::Op::Less => {
                            self.items.push(GeneratedCodeItem::Instruction("slt".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                        }
                        ir::Op::LessEq => {
                            self.items.push(GeneratedCodeItem::Instruction("sgt".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                            self.items.push(GeneratedCodeItem::Instruction("seqz".to_string(), vec!["t0".to_owned(), "t0".to_owned()]));
                        }
                        ir::Op::Mod => {
                            self.items.push(GeneratedCodeItem::Instruction("rem".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                        }
                        ir::Op::IntDiv => {
                            self.items.push(GeneratedCodeItem::Instruction("divw".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                        }
                        ir::Op::And => {
                            self.items.push(GeneratedCodeItem::Instruction("and".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                        }
                        ir::Op::Or => {
                            self.items.push(GeneratedCodeItem::Instruction("or".to_string(), vec!["t0".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                        }
                        _ => {
                            self.items.push(GeneratedCodeItem::Instruction("mv".to_string(), vec!["t0".to_owned(), "x0".to_owned()]));
                        }
                    }

                    self.store_register_to_value_storage(storage, "t0");
                } else {
                    let operand1_register = "fa1";
                    let operand2_register = "fa2";

                    self.load_value_storage_to_register(operand1, operand1_register);
                    self.load_value_storage_to_register(operand2, operand2_register);

                    let mut result_register = "t0";

                    match op {
                        ir::Op::Add => {
                            self.items.push(GeneratedCodeItem::Instruction("fadd.d".to_string(), vec!["ft0".to_owned(), operand1_register.to_owned(), operand2_register.to_owned()]));
                            result_register = "ft0";
                        }
                        ir::Op::Sub => {
                            self.items.push(GeneratedCodeItem::Instruction("fsub.d".to_string(), vec!["ft0".to_owned(), operand1_register.to_owned(), operand2_register.to_owned()]));
                            result_register = "ft0";
                        }
                        ir::Op::Mul => {
                            self.items.push(GeneratedCodeItem::Instruction("fmul.d".to_string(), vec!["ft0".to_owned(), operand1_register.to_owned(), operand2_register.to_owned()]));
                            result_register = "ft0";
                        }
                        ir::Op::Div => {
                            self.items.push(GeneratedCodeItem::Instruction("fdiv.d".to_string(), vec!["ft0".to_owned(), operand1_register.to_owned(), operand2_register.to_owned()]));
                            result_register = "ft0";
                        }
                        ir::Op::Eq => {
                            self.items.push(GeneratedCodeItem::Instruction("fsub.d".to_string(), vec!["ft0".to_owned(), operand1_register.to_owned(), operand2_register.to_owned()]));
                            self.items.push(GeneratedCodeItem::Instruction("fcvt.w.d".to_string(), vec!["t0".to_owned(), "ft0".to_owned()]));
                            self.items.push(GeneratedCodeItem::Instruction("seqz".to_string(), vec!["t0".to_owned(), "ft0".to_owned()]));
                        }
                        ir::Op::NotEq => {
                            self.items.push(GeneratedCodeItem::Instruction("fsub.d".to_string(), vec!["ft0".to_owned(), operand1_register.to_owned(), operand2_register.to_owned()]));
                            self.items.push(GeneratedCodeItem::Instruction("fcvt.w.d".to_string(), vec!["t0".to_owned(), "ft0".to_owned()]));
                            self.items.push(GeneratedCodeItem::Instruction("snez".to_string(), vec!["t0".to_owned(), "t0".to_owned()]));
                        }
                        ir::Op::Greater => {
                            self.items.push(GeneratedCodeItem::Instruction("fgt.d".to_string(), vec!["t0".to_owned(), operand1_register.to_owned(), operand2_register.to_owned()]));
                        }
                        ir::Op::GreaterEq => {
                            self.items.push(GeneratedCodeItem::Instruction("flt.d".to_string(), vec!["t0".to_owned(), operand1_register.to_owned(), operand2_register.to_owned()]));
                            self.items.push(GeneratedCodeItem::Instruction("seqz".to_string(), vec!["t0".to_owned(), "t0".to_owned()]));
                        }
                        ir::Op::Less => {
                            self.items.push(GeneratedCodeItem::Instruction("slt".to_string(), vec!["t0".to_owned(), operand1_register.to_owned(), operand2_register.to_owned()]));
                        }
                        ir::Op::LessEq => {
                            self.items.push(GeneratedCodeItem::Instruction("fgt.d".to_string(), vec!["t0".to_owned(), operand1_register.to_owned(), operand2_register.to_owned()]));
                            self.items.push(GeneratedCodeItem::Instruction("seqz".to_string(), vec!["t0".to_owned(), "t0".to_owned()]));
                        }
                        _ => {
                            self.items.push(GeneratedCodeItem::Instruction("mv".to_string(), vec!["t0".to_owned(), "x0".to_owned()]));
                        }
                    }

                    self.store_register_to_value_storage(storage, result_register);
                }
            }
            ir::IRItem::UnaryOp(storage, op, operand) => match op {
                ir::Op::Negative => {
                    let value_type = self.fetch_value_type(operand);

                    match value_type {
                        ast::ValueType::Int => {
                            self.load_value_storage_to_register(operand, "t1");

                            self.items.push(GeneratedCodeItem::Instruction("sub".to_string(), vec!["t1".to_owned(), "x0".to_owned(), "t1".to_owned()]));

                            self.store_register_to_value_storage(storage, "t0");
                        }
                        ast::ValueType::Real => {
                            self.items.push(GeneratedCodeItem::Instruction("fcvt.d.w".to_string(), vec!["t1".to_owned(), "ft0".to_owned(), "x0".to_owned()]));

                            self.items.push(GeneratedCodeItem::Instruction("fsub.d".to_string(), vec!["tf0".to_owned(), "ft1".to_owned()]));

                            self.store_register_to_value_storage(storage, "ft0");
                        }
                        _ => {}
                    }
                }
                ir::Op::Not => {
                    self.load_value_storage_to_register(operand, "t1");

                    self.items.push(GeneratedCodeItem::Instruction("seqz".to_string(), vec!["t0".to_owned(), "t1".to_owned()]));

                    self.store_register_to_value_storage(storage, "t0");
                }
                _ => {}
            },
            ir::IRItem::Call(label, s, items) => {
                let call_function = self.ir_context.function_map.get(label).unwrap();

                let mut i = 0;
                for (parameter, _) in &call_function.local_map {
                    self.copy_to_call_function(call_function, items.get(i).unwrap(), parameter);

                    i += 1;
                }

                self.items.push(GeneratedCodeItem::Instruction("call".to_string(), vec![label.to_string()]));

                match call_function.return_type {
                    ast::ValueType::Int => {
                        self.store_register_to_value_storage(s, "a0");
                    }
                    ast::ValueType::Real => {
                        self.store_register_to_value_storage(s, "fa0");
                    }
                    _ => {}
                }
            }
            ir::IRItem::Store(pointer, storage) => {
                let value_type = self.fetch_value_type(&pointer.0);

                let stack_offset_map = self.get_stack_offset_map();

                let offset = stack_offset_map.get(&pointer.0).unwrap().clone();

                self.load_value_storage_to_register(storage, "t3");

                self.load_value_storage_to_register(&pointer.1, "t1");
                self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["t2".to_owned(), "x0".to_owned(), "1".to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("subw".to_string(), vec!["t1".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["t0".to_owned(), "sp".to_owned(), format!("{}", offset)]));
                self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["t2".to_owned(), "x0".to_owned(), format!("{}", self.value_type_to_size(&value_type.plain()))]));
                self.items.push(GeneratedCodeItem::Instruction("mulw".to_string(), vec!["t1".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("addw".to_string(), vec!["t0".to_owned(), "t0".to_owned(), "t1".to_owned()]));

                match value_type.plain() {
                    ast::ValueType::Int => {
                        self.items.push(GeneratedCodeItem::Instruction("sw".to_string(), vec!["t3".to_owned(), "0(t0)".to_owned()]));
                    }
                    ast::ValueType::Real => {
                        self.items.push(GeneratedCodeItem::Instruction("fsd".to_string(), vec!["t3".to_owned(), "0(t0)".to_owned()]));
                    }
                    _ => {}
                }
            }
            ir::IRItem::Fetch(storage, pointer) => {
                let value_type = self.fetch_value_type(&pointer.0);

                let stack_offset_map = self.get_stack_offset_map();

                let offset = stack_offset_map.get(&pointer.0).unwrap().clone();

                self.load_value_storage_to_register(&pointer.1, "t1");
                self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["t2".to_owned(), "x0".to_owned(), "1".to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("subw".to_string(), vec!["t1".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["t0".to_owned(), "sp".to_owned(), format!("{}", offset)]));
                self.items.push(GeneratedCodeItem::Instruction("addi".to_string(), vec!["t2".to_owned(), "x0".to_owned(), format!("{}", self.value_type_to_size(&value_type.plain()))]));
                self.items.push(GeneratedCodeItem::Instruction("mulw".to_string(), vec!["t1".to_owned(), "t1".to_owned(), "t2".to_owned()]));
                self.items.push(GeneratedCodeItem::Instruction("addw".to_string(), vec!["t0".to_owned(), "t0".to_owned(), "t1".to_owned()]));

                match value_type.plain() {
                    ast::ValueType::Int => {
                        self.items.push(GeneratedCodeItem::Instruction("lw".to_string(), vec!["t3".to_owned(), "0(t0)".to_owned()]));
                        self.store_register_to_value_storage(storage, "t3");
                    }
                    ast::ValueType::Real => {
                        self.items.push(GeneratedCodeItem::Instruction("fld".to_string(), vec!["ft3".to_owned(), "0(t0)".to_owned()]));
                        self.store_register_to_value_storage(storage, "ft3");
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    pub fn build(ir_context: &'ir ir::IRContext<'input>) -> Vec<GeneratedCodeItem> {
        let mut generator = CodeGenerator::new(ir_context);

        generator.initialize_stack();

        generator.items.push(GeneratedCodeItem::Section(format!(".global {}", START_LABEL)));

        for ir_item in &ir_context.items {
            generator.visit(ir_item);
        }

        return generator.items;
    }
}
