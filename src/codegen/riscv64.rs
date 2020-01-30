use std::collections::{HashMap, VecDeque};
use std::fmt;

use crate::ast;
use crate::ir;
use crate::MAIN_FUNCTION;

#[derive(Clone, Debug)]
pub enum DataLocation {
    Memory(i64),
    MemoryWithRegister(i64, Register),
    Program(String),
}

impl fmt::Display for DataLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataLocation::Memory(offset) => write!(f, "{}(sp)", offset),
            DataLocation::MemoryWithRegister(offset, register) => write!(f, "{}({})", offset, register),
            DataLocation::Program(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Register {
    X0,
    RA,
    SP,
    // GP,
    // TP,
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

    FT7,
    FT8,
    FT9,
    FT10,
    FT11,
}

impl Register {
    fn is_integer_register(&self) -> bool {
        match self {
            Register::T0 | Register::T1 | Register::T2 | Register::T3 | Register::T4 | Register::T5 | Register::T6 => true,
            _ => false,
        }
    }

    fn is_float_register(&self) -> bool {
        match self {
            Register::FT0 | Register::FT1 | Register::FT2 | Register::FT3 | Register::FT4 | Register::FT5 | Register::FT6 | Register::FT7 | Register::FT8 | Register::FT9 | Register::FT10 | Register::FT11 => true,
            _ => false,
        }
    }

    fn is_full_width_register(&self) -> bool {
        match self {
            Register::RA | Register::SP => true,
            _ => self.is_float_register(),
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Register::X0 => write!(f, "x0"),
            Register::RA => write!(f, "ra"),
            Register::SP => write!(f, "sp"),

            Register::T0 => write!(f, "t0"),
            Register::T1 => write!(f, "t1"),
            Register::T2 => write!(f, "t2"),

            Register::S0 => write!(f, "s0"),
            Register::S1 => write!(f, "s1"),

            Register::A0 => write!(f, "a0"),
            Register::A1 => write!(f, "a1"),
            Register::A2 => write!(f, "a2"),
            Register::A3 => write!(f, "a3"),
            Register::A4 => write!(f, "a4"),
            Register::A5 => write!(f, "a5"),
            Register::A6 => write!(f, "a6"),
            Register::A7 => write!(f, "a7"),

            Register::S2 => write!(f, "s2"),
            Register::S3 => write!(f, "s3"),
            Register::S4 => write!(f, "s4"),
            Register::S5 => write!(f, "s5"),
            Register::S6 => write!(f, "s6"),
            Register::S7 => write!(f, "s7"),
            Register::S8 => write!(f, "s8"),
            Register::S9 => write!(f, "s9"),

            Register::S10 => write!(f, "s10"),
            Register::S11 => write!(f, "s11"),

            Register::T3 => write!(f, "t3"),
            Register::T4 => write!(f, "t4"),
            Register::T5 => write!(f, "t5"),
            Register::T6 => write!(f, "t6"),

            Register::FT0 => write!(f, "ft0"),
            Register::FT1 => write!(f, "ft1"),
            Register::FT2 => write!(f, "ft2"),
            Register::FT3 => write!(f, "ft3"),
            Register::FT4 => write!(f, "ft4"),
            Register::FT5 => write!(f, "ft5"),
            Register::FT6 => write!(f, "ft6"),

            Register::FS0 => write!(f, "fs0"),
            Register::FS1 => write!(f, "fs1"),

            Register::FA0 => write!(f, "fa0"),
            Register::FA1 => write!(f, "fa1"),
            Register::FA2 => write!(f, "fa2"),
            Register::FA3 => write!(f, "fa3"),
            Register::FA4 => write!(f, "fa4"),
            Register::FA5 => write!(f, "fa5"),
            Register::FA6 => write!(f, "fa6"),
            Register::FA7 => write!(f, "fa7"),

            Register::FS2 => write!(f, "s2"),
            Register::FS3 => write!(f, "s3"),
            Register::FS4 => write!(f, "s4"),
            Register::FS5 => write!(f, "s5"),
            Register::FS6 => write!(f, "s6"),
            Register::FS7 => write!(f, "s7"),
            Register::FS8 => write!(f, "s8"),
            Register::FS9 => write!(f, "s9"),
            Register::FS10 => write!(f, "s10"),
            Register::FS11 => write!(f, "s11"),

            Register::FT7 => write!(f, "ft7"),
            Register::FT8 => write!(f, "ft8"),
            Register::FT9 => write!(f, "ft9"),
            Register::FT10 => write!(f, "ft10"),
            Register::FT11 => write!(f, "ft11"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Jal(String),
    Jump(String),
    Call(String),
    Mv(Register, Register),
    ECall(),
    Ret(),

    Add(Register, Register, Register),
    AddI(Register, Register, String),
    And(Register, Register, Register),
    Beqz(Register, String),
    Div(Register, Register, Register),
    Load(Register, DataLocation),
    LoadD(Register, DataLocation),
    LoadA(Register, DataLocation),
    Mul(Register, Register, Register),
    Or(Register, Register, Register),
    Store(Register, DataLocation),
    StoreD(Register, DataLocation),
    Sub(Register, Register, Register),
    Seqz(Register, Register),
    Snez(Register, Register),
    Sgt(Register, Register, Register),
    Slt(Register, Register, Register),
    Rem(Register, Register, Register),

    ConvertFFromI(Register, Register),
    ConvertFIromF(Register, Register),

    FAdd(Register, Register, Register),
    FDiv(Register, Register, Register),
    FLoad(Register, DataLocation),
    FMul(Register, Register, Register),
    FStore(Register, DataLocation),
    FSub(Register, Register, Register),
    FSgt(Register, Register, Register),
    FSlt(Register, Register, Register),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Jal(s) => write!(f, "jal {}", s),
            Instruction::Jump(s) => write!(f, "j {}", s),
            Instruction::Call(s) => write!(f, "call {}", s),
            Instruction::Mv(x, y) => write!(f, "mv {}, {}", x, y),
            Instruction::ECall() => write!(f, "ecall"),
            Instruction::Ret() => write!(f, "ret"),

            Instruction::Add(x, y, z) => write!(f, "add {}, {}, {}", x, y, z),
            Instruction::AddI(x, y, z) => write!(f, "addi {}, {}, {}", x, y, z),
            Instruction::And(x, y, z) => write!(f, "and {}, {}, {}", x, y, z),
            Instruction::Beqz(x, y) => write!(f, "beqz {}, {}", x, y),
            Instruction::Div(x, y, z) => write!(f, "div {}, {}, {}", x, y, z),
            Instruction::Load(x, y) => write!(f, "lw {}, {}", x, y),
            Instruction::LoadD(x, y) => write!(f, "ld {}, {}", x, y),
            Instruction::LoadA(x, y) => write!(f, "la {}, {}", x, y),
            Instruction::Mul(x, y, z) => write!(f, "mul {}, {}, {}", x, y, z),
            Instruction::Or(x, y, z) => write!(f, "or {}, {}, {}", x, y, z),
            Instruction::Store(x, y) => write!(f, "sw {}, {}", x, y),
            Instruction::StoreD(x, y) => write!(f, "sd {}, {}", x, y),
            Instruction::Sub(x, y, z) => write!(f, "sub {}, {}, {}", x, y, z),
            Instruction::Seqz(x, y) => write!(f, "seqz {}, {}", x, y),
            Instruction::Snez(x, y) => write!(f, "snez {}, {}", x, y),
            Instruction::Sgt(x, y, z) => write!(f, "sgt {}, {}, {}", x, y, z),
            Instruction::Slt(x, y, z) => write!(f, "slt {}, {}, {}", x, y, z),
            Instruction::Rem(x, y, z) => write!(f, "rem {}, {}, {}", x, y, z),

            Instruction::ConvertFFromI(x, y) => write!(f, "fcvt.d.w {}, {}", x, y),
            Instruction::ConvertFIromF(x, y) => write!(f, "fcvt.w.d {}, {}", x, y),

            Instruction::FAdd(x, y, z) => write!(f, "fadd.d {}, {}, {}", x, y, z),
            Instruction::FDiv(x, y, z) => write!(f, "fdiv.d {}, {}, {}", x, y, z),
            Instruction::FLoad(x, y) => write!(f, "fld {}, {}", x, y),
            Instruction::FMul(x, y, z) => write!(f, "fmul.d {}, {}, {}", x, y, z),
            Instruction::FStore(x, y) => write!(f, "fsd {}, {}", x, y),
            Instruction::FSub(x, y, z) => write!(f, "fsub.d {}, {}, {}", x, y, z),
            Instruction::FSgt(x, y, z) => write!(f, "fgt.d {}, {}, {}", x, y, z),
            Instruction::FSlt(x, y, z) => write!(f, "flt.d {}, {}, {}", x, y, z),
        }
    }
}

#[derive(Clone, Debug)]
pub enum DataSectionItem {
    String(DataLocation, String),
    Word(DataLocation, String),
    Double(DataLocation, String),
    Byte(DataLocation, String),
    Zero(DataLocation, String),
}

impl fmt::Display for DataSectionItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataSectionItem::String(data_location, s) => write!(f, "{}: .string {}", data_location, s),
            DataSectionItem::Word(data_location, s) => write!(f, "{}: .word {}", data_location, s),
            DataSectionItem::Double(data_location, s) => write!(f, "{}: .double {}", data_location, s),
            DataSectionItem::Byte(data_location, s) => write!(f, "{}: .byte {}", data_location, s),
            DataSectionItem::Zero(data_location, s) => write!(f, "{}: .zero {}", data_location, s),
        }
    }
}

#[derive(Clone, Debug)]
pub enum GeneratedCodeItem {
    Global(String),
    Section(String),
    Label(String),
    DataSectionItem(DataSectionItem),
    Instruction(Instruction),
    Raw(String),
}

impl fmt::Display for GeneratedCodeItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GeneratedCodeItem::Global(s) => write!(f, ".global {}", s),
            GeneratedCodeItem::Section(s) => write!(f, ".section {}", s),
            GeneratedCodeItem::Label(s) => write!(f, "{}:", s),
            GeneratedCodeItem::DataSectionItem(i) => write!(f, "{}{}", " ".repeat(4), i),
            GeneratedCodeItem::Instruction(i) => write!(f, "{}{}", " ".repeat(4), i),
            GeneratedCodeItem::Raw(s) => write!(f, "{}", s),
        }
    }
}

impl From<Instruction> for GeneratedCodeItem {
    fn from(instruction: Instruction) -> Self {
        GeneratedCodeItem::Instruction(instruction)
    }
}

impl From<DataSectionItem> for GeneratedCodeItem {
    fn from(data_section_item: DataSectionItem) -> Self {
        GeneratedCodeItem::DataSectionItem(data_section_item)
    }
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
pub struct CodeGenerator<'input, 'ir> {
    ir_context: &'ir ir::IRContext<'input>,

    is_ro_data_section_started: bool,
    is_data_section_started: bool,
    is_text_section_started: bool,

    current_function: Option<&'ir ir::Function<'input>>,
    function_stack_offset_map: HashMap<String, (u64, HashMap<ir::ValueStorage, u64>)>,

    available_temporary_integer_registers: VecDeque<Register>,
    available_temporary_float_registers: VecDeque<Register>,

    items: Vec<GeneratedCodeItem>,
}

impl<'input, 'ir> CodeGenerator<'input, 'ir> {
    fn new(ir_context: &'ir ir::IRContext<'input>) -> Self {
        let mut generator = CodeGenerator {
            ir_context,

            is_ro_data_section_started: false,
            is_data_section_started: false,
            is_text_section_started: false,

            current_function: None,
            function_stack_offset_map: HashMap::new(),

            available_temporary_integer_registers: VecDeque::new(),
            available_temporary_float_registers: VecDeque::new(),

            items: Vec::new(),
        };

        // T0, T1 and FT0, FT1 reserved for i to f and f to i convert operations

        generator.available_temporary_integer_registers.push_front(Register::T2);
        generator.available_temporary_integer_registers.push_front(Register::T3);
        generator.available_temporary_integer_registers.push_front(Register::T4);
        generator.available_temporary_integer_registers.push_front(Register::T5);
        generator.available_temporary_integer_registers.push_front(Register::T6);

        generator.available_temporary_float_registers.push_front(Register::FT2);
        generator.available_temporary_float_registers.push_front(Register::FT3);
        generator.available_temporary_float_registers.push_front(Register::FT4);
        generator.available_temporary_float_registers.push_front(Register::FT5);
        generator.available_temporary_float_registers.push_front(Register::FT6);
        generator.available_temporary_float_registers.push_front(Register::FT7);
        generator.available_temporary_float_registers.push_front(Register::FT8);
        generator.available_temporary_float_registers.push_front(Register::FT9);
        generator.available_temporary_float_registers.push_front(Register::FT10);
        generator.available_temporary_float_registers.push_front(Register::FT11);

        generator
    }

    fn recycle_register(&mut self, register: Register) {
        match register {
            Register::T0 | Register::T1 | Register::T2 | Register::T3 | Register::T4 | Register::T5 | Register::T6 => self.available_temporary_integer_registers.push_back(register),

            Register::FT0 | Register::FT1 | Register::FT2 | Register::FT3 | Register::FT4 | Register::FT5 | Register::FT6 | Register::FT7 | Register::FT8 | Register::FT9 | Register::FT10 | Register::FT11 => self.available_temporary_float_registers.push_back(register),
            _ => unreachable!(),
        }
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
        let mut local_stack = VecDeque::new();

        let mut function_map = HashMap::new();
        let mut stack_offset_map = HashMap::new();

        for item in &self.ir_context.items {
            match item {
                ir::IRItem::Function(label) => {
                    let f: &'ir ir::Function<'input> = self.ir_context.function_map.get(label).unwrap();

                    current_function = Some(f);
                }
                ir::IRItem::Local(value_storage) => {
                    local_stack.push_back(value_storage);
                }
                ir::IRItem::Param(value_storage) => {
                    local_stack.push_back(value_storage);
                }
                ir::IRItem::EndFunction() => {
                    let f = current_function.unwrap();
                    let mut offset: u64 = 0;

                    while let Some(value_storage) = local_stack.pop_back() {
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
            self.items.push(GeneratedCodeItem::Section("rodata".to_string()));

            self.items.push(GeneratedCodeItem::DataSectionItem(DataSectionItem::Double(DataLocation::Program(FLOAT_10_LABEL.to_owned()), "10.0000".to_owned())));

            self.is_ro_data_section_started = true;
        }
    }

    fn check_data_section(&mut self) {
        if !self.is_data_section_started {
            self.items.push(GeneratedCodeItem::Section(".data".to_string()));
            self.is_data_section_started = true;
        }
    }

    fn check_text_section(&mut self) {
        if !self.is_text_section_started {
            self.items.push(GeneratedCodeItem::Section(".text".to_string()));
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
    fn get_jump_address(&self) -> DataLocation {
        let offset = &self.function_stack_offset_map.get(self.current_function.unwrap().name).unwrap().0;

        DataLocation::Memory(*offset as i64 - FULL_WIDTH_SIZE as i64)
    }

    fn value_storage_to_location(&self, s: &'ir ir::ValueStorage) -> DataLocation {
        match s {
            ir::ValueStorage::Const(i) => DataLocation::Program(format!("C{}", i)),
            ir::ValueStorage::Var(i) => DataLocation::Program(format!("V{}", i)),
            ir::ValueStorage::Local(_) => {
                let stack_offset_map = self.get_stack_offset_map();

                DataLocation::Memory(stack_offset_map.get(s).unwrap().to_owned() as i64)
            }
        }
    }

    #[inline]
    fn load_value_storage_address_to_register(&mut self, storage: &'ir ir::ValueStorage, register: Register) {
        self.items.push(Instruction::LoadA(register, self.value_storage_to_location(storage)).into());
    }

    #[allow(dead_code)]
    #[inline]
    fn load_value_storage_address(&mut self, storage: &'ir ir::ValueStorage) -> Register {
        let register = self.available_temporary_integer_registers.pop_back().unwrap();

        self.load_value_storage_address_to_register(storage, register.clone());

        register
    }

    #[inline]
    fn load_data_location_to_register(&mut self, data_location: DataLocation, register: Register) {
        self.items.push(Instruction::Load(register, data_location).into());
    }

    #[inline]
    fn load_value_storage_to_register(&mut self, storage: &'ir ir::ValueStorage, register: Register) {
        self.load_data_location_to_register(self.value_storage_to_location(storage), register);
    }

    #[inline]
    fn load_value_storage(&mut self, storage: &'ir ir::ValueStorage) -> Register {
        let value_type = self.fetch_value_type(storage);

        match value_type {
            ast::ValueType::Int => {
                let register = self.available_temporary_integer_registers.pop_back().unwrap();

                self.load_value_storage_to_register(storage, register.clone());

                register
            }
            ast::ValueType::Real => {
                let register = self.available_temporary_float_registers.pop_back().unwrap();

                self.load_value_storage_to_register(storage, register.clone());

                register
            }
            _ => unreachable!(),
        }
    }

    #[inline]
    fn store_to_data_location(&mut self, data_location: DataLocation, register: Register) {
        self.items.push(Instruction::Store(register.clone(), data_location).into());

        self.recycle_register(register);
    }

    #[inline]
    fn store_to_value_storage(&mut self, storage: &'ir ir::ValueStorage, register: Register) {
        self.store_to_data_location(self.value_storage_to_location(storage), register);
    }

    fn copy_to_call_function(&mut self, call_function: &'ir ir::Function, to: &'ir ir::ValueStorage, from: &'ir ir::ValueStorage) {
        let register = self.load_value_storage(from);

        let (offset, stack_offset_map) = self.function_stack_offset_map.get(call_function.name).unwrap();
        let variable_offset = -(*offset as i64) + (*stack_offset_map.get(to).unwrap() as i64);

        self.store_to_data_location(DataLocation::Memory(variable_offset), register);
    }

    fn visit(&mut self, ir_item: &'ir ir::IRItem<'input>) {
        match ir_item {
            ir::IRItem::Const(value_storage, const_value) => {
                self.check_ro_data_section();

                let data_location = self.value_storage_to_location(value_storage);

                match const_value {
                    ir::ConstValue::Bool(c) => {
                        if *c {
                            self.items.push(DataSectionItem::Byte(data_location, "1".to_owned()).into());
                        } else {
                            self.items.push(DataSectionItem::Byte(data_location, "0".to_owned()).into());
                        }
                    }
                    ir::ConstValue::String(s) => {
                        self.items.push(DataSectionItem::String(data_location, format!("\"{}\"", s)).into());
                    }
                    ir::ConstValue::Int(v) => {
                        self.items.push(DataSectionItem::Word(data_location, format!("{}", v)).into());
                    }
                    ir::ConstValue::Real(v) => {
                        self.items.push(DataSectionItem::Double(data_location, format!("{:.4}", v)).into());
                    }
                }
            }
            ir::IRItem::Var(value_storage) => {
                self.check_data_section();

                self.items.push(DataSectionItem::Zero(self.value_storage_to_location(value_storage), format!("{}", self.value_storage_to_size(value_storage))).into());
            }
            ir::IRItem::Start() => {
                self.check_text_section();

                self.items.push(GeneratedCodeItem::Label(START_LABEL.to_owned()));

                self.items.push(Instruction::Jal(MAIN_FUNCTION.to_owned()).into());
                self.items.push(Instruction::Load(Register::A0, DataLocation::Memory(-4)).into());
                self.items.push(Instruction::AddI(Register::A7, Register::X0, "93".to_owned()).into());
                self.items.push(Instruction::ECall().into());
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

                self.items.push(Instruction::AddI(Register::SP, Register::SP, format!("-{}", offset)).into());
                self.items.push(Instruction::Store(Register::RA, self.get_jump_address()).into());
            }
            ir::IRItem::Return(s) => {
                let f = self.current_function.unwrap();

                let offset = self.function_stack_offset_map.get(f.name).unwrap().0;

                let value_type = self.fetch_value_type(s);

                match value_type {
                    ast::ValueType::Int => {
                        self.load_value_storage_to_register(s, Register::A0.into());
                    }
                    ast::ValueType::Real => {
                        self.load_value_storage_to_register(s, Register::FA0.into());
                    }
                    _ => unreachable!(),
                }

                self.items.push(Instruction::Load(Register::RA, self.get_jump_address()).into());
                self.items.push(Instruction::AddI(Register::SP, Register::SP, format!("{}", offset)).into());
                self.items.push(Instruction::Ret().into());
            }
            ir::IRItem::Copy(s1, s2) => {
                let register = self.load_value_storage(s2);
                self.store_to_value_storage(s1, register);
            }
            ir::IRItem::Print(s) => {
                let value_type = self.fetch_value_type(s);

                match value_type {
                    ast::ValueType::String(_) => {
                        self.items.push(Instruction::AddI(Register::A0, Register::X0, "1".to_owned()).into());

                        self.load_value_storage_address_to_register(s, Register::A1.into());

                        self.items.push(Instruction::AddI(Register::A2, Register::X0, format!("{}", self.value_type_to_size(&value_type))).into());
                        self.items.push(Instruction::AddI(Register::A7, Register::X0, "64".to_owned()).into());
                        self.items.push(Instruction::ECall().into());
                    }
                    ast::ValueType::Int => {
                        self.load_value_storage_to_register(s, Register::A0.into());

                        self.items.push(Instruction::Call(".print_int".to_owned()).into());
                    }
                    ast::ValueType::Real => {
                        self.load_value_storage_to_register(s, Register::FA0.into());

                        self.items.push(Instruction::Call(".print_real".to_owned()).into());
                    }
                    _ => {}
                }
            }
            ir::IRItem::Jump(label) => {
                self.items.push(Instruction::Jump(label.to_owned()).into());
            }
            ir::IRItem::Bz(label, s) => {
                let register = self.load_value_storage(s);
                self.items.push(Instruction::Beqz(register, label.to_owned()).into());
            }
            ir::IRItem::Cast(to, from) => {
                let register = self.load_value_storage(from);
                let result_register = self.available_temporary_float_registers.pop_back().unwrap();

                self.items.push(Instruction::ConvertFFromI(result_register.clone(), register).into());

                self.store_to_value_storage(to, result_register);
            }
            ir::IRItem::BinaryOp(storage, op, operand1, operand2) => {
                let value_type1 = self.fetch_value_type(operand1);
                let value_type2 = self.fetch_value_type(operand2);

                let result_register;
                let mut middle_result_register: Option<Register> = None;

                if value_type1 == ast::ValueType::Real || value_type2 == ast::ValueType::Real {
                    middle_result_register = Some(self.available_temporary_float_registers.pop_back().unwrap());
                }

                if ((value_type1 == ast::ValueType::Real || value_type2 == ast::ValueType::Real) && (*op == ir::Op::Add || *op == ir::Op::Sub || *op == ir::Op::Mul)) || *op == ir::Op::Div {
                    result_register = self.available_temporary_float_registers.pop_back().unwrap();
                } else {
                    result_register = self.available_temporary_integer_registers.pop_back().unwrap();
                }

                let register1 = self.load_value_storage(operand1);
                let register2 = self.load_value_storage(operand2);

                match op {
                    ir::Op::Add => {
                        self.items.push(Instruction::Add(result_register.clone(), register1, register2).into());
                    }
                    ir::Op::Sub => {
                        self.items.push(Instruction::Sub(result_register.clone(), register1, register2).into());
                    }
                    ir::Op::Mul => {
                        self.items.push(Instruction::Mul(result_register.clone(), register1, register2).into());
                    }
                    ir::Op::Div => {
                        self.items.push(Instruction::Div(result_register.clone(), register1, register2).into());
                    }
                    ir::Op::Eq => {
                        self.items.push(Instruction::Sub(middle_result_register.clone().unwrap(), register1, register2).into());
                        self.items.push(Instruction::Seqz(result_register.clone(), middle_result_register.clone().unwrap()).into());
                    }
                    ir::Op::NotEq => {
                        self.items.push(Instruction::Sub(middle_result_register.clone().unwrap(), register1, register2).into());
                        self.items.push(Instruction::Snez(result_register.clone(), middle_result_register.clone().unwrap()).into());
                    }
                    ir::Op::Greater => {
                        self.items.push(Instruction::Sgt(result_register.clone(), register1, register2).into());
                    }
                    ir::Op::GreaterEq => {
                        self.items.push(Instruction::Slt(result_register.clone(), register1, register2).into());
                        self.items.push(Instruction::Seqz(result_register.clone(), result_register.clone()).into());
                    }
                    ir::Op::Less => {
                        self.items.push(Instruction::Slt(result_register.clone(), register1, register2).into());
                    }
                    ir::Op::LessEq => {
                        self.items.push(Instruction::Sgt(result_register.clone(), register1, register2).into());
                        self.items.push(Instruction::Seqz(result_register.clone(), result_register.clone()).into());
                    }
                    ir::Op::Mod => {
                        self.items.push(Instruction::Rem(result_register.clone(), register1, register2).into());
                    }
                    ir::Op::IntDiv => {
                        self.items.push(Instruction::Div(result_register.clone(), register1, register2).into());
                    }
                    ir::Op::And => {
                        self.items.push(Instruction::And(result_register.clone(), register1, register2).into());
                    }
                    ir::Op::Or => {
                        self.items.push(Instruction::Or(result_register.clone(), register1, register2).into());
                    }
                    _ => {}
                }

                self.store_to_value_storage(storage, result_register);
            }
            ir::IRItem::UnaryOp(storage, op, operand) => match op {
                ir::Op::Negative => {
                    let value_type = self.fetch_value_type(operand);

                    let result_register;

                    if value_type == ast::ValueType::Real {
                        result_register = self.available_temporary_float_registers.pop_back().unwrap();
                    } else {
                        result_register = self.available_temporary_integer_registers.pop_back().unwrap();
                    }

                    let register = self.load_value_storage(operand);

                    self.items.push(Instruction::Sub(result_register.clone(), Register::X0, register).into());

                    self.store_to_value_storage(storage, result_register);
                }
                ir::Op::Not => {
                    let result_register = self.available_temporary_integer_registers.pop_back().unwrap();

                    let register = self.load_value_storage(operand);

                    self.items.push(Instruction::Seqz(result_register.clone(), register).into());

                    self.store_to_value_storage(storage, result_register);
                }
                _ => {}
            },
            ir::IRItem::Call(label, s, items) => {
                let call_function = self.ir_context.function_map.get(label).unwrap();

                let mut i = 0;
                for (parameter, _) in &call_function.local_map {
                    self.copy_to_call_function(call_function, parameter, items.get(i).unwrap());

                    i += 1;
                }

                self.items.push(Instruction::Call((*label).to_owned()).into());

                match call_function.return_type {
                    ast::ValueType::Int => {
                        self.store_to_value_storage(s, Register::A0);
                    }
                    ast::ValueType::Real => {
                        self.store_to_value_storage(s, Register::FA0);
                    }
                    _ => {}
                }
            }
            ir::IRItem::CopyToPointer(pointer, storage) => {
                let value_type = self.fetch_value_type(storage);

                let stack_offset_map = self.get_stack_offset_map();
                let offset = stack_offset_map.get(&pointer.0).unwrap().clone();

                let right_register = self.load_value_storage(storage);
                let left_register = self.load_value_storage(&pointer.1);

                let temp_sp_register = self.available_temporary_integer_registers.pop_back().unwrap();
                let left_coefficient_register = self.available_temporary_integer_registers.pop_back().unwrap();

                self.items.push(Instruction::Mv(temp_sp_register.clone(), Register::SP).into());
                self.items.push(Instruction::AddI(temp_sp_register.clone(), temp_sp_register.clone(), format!("{}", offset)).into());
                self.items.push(Instruction::AddI(left_coefficient_register.clone(), Register::X0, format!("{}", self.value_type_to_size(&value_type.plain()))).into()); // t2
                self.items.push(Instruction::Mul(left_register.clone(), left_register.clone(), left_coefficient_register.clone()).into());
                self.items.push(Instruction::Add(temp_sp_register.clone(), temp_sp_register.clone(), left_register).into());

                self.store_to_data_location(DataLocation::MemoryWithRegister(0, temp_sp_register), right_register);
            }
            ir::IRItem::CopyFromPointer(storage, pointer) => {
                let value_type = self.fetch_value_type(&pointer.0).plain();

                let stack_offset_map = self.get_stack_offset_map();
                let offset = stack_offset_map.get(&pointer.0).unwrap().clone();

                let left_register = self.load_value_storage(storage);
                let right_register = self.load_value_storage(&pointer.1);

                let temp_sp_register = self.available_temporary_integer_registers.pop_back().unwrap();
                let right_coefficient_register = self.available_temporary_integer_registers.pop_back().unwrap();

                self.items.push(Instruction::Mv(temp_sp_register.clone(), Register::SP).into());
                self.items.push(Instruction::AddI(temp_sp_register.clone(), temp_sp_register.clone(), format!("{}", offset)).into());
                self.items.push(Instruction::AddI(right_coefficient_register.clone(), Register::X0, format!("{}", self.value_type_to_size(&value_type))).into()); // t2
                self.items.push(Instruction::Mul(right_register.clone(), right_register.clone(), right_coefficient_register).into());
                self.items.push(Instruction::Add(temp_sp_register.clone(), temp_sp_register.clone(), right_register).into());

                self.store_to_data_location(DataLocation::MemoryWithRegister(0, temp_sp_register), left_register);
            }
            _ => {}
        }
    }

    fn generate_fixed_generated_items(&mut self) -> Vec<GeneratedCodeItem> {
        let mut new_items: Vec<GeneratedCodeItem> = Vec::new();

        for item in &self.items {
            match item {
                GeneratedCodeItem::Instruction(instruction) => match instruction {
                    Instruction::Add(x, y, z) => {
                        if x.is_float_register() {
                            let mut operand1 = y.clone();
                            let mut operand2 = z.clone();

                            if !operand1.is_float_register() {
                                new_items.push(Instruction::ConvertFFromI(Register::FT0, operand1).into());
                                operand1 = Register::FT0;
                            }

                            if !operand2.is_float_register() {
                                new_items.push(Instruction::ConvertFFromI(Register::FT1, operand2).into());
                                operand2 = Register::FT1;
                            }

                            new_items.push(Instruction::FAdd(x.clone(), operand1, operand2).into());
                        } else {
                            new_items.push(instruction.clone().into());
                        }
                    }
                    Instruction::Beqz(x, y) => {
                        if x.is_float_register() {
                            new_items.push(Instruction::ConvertFIromF(Register::T0, x.clone()).into());
                            new_items.push(Instruction::Beqz(Register::T0, y.clone()).into());
                        } else {
                            new_items.push(instruction.clone().into());
                        }
                    }
                    Instruction::Div(x, y, z) => {
                        let mut operand1 = y.clone();
                        let mut operand2 = z.clone();

                        if x.is_float_register() {
                            if !operand1.is_float_register() {
                                new_items.push(Instruction::ConvertFFromI(Register::FT0, operand1).into());
                                operand1 = Register::FT0;
                            }

                            if !operand2.is_float_register() {
                                new_items.push(Instruction::ConvertFFromI(Register::FT1, operand2).into());
                                operand2 = Register::FT1;
                            }

                            new_items.push(Instruction::FDiv(x.clone(), operand1, operand2).into());
                        } else {
                            if operand1.is_float_register() {
                                new_items.push(Instruction::ConvertFIromF(Register::T0, operand1).into());
                                operand1 = Register::T0;
                            }

                            if operand2.is_float_register() {
                                new_items.push(Instruction::ConvertFIromF(Register::T1, operand2).into());
                                operand2 = Register::T1;
                            }

                            new_items.push(Instruction::Div(x.clone(), operand1, operand2).into());
                        }
                    }
                    Instruction::Load(register, data_location) => {
                        if register.is_integer_register() {
                            new_items.push(Instruction::Load(register.clone(), data_location.clone()).into());
                        } else if register.is_float_register() {
                            new_items.push(Instruction::FLoad(register.clone(), data_location.clone()).into());
                        } else if register.is_full_width_register() {
                            new_items.push(Instruction::LoadD(register.clone(), data_location.clone()).into());
                        }
                    }
                    Instruction::Mul(x, y, z) => {
                        if x.is_float_register() {
                            let mut operand1 = y.clone();
                            let mut operand2 = z.clone();

                            if !operand1.is_float_register() {
                                new_items.push(Instruction::ConvertFFromI(Register::FT0, operand1).into());
                                operand1 = Register::FT0;
                            }

                            if !operand2.is_float_register() {
                                new_items.push(Instruction::ConvertFFromI(Register::FT1, operand2).into());
                                operand2 = Register::FT1;
                            }

                            new_items.push(Instruction::FMul(x.clone(), operand1, operand2).into());
                        } else {
                            new_items.push(instruction.clone().into());
                        }
                    }
                    Instruction::Store(register, data_location) => {
                        if register.is_integer_register() {
                            new_items.push(Instruction::Store(register.clone(), data_location.clone()).into());
                        } else if register.is_float_register() {
                            new_items.push(Instruction::FStore(register.clone(), data_location.clone()).into());
                        } else if register.is_full_width_register() {
                            new_items.push(Instruction::StoreD(register.clone(), data_location.clone()).into());
                        }
                    }
                    Instruction::Sub(x, y, z) => {
                        if x.is_float_register() {
                            let mut operand1 = y.clone();
                            let mut operand2 = z.clone();

                            if !operand1.is_float_register() {
                                new_items.push(Instruction::ConvertFFromI(Register::FT0, operand1).into());
                                operand1 = Register::FT0;
                            }

                            if !operand2.is_float_register() {
                                new_items.push(Instruction::ConvertFFromI(Register::FT1, operand2).into());
                                operand2 = Register::FT1;
                            }

                            new_items.push(Instruction::FSub(x.clone(), operand1, operand2).into());
                        } else {
                            new_items.push(instruction.clone().into());
                        }
                    }
                    Instruction::Seqz(x, y) => {
                        if x.is_float_register() {
                            new_items.push(Instruction::ConvertFIromF(Register::T0, x.clone()).into());
                            new_items.push(Instruction::Seqz(Register::T0, y.clone()).into());
                        } else {
                            new_items.push(instruction.clone().into());
                        }
                    }
                    Instruction::Snez(x, y) => {
                        if x.is_float_register() {
                            new_items.push(Instruction::ConvertFIromF(Register::T0, x.clone()).into());
                            new_items.push(Instruction::Snez(Register::T0, y.clone()).into());
                        } else {
                            new_items.push(instruction.clone().into());
                        }
                    }
                    _ => new_items.push(instruction.clone().into()),
                },
                _ => new_items.push(item.clone()),
            }
        }

        new_items
    }

    pub fn build(ir_context: &'ir ir::IRContext<'input>) -> Vec<GeneratedCodeItem> {
        let mut generator = CodeGenerator::new(ir_context);

        generator.initialize_stack();

        generator.items.push(GeneratedCodeItem::Global(START_LABEL.to_owned()));

        for ir_item in &ir_context.items {
            generator.visit(ir_item);
        }

        let items = generator.generate_fixed_generated_items();

        return items;
    }
}
