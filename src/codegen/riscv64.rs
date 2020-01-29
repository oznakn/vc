use std::cmp::max;
use std::collections::{HashMap, VecDeque};

use crate::ast;
use crate::ir;
use crate::ir::ValueStorage;
use crate::MAIN_FUNCTION;
use test::TestType::IntegrationTest;

#[derive(Clone, Debug)]
enum DataLocation {
    Memory(i64),
    MemoryWithRegister(i64, Register),
    Program(String),
}

#[derive(Clone, Debug)]
enum IntegerRegister {
    X0,
    RA,
    SP,
    GP,
    TP,

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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
enum Register {
    IntegerRegister(IntegerRegister),
    FloatRegister(FloatRegister),
}

impl From<IntegerRegister> for Register {
    fn from(integer_register: IntegerRegister) -> Self {
        Register::IntegerRegister(general_register)
    }
}

impl From<FloatRegister> for Register {
    fn from(float_register: FloatRegister) -> Self {
        Register::FloatRegister(general_register)
    }
}

impl Register {
    fn unwrap_as_integer(&self) -> &IntegerRegister {
        match self {
            Register::IntegerRegister(integer_register) => integer_register,
            _ => unreachable!(),
        }
    }

    fn unwrap_as_float(&self) -> &FloatRegister {
        match self {
            Register::FloatRegister(float_register) => float_register,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
enum Instruction {
    Jal(String),
    Jump(String),
    Call(String),
    ECall(),
    Ret(),

    Add(IntegerRegister, IntegerRegister, IntegerRegister),
    AddI(IntegerRegister, IntegerRegister, String),
    And(IntegerRegister, IntegerRegister, IntegerRegister),
    Beq(IntegerRegister, IntegerRegister, String),
    Beqz(IntegerRegister, String),
    Bne(IntegerRegister, IntegerRegister, String),
    Div(IntegerRegister, IntegerRegister, IntegerRegister),
    LoadW(IntegerRegister, DataLocation),
    LoadD(IntegerRegister, DataLocation),
    LoadA(IntegerRegister, DataLocation),
    Mul(IntegerRegister, IntegerRegister, IntegerRegister),
    Or(IntegerRegister, IntegerRegister, IntegerRegister),
    StoreW(IntegerRegister, DataLocation),
    StoreD(IntegerRegister, DataLocation),
    Sub(IntegerRegister, IntegerRegister, IntegerRegister),
    Seqz(IntegerRegister, IntegerRegister),
    Snez(IntegerRegister, IntegerRegister),
    Sgt(IntegerRegister, IntegerRegister, IntegerRegister),
    Slt(IntegerRegister, IntegerRegister, IntegerRegister),
    Rem(IntegerRegister, IntegerRegister, IntegerRegister),

    FAdd(FloatRegister, FloatRegister, FloatRegister),
    FConvertFFromI(FloatRegister, IntegerRegister), // to, from
    FConvertIFromF(IntegerRegister, FloatRegister), // to, from
    FDiv(FloatRegister, FloatRegister, FloatRegister),
    FMul(FloatRegister, FloatRegister, FloatRegister),
    FSub(FloatRegister, FloatRegister, FloatRegister),
    FEq(IntegerRegister, FloatRegister, FloatRegister),
    FGt(IntegerRegister, FloatRegister, FloatRegister),
    FLt(IntegerRegister, FloatRegister, FloatRegister),
    FLoad(FloatRegister, DataLocation),
    FStore(FloatRegister, DataLocation),
}

#[derive(Clone, Debug)]
enum DataSectionItem {
    String(DataLocation, String),
    Word(DataLocation, String),
    Double(DataLocation, String),
    Byte(DataLocation, String),
    Zero(DataLocation, String),
}

#[derive(Clone, Debug)]
pub enum GeneratedCodeItem {
    Section(String),
    Label(String),
    DataSectionItem(DataSectionItem),
    Instruction(Instruction),
    Raw(String),
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
            self.items.push(GeneratedCodeItem::Section("rodata".to_string()));

            self.items.push(GeneratedCodeItem::Label(FLOAT_10_LABEL.to_owned()));
            self.items.push(GeneratedCodeItem::DataItem(DataItem::Double("10.0000".to_owned())));

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

    /* fn value_storage_to_string_for_label(&self, s: &'ir ir::ValueStorage) -> String {
        return match s {
            ir::ValueStorage::Const(i) => format!("C{}", i),
            ir::ValueStorage::Var(i) => format!("V{}", i),
            _ => format!(""),
        };
    } */

    fn value_storage_to_location(&self, s: &'ir ir::ValueStorage) -> DataLocation {
        let value_type = self.fetch_value_type(s);

        match s {
            ir::ValueStorage::Const(i) => DataLocation::Program(format!("C{}", i)),
            ir::ValueStorage::Var(i) => DataLocation::Program(format!("V{}", i)),
            ir::ValueStorage::Local(i) => {
                let stack_offset_map = self.get_stack_offset_map();

                DataLocation::Memory(stack_offset_map.get(s).unwrap().to_owned() as i64)
            }
        }
    }

    fn load_value_storage_address_to_register(&mut self, storage: &'ir ir::ValueStorage, register: Register) {
        self.items.push(Instruction::LoadA(register.unwrap_as_integer().to_owned(), self.value_storage_to_location(storage)).into());
    }

    fn load_value_storage_to_register(&mut self, storage: &'ir ir::ValueStorage, register: Register) {
        let value_type = self.fetch_value_type(storage);

        match value_type {
            ast::ValueType::Int => {
                self.items.push(Instruction::LoadW(register.unwrap_as_integer().to_owned(), self.value_storage_to_location(storage)).into());
            }
            ast::ValueType::Real => {
                self.items.push(Instruction::FLoad(register.unwrap_as_float().to_owned(), self.value_storage_to_location(storage)).into());
            }
            _ => {}
        }
    }

    fn store_register_to_value_storage(&mut self, storage: &'ir ir::ValueStorage, register: Register) {
        let value_type = self.fetch_value_type(storage);

        match value_type {
            ast::ValueType::Int => {
                self.items.push(Instruction::StoreW(register.unwrap_as_integer().to_owned(), self.value_storage_to_location(storage)).into());
            }
            ast::ValueType::Real => {
                self.items.push(Instruction::FStore(register.unwrap_as_float().to_owned(), self.value_storage_to_location(storage)).into());
            }
            _ => {}
        }
    }

    fn copy_to_call_function(&mut self, call_function: &'ir ir::Function, to: &'ir ir::ValueStorage, from: &'ir ir::ValueStorage) {
        let from_value_type = self.fetch_value_type(from);

        match from_value_type {
            ast::ValueType::Int => {
                self.items.push(Instruction::LoadW(IntegerRegister::A0, self.value_storage_to_location(storage)).into());
            }
            ast::ValueType::Real => {
                self.items.push(Instruction::FLoad(FloatRegister::FA0, self.value_storage_to_location(storage)).into());
            }
            _ => {}
        }

        let (offset, stack_offset_map) = self.function_stack_offset_map.get(call_function.name).unwrap();
        let variable_offset = -(*offset as i64) + (*stack_offset_map.get(to).unwrap() as i64);

        match from_value_type {
            ast::ValueType::Int => {
                self.items.push(Instruction::StoreW(IntegerRegister::A0, DataLocation::Memory(variable_offset)).into());
            }
            ast::ValueType::Real => {
                self.items.push(Instruction::FStore(FloatRegister::FA0, DataLocation::Memory(variable_offset)).into());
            }
            _ => {}
        }
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
                        self.items.push(DataSectionItem::String(data_location, format!("\"{}\", s")).into());
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
                self.items.push(Instruction::LoadW(IntegerRegister::A0, DataLocation::Memory(-4)).into());
                self.items.push(Instruction::AddI(IntegerRegister::A7, IntegerRegister::X0, "93".to_owned()).into());
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

                self.items.push(Instruction::AddI(IntegerRegister::SP, IntegerRegister::SP, format!("-{}", offset)).into());
                self.items.push(Instruction::StoreD(IntegerRegister::RA, self.get_jump_address()).into());
            }
            ir::IRItem::Return(s) => {
                let f = self.current_function.unwrap();

                let offset = self.function_stack_offset_map.get(f.name).unwrap().0;

                let value_type = self.fetch_value_type(s);

                match value_type {
                    ast::ValueType::Int => {
                        self.load_value_storage_to_register(s, IntegerRegister::A0.into());
                    }
                    ast::ValueType::Real => {
                        self.load_value_storage_to_register(s, FloatRegister::FA0.into());
                    }
                    _ => unreachable!(),
                }

                self.items.push(Instruction::LoadD(IntegerRegister::RA, self.get_jump_address()).into());
                self.items.push(Instruction::AddI(IntegerRegister::SP, IntegerRegister::SP, format!("{}", offset)).into());
                self.items.push(Instruction::Ret().into());
            }
            ir::IRItem::Copy(s1, s2) => {
                let value_type = self.fetch_value_type(s2);

                match value_type {
                    ast::ValueType::Int => {
                        self.load_value_storage_to_register(s2, IntegerRegister::T0.into());
                        self.store_register_to_value_storage(s1, IntegerRegister::T0.into());
                    }
                    ast::ValueType::Real => {
                        self.load_value_storage_to_register(s2, FloatRegister::FT0.into());
                        self.store_register_to_value_storage(s1, FloatRegister::FT0.into());
                    }
                    _ => {}
                }
            }
            ir::IRItem::Print(s) => {
                let value_type = self.fetch_value_type(s);

                match value_type {
                    ast::ValueType::String(_) => {
                        self.items.push(Instruction::AddI(IntegerRegister::A0, IntegerRegister::X0, "1".to_owned()).into());

                        self.load_value_storage_address_to_register(s, IntegerRegister::A1.into());

                        self.items.push(Instruction::AddI(IntegerRegister::A2, IntegerRegister::X0, format!("{}", self.value_type_to_size(&value_type))).into());
                        self.items.push(Instruction::AddI(IntegerRegister::A7, IntegerRegister::X0, "64".to_owned()).into());
                        self.items.push(Instruction::ECall().into());
                    }
                    ast::ValueType::Int => {
                        self.load_value_storage_to_register(s, IntegerRegister::A0.into());

                        self.items.push(Instruction::Call(".print_int".to_owned()).into());
                    }
                    ast::ValueType::Real => {
                        self.load_value_storage_to_register(s, FloatRegister::FA0.into());

                        self.items.push(Instruction::Call(".print_real".to_owned()).into());
                    }
                    _ => {}
                }
            }
            ir::IRItem::Jump(label) => {
                self.items.push(Instruction::Jump(label.to_owned()).into());
            }
            ir::IRItem::Bz(label, s) => {
                let value_type = self.fetch_value_type(s);

                match value_type {
                    ast::ValueType::Int => {
                        self.load_value_storage_to_register(s, IntegerRegister::T0.into());
                        self.items.push(Instruction::Beqz(IntegerRegister::T0, label.to_owned()).into());
                    }
                    ast::ValueType::Real => {
                        self.load_value_storage_to_register(s, FloatRegister::FT0.into());
                        self.items.push(Instruction::FConvertIFromF(IntegerRegister::T0, FloatRegister::FT0).into());
                        self.items.push(Instruction::Beqz(IntegerRegister::T0, label.to_owned()).into());
                    }
                    _ => {}
                }
            }
            ir::IRItem::Cast(to, from) => {
                /* self.load_value_storage_to_register(from, "t0");

                self.items.push(GeneratedCodeItem::Instruction("fcvt.d.w".to_string(), vec!["ft0".to_owned(), "t0".to_owned()]));

                self.store_register_to_value_storage(to, "ft0"); */
            }
            ir::IRItem::BinaryOp(storage, op, operand1, operand2) => {
                let value_type1 = self.fetch_value_type(operand1);
                let value_type2 = self.fetch_value_type(operand2);

                if value_type1 == ast::ValueType::Int && value_type2 == ast::ValueType::Int {
                    self.load_value_storage_to_register(operand1, IntegerRegister::T1.into());
                    self.load_value_storage_to_register(operand2, IntegerRegister::T2.into());

                    match op {
                        ir::Op::Add => {
                            self.items.push(Instruction::Add(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                        }
                        ir::Op::Sub => {
                            self.items.push(Instruction::Sub(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                        }
                        ir::Op::Mul => {
                            self.items.push(Instruction::Mul(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                        }
                        ir::Op::Eq => {
                            self.items.push(Instruction::Sub(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                            self.items.push(Instruction::Seqz(IntegerRegister::T0, IntegerRegister::T0).into());
                        }
                        ir::Op::NotEq => {
                            self.items.push(Instruction::Sub(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                            self.items.push(Instruction::Snez(IntegerRegister::T0, IntegerRegister::T0).into());
                        }
                        ir::Op::Greater => {
                            self.items.push(Instruction::Sgt(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                        }
                        ir::Op::GreaterEq => {
                            self.items.push(Instruction::Slt(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                            self.items.push(Instruction::Seqz(IntegerRegister::T0, IntegerRegister::T0).into());
                        }
                        ir::Op::Less => {
                            self.items.push(Instruction::Slt(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                        }
                        ir::Op::LessEq => {
                            self.items.push(Instruction::Sgt(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                            self.items.push(Instruction::Seqz(IntegerRegister::T0, IntegerRegister::T0).into());
                        }
                        ir::Op::Mod => {
                            self.items.push(Instruction::Rem(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                        }
                        ir::Op::IntDiv => {
                            self.items.push(Instruction::Div(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                        }
                        ir::Op::And => {
                            self.items.push(Instruction::And(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                        }
                        ir::Op::Or => {
                            self.items.push(Instruction::Or(IntegerRegister::T0, IntegerRegister::T1, IntegerRegister::T2).into());
                        }
                        _ => {}
                    }

                    self.store_register_to_value_storage(storage, IntegerRegister::T0.into());
                } else {
                    self.load_value_storage_to_register(operand1, FloatRegister::FT1.into());
                    self.load_value_storage_to_register(operand2, FloatRegister::FT2.into());

                    let mut is_result_integer = true;

                    match op {
                        ir::Op::Add => {
                            self.items.push(Instruction::FAdd(FloatRegister::FT0, FloatRegister::FT1, FloatRegister::FT2).into());
                            is_result_integer = false;
                        }
                        ir::Op::Sub => {
                            self.items.push(Instruction::FSub(FloatRegister::FT0, FloatRegister::FT1, FloatRegister::FT2).into());
                            is_result_integer = false;
                        }
                        ir::Op::Mul => {
                            self.items.push(Instruction::FMul(FloatRegister::FT0, FloatRegister::FT1, FloatRegister::FT2).into());
                            is_result_integer = false;
                        }
                        ir::Op::Div => {
                            self.items.push(Instruction::FDiv(FloatRegister::FT0, FloatRegister::FT1, FloatRegister::FT2).into());
                            is_result_integer = false;
                        }
                        ir::Op::Eq => {
                            self.items.push(Instruction::FSub(FloatRegister::FT0, FloatRegister::FT1, FloatRegister::FT2).into());
                            self.items.push(Instruction::Seqz(IntegerRegister::T0, IntegerRegister::T0).into());
                        }
                        ir::Op::NotEq => {
                            self.items.push(Instruction::FSub(FloatRegister::FT0, FloatRegister::FT1, FloatRegister::FT2).into());
                            self.items.push(Instruction::FConvertIFromF(IntegerRegister::T0, FloatRegister::FT0).into());
                            self.items.push(Instruction::Snez(IntegerRegister::T0, IntegerRegister::T0).into());
                        }
                        ir::Op::Greater => {
                            self.items.push(Instruction::FGt(IntegerRegister::T0, FloatRegister::FT1, FloatRegister::FT2).into());
                        }
                        ir::Op::GreaterEq => {
                            self.items.push(Instruction::FLt(IntegerRegister::T0, FloatRegister::FT1, FloatRegister::FT2).into());
                            self.items.push(Instruction::Seqz(IntegerRegister::T0, IntegerRegister::T0).into());
                        }
                        ir::Op::Less => {
                            self.items.push(Instruction::FLt(IntegerRegister::T0, FloatRegister::FT1, FloatRegister::FT2).into());
                        }
                        ir::Op::LessEq => {
                            self.items.push(Instruction::FGt(IntegerRegister::T0, FloatRegister::FT1, FloatRegister::FT2).into());
                            self.items.push(Instruction::Seqz(IntegerRegister::T0, IntegerRegister::T0).into());
                        }
                        _ => {}
                    }

                    if is_result_integer {
                        self.store_register_to_value_storage(storage, IntegerRegister::T0.into());
                    } else {
                        self.store_register_to_value_storage(storage, FloatRegister::FT0.into());
                    }
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
                    self.copy_to_call_function(call_function, parameter, items.get(i).unwrap());

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
            ir::IRItem::CopyToPointer(pointer, storage) => {
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
            ir::IRItem::CopyFromPointer(storage, pointer) => {
                let value_type = self.fetch_value_type(&pointer.0);

                let stack_offset_map = self.get_stack_offset_map();

                let offset = stack_offset_map.get(&pointer.0).unwrap().clone();

                self.load_value_storage_to_register(&pointer.1, "t1");
                self.items.push(Instruction("addi".to_string(), vec!["t2".to_owned(), "x0".to_owned(), "1".to_owned()]));
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
