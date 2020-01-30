use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::ast;
use crate::symbol_table;

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum ValueStorage {
    Const(u64),
    Var(u64),
    Local(u64),
}

// Use only for in representation
impl fmt::Display for ValueStorage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueStorage::Const(i) => write!(f, ".c{}", i),
            ValueStorage::Var(i) => write!(f, ".v{}", i),
            ValueStorage::Local(i) => write!(f, ".l{}", i),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ConstValue<'input> {
    Bool(bool),
    String(&'input str),
    Int(u64),
    Real(f64),
}

// Use only in IR representation
impl<'input> fmt::Display for ConstValue<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstValue::Bool(b) => write!(f, "{}", b),
            ConstValue::String(s) => write!(f, "\"{}\"", s),
            ConstValue::Int(v) => write!(f, "{}", v),
            ConstValue::Real(v) => write!(f, "{}", v),
        }
    }
}

pub type Label = String;
pub type VariablePointer = (ValueStorage, ValueStorage);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    IntDiv,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    And,
    Or,
    Not,
    Negative,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Mod => write!(f, "mod"),
            Op::IntDiv => write!(f, "div"),
            Op::Eq => write!(f, "="),
            Op::NotEq => write!(f, "<>"),
            Op::Less => write!(f, "<"),
            Op::LessEq => write!(f, "<="),
            Op::Greater => write!(f, ">"),
            Op::GreaterEq => write!(f, ">="),
            Op::And => write!(f, "and"),
            Op::Or => write!(f, "or"),
            Op::Not => write!(f, "not"),
            Op::Negative => write!(f, "-"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum IRItem<'input> {
    Const(ValueStorage, ConstValue<'input>),
    Start(),
    Label(Label),
    Function(&'input str),
    EndFunction(),
    Param(ValueStorage),
    Local(ValueStorage),
    Jump(Label),
    Copy(ValueStorage, ValueStorage),
    CopyToPointer(VariablePointer, ValueStorage),
    CopyFromPointer(ValueStorage, VariablePointer),
    Bz(Label, ValueStorage),
    Var(ValueStorage),
    Promote(ValueStorage, ValueStorage),
    BinaryOp(ValueStorage, Op, ValueStorage, ValueStorage),
    UnaryOp(ValueStorage, Op, ValueStorage),
    Print(ValueStorage),
    Read(ValueStorage),
    Call(&'input str, ValueStorage, Vec<ValueStorage>),
    Return(ValueStorage),
}

impl<'input> fmt::Display for IRItem<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IRItem::Const(value_storage, const_value) => write!(f, "const({}, {})", value_storage, const_value),
            IRItem::Start() => write!(f, "start()"),
            IRItem::Label(label) => write!(f, "{}:", label),
            IRItem::Function(label) => write!(f, "{}:", label),
            IRItem::EndFunction() => write!(f, "endfunc()"),
            IRItem::Local(value_storage) => write!(f, "local({})", value_storage),
            IRItem::Param(value_storage) => write!(f, "param({})", value_storage),
            IRItem::Jump(label) => write!(f, "jump({})", label),
            IRItem::Copy(to, from) => write!(f, "move({}, {})", to, from),
            IRItem::CopyToPointer(to, from) => write!(f, "store({}[{}], {})", &to.0, &to.1, from),
            IRItem::CopyFromPointer(to, from) => write!(f, "store({}[{}], {})", to, &from.0, &from.1),
            IRItem::Bz(label, value_storage) => write!(f, "bz({}, {})", label, value_storage),
            IRItem::Var(value_storage) => write!(f, "var({})", value_storage),
            IRItem::Promote(to, from) => write!(f, "promote({}, {})", to, from),
            IRItem::BinaryOp(target, op, operand1, operand2) => write!(f, "binary_op({}, {}, {}, {})", target, op, operand1, operand2),
            IRItem::UnaryOp(target, op, operand) => write!(f, "unary_op({}, {}, {})", target, op, operand),
            IRItem::Print(label) => write!(f, "print({})", label),
            IRItem::Read(label) => write!(f, "read({})", label),
            IRItem::Return(label) => write!(f, "return({})", label),
            IRItem::Call(label, return_label, arguments) => write!(f, "call({}, {}, [{}])", label, return_label, arguments.iter().map(|s| format!("{}", s)).collect::<Vec<String>>().join(", ")),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function<'input> {
    pub name: &'input str,
    pub return_type: ast::ValueType,
    pub local_map: IndexMap<ValueStorage, ast::ValueType>,
    variable_storage_map: HashMap<&'input str, ValueStorage>,
}

impl<'input> Function<'input> {
    fn new(name: &'input str, return_type: &'input ast::ValueType) -> Self {
        return Function {
            name,
            return_type: return_type.clone(),
            local_map: IndexMap::new(),
            variable_storage_map: HashMap::new(),
        };
    }
}

#[derive(Clone, Debug)]
pub struct IRContext<'input> {
    symbol_table: &'input symbol_table::SymbolTable<'input>,
    current_function: Option<&'input str>,

    labels: HashSet<String>,
    const_counter: u64,
    var_counter: u64,

    pub const_map: IndexMap<ValueStorage, ast::ValueType>,
    pub var_map: IndexMap<ValueStorage, ast::ValueType>,
    pub function_map: IndexMap<&'input str, Function<'input>>,

    pub string_map: HashMap<&'input str, ValueStorage>,
    pub int_map: HashMap<&'input str, ValueStorage>,
    pub real_map: HashMap<&'input str, ValueStorage>,

    variable_storage_map: HashMap<&'input str, ValueStorage>,

    pub items: Vec<IRItem<'input>>,
}

impl<'input> IRContext<'input> {
    fn new(table: &'input symbol_table::SymbolTable<'input>) -> Self {
        return IRContext {
            symbol_table: table,
            current_function: None,

            labels: HashSet::new(),
            const_counter: 0,
            var_counter: 0,

            const_map: IndexMap::new(),
            var_map: IndexMap::new(),
            function_map: IndexMap::new(),

            string_map: HashMap::new(),
            int_map: HashMap::new(),
            real_map: HashMap::new(),

            variable_storage_map: HashMap::new(),

            items: Vec::new(),
        };
    }

    fn generate_label(&mut self, suffix: u64) -> Label {
        let prefix = self.current_function.unwrap();
        let label = format!("{}__{}", prefix, suffix);

        if let Some(_) = self.labels.get(&label) {
            return self.generate_label(suffix + 1);
        }

        self.labels.insert(label.clone());

        label
    }

    #[inline]
    fn put_start(&mut self) {
        self.items.push(IRItem::Start());
    }

    #[inline]
    fn put_label(&mut self, label: Label) {
        self.items.push(IRItem::Label(label));
    }

    #[inline]
    fn generate_const(&mut self) -> ValueStorage {
        let index = self.const_counter;

        self.const_counter += 1;

        return ValueStorage::Const(index);
    }

    #[inline]
    fn put_const(&mut self, value_storage: ValueStorage, const_value: ConstValue<'input>) {
        self.items.push(IRItem::Const(value_storage, const_value));
    }

    fn generate_var(&mut self, value_type: &ast::ValueType) -> ValueStorage {
        let index = self.var_counter;

        self.var_counter += 1;

        let value_storage = ValueStorage::Var(index);

        self.var_map.insert(value_storage.to_owned(), value_type.to_owned());

        return value_storage;
    }

    #[inline]
    fn put_var(&mut self, value_storage: ValueStorage) {
        self.items.push(IRItem::Var(value_storage));
    }

    fn generate_local(&mut self, value_type: &ast::ValueType) -> ValueStorage {
        let current_function: &mut Function<'input> = self.function_map.get_mut(self.current_function.unwrap()).unwrap();

        let index = current_function.local_map.len() as u64;

        let value_storage = ValueStorage::Local(index);

        current_function.local_map.insert(value_storage.to_owned(), value_type.to_owned());

        return value_storage;
    }

    fn set_current_function_variable_storage_map_item(&mut self, name: &'input str, value_storage: ValueStorage) {
        let current_function: &mut Function<'input> = self.function_map.get_mut(self.current_function.unwrap()).unwrap();

        current_function.variable_storage_map.insert(name, value_storage);
    }

    #[inline]
    fn put_local(&mut self, value_storage: ValueStorage) {
        self.items.push(IRItem::Local(value_storage))
    }

    #[inline]
    fn put_param(&mut self, value_storage: ValueStorage) {
        self.items.push(IRItem::Param(value_storage))
    }

    #[inline]
    fn put_jump(&mut self, label: Label) {
        self.items.push(IRItem::Jump(label));
    }

    #[inline]
    fn put_bz(&mut self, label: Label, value_storage: ValueStorage) {
        self.items.push(IRItem::Bz(label, value_storage));
    }

    #[inline]
    fn put_promote(&mut self, to: ValueStorage, from: ValueStorage) {
        self.items.push(IRItem::Promote(to, from));
    }

    #[inline]
    fn put_print(&mut self, value_storage: ValueStorage) {
        self.items.push(IRItem::Print(value_storage));
    }

    #[inline]
    fn put_read(&mut self, value_storage: ValueStorage) {
        self.items.push(IRItem::Read(value_storage));
    }

    #[inline]
    fn put_copy(&mut self, to: ValueStorage, from: ValueStorage) {
        self.items.push(IRItem::Copy(to, from));
    }

    #[inline]
    fn put_copy_to_pointer(&mut self, to: VariablePointer, from: ValueStorage) {
        self.items.push(IRItem::CopyToPointer(to, from));
    }

    #[inline]
    fn put_copy_from_pointer(&mut self, to: ValueStorage, from: VariablePointer) {
        self.items.push(IRItem::CopyFromPointer(to, from));
    }

    #[inline]
    fn put_binary_op(&mut self, target: ValueStorage, op: Op, operand1: ValueStorage, operand2: ValueStorage) {
        self.items.push(IRItem::BinaryOp(target, op, operand1, operand2));
    }

    #[inline]
    fn put_unary_op(&mut self, target: ValueStorage, op: Op, operand: ValueStorage) {
        self.items.push(IRItem::UnaryOp(target, op, operand));
    }

    #[inline]
    fn put_return(&mut self, value_storage: ValueStorage) {
        self.items.push(IRItem::Return(value_storage));
    }

    #[inline]
    fn put_call(&mut self, label: &'input str, return_value_storage: ValueStorage, params: Vec<ValueStorage>) {
        self.items.push(IRItem::Call(label, return_value_storage, params));
    }

    #[inline]
    fn put_function(&mut self, name: &'input str) {
        self.items.push(IRItem::Function(name));
    }

    #[inline]
    fn put_end_function(&mut self) {
        self.items.push(IRItem::EndFunction());
    }

    fn fetch_variable(&self, variable: &str) -> Option<(ValueStorage, ast::ValueType)> {
        let current_function: &Function<'input> = self.function_map.get(self.current_function.unwrap()).unwrap();

        let mut value_storage_option = None;

        if let Some(value_storage) = current_function.variable_storage_map.get(variable) {
            value_storage_option = Some(value_storage.to_owned());
        } else if let Some(value_storage) = self.variable_storage_map.get(variable) {
            value_storage_option = Some(value_storage.to_owned());
        }

        if let Some(value_storage) = value_storage_option {
            let value_type = self.fetch_value_type(&value_storage);

            return Some((value_storage, value_type.to_owned()));
        }

        None
    }

    fn fetch_value_type(&self, value_storage: &ValueStorage) -> ast::ValueType {
        let current_function: &Function<'input> = self.function_map.get(self.current_function.unwrap()).unwrap();

        match value_storage {
            ValueStorage::Local(_) => current_function.local_map.get(value_storage).unwrap().to_owned(),
            ValueStorage::Var(_) => self.var_map.get(value_storage).unwrap().to_owned(),
            ValueStorage::Const(_) => self.const_map.get(value_storage).unwrap().to_owned(),
        }
    }

    fn build_statement(&mut self, ast_statement: &'input ast::Statement) {
        match ast_statement {
            ast::Statement::AssignmentStatement { variable, expression } => {
                let mut expression_value_storage = self.build_expression(expression);
                let expression_value_type = self.fetch_value_type(&expression_value_storage);

                let (variable_value_storage, variable_value_type) = self.fetch_variable(variable.name).unwrap();

                if expression_value_type.requires_promote(&variable_value_type) {
                    let temp = self.generate_local(&variable_value_type);
                    self.put_local(temp.to_owned());

                    self.put_promote(temp.to_owned(), expression_value_storage);

                    expression_value_storage = temp;
                }

                if variable_value_type.requires_index() {
                    let index_expression_value_storage = self.build_expression(&variable.expression);

                    self.put_copy_to_pointer((variable_value_storage, index_expression_value_storage), expression_value_storage);
                } else {
                    self.put_copy(variable_value_storage, expression_value_storage);
                }
            }
            ast::Statement::PrintStatement { parameter_list } => {
                let mut i: usize = 0;

                let space_string_item = self.string_map.get(" ").unwrap().to_owned();
                let newline_string_item = self.string_map.get("\\n").unwrap().to_owned();

                while i < parameter_list.len() {
                    let parameter = &parameter_list[i];

                    match parameter {
                        ast::Printable::Expression(expression) => {
                            let value_storage = self.build_expression(expression);

                            self.put_print(value_storage);
                        }
                        ast::Printable::String(s) => {
                            let string_value_storage = self.string_map.get(*s).unwrap().to_owned();

                            self.put_print(string_value_storage);
                        }
                    }

                    if i == parameter_list.len() - 1 {
                        self.put_print(newline_string_item.clone());
                    } else {
                        self.put_print(space_string_item.clone());
                    }

                    i += 1;
                }
            }
            ast::Statement::ReadStatement { parameter_list } => {
                for parameter in parameter_list {
                    let (variable_storage, variable_value_type) = self.fetch_variable(parameter.name).unwrap();

                    if variable_value_type.requires_index() {
                        let index_expression_value_storage = self.build_expression(&parameter.expression);

                        let temp = self.generate_local(&variable_value_type.plain());
                        self.put_local(temp.to_owned());

                        self.put_read(temp.to_owned());

                        self.put_copy_to_pointer((variable_storage, index_expression_value_storage), temp);
                    } else {
                        self.put_read(variable_storage);
                    }
                }
            }
            ast::Statement::IfStatement { expression, if_body, else_body, use_else } => {
                let if_expression_value_storage = self.build_expression(expression);

                let finish_label = self.generate_label(0);
                let else_label = self.generate_label(0);

                self.put_bz(else_label.clone(), if_expression_value_storage);

                for statement in if_body {
                    self.build_statement(statement);
                }
                self.put_jump(finish_label.clone());
                self.put_label(else_label);

                if *use_else {
                    for statement in else_body {
                        self.build_statement(statement);
                    }
                }
                self.put_label(finish_label);
            }
            ast::Statement::WhileStatement { expression, body } => {
                let start_label = self.generate_label(0);
                let continue_label = self.generate_label(0);

                self.put_label(start_label.clone());

                let expression_value_storage = self.build_expression(expression);

                self.put_bz(continue_label.clone(), expression_value_storage);

                for statement in body {
                    self.build_statement(statement);
                }
                self.put_jump(start_label);
                self.put_label(continue_label);
            }
            ast::Statement::ForStatement {
                init_variable,
                start_expression,
                to_expression,
                by_expression,
                body,
            } => {
                let start_label = self.generate_label(0);
                let continue_label = self.generate_label(0);

                let (init_variable_value_storage, init_variable_value_type) = self.fetch_variable(init_variable.name).unwrap();

                let start_expression_value = self.build_expression(start_expression);

                self.put_copy(init_variable_value_storage.to_owned(), start_expression_value);

                let to_expression_value_storage = self.build_expression(to_expression);
                let by_expression_value_storage;

                match by_expression {
                    ast::Expression::Empty => match init_variable_value_type {
                        ast::ValueType::Int => {
                            by_expression_value_storage = self.int_map.get("1").unwrap().to_owned();
                        }
                        ast::ValueType::Real => {
                            by_expression_value_storage = self.real_map.get("1.0").unwrap().to_owned();
                        }
                        _ => unreachable!(),
                    },
                    _ => {
                        by_expression_value_storage = self.build_expression(by_expression);
                    }
                }

                let check_variable_value_storage = self.generate_local(&init_variable_value_type);
                self.put_local(check_variable_value_storage.clone());

                self.put_label(start_label.clone());

                self.put_binary_op(check_variable_value_storage.clone(), Op::LessEq, init_variable_value_storage.to_owned(), to_expression_value_storage);
                self.put_bz(continue_label.clone(), check_variable_value_storage.clone());

                for statement in body {
                    self.build_statement(statement);
                }
                self.put_binary_op(init_variable_value_storage.to_owned(), Op::Add, init_variable_value_storage.to_owned(), by_expression_value_storage);

                self.put_jump(start_label);
                self.put_label(continue_label);
            }
            ast::Statement::ReturnStatement { expression } => {
                let variable = self.build_expression(expression);

                self.put_return(variable);
            }
        }
    }

    fn build_expression(&mut self, ast_expression: &'input ast::Expression<'input>) -> ValueStorage {
        match ast_expression {
            ast::Expression::FunctionCallExpression { name, argument_list: argument_expression_list } => {
                let mut arguments = Vec::new();
                let mut argument_types = Vec::new();

                for argument_expression in argument_expression_list {
                    let expression_value_storage = self.build_expression(argument_expression);

                    arguments.push(expression_value_storage);
                }

                for argument in &arguments {
                    argument_types.push(self.fetch_value_type(&argument));
                }

                let return_type = self.symbol_table.functions.get(*name).unwrap().return_type;
                let result_label = self.generate_local(&return_type);
                self.put_local(result_label.to_owned());

                self.put_call(name, result_label.to_owned(), arguments.clone());

                result_label
            }
            ast::Expression::VariableExpression(variable_identifier) => {
                let (variable_label, value_type) = self.fetch_variable(variable_identifier.name).unwrap();

                if !variable_identifier.use_index {
                    return variable_label;
                }

                let index_expression = self.build_expression(&variable_identifier.expression);

                let label_type = value_type.plain();
                let label = self.generate_local(&label_type);
                self.put_local(label.to_owned());

                self.put_copy_from_pointer(label.to_owned(), (variable_label, index_expression.to_owned()));

                label
            }
            ast::Expression::BinaryExpression { left_expression, operator, right_expression } => {
                let mut operand1 = self.build_expression(left_expression);
                let mut operand2 = self.build_expression(right_expression);

                let operand1_type = self.fetch_value_type(&operand1).to_owned();
                let operand2_type = self.fetch_value_type(&operand2).to_owned();

                let result_type;

                if *operator == ast::BinaryOperator::And
                    || *operator == ast::BinaryOperator::Or
                    || *operator == ast::BinaryOperator::IntDivision
                    || *operator == ast::BinaryOperator::Equal
                    || *operator == ast::BinaryOperator::NotEqual
                    || *operator == ast::BinaryOperator::Greater
                    || *operator == ast::BinaryOperator::GreaterEqual
                    || *operator == ast::BinaryOperator::Less
                    || *operator == ast::BinaryOperator::LessEqual
                {
                    result_type = ast::ValueType::Int;
                } else if *operator == ast::BinaryOperator::Division {
                    result_type = ast::ValueType::Real;
                } else if operand1_type == ast::ValueType::Real || operand2_type == ast::ValueType::Real {
                    result_type = ast::ValueType::Real;
                } else {
                    result_type = ast::ValueType::Int;
                }

                if operand1_type == ast::ValueType::Int && operand2_type == ast::ValueType::Real {
                    let temp = self.generate_local(&ast::ValueType::Real);
                    self.put_local(temp.to_owned());

                    self.put_promote(temp.to_owned(), operand1.to_owned());

                    operand1 = temp;
                } else if operand1_type == ast::ValueType::Real && operand2_type == ast::ValueType::Int {
                    let temp = self.generate_local(&ast::ValueType::Real);
                    self.put_local(temp.to_owned());

                    self.put_promote(temp.to_owned(), operand2.to_owned());

                    operand2 = temp;
                }

                let result_local = self.generate_local(&result_type);
                self.put_local(result_local.to_owned());

                let op = match operator {
                    ast::BinaryOperator::Addition => Op::Add,
                    ast::BinaryOperator::Subtraction => Op::Sub,
                    ast::BinaryOperator::Multiplication => Op::Mul,
                    ast::BinaryOperator::Division => Op::Div,
                    ast::BinaryOperator::Mod => Op::Mod,
                    ast::BinaryOperator::IntDivision => Op::IntDiv,
                    ast::BinaryOperator::Equal => Op::Eq,
                    ast::BinaryOperator::NotEqual => Op::NotEq,
                    ast::BinaryOperator::Less => Op::Less,
                    ast::BinaryOperator::LessEqual => Op::LessEq,
                    ast::BinaryOperator::Greater => Op::Greater,
                    ast::BinaryOperator::GreaterEqual => Op::GreaterEq,
                    ast::BinaryOperator::And => Op::And,
                    ast::BinaryOperator::Or => Op::Or,
                };

                self.put_binary_op(result_local.to_owned(), op, operand1.to_owned(), operand2.to_owned());

                result_local
            }
            ast::Expression::UnaryExpression { expression, operator } => {
                let operand = self.build_expression(expression);

                let value_type = self.fetch_value_type(&operand);

                let result_local = self.generate_local(&value_type);
                self.put_local(result_local.to_owned());

                let op = match operator {
                    ast::UnaryOperator::Negative => Op::Negative,
                    ast::UnaryOperator::Not => Op::Not,
                };

                self.put_unary_op(result_local.to_owned(), op, operand.to_owned());

                result_local
            }
            ast::Expression::IntExpression(value) => self.int_map.get(format!("{}", value).as_str()).unwrap().to_owned(),
            ast::Expression::RealExpression(value) => self.real_map.get(format!("{}", value).as_str()).unwrap().to_owned(),
            ast::Expression::Empty => unreachable!(),
        }
    }

    fn build_function(&mut self, ast_function: &'input ast::Function) {
        self.current_function = Some(&ast_function.name);

        self.put_function(&ast_function.name);

        for parameter in &ast_function.parameter_list {
            let parameter_value_storage = self.generate_local(&parameter.value_type);
            self.put_param(parameter_value_storage.to_owned());

            self.set_current_function_variable_storage_map_item(parameter.name, parameter_value_storage);
        }

        let return_variable_storage = self.generate_local(&ast_function.return_type);
        self.put_local(return_variable_storage);

        for declaration in &ast_function.declaration_list {
            for variable in &declaration.variable_list {
                let variable_value_storage = self.generate_local(&variable.value_type);
                self.put_local(variable_value_storage.to_owned());

                self.set_current_function_variable_storage_map_item(variable.name, variable_value_storage);
            }
        }

        for ast_statement in &ast_function.statement_list {
            self.build_statement(ast_statement);
        }

        self.put_end_function();
    }

    fn build_declaration(&mut self, ast_declaration: &'input ast::Declaration<'input>) {
        for variable in &ast_declaration.variable_list {
            let value_type = variable.value_type.clone();

            let label = self.generate_var(&value_type);
            self.put_var(label.to_owned());

            self.variable_storage_map.insert(variable.name, label);
        }
    }

    fn initialize_consts(&mut self) {
        for s in &self.symbol_table.strings {
            let value_storage = self.generate_const();

            self.put_const(value_storage.to_owned(), ConstValue::String(s));

            self.string_map.insert(s, value_storage.to_owned());
            self.const_map.insert(value_storage, ast::ValueType::String(s.len() as u64));
        }

        for (k, v) in &self.symbol_table.ints {
            let value_storage = self.generate_const();

            self.put_const(value_storage.to_owned(), ConstValue::Int(*v));

            self.int_map.insert(k, value_storage.to_owned());
            self.const_map.insert(value_storage, ast::ValueType::Int);
        }

        for (k, v) in &self.symbol_table.reals {
            let value_storage = self.generate_const();

            self.put_const(value_storage.to_owned(), ConstValue::Real(*v));

            self.real_map.insert(k, value_storage.to_owned());
            self.const_map.insert(value_storage, ast::ValueType::Real);
        }
    }

    pub fn build(ast_program: &'input ast::Program<'input>, symbol_table: &'input symbol_table::SymbolTable<'input>) -> IRContext<'input> {
        let mut ir_context = IRContext::new(symbol_table);

        ir_context.initialize_consts();

        for ast_declaration in &ast_program.declaration_list {
            ir_context.build_declaration(&mut &ast_declaration);
        }

        ir_context.put_start();

        for ast_function in &ast_program.function_list {
            let current_function = Function::new(&ast_function.name, &ast_function.return_type);

            ir_context.function_map.insert(&ast_function.name, current_function);
        }

        for ast_function in &ast_program.function_list {
            ir_context.build_function(ast_function);
        }

        return ir_context;
    }
}
