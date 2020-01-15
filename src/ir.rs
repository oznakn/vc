use std::collections::{HashSet, HashMap};
use std::fmt;

use crate::ast;
use crate::symbol_table;
use crate::MAIN_FUNCTION;

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum ValueStorage {
    Const(u64),
    Var(u64),
    Local(u64),
}

// Use only developmental purposes
impl fmt::Display for ValueStorage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueStorage::Const(i) => write!(f, ".c{}", i),
            ValueStorage::Var(i) => write!(f, ".v{}", i),
            ValueStorage::Local(i) => write!(f, ".l{}", i),
        }
    }
}

pub type Label = String;
pub type VariablePointer = (ValueStorage, ValueStorage);

#[derive(Clone, Debug)]
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
pub enum IRItem {
    ConstString(ValueStorage, String),
    ConstInt(ValueStorage, i64),
    ConstReal(ValueStorage, f64),
    Start(),
    Label(Label),
    Function(Label),
    EndFunction(),
    Param(ValueStorage, u64),
    Local(ValueStorage, u64),
    Jump(Label),
    Move(ValueStorage, ValueStorage),
    Store(VariablePointer, ValueStorage),
    Fetch(ValueStorage, VariablePointer),
    Bz(Label, ValueStorage),
    Var(ValueStorage, u64),
    Promote(ValueStorage, ValueStorage),
    BinaryOp(ValueStorage, Op, ValueStorage, ValueStorage),
    UnaryOp(ValueStorage, Op, ValueStorage),
    Print(ValueStorage),
    Read(ValueStorage, u64),
    Call(Label, ValueStorage, Vec<ValueStorage>),
    Return(ValueStorage),
}

impl fmt::Display for IRItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IRItem::ConstString(label, s ) =>
                write!(f, "const_string({}, \"{}\")", label, s),
            IRItem::ConstInt(label, v ) =>
                write!(f, "const_int({}, {})", label, v),
            IRItem::ConstReal(label, v ) =>
                write!(f, "const_real({}, {})", label, v),
            IRItem::Start() =>
                write!(f, "start()"),
            IRItem::Label(label) =>
                write!(f, "{}:", label),
            IRItem::Function(label) =>
                write!(f, "{}:", label),
            IRItem::EndFunction() =>
                write!(f, "endfunc()"),
            IRItem::Local(variable, size) =>
                write!(f, "local({}, {})", variable, size),
            IRItem::Param(variable, size) =>
                write!(f, "param({}, {})", variable, size),
            IRItem::Jump(label) =>
                write!(f, "jump({})", label),
            IRItem::Move(to, from) =>
                write!(f, "move({}, {})", to, from),
            IRItem::Store(to, from) =>
                write!(f, "store({}[{}], {})", &to.0, &to.1, from),
            IRItem::Fetch(to, from) =>
                write!(f, "store({}[{}], {})", to, &from.0, &from.1),
            IRItem::Bz(label, variable) =>
                write!(f, "bz({}, {})", label, variable),
            IRItem::Var(variable, size) =>
                write!(f, "var({}, {})", variable, size),
            IRItem::Promote(to, from) =>
                write!(f, "promote({}, {})", to, from),
            IRItem::BinaryOp(target, op, operand1, operand2) =>
                write!(f, "binary_op({}, {}, {}, {})", target, op, operand1, operand2),
            IRItem::UnaryOp(target, op, operand) =>
                write!(f, "unary_op({}, {}, {})", target, op, operand),
            IRItem::Print(label) =>
                write!(f, "print({})", label),
            IRItem::Read(label, size) =>
                write!(f, "read({}, {})", label, size),
            IRItem::Return(label) =>
                write!(f, "return({})", label),
            IRItem::Call(label, return_label, arguments) =>
                write!(f,
                       "call({}, {}, [{}])",
                       label,
                       return_label,
                       arguments.iter()
                           .map(|s| format!("{}", s))
                           .collect::<Vec<String>>()
                           .join(","))
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub return_type: ast::VariableType,
    pub stack_list: Vec<(ValueStorage, ast::VariableType)>,
    pub stack_map: HashMap<ValueStorage, ast::VariableType>,
    variable_label_map: HashMap<String, ValueStorage>,
}

impl<'input> Function {
    fn new(name: &'input str, return_type: &'input ast::VariableType) -> Self {
        return Function {
            name: String::from(name),
            return_type: return_type.clone(),
            stack_list: Vec::new(),
            stack_map: HashMap::new(),
            variable_label_map: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct IRContext {
    pub items: Vec<IRItem>,
    pub function_map: HashMap<String, Function>,
    var_map: HashMap<ValueStorage, ast::VariableType>,
    variable_label_map: HashMap<String, ValueStorage>,
    pub string_map: HashMap<String, (ValueStorage, u64)>,
    pub int_map: HashMap<String, ValueStorage>,
    pub real_map: HashMap<String, ValueStorage>,
    const_map: HashMap<ValueStorage, ast::VariableType>,
}

impl IRContext {
    fn new() -> Self {
        return IRContext {
            items: Vec::new(),
            function_map: HashMap::new(),
            var_map: HashMap::new(),
            variable_label_map: HashMap::new(),
            string_map: HashMap::new(),
            int_map: HashMap::new(),
            real_map: HashMap::new(),
            const_map: HashMap::new(),
        }
    }
}

pub fn fetch_variable_type(ir_context: &IRContext, function: &Function, value_storage: &ValueStorage) -> ast::VariableType {
    return match value_storage {
        ValueStorage::Local(_) => {
            function.stack_map.get(value_storage).unwrap().to_owned()
        },
        ValueStorage::Var(_) => {
            ir_context.var_map.get(value_storage).unwrap().to_owned()
        },
        ValueStorage::Const(_) => {
            ir_context.const_map.get(value_storage).unwrap().to_owned()
        }
    };
}

#[derive(Clone, Debug)]
pub struct Builder {
    labels: HashSet<String>,
    const_counter: u64,
    var_counter: u64,
}

impl<'input> Builder {
    fn new() -> Self {
        return Builder {
            labels: HashSet::new(),
            const_counter: 0,
            var_counter: 0,
        };
    }

    fn generate_label(&mut self, prefix: &str, suffix: u64) -> Label {
        let label;

        if suffix == 0 {
            label = prefix.to_string();
        } else {
            label = format!("{}__{}", prefix, suffix);
        }

        if let Some(_) = self.labels.get(&label) {
            return self.generate_label(prefix, suffix + 1);
        } else {
            self.labels.insert(label.clone());
        }

        return label;
    }

    #[inline]
    fn put_start(&mut self, ir_context: &mut IRContext) {
        ir_context.items.push(IRItem::Start());
    }

    #[inline]
    fn put_label(&mut self, ir_context: &mut IRContext, label: Label) {
        ir_context.items.push(IRItem::Label(label));
    }

    #[inline]
    fn generate_const(&mut self, _ir_context: &mut IRContext) -> ValueStorage {
        let index = self.const_counter;

        self.const_counter += 1;

        return ValueStorage::Const(index);
    }

    #[inline]
    fn put_const_string(&mut self, ir_context: &mut IRContext, label: ValueStorage, s: String) {
        ir_context.items.push(IRItem::ConstString(label, s));
    }

    #[inline]
    fn put_const_int(&mut self, ir_context: &mut IRContext, label: ValueStorage, v: i64) {
        ir_context.items.push(IRItem::ConstInt(label, v));
    }

    #[inline]
    fn put_const_real(&mut self, ir_context: &mut IRContext, label: ValueStorage, v: f64) {
        ir_context.items.push(IRItem::ConstReal(label, v));
    }

    fn generate_var(&mut self, ir_context: &mut IRContext, variable_type: &'input ast::VariableType) -> ValueStorage {
        let index = self.var_counter;

        self.var_counter += 1;

        let value_storage = ValueStorage::Var(index);

        ir_context.var_map.insert(value_storage.to_owned(), variable_type.clone());

        return value_storage;
    }

    #[inline]
    fn put_var(&mut self, ir_context: &mut IRContext, label: ValueStorage, variable_type: &'input ast::VariableType) {
        ir_context.items.push(IRItem::Var(label, variable_type.size()));
    }

    fn generate_local(&mut self, function: &mut Function, variable_type: &'input ast::VariableType) -> ValueStorage {
        let index = function.stack_list.len() as u64;

        let value_storage = ValueStorage::Local(index);

        function.stack_list.push((value_storage.to_owned(), variable_type.to_owned()));
        function.stack_map.insert(value_storage.to_owned(), variable_type.to_owned());

        return value_storage;
    }

    #[inline]
    fn put_local(&self, ir_context: &mut IRContext, label: ValueStorage, variable_type: &'input ast::VariableType) {
        ir_context.items.push(IRItem::Local(label, variable_type.size()))
    }

    #[inline]
    fn put_param(&self, ir_context: &mut IRContext, label: ValueStorage, variable_type: &'input ast::VariableType) {
        ir_context.items.push(IRItem::Param(label, variable_type.size()))
    }

    #[inline]
    fn put_jump(&self, ir_context: &mut IRContext, label: Label) {
        ir_context.items.push(IRItem::Jump(label));
    }

    #[inline]
    fn put_bz(&self, ir_context: &mut IRContext, label: Label, variable: ValueStorage) {
        ir_context.items.push(IRItem::Bz(label, variable));
    }

    #[inline]
    fn put_promote(&self, ir_context: &mut IRContext, to: ValueStorage, from: ValueStorage) {
        ir_context.items.push(IRItem::Promote(to, from));
    }

    #[inline]
    fn put_print(&self, ir_context: &mut IRContext, label: ValueStorage) {
        ir_context.items.push(IRItem::Print(label));
    }

    #[inline]
    fn put_read(&self, ir_context: &mut IRContext, label: ValueStorage, size: u64) {
        ir_context.items.push(IRItem::Read(label, size));
    }

    #[inline]
    fn put_move(&self, ir_context: &mut IRContext, to: ValueStorage, from: ValueStorage) {
        ir_context.items.push(IRItem::Move(to, from));
    }

    #[inline]
    fn put_store(&mut self, ir_context: &mut IRContext, to: VariablePointer, from: ValueStorage) {
        ir_context.items.push(IRItem::Store(to, from));
    }

    #[inline]
    fn put_fetch(&self, ir_context: &mut IRContext, to: ValueStorage, from: VariablePointer) {
        ir_context.items.push(IRItem::Fetch(to, from));
    }

    #[inline]
    fn put_binary_op(&self, ir_context: &mut IRContext, target: ValueStorage, op: Op, operand1: ValueStorage, operand2: ValueStorage) {
        ir_context.items.push(IRItem::BinaryOp(target, op, operand1, operand2));
    }

    #[inline]
    fn put_unary_op(&self, ir_context: &mut IRContext, target: ValueStorage, op: Op, operand: ValueStorage) {
        ir_context.items.push(IRItem::UnaryOp(target, op, operand));
    }

    #[inline]
    fn put_return(&self, ir_context: &mut IRContext, label: ValueStorage) {
        ir_context.items.push(IRItem::Return(label));
    }

    #[inline]
    fn put_call(&self, ir_context: &mut IRContext, label: Label, return_label: ValueStorage, params: Vec<ValueStorage>) {
        ir_context.items.push(IRItem::Call(label, return_label, params));
    }

    #[inline]
    fn put_end_function(&self, ir_context: &mut IRContext) {
        ir_context.items.push(IRItem::EndFunction());
    }

    fn generate_function_label_from_signature(&self, symbol_table: &'input symbol_table::SymbolTable<'input>, name: &'input str, arguments: &Vec<ast::VariableType>) -> Label {
        let argument_map = symbol_table.function_call_argument_map.get(name).unwrap();

        if argument_map.len() <= 1 {
            return name.to_string();
        }

        return format!(
            "{}_{}",
            name,
            arguments
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<String>>()
                .join("_"),
        ) as Label;
    }

    fn fetch_variable(&mut self, ir_context: &mut IRContext, function: &mut Function, variable: &str) -> Option<(ValueStorage, ast::VariableType)> {
        let mut value_storage_option = None;

        if let Some(_value_storage) = function.variable_label_map.get(variable) {
            value_storage_option = Some(_value_storage.to_owned());
        } else if let Some(_value_storage) = ir_context.variable_label_map.get(variable) {
            value_storage_option = Some(_value_storage.to_owned());
        }

        if let Some(value_storage) = value_storage_option {
            let variable_type = self.fetch_value_type(ir_context, function, &value_storage);

            return Some((value_storage, variable_type));
        }

        return None;
    }

    fn fetch_value_type(&mut self, ir_context: &mut IRContext, function: &mut Function, value_storage: &ValueStorage) -> ast::VariableType {
        return fetch_variable_type(ir_context, function, value_storage);
    }

    fn build_function(&mut self, ir_context: &mut IRContext, symbol_table: &'input symbol_table::SymbolTable<'input>, ast_function: &'input ast::Function, arguments: &Vec<ast::VariableType>) {
        let label_string = self.generate_function_label_from_signature(symbol_table, ast_function.name, arguments);

        let mut function = Function::new(&ast_function.name, &ast_function.return_type);
        let function_place_in_items: usize;

        let function_label = self.generate_label(ast_function.name, 0);
        self.put_label(ir_context, function_label.clone());

        function_place_in_items = ir_context.items.len() - 1;

        for parameter in &ast_function.parameter_list {
            let parameter_label = self.generate_local(&mut function, &parameter.variable_type);
            self.put_param(ir_context, parameter_label.to_owned(), &parameter.variable_type);

            function.variable_label_map.insert(parameter.name.to_string(), parameter_label);
        }

        let return_variable = self.generate_local(&mut function, &ast_function.return_type);
        self.put_local(ir_context, return_variable, &ast_function.return_type);

        for declaration in &ast_function.declaration_list {
            for variable in &declaration.variable_list {
                let label = self.generate_local(&mut function, &variable.variable_type);
                self.put_local(ir_context, label.to_owned(), &variable.variable_type);

                function.variable_label_map.insert(String::from(variable.name), label);
            }
        }

        for ast_statement in &ast_function.statement_list {
            self.build_statement(ir_context, symbol_table, &mut function, ast_statement);
        }

        self.put_end_function(ir_context);

        *ir_context.items.get_mut(function_place_in_items).unwrap() = IRItem::Function(label_string.clone());
        ir_context.function_map.insert(label_string, function);
    }

    fn build_statement(&mut self, ir_context: &mut IRContext, symbol_table: &'input symbol_table::SymbolTable<'input>, function: &mut Function, ast_statement: &'input ast::Statement) {
        match ast_statement {
            ast::Statement::AssignmentStatement { variable, expression } => {
                let mut result = self.build_expression(ir_context, symbol_table, function, expression);
                let result_type = self.fetch_value_type(ir_context, function, &result);

                let (variable_label, variable_type)  = self.fetch_variable(ir_context, function, variable.name).unwrap();

                if variable_type.plain() == ast::VariableType::Real && result_type == ast::VariableType::Int {
                    let temp = self.generate_local(function, &ast::VariableType::Real);
                    self.put_local(ir_context, temp.to_owned(), &ast::VariableType::Real);

                    self.put_move(ir_context, temp.to_owned(), result);

                    result = temp;
                }

                if variable_type.requires_index() {
                    let index_expression = self.build_expression(ir_context, symbol_table, function, &variable.expression);

                    self.put_store(ir_context, (variable_label, index_expression), result);
                } else {
                    self.put_move(ir_context, variable_label, result);
                }
            },
            ast::Statement::PrintStatement { parameter_list } => {
                let mut i: usize = 0;

                let space_string_item = ir_context.string_map.get(" ").unwrap().to_owned();
                let newline_string_item = ir_context.string_map.get("\\n").unwrap().to_owned();

                while i < parameter_list.len() {
                    let parameter = &parameter_list[i];

                    match parameter {
                        ast::Printable::Expression(e) => {
                            let label = self.build_expression(ir_context, symbol_table, function, e);

                            self.put_print(ir_context, label);
                        },
                        ast::Printable::String(s) => {
                            let string_item = ir_context.string_map.get(*s).unwrap().to_owned();

                            self.put_print(ir_context, string_item.0);
                        },
                    }

                    if i == parameter_list.len() - 1 {
                        self.put_print(ir_context, newline_string_item.0.to_owned());
                    } else {
                        self.put_print(ir_context, space_string_item.0.to_owned());
                    }

                    i += 1;
                }
            },
            ast::Statement::ReadStatement { parameter_list } => {
                for parameter in parameter_list {
                    let (variable_label, variable_type) = self.fetch_variable(ir_context, function, parameter.name).unwrap();

                    if !variable_type.requires_index() {
                        self.put_read(ir_context, variable_label, variable_type.plain().size());
                    } else {
                        let index_expression = self.build_expression(ir_context, symbol_table, function, &parameter.expression);

                        let temp = self.generate_local(function, &variable_type.plain());
                        self.put_local(ir_context, temp.to_owned(), &variable_type.plain());

                        self.put_read(ir_context, temp.to_owned(), variable_type.plain().size());

                        self.put_store(ir_context, (variable_label, index_expression), temp);
                    }
                }
            },
            ast::Statement::IfStatement { expression, if_body, else_body, use_else } => {
                let if_expression_label = self.build_expression(ir_context, symbol_table, function, expression);

                let finish_label = self.generate_label(&function.name, 0);
                let else_label = self.generate_label(&function.name, 0);

                self.put_bz(ir_context, else_label.clone(), if_expression_label);

                for statement in if_body {
                    self.build_statement(ir_context, symbol_table, function, statement);
                }
                self.put_jump(ir_context, finish_label.clone());
                self.put_label(ir_context, else_label);

                if *use_else {
                    for statement in else_body {
                        self.build_statement(ir_context, symbol_table, function, statement);
                    }
                }
                self.put_label(ir_context, finish_label);
            },
            ast::Statement::WhileStatement { expression, body } => {
                let start_label = self.generate_label(&function.name, 0);
                let continue_label = self.generate_label(&function.name, 0);

                self.put_label(ir_context, start_label.clone());

                let expression_value = self.build_expression(ir_context, symbol_table, function, expression);

                self.put_bz(ir_context, continue_label.clone(), expression_value);

                for statement in body {
                    self.build_statement(ir_context, symbol_table, function, statement);
                }
                self.put_jump(ir_context, start_label);
                self.put_label(ir_context, continue_label);
            },
            ast::Statement::ForStatement { init_variable, start_expression, to_expression, by_expression, body } => {
                let start_label = self.generate_label(&function.name, 0);
                let continue_label = self.generate_label(&function.name, 0);

                let init_variable_context = self.fetch_variable(ir_context, function, init_variable.name).unwrap();

                let start_expression_value = self.build_expression(ir_context, symbol_table, function, start_expression);

                self.put_move(ir_context, init_variable_context.0.to_owned(), start_expression_value.clone());

                let to_expression_value = self.build_expression(ir_context, symbol_table, function, to_expression);
                let by_expression_value;

                match by_expression {
                    ast::Expression::Empty => {
                        match init_variable_context.1 {
                            ast::VariableType::Int => {
                                by_expression_value = ir_context.int_map.get("1").unwrap().to_owned();
                            },
                            ast::VariableType::Real => {
                                by_expression_value = ir_context.real_map.get("1.0").unwrap().to_owned();
                            },
                            _ => unreachable!(),
                        }
                    },
                    _ => {
                        by_expression_value = self.build_expression(ir_context, symbol_table, function, by_expression);
                    }
                }

                let check_variable_value_storage = self.generate_local(function, &init_variable_context.1);
                self.put_local(ir_context, check_variable_value_storage.clone(), &init_variable_context.1);

                self.put_label(ir_context, start_label.clone());

                self.put_binary_op(ir_context, check_variable_value_storage.clone(), Op::LessEq, init_variable_context.0.to_owned(), to_expression_value);
                self.put_bz(ir_context, continue_label.clone(), check_variable_value_storage);

                for statement in body {
                    self.build_statement(ir_context, symbol_table, function, statement);
                }
                self.put_binary_op(ir_context,  start_expression_value.clone(), Op::Add,start_expression_value, by_expression_value);

                self.put_jump(ir_context, start_label);
                self.put_label(ir_context, continue_label);
            },
            ast::Statement::ReturnStatement { expression } => {
                let variable = self.build_expression(ir_context, symbol_table, function, expression);

                self.put_return(ir_context, variable);
            }
        }
    }

    fn build_expression(&mut self, ir_context: &mut IRContext, symbol_table: &'input symbol_table::SymbolTable<'input>, function: &mut Function, ast_expression: &'input ast::Expression<'input>) -> ValueStorage {
        match ast_expression {
            ast::Expression::FunctionCallExpression { name, argument_list: argument_expression_list } => {
                let mut arguments = Vec::new();
                let mut argument_types = Vec::new();

                for argument_expression in argument_expression_list {
                    let expression_value_storage = self.build_expression(ir_context, symbol_table, function, argument_expression);

                    arguments.push(expression_value_storage);
                }

                for argument in &arguments {
                    argument_types.push(self.fetch_value_type(ir_context, function, &argument));
                }

                let return_type = symbol_table.functions.get(*name).unwrap().return_type;
                let result_label = self.generate_local(function, &return_type);
                self.put_local(ir_context, result_label.to_owned(), &return_type);

                let call_label = self.generate_function_label_from_signature(symbol_table, *name, &argument_types);

                self.put_call(ir_context, call_label, result_label.to_owned(), arguments.clone());

                return result_label;
            },
            ast::Expression::VariableExpression(variable_identifier) => {
                let (variable_label, variable_type) = self.fetch_variable(ir_context, function, variable_identifier.name).unwrap();

                if !variable_identifier.use_index {
                    return variable_label;
                }

                let index_expression = self.build_expression(ir_context, symbol_table, function, &variable_identifier.expression);

                let label_type = variable_type.plain();
                let label = self.generate_local(function, &label_type);
                self.put_local(ir_context, label.to_owned(), &label_type);

                self.put_fetch(ir_context, label.to_owned(), (variable_label, index_expression.to_owned()));

                return label;
            },
            ast::Expression::BinaryExpression { left_expression, operator, right_expression} => {
                let mut operand1 = self.build_expression(ir_context, symbol_table, function, left_expression);
                let mut operand2 = self.build_expression(ir_context, symbol_table, function, right_expression);

                let operand1_type = self.fetch_value_type(ir_context, function, &operand1);
                let operand2_type = self.fetch_value_type(ir_context, function, &operand1);

                let mut result_type = operand1_type.clone();

                if *operator == ast::BinaryOperator::And || *operator == ast::BinaryOperator::Or {
                    result_type = ast::VariableType::Int;
                }
                else {
                    if operand1_type == ast::VariableType::Int && operand2_type == ast::VariableType::Real {
                        let temp = self.generate_local(function, &ast::VariableType::Real);
                        self.put_local(ir_context, temp.to_owned(), &ast::VariableType::Real);

                        self.put_promote(ir_context, temp.to_owned(), operand1.to_owned());

                        operand1 = temp;
                        result_type = ast::VariableType::Real;
                    }
                    else if operand1_type == ast::VariableType::Real && operand2_type == ast::VariableType::Int {
                        let temp = self.generate_local(function, &ast::VariableType::Real);
                        self.put_local(ir_context, temp.to_owned(), &ast::VariableType::Real);

                        self.put_promote(ir_context, temp.to_owned(), operand2.to_owned());

                        operand2 = temp;
                        result_type = ast::VariableType::Real;
                    }
                }

                let result_local = self.generate_local(function, &result_type);
                self.put_local(ir_context, result_local.to_owned(), &result_type);

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

                self.put_binary_op(ir_context, result_local.to_owned(), op, operand1.to_owned(), operand2.to_owned());

                return result_local;
            },
            ast::Expression::UnaryExpression { expression, operator } => {
                let operand = self.build_expression(ir_context, symbol_table, function, expression);

                let variable_type = self.fetch_value_type(ir_context, function, &operand);

                let result_local = self.generate_local(function, &variable_type);
                self.put_local(ir_context, result_local.to_owned(), &variable_type);

                let op = match operator {
                    ast::UnaryOperator::Negative => Op::Negative,
                    ast::UnaryOperator::Not => Op::Not,
                };

                self.put_unary_op(ir_context, result_local.to_owned(), op, operand.to_owned());

                return result_local;
            },
            ast::Expression::IntExpression(value) => {
                return ir_context.int_map.get(&format!("{}", value)).unwrap().to_owned();
            },
            ast::Expression::RealExpression(value) => {
                return ir_context.real_map.get(&format!("{}", value)).unwrap().to_owned();
            },
            ast::Expression::Empty => unreachable!(),
        };
    }

    fn build_declaration(&mut self, ir_context: &mut IRContext, ast_declaration: &'input ast::Declaration<'input>) {
        for variable in &ast_declaration.variable_list {
            let variable_type = variable.variable_type.clone();

            let label = self.generate_var(ir_context, &variable_type);
            self.put_var(ir_context, label.to_owned(), &variable_type);

            ir_context.variable_label_map.insert(String::from(variable.name), label);
        }
    }

    fn initialize_consts(&mut self, ir_context: &mut IRContext, symbol_table: &'input symbol_table::SymbolTable<'input>) {
        for s in &symbol_table.strings {
            let label = self.generate_const(ir_context);
            self.put_const_string(ir_context, label.to_owned(), (*s).to_owned());

            let size = s.to_string()
                .clone()
                .replace("\\n", " ")
                .replace("\\r", " ")
                .replace("\\\"", " ")
                .len() as u64;

            ir_context.string_map.insert((*s).to_owned(), (label.clone(), size));
            ir_context.const_map.insert(label, ast::VariableType::String(size));
        }

        for v in &symbol_table.ints {
            let label = self.generate_const(ir_context);
            self.put_const_int(ir_context, label.to_owned(), *v);

            ir_context.int_map.insert(format!("{}", v), label.clone());
            ir_context.const_map.insert(label, ast::VariableType::Int);
        }

        for v in &symbol_table.reals {
            let label = self.generate_const(ir_context);
            self.put_const_real(ir_context, label.to_owned(), *v);

            ir_context.real_map.insert(format!("{}", v), label.clone());
            ir_context.const_map.insert(label, ast::VariableType::Real);
        }
    }

    pub fn build(ast_program: &'input ast::Program<'input>, symbol_table: &'input symbol_table::SymbolTable<'input>) -> IRContext {
        let mut builder = Builder::new();
        let mut ir_context = IRContext::new();

        builder.initialize_consts(&mut ir_context, symbol_table);

        for ast_declaration in &ast_program.declaration_list {
            builder.build_declaration(&mut ir_context, &ast_declaration);
        }

        builder.put_start(&mut ir_context);

        for ast_function in &ast_program.function_list {
            let call_argument_list = symbol_table.function_call_argument_map.get(ast_function.name).unwrap();

            if call_argument_list.len() != 0 {
                for arguments in call_argument_list {
                    builder.build_function(&mut ir_context, symbol_table, ast_function, arguments);
                }
            } else if ast_function.name.eq(MAIN_FUNCTION) {
                builder.build_function(&mut ir_context, symbol_table, ast_function, &Vec::new());
            }
        }

        // For debugging purposes
        for item in &ir_context.items {
            match item {
                IRItem::Label(_) => println!("{}", item),
                IRItem::Function(_) => println!("{}", item),
                _ => println!("    {}", item),
            }
        }

        return ir_context;
    }
}