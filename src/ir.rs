use std::collections::{HashSet, HashMap, VecDeque};
use std::fmt;

use crate::ast;
use crate::symbol_table;

pub const MAIN_FUNCTION: &str = "main";

pub type VariableLabel = i64;
pub type Label = String;
pub type VariablePointer = (VariableLabel, VariableLabel);

#[inline]
pub fn format_variable_label(label: &VariableLabel) -> String {
    if *label < 0 {
        return format!("v{}", -(*label));
    }

    return format!("t{}", label);
}

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
        }
    }
}

#[derive(Clone, Debug)]
pub enum IRItem {
    Start(),
    Label(Label),
    Function(Label, Function),
    Param(VariableLabel, u64),
    Local(VariableLabel, u64),
    Jump(Label),
    LoadInt(VariableLabel, i64),
    LoadFloat(VariableLabel, f64),
    Move(VariableLabel, VariableLabel),
    Store(VariablePointer, VariableLabel),
    Fetch(VariableLabel, VariablePointer),
    Bz(Label, VariableLabel),
    Var(VariableLabel, u64),
    VarString(VariableLabel, String),
    Promote(VariableLabel, VariableLabel),
    Op(VariableLabel, Op, VariableLabel, VariableLabel),
    Print(VariableLabel),
    Read(VariableLabel, u64),
    Call(Label, VariableLabel, Vec<VariableLabel>),
    Return(),
}

impl fmt::Display for IRItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IRItem::Start() =>
                write!(f, "start()"),
            IRItem::Label(label) =>
                write!(f, "{}:", label),
            IRItem::Function(label, _) =>
                write!(f, "{}:", label),
            IRItem::Local(variable, size) =>
                write!(f, "local({}, {})", format_variable_label(variable), size),
            IRItem::Param(variable, size) =>
                write!(f, "param({}, {})", format_variable_label(variable), size),
            IRItem::Jump(label) =>
                write!(f, "jump({})", label),
            IRItem::LoadInt(variable, value) =>
                write!(f, "load_int({}, {})", format_variable_label(variable), value),
            IRItem::LoadFloat(variable, value) =>
                write!(f, "load_float({}, {})", format_variable_label(variable), value),
            IRItem::Move(to, from) =>
                write!(f, "move({}, {})", format_variable_label(to), format_variable_label(from)),
            IRItem::Store(to, from) =>
                write!(f, "store({}[{}], {})", format_variable_label(&to.0), format_variable_label(&to.1), format_variable_label(from)),
            IRItem::Fetch(to, from) =>
                write!(f, "store({}[{}], {})", format_variable_label(to), format_variable_label(&from.0), format_variable_label(&from.1)),
            IRItem::Bz(label, variable) =>
                write!(f, "bz({}, {})", label, format_variable_label(variable)),
            IRItem::Var(variable, size) =>
                write!(f, "var({}, {})", format_variable_label(variable), size),
            IRItem::VarString(variable, s) =>
                write!(f, "var({}, \"{}\")", format_variable_label(variable), s),
            IRItem::Promote(to, from) =>
                write!(f, "promote({}, {})", format_variable_label(to), format_variable_label(from)),
            IRItem::Op(target,op, operand1, operand2) =>
                write!(f, "op({}, {}, {}, {})", format_variable_label(target), op, format_variable_label(operand1), format_variable_label(operand2)),
            IRItem::Print(label) =>
                write!(f, "print({})", format_variable_label(label)),
            IRItem::Read(label, size) =>
                write!(f, "read({}, {})", format_variable_label(label), size),
            IRItem::Return() =>
                write!(f, "return()"),
            IRItem::Call(label, return_label, arguments) =>
                write!(f,
                       "call({}, {}, [{}])",
                       label,
                       format_variable_label(return_label),
                       arguments.iter()
                           .map(|t|  format_variable_label(t))
                           .collect::<Vec<String>>()
                           .join(","))
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    name: String,
    return_type: ast::VariableType,
    stack_list: Vec<(VariableLabel, ast::VariableType, bool)>, // is_temp
    stack_map: HashMap<VariableLabel, (ast::VariableType, bool)>, // is_temp
    variable_label_map: HashMap<String, VariableLabel>,
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
    var_map: HashMap<VariableLabel, ast::VariableType>,
    variable_label_map: HashMap<String, VariableLabel>,
    string_map: HashMap<String, (VariableLabel, u64)>,
    function_stack_offset_map: HashMap<String, HashMap<VariableLabel, u64>>,
}

impl IRContext {
    fn new() -> Self {
        return IRContext {
            items: Vec::new(),
            var_map: HashMap::new(),
            variable_label_map: HashMap::new(),
            string_map: HashMap::new(),
            function_stack_offset_map: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Builder {
    labels: HashSet<String>,
    counter: i64,
    stack_recycle_list: Vec<usize>,
}

impl<'input> Builder {
    fn new() -> Self {
        return Builder {
            labels: HashSet::new(),
            stack_recycle_list: Vec::new(),
            counter: -1,
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

    fn generate_var(&mut self, ir_context: &mut IRContext, variable_type: &'input ast::VariableType) -> VariableLabel {
        let index = self.counter;

        self.counter -= 1;

        ir_context.var_map.insert(index, variable_type.clone());

        return index;
    }

    #[inline]
    fn put_var(&mut self, ir_context: &mut IRContext, label: VariableLabel, variable_type: &'input ast::VariableType) {
        ir_context.items.push(IRItem::Var(label, variable_type.size()));
    }

    #[inline]
    fn generate_var_string(&mut self, _ir_context: &mut IRContext) -> VariableLabel {
        let index = self.counter;

        self.counter -= 1;

        return index;
    }

    #[inline]
    fn put_var_string(&mut self, ir_context: &mut IRContext, label: VariableLabel, s: String) {
        ir_context.items.push(IRItem::VarString(label, s));
    }

    fn generate_local(&mut self, function: &mut Function, variable_type: &'input ast::VariableType) -> VariableLabel {
        let mut index: usize = 0;
        let mut index_found = false;

        let mut i = 0;
        while i < self.stack_recycle_list.len() {
            if function.stack_list[self.stack_recycle_list[i]].1 == *variable_type {
                index = self.stack_recycle_list[i];
                self.stack_recycle_list.remove(i);

                index_found = true;
                break;
            }

            i += 1;
        }

        if !index_found {
            index = function.stack_list.len();
        }

        if !index_found {
            function.stack_list.push((index as i64, variable_type.to_owned(), false));
            function.stack_map.insert(index as i64, (variable_type.to_owned(), false));
        }

        return index as i64;
    }

    #[inline]
    fn put_local(&self, ir_context: &mut IRContext, label: VariableLabel, variable_type: &'input ast::VariableType) {
        ir_context.items.push(IRItem::Local(label, variable_type.size()))
    }

    fn recycle_local(&mut self, _ir_context: &mut IRContext, function: &Function, label: &VariableLabel) {
        if function.stack_map.get(label).unwrap().1 {
            if (*label as i64) > 0 {
                self.stack_recycle_list.push(*label as usize);
            } else {
                unreachable!();
            }
        }
    }

    #[inline]
    fn put_param(&self, ir_context: &mut IRContext, label: VariableLabel, variable_type: &'input ast::VariableType) {
        ir_context.items.push(IRItem::Param(label, variable_type.size()))
    }

    #[inline]
    fn put_jump(&self, ir_context: &mut IRContext, label: Label) {
        ir_context.items.push(IRItem::Jump(label));
    }

    #[inline]
    fn put_bz(&self, ir_context: &mut IRContext, label: Label, variable: VariableLabel) {
        ir_context.items.push(IRItem::Bz(label, variable));
    }

    #[inline]
    fn put_promote(&self, ir_context: &mut IRContext, to: VariableLabel, from: VariableLabel) {
        ir_context.items.push(IRItem::Promote(to, from));
    }

    #[inline]
    fn put_print(&self, ir_context: &mut IRContext, label: VariableLabel) {
        ir_context.items.push(IRItem::Print(label));
    }

    #[inline]
    fn put_read(&self, ir_context: &mut IRContext, label: VariableLabel, size: u64) {
        ir_context.items.push(IRItem::Read(label, size));
    }

    #[inline]
    fn put_load_int(&mut self, ir_context: &mut IRContext, to: VariableLabel, value: i64) {
        ir_context.items.push(IRItem::LoadInt(to, value));
    }

    #[inline]
    fn put_load_float(&mut self, ir_context: &mut IRContext, to: VariableLabel, value: f64) {
        ir_context.items.push(IRItem::LoadFloat(to, value));
    }

    #[inline]
    fn put_move(&self, ir_context: &mut IRContext, to: VariableLabel, from: VariableLabel) {
        ir_context.items.push(IRItem::Move(to, from));
    }

    #[inline]
    fn put_store(&mut self, ir_context: &mut IRContext, to: VariablePointer, from: VariableLabel) {
        ir_context.items.push(IRItem::Store(to, from));
    }

    #[inline]
    fn put_fetch(&self, ir_context: &mut IRContext, to: VariableLabel, from: VariablePointer) {
        ir_context.items.push(IRItem::Fetch(to, from));
    }

    #[inline]
    fn put_op(&self, ir_context: &mut IRContext, target: VariableLabel, op: Op, operand1: VariableLabel, operand2: VariableLabel) {
        ir_context.items.push(IRItem::Op(target, op, operand1, operand2));
    }

    #[inline]
    fn put_return(&self, ir_context: &mut IRContext) {
        ir_context.items.push(IRItem::Return());
    }

    #[inline]
    fn put_call(&self, ir_context: &mut IRContext, label: Label, return_label: VariableLabel, params: Vec<VariableLabel>) {
        ir_context.items.push(IRItem::Call(label, return_label, params));
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

    fn fetch_variable(&mut self, ir_context: &mut IRContext, function: &mut Function, variable: &str) -> Option<(VariableLabel, ast::VariableType)> {
        if let Some(label) = function.variable_label_map.get(variable) {
            let variable_type = function.stack_map.get(label).unwrap().0.to_owned();

            return Some((*label, variable_type));
        } else if let Some(label) = ir_context.variable_label_map.get(variable) {
            let variable_type = ir_context.var_map.get(label).unwrap().to_owned();

            return Some((*label, variable_type));
        }

        return None;
    }

    fn build_function(&mut self, ir_context: &mut IRContext, symbol_table: &'input symbol_table::SymbolTable<'input>, ast_function: &'input ast::Function, arguments: &Vec<ast::VariableType>) {
        self.stack_recycle_list.clear();

        let label_string = self.generate_function_label_from_signature(symbol_table, ast_function.name, arguments);

        let mut function = Function::new(&ast_function.name, &ast_function.return_type);
        let function_place_in_items: usize;

        let function_label = self.generate_label(ast_function.name, 0);
        self.put_label(ir_context, function_label.clone());

        function_place_in_items = ir_context.items.len() - 1;

        let return_variable = self.generate_local(&mut function, &ast_function.return_type);
        self.put_local(ir_context, return_variable, &ast_function.return_type); // no need to put non recycle list

        for parameter in &ast_function.parameter_list {
            let parameter_label = self.generate_local(&mut function, &parameter.variable_type);
            self.put_param(ir_context, parameter_label, &parameter.variable_type);

            function.variable_label_map.insert(String::from(parameter.name), parameter_label);
        }

        for declaration in &ast_function.declaration_list {
            for variable in &declaration.variable_list {
                let label = self.generate_local(&mut function, &variable.variable_type);
                self.put_local(ir_context, label, &variable.variable_type);

                function.variable_label_map.insert(String::from(variable.name), label);
            }
        }

        for ast_statement in &ast_function.statement_list {
            self.build_statement(ir_context, symbol_table, &mut function, ast_statement);
        }

        *ir_context.items.get_mut(function_place_in_items).unwrap() = IRItem::Function(label_string, function);
    }

    #[allow(dead_code)]
    fn build_statement(&mut self, ir_context: &mut IRContext, symbol_table: &'input symbol_table::SymbolTable<'input>, function: &mut Function, ast_statement: &'input ast::Statement) {
        match ast_statement {
            ast::Statement::AssignmentStatement { variable, expression } => {
                let mut result = self.build_expression(ir_context, symbol_table, function, expression);
                let result_type = function.stack_map.get(&result).unwrap().0.to_owned();

                let (variable_label, variable_type)  = self.fetch_variable(ir_context, function, variable.name).unwrap();

                if variable_type.plain() == ast::VariableType::Real && result_type == ast::VariableType::Int {
                    let temp = self.generate_local(function, &ast::VariableType::Real);
                    self.put_local(ir_context, temp, &ast::VariableType::Real);

                    self.put_move(ir_context, temp, result);

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

                    if i != parameter_list.len() - 1 {
                        self.put_print(ir_context, space_string_item.0);
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
                        self.put_local(ir_context, temp, &variable_type.plain());

                        self.put_read(ir_context, temp, variable_type.plain().size());

                        self.put_store(ir_context, (variable_label, index_expression), temp);
                    }
                }
            },
            ast::Statement::IfStatement { expression, if_body, else_body, use_else } => {
                let if_expression_label = self.build_expression(ir_context, symbol_table, function, expression);

                let continue_label = self.generate_label(&function.name, 0);

                self.put_bz(ir_context, continue_label.clone(), if_expression_label);

                for statement in if_body {
                    self.build_statement(ir_context, symbol_table, function, statement);
                }
                self.put_label(ir_context, continue_label);

                if *use_else {
                    for statement in else_body {
                        self.build_statement(ir_context, symbol_table, function, statement);
                    }
                }
            },
            ast::Statement::WhileStatement { expression, body } => {
                let start_label = self.generate_label(&function.name, 0);
                let continue_label = self.generate_label(&function.name, 0);

                self.put_label(ir_context, start_label.clone());

                let expression_label = self.build_expression(ir_context, symbol_table, function, expression);

                self.put_bz(ir_context, continue_label.clone(), expression_label);

                for statement in body {
                    self.build_statement(ir_context, symbol_table, function, statement);
                }
                self.put_jump(ir_context, start_label);
                self.put_label(ir_context, continue_label);
            },
            ast::Statement::ForStatement { .. } => {
                // TODO
            },
            ast::Statement::ReturnStatement { expression } => {
                let variable = self.build_expression(ir_context, symbol_table, function, expression);

                self.put_move(ir_context, 0, variable);

                self.put_return(ir_context);
            }
        }
    }

    #[allow(dead_code)]
    fn build_expression(&mut self, ir_context: &mut IRContext, symbol_table: &'input symbol_table::SymbolTable<'input>, function: &mut Function, ast_expression: &'input ast::Expression<'input>) -> VariableLabel {
        return match ast_expression {
            ast::Expression::FunctionCallExpression { name, argument_list: argument_expression_list } => {
                let mut arguments = Vec::new();
                let mut argument_types = Vec::new();

                for argument_expression in argument_expression_list {
                    let expression_label = self.build_expression(ir_context, symbol_table, function, argument_expression);

                    arguments.push(expression_label);
                }

                for argument in &arguments {
                    argument_types.push(function.stack_map.get(&argument).unwrap().0.to_owned());
                }

                let return_type = symbol_table.functions.get(*name).unwrap().return_type;
                let result_label = self.generate_local(function, &return_type);
                self.put_local(ir_context, result_label, &return_type);

                let call_label = self.generate_function_label_from_signature(symbol_table, *name, &argument_types);

                self.put_call(ir_context, call_label, result_label, arguments.clone());

                for argument in &arguments {
                    self.recycle_local(ir_context, function, argument);
                }

                result_label
            },
            ast::Expression::VariableExpression(variable_identifier) => {
                let (variable_label, variable_type) = self.fetch_variable(ir_context, function, variable_identifier.name).unwrap();

                if !variable_identifier.use_index {
                    return variable_label;
                }

                let index_expression = self.build_expression(ir_context, symbol_table, function, &variable_identifier.expression);

                let label_type = variable_type.plain();
                let label = self.generate_local(function, &label_type);
                self.put_local(ir_context, label, &label_type);

                self.put_fetch(ir_context, label, (variable_label, index_expression));

                self.recycle_local(ir_context, function, &index_expression);

                label
            },
            ast::Expression::BinaryExpression { left_expression, operator, right_expression} => {
                let mut operand1 = self.build_expression(ir_context, symbol_table, function, left_expression);
                let mut operand2 = self.build_expression(ir_context, symbol_table, function, right_expression);

                let mut operand1_type = function.stack_map.get(&operand1).unwrap().0.to_owned();
                let operand2_type = function.stack_map.get(&operand2).unwrap().0.to_owned();

                if operand1_type == ast::VariableType::Int && operand2_type == ast::VariableType::Real {
                    let temp = self.generate_local(function, &ast::VariableType::Real);
                    self.put_local(ir_context, temp, &ast::VariableType::Real);

                    self.put_promote(ir_context, temp, operand1);

                    self.recycle_local(ir_context, function, &operand1);

                    operand1 = temp;
                    operand1_type = ast::VariableType::Real;

                } else if operand1_type == ast::VariableType::Real && operand2_type == ast::VariableType::Int {
                    let temp = self.generate_local(function, &ast::VariableType::Real);
                    self.put_local(ir_context, temp, &ast::VariableType::Real);

                    self.put_promote(ir_context, temp, operand2);

                    self.recycle_local(ir_context, function, &operand2);

                    operand2 = temp;
                    // operand2_type = ast::VariableType::Real; // not needed since never used later
                }

                let result_local = self.generate_local(function, &operand1_type);
                self.put_local(ir_context, result_local, &operand1_type);

                match operator {
                    ast::BinaryOperator::Addition =>
                        self.put_op(ir_context, result_local, Op::Add, operand1, operand2),
                    ast::BinaryOperator::Subtraction =>
                        self.put_op(ir_context, result_local, Op::Sub, operand1, operand2),
                    ast::BinaryOperator::Multiplication =>
                        self.put_op(ir_context, result_local, Op::Mul, operand1, operand2),
                    ast::BinaryOperator::Division =>
                        self.put_op(ir_context, result_local, Op::Div, operand1, operand2),
                    _ => {}, // TODO: add more operator
                }

                self.recycle_local(ir_context, function, &operand1);
                self.recycle_local(ir_context, function, &operand2);

                result_local
            },
            ast::Expression::UnaryExpression { expression, operator: _} => {
                let operand = self.build_expression(ir_context, symbol_table, function, expression);

                let variable_type = function.stack_map.get(&operand).unwrap().0.to_owned();

                let negate_operand = self.generate_local(function, &variable_type);
                self.put_local(ir_context, negate_operand, &variable_type);

                self.put_load_int(ir_context, negate_operand, -1);

                let result_local = self.generate_local(function, &variable_type);
                self.put_local(ir_context, result_local, &variable_type);

                self.put_op(ir_context, result_local, Op::Mul, negate_operand, operand);

                self.recycle_local(ir_context, function, &operand);
                self.recycle_local(ir_context, function,&negate_operand);

                result_local
            },
            ast::Expression::IntExpression(value) => {
                let variable_type = ast::VariableType::Int;

                let local = self.generate_local(function, &variable_type);
                self.put_local(ir_context, local, &variable_type);

                self.put_load_int(ir_context, local, *value);

                local
            },
            ast::Expression::RealExpression(value) => {
                let variable_type = ast::VariableType::Real;

                let local = self.generate_local(function, &variable_type);
                self.put_local(ir_context, local, &variable_type);

                self.put_load_float(ir_context, local, *value);

                local
            },
            ast::Expression::Empty => unreachable!(),
        }
    }

    fn build_declaration(&mut self, ir_context: &mut IRContext, ast_declaration: &'input ast::Declaration<'input>) {
        for variable in &ast_declaration.variable_list {
            let variable_type = variable.variable_type.clone();

            let label = self.generate_var(ir_context, &variable_type);
            self.put_var(ir_context, label, &variable_type);

            ir_context.variable_label_map.insert(String::from(variable.name), label);
        }
    }

    pub fn initialize_stack(&mut self, ir_context: &mut IRContext) {
        let mut current_function = None;
        let mut local_queue = VecDeque::new();

        let mut function_map = HashMap::new();
        let mut stack_offset_map = HashMap::new();

        for item in &ir_context.items {
            match item {
                IRItem::Function(_, f) =>  {
                    current_function = Some(f);
                },
                IRItem::Local(l, s) => {
                    local_queue.push_back((l, s));
                },
                IRItem::Return() => {
                    let mut offset: u64 = 0;

                    while let Some((l, s)) = local_queue.pop_back() {
                        stack_offset_map.insert(*l, offset);
                        offset += *s;
                    }

                    if let Some(f) = current_function {
                        function_map.insert(f.name.to_owned(), stack_offset_map);
                    }
                    stack_offset_map = HashMap::new();
                },
                _ => {},
            }
        }

        ir_context.function_stack_offset_map = function_map;
    }

    pub fn build(ast_program: &'input ast::Program<'input>, symbol_table: &'input symbol_table::SymbolTable<'input>) -> IRContext {
        let mut builder = Builder::new();
        let mut ir_context = IRContext::new();

        for s in &symbol_table.strings {
            let label = builder.generate_var_string(&mut ir_context);
            builder.put_var_string(&mut ir_context, label, (*s).to_owned());

            ir_context.string_map.insert((*s).to_owned(), (label, s.len() as u64));
        }

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

        builder.initialize_stack(&mut ir_context);

        // For debugging purposes
        /* for item in &ir_context.items {
            match item {
                IRItem::Label(_) => println!("{}", item),
                IRItem::Function(_, _) => println!("{}", item),
                _ => println!("    {}", item),
            }
        } */

        return ir_context;
    }
}