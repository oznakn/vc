use std::collections::{HashSet, HashMap};
use std::fmt;

use crate::ast;

pub type VariableLabel = i64;
pub type Label = String;
pub type VariablePointer = (VariableLabel, VariableLabel);

#[derive(Clone, Debug)]
pub struct Function {
    name: String,
    return_type: ast::VariableType,
    stack: Vec<ast::VariableType>,
    stack_map: HashMap<VariableLabel, ast::VariableType>,
    variable_map: HashMap<String, VariableLabel>,
}

#[derive(Clone, Debug)]
pub struct IRContext {
    items: Vec<IRItem>,
    var_map: HashMap<VariableLabel, ast::VariableType>,
    variable_map: HashMap<String, VariableLabel>,
}

impl IRContext {
    fn new() -> Self {
        return IRContext {
            items: Vec::new(),
            var_map: HashMap::new(),
            variable_map: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Builder {
    function_return_types: HashMap<String, ast::VariableType>,
    labels: HashSet<String>,
    counter: i64,
    stack_recycle: Vec<usize>,
    non_recyclable_items: HashSet<i64>,
}

#[derive(Clone, Debug)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Div2,
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
            Op::Div2 => write!(f, "div"),
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
    Label(Label),
    Function(Label, Function),
    Local(VariableLabel, u64),
    Jump(Label),
    Load(VariableLabel, i64),
    Move(VariableLabel, VariableLabel),
    Store(VariablePointer, VariableLabel),
    Fetch(VariableLabel, VariablePointer),
    Bz(Label, VariableLabel),
    Var(VariableLabel, u64),
    Promote(VariableLabel, VariableLabel),
    Op(VariableLabel, Op, VariableLabel, VariableLabel),
    Print(VariableLabel),
    PrintString(String),
    Read(VariableLabel, u64),
    Call(Label, VariableLabel, Vec<VariableLabel>),
    Return(),
}

impl fmt::Display for IRItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IRItem::Label(label) => write!(f, "{}:", label),
            IRItem::Function(label, _) => write!(f, "{}:", label),
            IRItem::Local(variable, size) => write!(f, "local(.v{}, {})", variable, size),
            IRItem::Jump(label) => write!(f, "jump({})", label),
            IRItem::Load(variable, value) => write!(f, "load(.v{}, {})", variable, value),
            IRItem::Move(to, from) => write!(f, "move(.v{}, .v{})", to, from),
            IRItem::Store(to, from) => write!(f, "store(.v{}[.v{}], .v{})", to.0, to.1, from),
            IRItem::Fetch(to, from) => write!(f, "move(.v{}, .v{}[.v{}])", to, from.0, from.1),
            IRItem::Bz(label, variable) => write!(f, "bz({}, .v{})", label, variable),
            IRItem::Var(variable, size) => write!(f, "var(.v{}, {})", variable, size),
            IRItem::Promote(to, from) => write!(f, "promote(.v{}, .v{})", to, from),
            IRItem::Op(target,op, operand1, operand2) => write!(f, "op(.v{}, {}, .v{}, .v{})", target, op, operand1, operand2),
            IRItem::Print(label) => write!(f, "print(.v{})", label),
            IRItem::PrintString(s) => write!(f, "print({})", s),
            IRItem::Read(label, size) => write!(f, "read(.v{}, {})", label, size),
            IRItem::Return() => write!(f, "return()"),
            IRItem::Call(label, return_label, arguments) => write!(f, "call({}, .v{}, {:?})", label, return_label, arguments),
        }
    }
}

impl<'input> Function {
    fn new(name: &'input str, return_type: &'input ast::VariableType) -> Self {
        return Function {
            name: String::from(name),
            return_type: return_type.clone(),
            stack: Vec::new(),
            stack_map: HashMap::new(),
            variable_map: HashMap::new(),
        }
    }
}

impl<'input> Builder {
    fn new() -> Self {
        return Builder {
            function_return_types: HashMap::new(),
            labels: HashSet::new(),
            stack_recycle: Vec::new(),
            non_recyclable_items: HashSet::new(),
            counter: -1,
        };
    }

    fn generate_label(&mut self, prefix: &str, suffix: u64) -> Label {
        let label;

        if suffix == 0 {
            label = prefix.to_string();
        } else {
            label = format!("{}_{}", prefix, suffix);
        }

        if let Some(_) = self.labels.get(&label) {
            return self.generate_label(prefix, suffix + 1);
        } else {
            self.labels.insert(label.clone());
        }

        return label;
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

    fn generate_local(&mut self, function: &mut Function, variable_type: &'input ast::VariableType) -> VariableLabel {
        let mut index: usize = 0;
        let mut index_found = false;

        let mut i = 0;
        while i < self.stack_recycle.len() {
            if function.stack[self.stack_recycle[i]] == *variable_type {
                index = self.stack_recycle[i];
                self.stack_recycle.remove(i);

                index_found = true;
                break;
            }

            i += 1;
        }

        if !index_found {
            index = function.stack.len();
        }

        if !index_found {
            function.stack_map.insert(index as i64,variable_type.clone());
            function.stack.push(variable_type.clone());
        }

        return index as i64;
    }

    #[inline]
    fn generate_local_from_same_variable_type(&mut self, function: &mut Function, variable: &VariableLabel) -> VariableLabel {
        let variable_type = function.stack_map.get(variable).unwrap().to_owned();

        return self.generate_local(function, &variable_type);
    }

    #[inline]
    fn put_local(&self, ir_context: &mut IRContext, label: VariableLabel, variable_type: &'input ast::VariableType) {
        ir_context.items.push(IRItem::Local(label, variable_type.size()))
    }

    #[inline]
    fn put_local_from_same_variable_type(&mut self, ir_context: &mut IRContext, function: &Function, label: VariableLabel, variable: &VariableLabel) {
        let variable_type = function.stack_map.get(variable).unwrap().to_owned();

        self.put_local(ir_context, label, &variable_type);
    }

    fn recycle_local(&mut self, label: &VariableLabel) {
        if let None = self.non_recyclable_items.get(label) {
            if (*label as i64) > 0 {
                self.stack_recycle.push(*label as usize);
            }
        }
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
    fn put_print_string(&self, ir_context: &mut IRContext, s: String) {
        ir_context.items.push(IRItem::PrintString(s));
    }

    #[inline]
    fn put_read(&self, ir_context: &mut IRContext, label: VariableLabel, size: u64) {
        ir_context.items.push(IRItem::Read(label, size));
    }

    #[inline]
    fn put_load(&mut self, ir_context: &mut IRContext, to: VariableLabel, value: i64) {
        ir_context.items.push(IRItem::Load(to, value));
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

    fn fetch_variable(&mut self, ir_context: &mut IRContext, function: &mut Function, variable: &str) -> Option<(VariableLabel, ast::VariableType)> {
        if let Some(label) = function.variable_map.get(variable) {
            let variable_type = function.stack_map.get(label).unwrap().to_owned();

            return Some((*label, variable_type));
        } else if let Some(label) = ir_context.variable_map.get(variable) {
            let variable_type = ir_context.var_map.get(label).unwrap().to_owned();

            return Some((*label, variable_type));
        }

        return None;
    }

    fn build_function(&mut self, ir_context: &mut IRContext, ast_function: &'input ast::Function) {
        self.stack_recycle.clear();
        self.non_recyclable_items.clear();

        let mut function = Function::new(ast_function.name, &ast_function.return_type);
        let function_place_in_items: usize;

        let label = self.generate_label(ast_function.name, 0);
        self.put_label(ir_context, label.clone());

        function_place_in_items = ir_context.items.len() - 1;

        let return_variable = self.generate_local(&mut function, &ast_function.return_type);
        self.put_local(ir_context, return_variable, &ast_function.return_type); // no need to put non recycle list

        for parameter in &ast_function.parameter_list {
            let parameter_label = self.generate_local(&mut function, &parameter.variable_type);
            self.put_local(ir_context, parameter_label, &parameter.variable_type);

            function.variable_map.insert(String::from(parameter.name), parameter_label);
        }

        for declaration in &ast_function.declaration_list {
            for variable in &declaration.variable_list {
                let label = self.generate_local(&mut function, &variable.variable_type);
                self.put_local(ir_context, label, &variable.variable_type);

                self.put_load(ir_context, label, 0);

                function.variable_map.insert(String::from(variable.name), label);
            }
        }

        for ast_statement in &ast_function.statement_list {
            self.build_statement(ir_context, &mut function, ast_statement);
        }

        *ir_context.items.get_mut(function_place_in_items).unwrap() = IRItem::Function(label, function);
    }

    #[allow(dead_code)]
    fn build_statement(&mut self, ir_context: &mut IRContext, function: &mut Function, ast_statement: &'input ast::Statement) {
        match ast_statement {
            ast::Statement::AssignmentStatement { variable, expression } => {
                let mut result = self.build_expression(ir_context, function, expression);
                let result_type = function.stack_map.get(&result).unwrap().to_owned();

                let (variable_label, variable_type)  = self.fetch_variable(ir_context, function, variable.name).unwrap();

                if variable_type.plain() == ast::VariableType::Real && result_type == ast::VariableType::Int {
                    let temp = self.generate_local(function, &ast::VariableType::Real);
                    self.put_local(ir_context, temp, &ast::VariableType::Real);

                    self.put_move(ir_context, temp, result);

                    result = temp;
                }

                if variable_type.requires_index() {
                    let index_expression = self.build_expression(ir_context, function, &variable.expression);

                    self.put_store(ir_context, (variable_label, index_expression), result);
                } else {
                    self.put_move(ir_context, variable_label, result);
                }
            },
            ast::Statement::PrintStatement { parameter_list } => {
                let mut i: usize = 0;

                while i < parameter_list.len() {
                    let parameter = &parameter_list[i];

                    match parameter {
                        ast::Printable::Expression(e) => {
                            let label = self.build_expression(ir_context, function, e);

                            self.put_print(ir_context, label);
                        },
                        ast::Printable::String(s) => {
                            self.put_print_string(ir_context, (*s).to_owned());
                        },
                    }

                    if i == parameter_list.len() - 1 {
                        self.put_print_string(ir_context, String::from("\\n"));
                    } else {
                        self.put_print_string(ir_context, String::from(" "));
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
                        let index_expression = self.build_expression(ir_context, function, &parameter.expression);

                        let temp = self.generate_local(function, &variable_type.plain());
                        self.put_local(ir_context, temp, &variable_type.plain());

                        self.put_read(ir_context, temp, variable_type.plain().size());

                        self.put_store(ir_context, (variable_label, index_expression), temp);
                    }
                }
            },
            ast::Statement::IfStatement { expression, if_body, else_body, use_else } => {
                let if_expression_label = self.build_expression(ir_context, function, expression);

                let continue_label = self.generate_label(&function.name, 0);

                self.put_bz(ir_context, continue_label.clone(), if_expression_label);

                for statement in if_body {
                    self.build_statement(ir_context, function, statement);
                }
                self.put_label(ir_context, continue_label);

                if *use_else {
                    for statement in else_body {
                        self.build_statement(ir_context, function, statement);
                    }
                }
            },
            ast::Statement::WhileStatement { expression, body } => {
                let start_label = self.generate_label(&function.name, 0);
                let continue_label = self.generate_label(&function.name, 0);

                self.put_label(ir_context, start_label.clone());

                let expression_label = self.build_expression(ir_context, function, expression);

                self.put_bz(ir_context, continue_label.clone(), expression_label);

                for statement in body {
                    self.build_statement(ir_context, function, statement);
                }
                self.put_jump(ir_context, start_label);
                self.put_label(ir_context, continue_label);
            },
            ast::Statement::ForStatement { .. } => {
                // TODO
            },
            ast::Statement::ReturnStatement { expression } => {
                let variable = self.build_expression(ir_context, function, expression);

                self.put_move(ir_context, 0, variable);

                self.put_return(ir_context);
            }
        }
    }

    #[allow(dead_code)]
    fn build_expression(&mut self, ir_context: &mut IRContext, function: &mut Function, ast_expression: &'input ast::Expression<'input>) -> VariableLabel {
        return match ast_expression {
            ast::Expression::FunctionCallExpression { name, argument_list } => {
                let mut arguments = Vec::new();

                for argument in argument_list {
                    let expression_label = self.build_expression(ir_context, function, argument);

                    arguments.push(expression_label);
                }

                let return_type = self.function_return_types.get(*name).unwrap().clone();
                let result_label = self.generate_local(function, &return_type);
                self.put_local(ir_context, result_label, &function.return_type);

                self.put_call(ir_context, String::from(&function.name), result_label, arguments.clone());

                for argument in &arguments {
                    self.recycle_local(argument);
                }

                result_label
            },
            ast::Expression::VariableExpression(variable_identifier) => {
                let (variable_label, variable_type) = self.fetch_variable(ir_context, function, variable_identifier.name).unwrap();

                if !variable_type.requires_index() {
                    return variable_label;
                }

                let index_expression = self.build_expression(ir_context, function, &variable_identifier.expression);

                let label_type = variable_type.plain();
                let label = self.generate_local(function, &label_type);
                self.put_local(ir_context, label, &label_type);

                self.put_fetch(ir_context, label, (variable_label, index_expression));

                self.recycle_local(&index_expression);

                label
            },
            ast::Expression::BinaryExpression { left_expression, operator, right_expression} => {
                let mut operand1 = self.build_expression(ir_context, function, left_expression);
                let mut operand2 = self.build_expression(ir_context, function, right_expression);

                let mut operand1_type = function.stack_map.get(&operand1).unwrap().to_owned();
                let operand2_type = function.stack_map.get(&operand2).unwrap().to_owned();

                if operand1_type == ast::VariableType::Int && operand2_type == ast::VariableType::Real {
                    let temp = self.generate_local(function, &ast::VariableType::Real);
                    self.put_local(ir_context, temp, &ast::VariableType::Real);

                    self.put_promote(ir_context, temp, operand1);

                    self.recycle_local(&operand1);

                    operand1 = temp;
                    operand1_type = ast::VariableType::Real;

                } else if operand1_type == ast::VariableType::Real && operand2_type == ast::VariableType::Int {
                    let temp = self.generate_local(function, &ast::VariableType::Real);
                    self.put_local(ir_context, temp, &ast::VariableType::Real);

                    self.put_promote(ir_context, temp, operand2);

                    self.recycle_local(&operand2);

                    operand2 = temp;
                    // operand2_type = ast::VariableType::Real; // not needed since never used later
                }

                let result_local = self.generate_local(function, &operand1_type.to_owned());
                self.put_local(ir_context, result_local, &operand1_type.to_owned());

                match operator {
                    ast::BinaryOperator::Addition =>
                        self.put_op(ir_context, result_local, Op::Add, operand1, operand2),
                    ast::BinaryOperator::Subtraction =>
                        self.put_op(ir_context, result_local, Op::Sub, operand1, operand2),
                    ast::BinaryOperator::Multiplication =>
                        self.put_op(ir_context, result_local, Op::Mul, operand1, operand2),
                    ast::BinaryOperator::Division =>
                        self.put_op(ir_context, result_local, Op::Div, operand1, operand2),
                    _ => {},
                }

                self.recycle_local(&operand1);
                self.recycle_local(&operand2);

                result_local
            },
            ast::Expression::UnaryExpression { expression, operator: _} => {
                let operand = self.build_expression(ir_context, function, expression);

                let negate_operand = self.generate_local_from_same_variable_type(function, &operand);
                self.put_local_from_same_variable_type(ir_context, function, negate_operand, &operand);

                self.put_load(ir_context, negate_operand, -1);

                let result_local = self.generate_local_from_same_variable_type(function, &operand);
                self.put_local_from_same_variable_type(ir_context, function, result_local, &operand);

                self.put_op(ir_context, result_local, Op::Mul, negate_operand, operand);

                self.recycle_local(&operand);
                self.recycle_local(&negate_operand);

                result_local
            },
            ast::Expression::IntExpression(value) => {
                let local = self.generate_local(function, &ast::VariableType::Int);
                self.put_local(ir_context, local, &ast::VariableType::Int);

                self.put_load(ir_context, local, *value);

                local
            },
            ast::Expression::RealExpression(value) => {
                let local = self.generate_local(function, &ast::VariableType::Real);
                self.put_local(ir_context, local, &ast::VariableType::Real);

                self.put_load(ir_context, local, *(value) as i64); // TODO: real representation

                local
            },
            ast::Expression::Empty => 0,
        }
    }

    fn build_declaration(&mut self, ir_context: &mut IRContext, ast_declaration: &'input ast::Declaration<'input>) {
        for variable in &ast_declaration.variable_list {
            let variable_type = variable.variable_type.clone();

            let label = self.generate_var(ir_context, &variable_type);
            self.put_var(ir_context, label, &variable_type);

            self.put_load(ir_context, label, 0);

            ir_context.variable_map.insert(String::from(variable.name), label);
        }
    }

    pub fn build(ast_program: &'input ast::Program<'input>) -> IRContext {
        let mut builder = Builder::new();
        let mut ir_context = IRContext::new();

        let start_label = builder.generate_label("__start__", 0);
        builder.put_label(&mut ir_context, start_label);

        for ast_declaration in &ast_program.declaration_list {
            builder.build_declaration(&mut ir_context, &ast_declaration);
        }

        builder.put_jump(&mut ir_context, String::from("main"));

        for ast_function in &ast_program.function_list {
            builder.function_return_types.insert(String::from(ast_function.name), ast_function.return_type.clone());
        }

        for ast_function in &ast_program.function_list {
            builder.build_function(&mut ir_context, ast_function);
        }

        for item in &ir_context.items {
            match item {
                IRItem::Label(_) => println!("{}", item),
                IRItem::Function(_, _) => println!("{}", item),
                _ => println!("    {}", item),
            }
        }

        return ir_context;
    }
}