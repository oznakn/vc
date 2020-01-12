use std::collections::{HashSet, HashMap};
use std::fmt;

use crate::ast;

pub type VariableLabel = String;
pub type Label = String;

const RETURN_VARIABLE: &'static str = "__return__";

#[derive(Clone, Debug)]
pub struct Function {
    stack: Vec<ast::VariableType>,
    stack_map: HashMap<VariableLabel, ast::VariableType>,
    stack_recycle: Vec<usize>,
    variable_map: HashMap<String, VariableLabel>,
}

#[derive(Clone, Debug)]
pub struct Builder {
    labels: HashSet<String>,
    var_counter: usize,
    var_map: HashMap<VariableLabel, ast::VariableType>,
    variable_map: HashMap<String, VariableLabel>,
}

#[derive(Clone, Debug)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
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
    Bz(Label, VariableLabel),
    Bnz(Label, VariableLabel),
    Var(VariableLabel, u64),
    Promote(VariableLabel, VariableLabel),
    Op(VariableLabel, Op, VariableLabel, VariableLabel),
    Print(VariableLabel),
    Read(VariableLabel),
    Return(),
}

impl fmt::Display for IRItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IRItem::Label(label) => write!(f, "{}:", label),
            IRItem::Function(label, _) => write!(f, "{}:", label),
            IRItem::Local(variable, size) => write!(f, "local({}, {})", variable, size),
            IRItem::Jump(label) => write!(f, "jump({})", label),
            IRItem::Load(variable, value) => write!(f, "load({}, {})", variable, value),
            IRItem::Move(to, from) => write!(f, "move({}, {})", to, from),
            IRItem::Bz(label, variable) => write!(f, "bz({}, {})", label, variable),
            IRItem::Bnz(label, variable) => write!(f, "bnz({}, {})", label, variable),
            IRItem::Var(variable, size) => write!(f, "var({}, {})", variable, size),
            IRItem::Promote(to, from) => write!(f, "promote({}, {})", to, from),
            IRItem::Op(target,op, operand1, operand2) => write!(f, "op({}, {}, {}, {})", target, op, operand1, operand2),
            IRItem::Return() => write!(f, "return()"),
            _ => write!(f, ""),
        }
    }
}

impl Function {
    fn new() -> Self {
        return Function {
            stack: Vec::new(),
            stack_map: HashMap::new(),
            stack_recycle: Vec::new(),
            variable_map: HashMap::new(),
        }
    }
}

impl<'input> Builder {
    fn new() -> Self {
        return Builder {
            labels: HashSet::new(),
            variable_map: HashMap::new(),
            var_map: HashMap::new(),
            var_counter: 0,
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
    fn new_label(&mut self, items: &mut Vec<IRItem>, label: Label) {
        items.push(IRItem::Label(label));
    }

    fn generate_var(&mut self, variable_type: &'input ast::VariableType) -> VariableLabel {
        let label = format!("s{}", self.var_counter);

        self.var_counter += 1;

        self.var_map.insert(label.clone(), variable_type.clone());

        return label;
    }

    #[inline]
    fn new_var(&mut self, items: &mut Vec<IRItem>, label: VariableLabel, variable_type: &'input ast::VariableType) {
        items.push(IRItem::Var(label.clone(), variable_type.size()));
    }

    fn generate_local(&mut self, function: &mut Function, variable_type: &'input ast::VariableType) -> VariableLabel {
        let mut index: usize = 0;
        let mut index_found = false;

        let mut i = 0;
        while i < function.stack_recycle.len() {
            if function.stack[function.stack_recycle[i]] == *variable_type {
                index = function.stack_recycle[i];
                function.stack_recycle.remove(i);

                index_found = true;
                break;
            }

            i += 1;
        }

        if !index_found {
            index = function.stack.len();
        }

        let label = format!("t{}", index);

        if !index_found {
            function.stack.push(variable_type.clone());
            function.stack_map.insert(label.clone(),variable_type.clone());
        }

        return label;
    }

    #[inline]
    fn generate_local_from_same_variable_type(&mut self, function: &mut Function, variable: &VariableLabel) -> VariableLabel {
        let variable_type = function.stack_map.get(variable).unwrap().to_owned();

        return self.generate_local(function, &variable_type);
    }

    #[inline]
    fn new_local(&self, items: &mut Vec<IRItem>, label: VariableLabel, variable_type: &'input ast::VariableType) {
        items.push(IRItem::Local(label, variable_type.size()))
    }

    #[inline]
    fn new_local_from_same_variable_type(&mut self, items: &mut Vec<IRItem>, function: &Function, label: VariableLabel, variable: &VariableLabel) {
        let variable_type = function.stack_map.get(variable).unwrap().to_owned();

        self.new_local(items, label, &variable_type);
    }

    #[inline]
    fn recycle_local(&mut self, function: &mut Function, index: usize) {
        function.stack_recycle.push(index);
    }

    #[inline]
    fn new_jump(&self, items: &mut Vec<IRItem>, label: Label) {
        items.push(IRItem::Jump(label));
    }

    #[inline]
    fn new_bz(&self, items: &mut Vec<IRItem>, label: Label, variable: VariableLabel) {
        items.push(IRItem::Bz(label, variable));
    }

    #[inline]
    fn new_bnz(&self, items: &mut Vec<IRItem>, label: Label, variable: VariableLabel) {
        items.push(IRItem::Bnz(label, variable));
    }

    #[inline]
    fn new_promote(&self, items: &mut Vec<IRItem>, to: VariableLabel, from: VariableLabel) {
        items.push(IRItem::Promote(to, from));
    }

    #[inline]
    fn new_load(&mut self, items: &mut Vec<IRItem>, to: VariableLabel, value: i64) {
        items.push(IRItem::Load(to, value));
    }

    #[inline]
    fn new_move(&self, items: &mut Vec<IRItem>, to: VariableLabel, from: VariableLabel) {
        items.push(IRItem::Move(to, from));
    }

    #[inline]
    fn new_op(&self, items: &mut Vec<IRItem>, target: VariableLabel, op: Op, operand1: VariableLabel, operand2: VariableLabel) {
        items.push(IRItem::Op(target, op, operand1, operand2));
    }

    #[inline]
    fn new_return(&self, items: &mut Vec<IRItem>) {
        items.push(IRItem::Return());
    }

    fn fetch_variable(&mut self, function: &mut Function, variable: &str) -> Option<(String, ast::VariableType)> {
        let mut variable_label_option: Option<&VariableLabel> = None;
        let mut variable_type_option: Option<&ast::VariableType> = None;

        variable_label_option = function.variable_map.get(variable);

        if let Some(_label) = variable_label_option {
            variable_type_option = function.stack_map.get(_label);
        } else {
            variable_label_option = self.variable_map.get(variable);

            if let Some(_label) = variable_label_option {
                variable_type_option = self.var_map.get(_label);
            }
        }

        if let Some(_variable_label) = variable_label_option {
            return Some((_variable_label.to_owned(), variable_type_option.unwrap().to_owned()));
        }

        return None;
    }

    fn build_function(&mut self, items: &mut Vec<IRItem>, ast_function: &'input ast::Function) {
        let mut function = Function::new();
        let mut function_place_in_items: usize;

        let label = self.generate_label(ast_function.name, 0);
        self.new_label(items, label.clone());

        function_place_in_items = items.len() - 1;

        self.new_local(items, String::from(RETURN_VARIABLE), &ast_function.return_type);

        for parameter in &ast_function.parameter_list {
            let parameter_label = self.generate_local(&mut function, &parameter.variable_type);
            self.new_local(items, parameter_label.clone(), &parameter.variable_type);

            function.variable_map.insert(String::from(parameter.name), parameter_label);
        }

        for declaration in &ast_function.declaration_list {
            for variable in &declaration.variable_list {
                let label = self.generate_local(&mut function, &variable.variable_type);
                self.new_local(items, label.clone(), &variable.variable_type);

                self.new_load(items, label.clone(), 0);

                function.variable_map.insert(String::from(variable.name), label);
            }
        }

        for ast_statement in &ast_function.statement_list {
            self.build_statement(items, &mut function, ast_statement);
        }

        *items.get_mut(function_place_in_items).unwrap() = IRItem::Function(label, function);
    }

    #[allow(dead_code)]
    fn build_statement(&mut self, items: &mut Vec<IRItem>, function: &mut Function, ast_statement: &'input ast::Statement) {
        match ast_statement {
            ast::Statement::AssignmentStatement { variable, expression } => {
                let mut result = self.build_expression(items, function, expression);

                let (variable_label, variable_type)  = self.fetch_variable(function, variable.name).unwrap();

                let mut result_type = function.stack_map.get(&result).unwrap();

                if variable_type.plain() == ast::VariableType::Real && *result_type == ast::VariableType::Int {
                    let temp = self.generate_local(function, &ast::VariableType::Real);
                    self.new_local(items, temp.clone(), &ast::VariableType::Real);

                    self.new_move(items, temp.clone(), result);

                    result = temp;
                    result_type = &ast::VariableType::Real;
                }

                self.new_move(items, variable_label, result);
            },
            ast::Statement::PrintStatement { parameter_list } => {

            },
            ast::Statement::ReadStatement { parameter_list } => {

            },
            ast::Statement::IfStatement { expression, if_body, else_body, use_else } => {

            },
            ast::Statement::WhileStatement { expression, body } => {

            },
            ast::Statement::ForStatement { init_variable, to_expression, by_expression, body } => {

            },
            ast::Statement::ReturnStatement { expression } => {
                let variable = self.build_expression(items, function, expression);

                self.new_move(items, String::from(RETURN_VARIABLE), variable);

                self.new_return(items);
            }
        }
    }

    #[allow(dead_code)]
    fn build_expression(&mut self, items: &mut Vec<IRItem>, function: &mut Function, ast_expression: &'input ast::Expression<'input>) -> VariableLabel {
        return match ast_expression {
            /* ast::Expression::FunctionCallExpression { name, argument_list } => {

            },
            */
            /*ast::Expression::VariableExpression(variable_identifier) => {
                let (variable_label, variable_type) = self.fetch_variable(function, variable.name).unwrap();

                if !variable_type.requires_index() {
                    return variable_label;
                }

                let index_expression = self.build_expression(items, function, &variable_identifier.expression);

                let label_type = variable_type.plain();
                let label = self.generate_local(function, &label_type);
                self.new_local(items, label.clone(), &label_type);

                self.new_move(items, VariableLabel::Label(label.clone()), VariableLabel::LabelWithIndex(variable_label, index_expression));
            },*/
            ast::Expression::BinaryExpression { left_expression, operator, right_expression} => {
                let mut operand1 = self.build_expression(items, function, left_expression);
                let mut operand2 = self.build_expression(items, function, left_expression);

                let mut operand1_type = function.stack_map.get(&operand1).unwrap().to_owned();
                let mut operand2_type = function.stack_map.get(&operand2).unwrap().to_owned();

                if operand1_type == ast::VariableType::Int && operand2_type == ast::VariableType::Real {
                    let temp = self.generate_local(function, &ast::VariableType::Real);
                    self.new_local(items, temp.clone(), &ast::VariableType::Real);

                    self.new_promote(items, temp.clone(), operand1);

                    operand1 = temp;
                    operand1_type = ast::VariableType::Real;

                } else if operand1_type == ast::VariableType::Real && operand2_type == ast::VariableType::Int {
                    let temp = self.generate_local(function, &ast::VariableType::Real);
                    self.new_local(items, temp.clone(), &ast::VariableType::Real);

                    self.new_promote(items, temp.clone(), operand2);

                    operand2 = temp;
                    operand2_type = ast::VariableType::Real;
                }

                let result_local = self.generate_local(function, &operand1_type.to_owned());
                self.new_local(items, result_local.clone(), &operand1_type.to_owned());

                match operator {
                    ast::BinaryOperator::Addition => self.new_op(items, result_local.clone(), Op::Add, operand1, operand2),
                    ast::BinaryOperator::Subtraction => self.new_op(items, result_local.clone(), Op::Sub, operand1, operand2),
                    ast::BinaryOperator::Multiplication => self.new_op(items, result_local.clone(), Op::Mul, operand1, operand2),
                    ast::BinaryOperator::Division => self.new_op(items, result_local.clone(), Op::Div, operand1, operand2),
                    _ => {}
                }

                result_local
            },
            ast::Expression::UnaryExpression { expression, operator: _} => {
                let operand = self.build_expression(items, function, expression);

                let negate_operand = self.generate_local_from_same_variable_type(function, &operand);
                self.new_local_from_same_variable_type(items, function, negate_operand.clone(), &operand);

                self.new_load(items, negate_operand.clone(), -1);

                let result_local = self.generate_local_from_same_variable_type(function, &operand);
                self.new_local_from_same_variable_type(items, function, result_local.clone(), &operand);

                self.new_op(items, result_local.clone(), Op::Mul, negate_operand, operand);

                result_local
            },
            ast::Expression::IntExpression(value) => {
                let local = self.generate_local(function, &ast::VariableType::Int);
                self.new_local(items, local.clone(), &ast::VariableType::Int);

                self.new_load(items, local.clone(), *value);

                local
            },
            ast::Expression::RealExpression(value) => {
                let local = self.generate_local(function, &ast::VariableType::Real);
                self.new_local(items, local.clone(), &ast::VariableType::Real);

                self.new_load(items, local.clone(), *(value) as i64); // TODO: real representation

                local
            },
            ast::Expression::Empty => return String::from("empty"),
            _ => return String::from("empty"),
        }
    }

    fn build_declaration(&mut self, items: &mut Vec<IRItem>, ast_declaration: &'input ast::Declaration<'input>) {
        for variable in &ast_declaration.variable_list {
            let variable_type = variable.variable_type.clone();

            let label = self.generate_var(&variable_type);
            self.new_var(items,label.clone(), &variable_type);

            self.new_load(items, label.clone(), 0);

            self.variable_map.insert(String::from(variable.name), label);
        }
    }

    pub fn build(ast_program: &'input ast::Program<'input>) -> Vec<IRItem>  {
        let mut builder = Builder::new();
        let mut items: Vec<IRItem> = Vec::new();

        let start_label = builder.generate_label("__start__", 0);
        builder.new_label(&mut items, start_label);

        for ast_declaration in &ast_program.declaration_list {
            builder.build_declaration(&mut items, &ast_declaration);
        }

        builder.new_jump(&mut items, String::from("main"));

        for ast_function in &ast_program.function_list {
            builder.build_function(&mut items, ast_function);
        }

        for item in &items {
            match item {
                IRItem::Label(_) => println!("{}", item),
                IRItem::Function(_, _) => println!("{}", item),
                _ => println!("    {}", item),
            }
        }

        return items;
    }
}