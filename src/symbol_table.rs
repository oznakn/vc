use std::collections::{HashMap, HashSet};
use std::fmt;
use std::error::Error;

use crate::ast;
use colored::Colorize;

#[derive(Debug)]
pub enum SymbolTableError<'input> {
    VariableAlreadyDefinedError {
        name: &'input str,
    },
    FunctionAlreadyDefinedError {
        name: &'input str,
    },
    VariableNotFoundError {
        name: &'input str,
    },
    VariableTypesNotMatchError {
        name: &'input str,
    },
    FunctionNotFoundError {
        name: &'input str,
    },
}

impl<'input> Error for SymbolTableError<'input> {}

impl<'input> fmt::Display for SymbolTableError<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            SymbolTableError::VariableAlreadyDefinedError { name } => {
                write!(f, "Variable `{}` already defined", name.purple())
            },
            SymbolTableError::FunctionAlreadyDefinedError { name } => {
                write!(f, "Function `{}` already defined", name.purple())
            },
            SymbolTableError::VariableNotFoundError { name } => {
                write!(f, "Variable `{}` not found", name.purple())
            },
            SymbolTableError::VariableTypesNotMatchError { name } => {
                write!(f, "Wrong type detected on variable `{}`", name.purple())
            },
            SymbolTableError::FunctionNotFoundError { name } => {
                write!(f, "Function `{}` not found", name.purple())
            },
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionScope<'input> {
    pub name: &'input str,
    pub return_type: &'input ast::VariableType,
    pub parameter_list: Vec<(&'input str, &'input ast::VariableType)>,
    pub variables: HashMap<&'input str, &'input ast::VariableType>,
}

impl<'input> FunctionScope<'input> {
    fn new(name: &'input str, return_type: &'input ast::VariableType) -> Self {
        return FunctionScope {
            name,
            return_type,
            parameter_list: Vec::new(),
            variables: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SymbolTable<'input> {
    pub functions: HashMap<&'input str, FunctionScope<'input>>,
    pub variables: HashMap<&'input str, &'input ast::VariableType>,
    function_call_list: HashSet<&'input str>,
    pub function_call_argument_map: HashMap<&'input str, Vec<Vec<ast::VariableType>>>,
    pub strings: HashSet<&'input str>,
    pub ints: HashSet<i64>,
    pub reals: Vec<f64>,
}

impl<'input> SymbolTable<'input> {
    pub fn new() -> Self {
        return SymbolTable {
            functions: HashMap::new(),
            variables: HashMap::new(),
            function_call_list: HashSet::new(),
            function_call_argument_map: HashMap::new(),
            strings: HashSet::new(),
            ints: HashSet::new(),
            reals: Vec::new(),
        };
    }

    fn add_to_function_call_argument_map_if_not_exists(&mut self, function_name: &'input str, arguments: Vec<ast::VariableType>) {
        let mut found = false;

        for list_of_parameters in self.function_call_argument_map.get(function_name) {
            for parameters in list_of_parameters {
                if parameters.len() == arguments.len() {
                    let mut i: usize = 0;
                    let mut matches = true;

                    while i < parameters.len() {
                        if !parameters.get(i).unwrap().eq(arguments.get(i).unwrap()) {
                            matches = false;
                            break;
                        }

                        i += 1;
                    }

                    if matches {
                        found = true;
                        break;
                    }
                }
            }
        }

        if !found {
            self.function_call_argument_map.get_mut(function_name).unwrap().push(arguments);
        }
    }

    fn check_variable_not_exists(&self, function_scope: &FunctionScope<'input>, name: &'input str) -> Result<bool, SymbolTableError<'input>> {
        let mut found = false;

        for (p_name, _) in &function_scope.parameter_list {
            if (*p_name).eq(name) {
                found = true;
                break;
            }
        }

        found = found || function_scope.variables.contains_key(name);

        if found {
            return Err(SymbolTableError::VariableAlreadyDefinedError {
                name,
            });
        }

        return Ok(true);
    }

    #[inline]
    fn check_variable_type_matches(&mut self, function_scope: &FunctionScope<'input>, variable_identifier: &'input ast::VariableIdentifier<'input>) -> Result<ast::VariableType, SymbolTableError<'input>> {
        if let Some(v) = function_scope.variables.get(variable_identifier.name) {
            if !v.requires_index() && variable_identifier.use_index {
                return Err(SymbolTableError::VariableTypesNotMatchError {
                    name: variable_identifier.name,
                });
            }

            if variable_identifier.use_index {
                return Ok((*v).plain());
            }

            return Ok((*v).clone());
        }

        for (p_name, p_type) in &function_scope.parameter_list {
            if (*p_name).eq(variable_identifier.name) {
                if !p_type.requires_index() && variable_identifier.use_index {
                    return Err(SymbolTableError::VariableTypesNotMatchError {
                        name: variable_identifier.name,
                    });
                }

                if variable_identifier.use_index {
                    return Ok((*p_type).plain());
                }

                return Ok((*p_type).clone());
            }
        }

        if let Some(v) = self.variables.get(variable_identifier.name) {
            if !v.requires_index() && variable_identifier.use_index {
                return Err(SymbolTableError::VariableTypesNotMatchError {
                    name: variable_identifier.name,
                });
            }

            if variable_identifier.use_index {
                return Ok((*v).plain());
            }

            return Ok((*v).clone());
        }

        return Err(SymbolTableError::VariableNotFoundError {
            name: variable_identifier.name,
        });
    }

    #[inline]
    fn check_statement(&mut self, functions: &HashMap<&'input str, FunctionScope<'input>>, function_scope: &FunctionScope<'input>, statement: &'input ast::Statement<'input>) -> Result<(), SymbolTableError<'input>> {
        match statement {
            ast::Statement::AssignmentStatement { variable, expression} => {
                let variable_type = self.check_variable_type_matches(&function_scope, variable)?;
                let expression_type = self.check_expression(functions, function_scope, expression)?;

                if variable_type.plain() == ast::VariableType::Int && expression_type.plain() == ast::VariableType::Real {
                    // TODO: Not allowed
                }
            },
            ast::Statement::PrintStatement { parameter_list } => {
                for parameter in parameter_list {
                    match parameter {
                        ast::Printable::String(s) => {
                            self.strings.insert(s);
                        },
                        ast::Printable::Expression(e) => {
                            let expression_type = self.check_expression(functions, function_scope, e)?;

                            if expression_type.requires_index() {
                                // TODO: trying to print a vector
                            }
                        }
                    }
                }
            },
            ast::Statement::ReadStatement { parameter_list } => {
                for parameter in parameter_list {
                    let expression_type = self.check_variable_type_matches(&function_scope, parameter)?;

                    if expression_type.requires_index() {
                        // TODO: trying to print a vector
                    }
                }
            },
            ast::Statement::IfStatement { expression, if_body, else_body, use_else } => {
                let if_expression_type = self.check_expression(functions, function_scope, expression)?;

                if if_expression_type.requires_index() || if_expression_type.plain() == ast::VariableType::Real {
                    // TODO: error
                }

                for item in if_body {
                    self.check_statement(functions, function_scope, item)?;
                }

                if *use_else {
                    for item in else_body {
                        self.check_statement(functions, function_scope, item)?;
                    }
                }
            },
            ast::Statement::WhileStatement { expression, body } => {
                let expression_type = self.check_expression(functions, function_scope, expression)?;

                if expression_type.requires_index() || expression_type.plain() == ast::VariableType::Real {
                    // TODO: error
                }

                for item in body {
                    self.check_statement(functions, function_scope, item)?;
                }
            },
            ast::Statement::ForStatement { init_variable, to_expression, by_expression, body } => {
                if self.check_variable_not_exists(&function_scope, init_variable.name)? { // TODO
                    self.check_expression(functions, function_scope, to_expression)?;
                    self.check_expression(functions, function_scope, by_expression)?;

                    for item in body {
                        self.check_statement(functions, function_scope, item)?;
                    }
                }
            },
            ast::Statement::ReturnStatement { expression } => {
                let expression_type = self.check_expression(functions, function_scope, expression)?;

                if expression_type.requires_index() {
                    // TODO: error
                }
            },
        }

        return Ok(());
    }

    fn check_expression(&mut self, functions: &HashMap<&'input str, FunctionScope<'input>>, function_scope: &FunctionScope<'input>, expression: &'input ast::Expression<'input>) -> Result<ast::VariableType, SymbolTableError<'input>> {
        match expression {
            ast::Expression::FunctionCallExpression { name, argument_list } => {
                self.function_call_list.insert(name);

                let mut argument_types = Vec::new();

                for expression in argument_list {
                    let expression_type = self.check_expression(functions, function_scope, expression)?;

                    argument_types.push(expression_type);
                }

                let call_function = functions.get(*name).unwrap();

                if call_function.parameter_list.len() != argument_types.len() {
                    // TODO: err
                }

                let mut i = 0;
                while i < call_function.parameter_list.len() {
                    let argument = argument_types.get(i).unwrap();
                    let parameter = call_function.parameter_list.get(i).unwrap();

                    if !argument.is_fits_to_parameter(parameter.1) {
                        // TODO: err
                    }

                    i += 1;
                }

                self.add_to_function_call_argument_map_if_not_exists(*name,argument_types);

                return Ok(call_function.return_type.clone());
            },
            ast::Expression::VariableExpression(variable_identifier) => {
                return Ok(self.check_variable_type_matches(&function_scope, variable_identifier)?.clone());
            },
            ast::Expression::BinaryExpression { left_expression, operator, right_expression} => {
                let operand1_type = self.check_expression(functions, function_scope, left_expression)?;
                let operand2_type = self.check_expression(functions, function_scope, right_expression)?;

                if *operator == ast::BinaryOperator::IntDivision && (operand1_type != ast::VariableType::Int || operand2_type != ast::VariableType::Int) {
                    // TODO: div applies to int-valued operands only
                }

                if (operand1_type == ast::VariableType::Int && operand2_type == ast::VariableType::Real)
                    || (operand1_type == ast::VariableType::Real && operand2_type == ast::VariableType::Int ) {
                    return Ok(ast::VariableType::Real);
                }

                return Ok(operand1_type);
            },
            ast::Expression::UnaryExpression { expression, operator: _} => {
                return Ok(self.check_expression(functions, function_scope, expression)?.clone());
            },
            ast::Expression::IntExpression(value) => {
                self.ints.insert(*value);

                return Ok(ast::VariableType::Int);
            },
            ast::Expression::RealExpression(value) => {
                self.reals.push(*value);

                return Ok(ast::VariableType::Real);
            },
            ast::Expression::Empty => unreachable!(),
        }
    }

    pub fn build(program: &'input ast::Program<'input>) -> Result<SymbolTable<'input>, SymbolTableError<'input>> {
        let mut functions: HashMap<&'input str, FunctionScope<'input>> = HashMap::new();
        let mut symbol_table = SymbolTable::new();

        for declaration in &program.declaration_list {
            for variable in &declaration.variable_list {
                if symbol_table.variables.contains_key(variable.name) {
                    return Err(SymbolTableError::VariableAlreadyDefinedError {
                        name: variable.name,
                    });
                }

                symbol_table.variables.insert(variable.name, &variable.variable_type);
            }
        }

        for function in &program.function_list {
            if functions.contains_key(function.name) {
                return Err(SymbolTableError::FunctionAlreadyDefinedError {
                    name: function.name,
                });
            }

            symbol_table.function_call_argument_map.insert(function.name, Vec::new());

            let mut function_scope = FunctionScope::new(function.name, &function.return_type);

            for parameter in &function.parameter_list {
                if symbol_table.check_variable_not_exists(&function_scope, parameter.name)? {
                    function_scope.parameter_list.push((parameter.name, &parameter.variable_type));
                }
            }

            for declaration in &function.declaration_list {
                for variable in &declaration.variable_list {
                    if symbol_table.check_variable_not_exists(&function_scope, variable.name)? {
                        function_scope.variables.insert(variable.name, &variable.variable_type);
                    }
                }
            }

            functions.insert(function.name, function_scope);
        }

        for function in &program.function_list {
            let function_scope = functions.get(function.name).unwrap();

            for statement in &function.statement_list {
                symbol_table.check_statement(&functions, function_scope, statement)?;
            }
        }

        for function_name in &symbol_table.function_call_list {
            if !functions.contains_key(function_name) {
                return Err(SymbolTableError::FunctionNotFoundError {
                    name: function_name,
                });
            }
        }

        // dbg!(&self.self);

        symbol_table.strings.insert(" "); // used in comma separated print
        symbol_table.functions.extend(functions);

        return Ok(symbol_table);
    }
}