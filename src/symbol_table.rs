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
struct SymbolTableVariableItem<'input> {
    variable_type: &'input ast::VariableType,
}

#[derive(Clone, Debug)]
struct SymbolTableFunctionItem<'input> {
    name: &'input str,
    return_type: &'input ast::VariableType,
    parameters: HashMap<&'input str, SymbolTableVariableItem<'input>>,
    variables: HashMap<&'input str, SymbolTableVariableItem<'input>>,
}

#[derive(Clone, Debug)]
struct SymbolTable<'input> {
    functions: HashMap<&'input str, SymbolTableFunctionItem<'input>>,
    variables: HashMap<&'input str, SymbolTableVariableItem<'input>>,
}

pub struct Builder<'input> {
    function_call_list: HashSet<&'input str>,
    function_parameter_list_map: HashMap<&'input str, Vec<&'input str>>,
    function_parameter_map: HashMap<&'input str, HashMap<&'input str, &'input ast::VariableType>>,
    function_return_type_map: HashMap<&'input str, &'input ast::VariableType>,
}

impl<'input> SymbolTableFunctionItem<'input> {
    fn new(name: &'input str, return_type: &'input ast::VariableType) -> Self {
        return SymbolTableFunctionItem {
            name,
            return_type,
            parameters: HashMap::new(),
            variables: HashMap::new(),
        };
    }
}

impl<'input> SymbolTable<'input> {
    pub fn new() -> Self {
        return SymbolTable {
            functions: HashMap::new(),
            variables: HashMap::new(),
        };
    }
}

impl<'input> Builder<'input> {
    fn new() -> Self {
        return Builder {
            function_call_list: HashSet::new(),
            function_parameter_list_map: HashMap::new(),
            function_parameter_map: HashMap::new(),
            function_return_type_map: HashMap::new(),
        }
    }

    #[inline]
    fn check_variable_not_exists(&self, symbol_table: &SymbolTable<'input>, function_scope: &SymbolTableFunctionItem<'input>, name: &'input str) -> Result<bool, SymbolTableError<'input>> {
        if function_scope.parameters.contains_key(name) || function_scope.variables.contains_key(name) || symbol_table.variables.contains_key(name) {
            return Err(SymbolTableError::VariableAlreadyDefinedError {
                name,
            });
        }

        return Ok(true);
    }

    #[inline]
    fn check_variable_type_matches(&mut self, symbol_table: &SymbolTable<'input>, function_scope: &SymbolTableFunctionItem<'input>, variable_identifier: &'input ast::VariableIdentifier<'input>) -> Result<ast::VariableType, SymbolTableError<'input>> {
        if let Some(p) = function_scope.parameters.get(variable_identifier.name) {
            if !p.variable_type.requires_index() && variable_identifier.use_index {
                return Err(SymbolTableError::VariableTypesNotMatchError {
                    name: variable_identifier.name,
                });
            }

            if variable_identifier.use_index {
                return Ok(p.variable_type.plain());
            }

            return Ok(p.variable_type.clone());
        }
        else if let Some(v) = function_scope.variables.get(variable_identifier.name) {
            if !v.variable_type.requires_index() && variable_identifier.use_index {
                return Err(SymbolTableError::VariableTypesNotMatchError {
                    name: variable_identifier.name,
                });
            }

            if variable_identifier.use_index {
                return Ok(v.variable_type.plain());
            }

            return Ok(v.variable_type.clone());
        }
        else if let Some(v) = symbol_table.variables.get(variable_identifier.name) {
            if !v.variable_type.requires_index() && variable_identifier.use_index {
                return Err(SymbolTableError::VariableTypesNotMatchError {
                    name: variable_identifier.name,
                });
            }

            if variable_identifier.use_index {
                return Ok(v.variable_type.plain());
            }

            return Ok(v.variable_type.clone());
        }

        return Err(SymbolTableError::VariableNotFoundError {
            name: variable_identifier.name,
        });
    }

    #[inline]
    fn check_statement(&mut self, symbol_table: &SymbolTable<'input>, function_scope: &SymbolTableFunctionItem<'input>, statement: &'input ast::Statement<'input>) -> Result<(), SymbolTableError<'input>> {
        match statement {
            ast::Statement::AssignmentStatement { variable, expression} => {
                let variable_type = self.check_variable_type_matches(&symbol_table, &function_scope, variable)?;
                let expression_type = self.check_expression(symbol_table, function_scope, expression)?;

                if variable_type.plain() == ast::VariableType::Int && expression_type.plain() == ast::VariableType::Real {
                    // TODO: Not allowed
                }
            },
            ast::Statement::PrintStatement { parameter_list } => {
                for parameter in parameter_list {
                    match parameter {
                        ast::Printable::String(_) => {},
                        ast::Printable::Expression(e) => {
                            let expression_type = self.check_expression(symbol_table, function_scope, e)?;

                            if expression_type.requires_index() {
                                // TODO: trying to print a vector
                            }
                        }
                    }
                }
            },
            ast::Statement::ReadStatement { parameter_list } => {
                for parameter in parameter_list {
                    let expression_type = self.check_variable_type_matches(&symbol_table, &function_scope, parameter)?;

                    if expression_type.requires_index() {
                        // TODO: trying to print a vector
                    }
                }
            },
            ast::Statement::IfStatement { expression, if_body, else_body, use_else } => {
                let if_expression_type = self.check_expression(symbol_table, function_scope, expression)?;

                if if_expression_type.requires_index() || if_expression_type.plain() == ast::VariableType::Real {
                    // TODO: error
                }

                for item in if_body {
                    self.check_statement(symbol_table, function_scope, item)?;
                }

                if *use_else {
                    for item in else_body {
                        self.check_statement(symbol_table, function_scope, item)?;
                    }
                }
            },
            ast::Statement::WhileStatement { expression, body } => {
                let expression_type = self.check_expression(symbol_table, function_scope, expression)?;

                if expression_type.requires_index() || expression_type.plain() == ast::VariableType::Real {
                    // TODO: error
                }

                for item in body {
                    self.check_statement(symbol_table, function_scope, item)?;
                }
            },
            ast::Statement::ForStatement { init_variable, to_expression, by_expression, body } => {
                if self.check_variable_not_exists(&symbol_table, &function_scope, init_variable.name)? { // TODO
                    self.check_expression(symbol_table, function_scope, to_expression)?;
                    self.check_expression(symbol_table, function_scope, by_expression)?;

                    for item in body {
                        self.check_statement(symbol_table, function_scope, item)?;
                    }
                }
            },
            ast::Statement::ReturnStatement { expression } => {
                let expression_type = self.check_expression(symbol_table, function_scope, expression)?;

                if expression_type.requires_index() {
                    // TODO: error
                }
            },
        }

        return Ok(());
    }

    #[inline]
    fn check_expression(&mut self, symbol_table: &SymbolTable<'input>, function_scope: &SymbolTableFunctionItem<'input>, expression: &'input ast::Expression<'input>) -> Result<ast::VariableType, SymbolTableError<'input>> {
        return match expression {
            ast::Expression::FunctionCallExpression { name, argument_list } => {
                self.function_call_list.insert(name);

                let mut argument_types = Vec::new();

                for expression in argument_list {
                    let expression_type = self.check_expression(symbol_table, function_scope, expression)?;

                    argument_types.push(expression_type);
                }

                let return_type = self.function_return_type_map.get(*name).unwrap().to_owned();
                let parameter_list = self.function_parameter_list_map.get(*name).unwrap();
                let parameters = self.function_parameter_map.get(*name).unwrap();

                let mut i = 0;
                while i < parameter_list.len() {
                    let argument = argument_types.get(i).unwrap();
                    let parameter_name = parameter_list.get(i).unwrap();
                    let parameter = parameters.get(parameter_name).unwrap();

                    if !argument.is_fits_to_parameter(parameter) {
                        // TODO: err
                    }

                    i += 1;
                }

                return Ok(return_type.clone());
            },
            ast::Expression::VariableExpression(variable_identifier) => {
                return Ok(self.check_variable_type_matches(&symbol_table, &function_scope, variable_identifier)?.clone());
            },
            ast::Expression::BinaryExpression { left_expression, operator, right_expression} => {
                let operand1_type = self.check_expression(symbol_table, function_scope, left_expression)?;
                let operand2_type = self.check_expression(symbol_table, function_scope, right_expression)?;

                if *operator == ast::BinaryOperator::Div && (operand1_type != ast::VariableType::Int || operand2_type != ast::VariableType::Int) {
                    // TODO: div applies to int-valued operands only
                }

                if (operand1_type == ast::VariableType::Int && operand2_type == ast::VariableType::Real)
                    || (operand1_type == ast::VariableType::Real && operand2_type == ast::VariableType::Int ) {
                    return Ok(ast::VariableType::Real);
                }

                return Ok(operand1_type);
            },
            ast::Expression::UnaryExpression { expression, operator: _} => {
                return Ok(self.check_expression(symbol_table, function_scope, expression)?.clone());
            },
            ast::Expression::IntExpression(_) => Ok(ast::VariableType::Int),
            ast::Expression::RealExpression(_) => Ok(ast::VariableType::Real),
            ast::Expression::Empty => unreachable!(),
        }
    }

    pub fn build(program: &'input ast::Program<'input>) -> Result<(), SymbolTableError<'input>> {
        let mut builder = Builder::new();
        let mut symbol_table = SymbolTable::new();

        for declaration in &program.declaration_list {
            for variable in &declaration.variable_list {
                if symbol_table.variables.contains_key(variable.name) {
                    return Err(SymbolTableError::VariableAlreadyDefinedError {
                        name: variable.name,
                    });
                }

                symbol_table.variables.insert(variable.name, SymbolTableVariableItem {
                    variable_type: &variable.variable_type,
                });
            }
        }

        for function in &program.function_list {
            if symbol_table.functions.contains_key(function.name) {
                return Err(SymbolTableError::FunctionAlreadyDefinedError {
                    name: function.name,
                });
            }

            let mut parameter_list = Vec::new();
            let mut parameter_map = HashMap::new();

            for parameter in &function.parameter_list {
                parameter_map.insert(parameter.name, &parameter.variable_type);
                parameter_list.push(parameter.name);
            }

            builder.function_parameter_map.insert(function.name, parameter_map);
            builder.function_parameter_list_map.insert(function.name, parameter_list);
            builder.function_return_type_map.insert(function.name, &function.return_type);
        }

        for function in &program.function_list {
            let mut function_scope = SymbolTableFunctionItem::new(function.name, &function.return_type);

            for parameter in &function.parameter_list {
                if builder.check_variable_not_exists(&symbol_table, &function_scope, parameter.name)? {
                    function_scope.parameters.insert(parameter.name, SymbolTableVariableItem {
                        variable_type: &parameter.variable_type,
                    });
                }
            }

            for declaration in &function.declaration_list {
                for variable in &declaration.variable_list {
                    if builder.check_variable_not_exists(&symbol_table, &function_scope, variable.name)? {
                        function_scope.variables.insert(variable.name, SymbolTableVariableItem {
                            variable_type: &variable.variable_type,
                        });
                    }
                }
            }

            for statement in &function.statement_list {
                builder.check_statement(&symbol_table, &function_scope, statement)?;
            }

            symbol_table.functions.insert(function.name, function_scope);
        }

        for function_name in &builder.function_call_list {
            if !symbol_table.functions.contains_key(function_name) {
                return Err(SymbolTableError::FunctionNotFoundError {
                    name: function_name,
                });
            }
        }

        // dbg!(&self.symbol_table);

        return Ok(());
    }
}