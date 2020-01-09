use std::collections::{HashMap, VecDeque, HashSet};
use std::fmt;

use crate::ast;

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

impl<'input> fmt::Display for SymbolTableError<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            SymbolTableError::VariableAlreadyDefinedError { name } => {
                write!(f, "Variable already defined: {}", name)
            },
            SymbolTableError::FunctionAlreadyDefinedError { name } => {
                write!(f, "Function already defined: {}", name)
            },
            SymbolTableError::VariableNotFoundError { name } => {
                write!(f, "VariableNotFoundError: {}", name)
            },
            SymbolTableError::VariableTypesNotMatchError { name } => {
                write!(f, "VariableTypesNotMatchError: {}", name)
            },
            SymbolTableError::FunctionNotFoundError { name } => {
                write!(f, "FunctionNotFoundError: {}", name)
            },
        }
    }
}

#[derive(Clone, Debug)]
struct SymbolTableVariableItem<'input> {
    variable_type: &'input ast::VariableType,
}

#[derive(Clone, Debug)]
struct SymbolTableParameterItem<'input> {
    parameter_type: &'input ast::ParameterType,
}


#[derive(Clone, Debug)]
struct SymbolTableFunctionItem<'input> {
    name: &'input str,
    parameters: HashMap<&'input str, SymbolTableParameterItem<'input>>,
    variables: HashMap<&'input str, SymbolTableVariableItem<'input>>,
}

#[derive(Clone, Debug)]
pub struct SymbolTable<'input> {
    functions: HashMap<&'input str, SymbolTableFunctionItem<'input>>,
    variables: HashMap<&'input str, SymbolTableVariableItem<'input>>,
}

impl<'input> SymbolTableFunctionItem<'input> {
    fn new(name: &'input str) -> Self {
        return SymbolTableFunctionItem {
            name,
            variables: HashMap::new(),
            parameters: HashMap::new(),
        };
    }
}

impl<'input> SymbolTable<'input> {
    fn new() -> Self {
        return SymbolTable {
            functions: HashMap::new(),
            variables: HashMap::new(),
        };
    }

    //noinspection ALL
    pub fn build(program: &'input ast::Program) -> Result<SymbolTable<'input>, SymbolTableError<'input>> {
        let mut symbol_table = SymbolTable::new();

        let mut function_call_list = HashSet::new();

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

            let mut function_scope = SymbolTableFunctionItem::new(function.name);

            for parameter in &function.parameter_list {
                if function_scope.parameters.contains_key(parameter.name) || symbol_table.variables.contains_key(parameter.name) {
                    return Err(SymbolTableError::VariableAlreadyDefinedError {
                        name: parameter.name,
                    });
                }

                function_scope.parameters.insert(parameter.name, SymbolTableParameterItem {
                    parameter_type: &parameter.parameter_type,
                });
            }

            for declaration in &function.declaration_list {
                for variable in &declaration.variable_list {
                    if function_scope.parameters.contains_key(variable.name) || function_scope.variables.contains_key(variable.name) || symbol_table.variables.contains_key(variable.name) {
                        return Err(SymbolTableError::VariableAlreadyDefinedError {
                            name: variable.name,
                        });
                    }

                    function_scope.variables.insert(variable.name, SymbolTableVariableItem {
                        variable_type: &variable.variable_type,
                    });
                }
            }

            let mut statement_queue = VecDeque::new();
            let mut expression_queue = VecDeque::new();

            for statement in &function.statement_list {
                statement_queue.push_back(statement);
            }

            while let Some(statement) = statement_queue.pop_front() {
                match statement {
                    ast::Statement::AssignmentStatement { variable, expression} => {
                        if let Some(p) = function_scope.parameters.get(variable.identifier) {
                            if p.parameter_type.requires_index() != variable.use_index {
                                return Err(SymbolTableError::VariableTypesNotMatchError {
                                    name: variable.identifier,
                                });
                            }
                        }
                        else if let Some(v) = function_scope.variables.get(variable.identifier) {
                            if v.variable_type.requires_index() != variable.use_index {
                                return Err(SymbolTableError::VariableTypesNotMatchError {
                                    name: variable.identifier,
                                });
                            }
                        }
                        else if let Some(v) = symbol_table.variables.get(variable.identifier) {
                            if v.variable_type.requires_index() != variable.use_index {
                                return Err(SymbolTableError::VariableTypesNotMatchError {
                                    name: variable.identifier,
                                });
                            }
                        }
                        else {
                            return Err(SymbolTableError::VariableNotFoundError {
                                name: variable.identifier,
                            });
                        }

                        expression_queue.push_back(expression);
                    },
                    ast::Statement::PrintStatement { parameter_list } => {
                        for parameter in parameter_list {
                            match parameter {
                                ast::Printable::String(_) => {},
                                ast::Printable::Expression(e) => {
                                    expression_queue.push_back(&e);
                                }
                            }
                        }
                    },
                    ast::Statement::ReadStatement { parameter_list } => {
                        for parameter in parameter_list {
                            if let Some(p) = function_scope.parameters.get(parameter.identifier) {
                                if p.parameter_type.requires_index() != parameter.use_index {
                                    return Err(SymbolTableError::VariableTypesNotMatchError {
                                        name: parameter.identifier,
                                    });
                                }
                            }
                            else if let Some(v) = function_scope.variables.get(parameter.identifier) {
                                if v.variable_type.requires_index() != parameter.use_index {
                                    return Err(SymbolTableError::VariableTypesNotMatchError {
                                        name: parameter.identifier,
                                    });
                                }
                            }
                            else if let Some(v) = symbol_table.variables.get(parameter.identifier) {
                                if v.variable_type.requires_index() != parameter.use_index {
                                    return Err(SymbolTableError::VariableTypesNotMatchError {
                                        name: parameter.identifier,
                                    });
                                }
                            }
                            else {
                                return Err(SymbolTableError::VariableNotFoundError {
                                    name: parameter.identifier,
                                });
                            }
                        }
                    },
                    ast::Statement::IfStatement { expression, if_body, else_body, use_else } => {
                        expression_queue.push_back(expression);

                        for item in if_body {
                            statement_queue.push_back(item);
                        }

                        if *use_else {
                            for item in else_body {
                                statement_queue.push_back(item);
                            }
                        }
                    },
                    ast::Statement::WhileStatement { expression, body } => {
                        expression_queue.push_back(expression);

                        for item in body {
                            statement_queue.push_back(item);
                        }
                    },
                    ast::Statement::ForStatement { init_variable, to_expression, by_expression, body } => {
                        if let Some(p) = function_scope.parameters.get(init_variable.name) {
                            if p.parameter_type.requires_index() == init_variable.variable_type.requires_index() {
                                return Err(SymbolTableError::VariableTypesNotMatchError {
                                    name: init_variable.name,
                                });
                            }
                        }
                        else if let Some(v) = function_scope.variables.get(init_variable.name) {
                            if v.variable_type.requires_index() == init_variable.variable_type.requires_index() {
                                return Err(SymbolTableError::VariableTypesNotMatchError {
                                    name: init_variable.name,
                                });
                            }
                        }
                        else if let Some(v) = symbol_table.variables.get(init_variable.name) {
                            if v.variable_type.requires_index() == init_variable.variable_type.requires_index() {
                                return Err(SymbolTableError::VariableTypesNotMatchError {
                                    name: init_variable.name,
                                });
                            }
                        }
                        else {
                            return Err(SymbolTableError::VariableNotFoundError {
                                name: init_variable.name,
                            });
                        }

                        expression_queue.push_back(to_expression);
                        expression_queue.push_back(by_expression);

                        for item in body {
                            statement_queue.push_back(item);
                        }
                    },
                    _ => {}
                }
            }

            while let Some(expression) = expression_queue.pop_front() {
                match expression {
                    ast::Expression::FunctionCallExpression { identifier, argument_list } => {
                        function_call_list.insert(identifier);

                        for expression in argument_list {
                            expression_queue.push_back(expression);
                        }
                    },
                    ast::Expression::VariableExpression(variable_identifier) => {
                        if let Some(p) = function_scope.parameters.get(variable_identifier.identifier) {
                            if p.parameter_type.requires_index() != variable_identifier.use_index {
                                return Err(SymbolTableError::VariableTypesNotMatchError {
                                    name: variable_identifier.identifier,
                                });
                            }
                        }
                        else if let Some(v) = function_scope.variables.get(variable_identifier.identifier) {
                            if v.variable_type.requires_index() != variable_identifier.use_index {
                                return Err(SymbolTableError::VariableTypesNotMatchError {
                                    name: variable_identifier.identifier,
                                });
                            }
                        }
                        else if let Some(v) = symbol_table.variables.get(variable_identifier.identifier) {
                            if v.variable_type.requires_index() != variable_identifier.use_index {
                                return Err(SymbolTableError::VariableTypesNotMatchError {
                                    name: variable_identifier.identifier,
                                });
                            }
                        }
                        else {
                            return Err(SymbolTableError::VariableNotFoundError {
                                name: variable_identifier.identifier,
                            });
                        }
                    },
                    ast::Expression::BinaryExpression { left_expression, operator: _, right_expression} => {
                        expression_queue.push_back(left_expression);
                        expression_queue.push_back(right_expression);
                        // TODO: type check for operators
                    },
                    ast::Expression::UnaryExpression { expression, operator: _} => {
                        expression_queue.push_back(expression);
                        // TODO: type check for operators
                    },
                    ast::Expression::IntExpression(_) => {},
                    ast::Expression::RealExpression(_) => {},
                    ast::Expression::Empty => {},
                }
            }

            symbol_table.functions.insert(function.name, function_scope);
        }

        for function_name in function_call_list {
            if !symbol_table.functions.contains_key(function_name) {
                return Err(SymbolTableError::FunctionNotFoundError {
                    name: function_name,
                });
            }
        }

        dbg!(&symbol_table);

        return Ok(symbol_table);
    }
}