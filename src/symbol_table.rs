use std::collections::{HashMap, VecDeque, HashSet};
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
struct SymbolTable<'input> {
    functions: HashMap<&'input str, SymbolTableFunctionItem<'input>>,
    variables: HashMap<&'input str, SymbolTableVariableItem<'input>>,
}

pub struct Builder<'input> {
    symbol_table: SymbolTable<'input>,
    program: &'input ast::Program<'input>,
    statement_queue: VecDeque<&'input ast::Statement<'input>>,
    expression_queue: VecDeque<&'input ast::Expression<'input>>,
    function_call_list: HashSet<&'input str>,
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
    pub fn new() -> Self {
        return SymbolTable {
            functions: HashMap::new(),
            variables: HashMap::new(),
        };
    }
}

impl<'input> Builder<'input> {
    pub fn new(program: &'input ast::Program) -> Self {
        return Builder {
            program,
            statement_queue: VecDeque::new(),
            expression_queue: VecDeque::new(),
            symbol_table: SymbolTable::new(),
            function_call_list: HashSet::new(),
        }
    }

    #[inline]
    fn check_variable_not_exists(&self, function_scope: &SymbolTableFunctionItem<'input>, name: &'input str) -> Result<bool, SymbolTableError<'input>> {
        if function_scope.parameters.contains_key(name) || function_scope.variables.contains_key(name) || self.symbol_table.variables.contains_key(name) {
            return Err(SymbolTableError::VariableAlreadyDefinedError {
                name,
            });
        }

        return Ok(true);
    }

    #[inline]
    fn check_variable_type_matches(&mut self, function_scope: &SymbolTableFunctionItem<'input>, variable_identifier: &'input ast::VariableIdentifier<'input>) -> Result<bool, SymbolTableError<'input>> {
        if let Some(p) = function_scope.parameters.get(variable_identifier.name) {
            if p.parameter_type.requires_index() != variable_identifier.use_index {
                return Err(SymbolTableError::VariableTypesNotMatchError {
                    name: variable_identifier.name,
                });
            }
        }
        else if let Some(v) = function_scope.variables.get(variable_identifier.name) {
            if v.variable_type.requires_index() != variable_identifier.use_index {
                return Err(SymbolTableError::VariableTypesNotMatchError {
                    name: variable_identifier.name,
                });
            }
        }
        else if let Some(v) = self.symbol_table.variables.get(variable_identifier.name) {
            if v.variable_type.requires_index() != variable_identifier.use_index {
                return Err(SymbolTableError::VariableTypesNotMatchError {
                    name: variable_identifier.name,
                });
            }
        }
        else {
            return Err(SymbolTableError::VariableNotFoundError {
                name: variable_identifier.name,
            });
        }

        return Ok(true);
    }

    #[inline]
    fn check_statement(&mut self, function_scope: &SymbolTableFunctionItem<'input>, statement: &'input ast::Statement<'input>) -> Result<(), SymbolTableError<'input>> {
        match statement {
            ast::Statement::AssignmentStatement { variable, expression} => {
                if self.check_variable_type_matches(&function_scope, variable)? {
                    self.expression_queue.push_back(expression);
                }
            },
            ast::Statement::PrintStatement { parameter_list } => {
                for parameter in parameter_list {
                    match parameter {
                        ast::Printable::String(_) => {},
                        ast::Printable::Expression(e) => {
                            self.expression_queue.push_back(e);
                        }
                    }
                }
            },
            ast::Statement::ReadStatement { parameter_list } => {
                for parameter in parameter_list {
                    self.check_variable_type_matches(&function_scope, parameter)?;
                }
            },
            ast::Statement::IfStatement { expression, if_body, else_body, use_else } => {
                self.expression_queue.push_back(expression);

                for item in if_body {
                    self.statement_queue.push_back(item);
                }

                if *use_else {
                    for item in else_body {
                        self.statement_queue.push_back(item);
                    }
                }
            },
            ast::Statement::WhileStatement { expression, body } => {
                self.expression_queue.push_back(expression);

                for item in body {
                    self.statement_queue.push_back(item);
                }
            },
            ast::Statement::ForStatement { init_variable, to_expression, by_expression, body } => {
                if self.check_variable_not_exists(&function_scope, init_variable.name)? {
                    self.expression_queue.push_back(to_expression);
                    self.expression_queue.push_back(by_expression);

                    for item in body {
                        self.statement_queue.push_back(item);
                    }
                }
            },
            _ => {}
        }

        return Ok(());
    }

    #[inline]
    fn check_expression(&mut self, function_scope: &SymbolTableFunctionItem<'input>, expression: &'input ast::Expression<'input>) -> Result<(), SymbolTableError<'input>> {
        match expression {
            ast::Expression::FunctionCallExpression { name, argument_list } => {
                self.function_call_list.insert(name);

                for expression in argument_list {
                    self.expression_queue.push_back(expression);
                }
            },
            ast::Expression::VariableExpression(variable_identifier) => {
                self.check_variable_type_matches(&function_scope, variable_identifier)?;
            },
            ast::Expression::BinaryExpression { left_expression, operator: _, right_expression} => {
                self.expression_queue.push_back(left_expression);
                self.expression_queue.push_back(right_expression);
                // TODO: div applies to int-valued operands only
            },
            ast::Expression::UnaryExpression { expression, operator: _} => {
                self.expression_queue.push_back(expression);
            },
            ast::Expression::IntExpression(_) => {},
            ast::Expression::RealExpression(_) => {},
            ast::Expression::Empty => {},
        }

        return Ok(());
    }

    pub fn build(&mut self) -> Result<(), SymbolTableError<'input>> {
        for declaration in &self.program.declaration_list {
            for variable in &declaration.variable_list {
                if self.symbol_table.variables.contains_key(variable.name) {
                    return Err(SymbolTableError::VariableAlreadyDefinedError {
                        name: variable.name,
                    });
                }

                self.symbol_table.variables.insert(variable.name, SymbolTableVariableItem {
                    variable_type: &variable.variable_type,
                });
            }
        }

        for function in &self.program.function_list {
            if self.symbol_table.functions.contains_key(function.name) {
                return Err(SymbolTableError::FunctionAlreadyDefinedError {
                    name: function.name,
                });
            }

            let mut function_scope = SymbolTableFunctionItem::new(function.name);

            for parameter in &function.parameter_list {
                if self.check_variable_not_exists(&function_scope, parameter.name)? {
                    function_scope.parameters.insert(parameter.name, SymbolTableParameterItem {
                        parameter_type: &parameter.parameter_type,
                    });
                }
            }

            for declaration in &function.declaration_list {
                for variable in &declaration.variable_list {
                    if self.check_variable_not_exists(&function_scope, variable.name)? {
                        function_scope.variables.insert(variable.name, SymbolTableVariableItem {
                            variable_type: &variable.variable_type,
                        });
                    }
                }
            }

            for statement in &function.statement_list {
                self.statement_queue.push_back(statement);
            }

            while let Some(statement) = self.statement_queue.pop_front() {
                self.check_statement(&function_scope, statement)?;
            }

            while let Some(expression) = self.expression_queue.pop_front() {
                self.check_expression(&function_scope, expression)?;
            }

            self.symbol_table.functions.insert(function.name, function_scope);
        }

        for function_name in &self.function_call_list {
            if !self.symbol_table.functions.contains_key(function_name) {
                return Err(SymbolTableError::FunctionNotFoundError {
                    name: function_name,
                });
            }
        }

        dbg!(&self.symbol_table);

        return Ok(());
    }
}