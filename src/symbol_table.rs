use indexmap::{IndexMap, IndexSet};
use std::collections::{HashMap, HashSet};

use crate::error::SymbolTableError;
use crate::{ast, MAIN_FUNCTION};

#[derive(Clone, Debug)]
pub struct Function<'input> {
    pub name: &'input str,
    pub return_type: &'input ast::ValueType,
    pub parameters: IndexMap<&'input str, &'input ast::ValueType>,
    pub variables: HashMap<&'input str, &'input ast::ValueType>,
}

impl<'input> Function<'input> {
    fn new(name: &'input str, return_type: &'input ast::ValueType) -> Self {
        return Function {
            name,
            return_type,
            parameters: IndexMap::new(),
            variables: HashMap::new(),
        };
    }
}

#[derive(Clone, Debug)]
pub struct SymbolTable<'input> {
    pub ints: IndexMap<String, u64>,
    pub reals: IndexMap<String, f64>,
    pub strings: IndexSet<&'input str>,
    pub functions: HashMap<&'input str, Function<'input>>,
    pub variables: HashMap<&'input str, &'input ast::ValueType>,
    current_function: Option<&'input str>,
    function_call_set: HashSet<&'input str>,
}

impl<'input> SymbolTable<'input> {
    pub fn new() -> Self {
        return SymbolTable {
            ints: IndexMap::new(),
            reals: IndexMap::new(),
            strings: IndexSet::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),
            current_function: None,
            function_call_set: HashSet::new(),
        };
    }

    #[inline]
    fn check_variable_not_exists(&self, name: &'input str) -> Result<bool, SymbolTableError<'input>> {
        let current_function = self.functions.get(self.current_function.unwrap()).unwrap();

        if current_function.parameters.contains_key(name) || current_function.variables.contains_key(name) || self.variables.contains_key(name) {
            return Err(SymbolTableError::VariableAlreadyDefinedError { name });
        }

        return Ok(true);
    }

    #[inline]
    fn fetch_value_type(&mut self, variable_identifier: &'input ast::VariableIdentifier<'input>) -> Result<ast::ValueType, SymbolTableError<'input>> {
        let current_function = self.functions.get(self.current_function.unwrap()).unwrap();

        if let Some(v) = current_function.variables.get(variable_identifier.name) {
            return Ok((*v).clone());
        }

        if let Some(v) = current_function.parameters.get(variable_identifier.name) {
            return Ok((*v).clone());
        }

        if let Some(v) = self.variables.get(variable_identifier.name) {
            return Ok((*v).clone());
        }

        return Err(SymbolTableError::VariableNotFoundError { name: variable_identifier.name });
    }

    #[inline]
    fn check_value_type_matches(&mut self, variable_identifier: &'input ast::VariableIdentifier<'input>) -> Result<ast::ValueType, SymbolTableError<'input>> {
        let value_type = self.fetch_value_type(variable_identifier)?;

        if !value_type.requires_index() && variable_identifier.use_index {
            return Err(SymbolTableError::TypesNotMatchError);
        }

        if variable_identifier.use_index {
            return Ok(value_type.plain());
        }

        return Ok(value_type);
    }

    fn insert_parameter_to_current_function(&mut self, name: &'input str, parameter_type: &'input ast::ValueType) {
        let current_function = self.functions.get_mut(self.current_function.unwrap()).unwrap();

        current_function.parameters.insert(name, parameter_type);
    }

    fn insert_variable_to_current_function(&mut self, name: &'input str, parameter_type: &'input ast::ValueType) {
        let current_function = self.functions.get_mut(self.current_function.unwrap()).unwrap();

        current_function.variables.insert(name, parameter_type);
    }

    fn check_statement(&mut self, statement: &'input ast::Statement<'input>) -> Result<(), SymbolTableError<'input>> {
        match statement {
            ast::Statement::AssignmentStatement { variable, expression } => {
                if variable.use_index {
                    self.check_expression(&variable.expression)?;
                }

                let variable_value_type = self.check_value_type_matches(variable)?;
                let expression_value_type = self.check_expression(expression)?;

                if !expression_value_type.is_fits_into(&variable_value_type) {
                    return Err(SymbolTableError::TypesNotMatchError);
                }
            }
            ast::Statement::PrintStatement { parameter_list } => {
                for parameter in parameter_list {
                    match parameter {
                        ast::Printable::String(s) => {
                            self.strings.insert(s);
                        }
                        ast::Printable::Expression(e) => {
                            let expression_value_type = self.check_expression(e)?;

                            if expression_value_type.requires_index() {
                                return Err(SymbolTableError::TypesNotMatchError);
                            }
                        }
                    }
                }
            }
            ast::Statement::ReadStatement { parameter_list } => {
                for parameter in parameter_list {
                    let expression_value_type = self.check_value_type_matches(parameter)?;

                    if expression_value_type.requires_index() {
                        return Err(SymbolTableError::TypesNotMatchError);
                    }
                }
            }
            ast::Statement::IfStatement {
                expression,
                if_body,
                else_body,
                use_else,
            } => {
                let if_expression_value_type = self.check_expression(expression)?;

                if !if_expression_value_type.is_represents_bool() {
                    return Err(SymbolTableError::TypesNotMatchError);
                }

                for item in if_body {
                    self.check_statement(item)?;
                }

                if *use_else {
                    for item in else_body {
                        self.check_statement(item)?;
                    }
                }
            }
            ast::Statement::WhileStatement { expression, body } => {
                let expression_value_type = self.check_expression(expression)?;

                if !expression_value_type.is_represents_bool() {
                    return Err(SymbolTableError::TypesNotMatchError);
                }

                for item in body {
                    self.check_statement(item)?;
                }
            }
            ast::Statement::ForStatement {
                init_variable,
                start_expression,
                to_expression,
                by_expression,
                body,
            } => {
                let init_variable_value_type = self.fetch_value_type(init_variable)?;

                let start_expression_value_type = self.check_expression(start_expression)?;
                if !start_expression_value_type.is_fits_into(&init_variable_value_type) {
                    return Err(SymbolTableError::TypesNotMatchError);
                }

                let to_expression_value_type = self.check_expression(to_expression)?;
                if !to_expression_value_type.is_fits_into(&init_variable_value_type) {
                    return Err(SymbolTableError::TypesNotMatchError);
                }

                match by_expression {
                    ast::Expression::Empty => {}
                    _ => {
                        let by_expression_value_type = self.check_expression(by_expression)?;

                        if !by_expression_value_type.is_fits_into(&init_variable_value_type) {
                            return Err(SymbolTableError::TypesNotMatchError);
                        }
                    }
                }

                for item in body {
                    self.check_statement(item)?;
                }
            }
            ast::Statement::ReturnStatement { expression } => {
                let expression_type = self.check_expression(expression)?;

                if expression_type.requires_index() {
                    return Err(SymbolTableError::TypesNotMatchError);
                }
            }
        }

        return Ok(());
    }

    fn check_expression(&mut self, expression: &'input ast::Expression<'input>) -> Result<ast::ValueType, SymbolTableError<'input>> {
        match expression {
            ast::Expression::FunctionCallExpression { name, argument_list } => {
                if !self.functions.contains_key(*name) {
                    return Err(SymbolTableError::FunctionNotFoundError { name });
                }

                self.function_call_set.insert(name);

                let mut argument_types = Vec::new();
                for expression in argument_list {
                    let expression_type = self.check_expression(expression)?;

                    argument_types.push(expression_type);
                }

                let call_function = self.functions.get(*name).unwrap();
                if call_function.parameters.len() != argument_types.len() {
                    return Err(SymbolTableError::WrongNumberOfArguments { name: call_function.name });
                }

                let mut i = 0;

                for parameter in &call_function.parameters {
                    let argument = argument_types.get(i).unwrap();

                    if !argument.eq(parameter.1) {
                        return Err(SymbolTableError::TypesNotMatchError);
                    }

                    i += 1;
                }

                Ok(call_function.return_type.to_owned())
            }
            ast::Expression::VariableExpression(variable_identifier) => {
                if variable_identifier.use_index {
                    self.check_expression(&variable_identifier.expression)?;
                }

                Ok(self.check_value_type_matches(variable_identifier)?.to_owned())
            }
            ast::Expression::BinaryExpression {
                left_expression,
                operator,
                right_expression,
            } => {
                let operand1_type = self.check_expression(left_expression)?;
                let operand2_type = self.check_expression(right_expression)?;

                if *operator == ast::BinaryOperator::And
                    || *operator == ast::BinaryOperator::Or
                    || *operator == ast::BinaryOperator::IntDivision
                    || *operator == ast::BinaryOperator::NotEqual
                    || *operator == ast::BinaryOperator::Greater
                    || *operator == ast::BinaryOperator::GreaterEqual
                    || *operator == ast::BinaryOperator::Less
                    || *operator == ast::BinaryOperator::LessEqual
                {
                    return Ok(ast::ValueType::Int);
                } else if *operator == ast::BinaryOperator::Division {
                    return Ok(ast::ValueType::Real);
                } else if operand1_type == ast::ValueType::Real || operand2_type == ast::ValueType::Real {
                    return Ok(ast::ValueType::Real);
                }

                Ok(operand1_type)
            }
            ast::Expression::UnaryExpression { expression, operator: _ } => Ok(self.check_expression(expression)?.to_owned()),
            ast::Expression::IntExpression(value) => {
                self.ints.insert(format!("{}", value), *value);

                Ok(ast::ValueType::Int)
            }
            ast::Expression::RealExpression(value) => {
                self.reals.insert(format!("{}", value), *value);

                Ok(ast::ValueType::Real)
            }
            ast::Expression::Empty => unreachable!(),
        }
    }

    pub fn build(program: &'input ast::Program<'input>) -> Result<SymbolTable<'input>, SymbolTableError<'input>> {
        let mut symbol_table = SymbolTable::new();

        for declaration in &program.declaration_list {
            for variable in &declaration.variable_list {
                if !symbol_table.variables.contains_key(variable.name) {
                    symbol_table.variables.insert(variable.name, &variable.value_type);
                }
            }
        }

        for function in &program.function_list {
            if symbol_table.functions.contains_key(function.name) {
                return Err(SymbolTableError::FunctionAlreadyDefinedError { name: function.name });
            }

            symbol_table.functions.insert(function.name, Function::new(function.name, &function.return_type));
        }

        for function in &program.function_list {
            symbol_table.current_function = Some(function.name);

            let mut current_function_parameter_set = HashSet::new();

            for parameter in &function.parameter_list {
                if !current_function_parameter_set.contains(parameter.name) {
                    current_function_parameter_set.insert(parameter.name);
                    symbol_table.insert_parameter_to_current_function(parameter.name, &parameter.value_type);
                }
            }

            for declaration in &function.declaration_list {
                for variable in &declaration.variable_list {
                    if symbol_table.check_variable_not_exists(variable.name)? {
                        symbol_table.insert_variable_to_current_function(variable.name, &variable.value_type);
                    }
                }
            }

            for statement in &function.statement_list {
                symbol_table.check_statement(statement)?;
            }
        }

        for function_name in &symbol_table.function_call_set {
            if !symbol_table.functions.contains_key(function_name) {
                return Err(SymbolTableError::FunctionNotFoundError { name: function_name });
            }
        }

        if let Some(main_function) = &symbol_table.functions.get(MAIN_FUNCTION) {
            if !main_function.return_type.eq(&ast::ValueType::Int) {
                return Err(SymbolTableError::MainFunctionReturnTypeMustBeInt);
            }
        } else {
            return Err(SymbolTableError::MainFunctionDoesNotExists);
        }

        symbol_table.strings.insert(" "); // used in comma separated print
        symbol_table.strings.insert("\\n"); // used in print
        symbol_table.ints.insert("1".to_owned(), 1); // for by queries
        symbol_table.reals.insert("1.0".to_owned(), 1.0); // for by queries

        return Ok(symbol_table);
    }
}
