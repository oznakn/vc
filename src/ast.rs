use std::fmt;

#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Negative,
    Not,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Mod,
    Div,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
}

#[derive(Clone, Debug)]
pub struct Program<'input> {
    pub declaration_list: Vec<Declaration<'input>>,
    pub function_list:  Vec<Function<'input>>,
}

#[derive(Clone, Debug)]
pub struct Declaration<'input> {
    pub variable_list: Vec<Variable<'input>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum VariableType {
    String(u64),
    Int,
    IntVector(u64, bool),
    Real,
    RealVector(u64, bool),
}

impl VariableType {
    pub fn is_fits_to_parameter(&self, parameter: &VariableType) -> bool {
        return match parameter {
            VariableType::IntVector { .. } => self.requires_index() && self.plain() == VariableType::Int,
            VariableType::RealVector { .. } => self.requires_index() && self.plain() == VariableType::Real,
            _ => self.eq(parameter),
        }
    }

    pub fn requires_index(&self) -> bool {
        return match *self {
            VariableType::IntVector { .. } => true,
            VariableType::RealVector { .. } => true,
            _ => false
        };
    }

    pub fn size(&self) -> u64 {
        return match self {
            VariableType::Int => 4,
            VariableType::Real => 8,
            VariableType::IntVector(size, _) => (*size) * 4,
            VariableType::RealVector(size,_) => (*size) * 8,
            VariableType::String(size) => *size,
        }
    }

    pub fn plain(&self) -> Self {
        return match self {
            VariableType::IntVector { .. } => VariableType::Int,
            VariableType::RealVector { .. } => VariableType::Real,
            _ => self.clone(),
        }
    }
}

impl fmt::Display for VariableType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            VariableType::Int => write!(f, "int"),
            VariableType::Real => write!(f, "real"),
            VariableType::IntVector(size, _) => write!(f, "int_{}", size),
            VariableType::RealVector(size, _) => write!(f, "real_{}", size),
            VariableType::String (size)=> write!(f, "string_{}", size),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Variable<'input> {
    pub name: &'input str,
    pub variable_type: VariableType,
}

#[derive(Clone, Debug)]
pub struct Parameter<'input> {
    pub name: &'input str,
    pub variable_type: VariableType,
}

#[derive(Clone, Debug)]
pub struct Function<'input> {
    pub return_type: VariableType,
    pub name: &'input str,
    pub parameter_list: Vec<Parameter<'input>>,
    pub declaration_list: Vec<Declaration<'input>>,
    pub statement_list: Vec<Statement<'input>>,
}

#[derive(Clone, Debug)]
pub struct VariableIdentifier<'input> {
    pub name: &'input str,
    pub use_index: bool,
    pub expression: Box<Expression<'input>>,
}

#[derive(Clone, Debug)]
pub enum Expression<'input> {
    IntExpression(i64),
    RealExpression(f64),
    VariableExpression(VariableIdentifier<'input>),
    FunctionCallExpression {
        name: &'input str,
        argument_list: Vec<Expression<'input>>,
    },
    UnaryExpression {
        expression: Box<Expression<'input>>,
        operator: UnaryOperator,
    },
    BinaryExpression{
        left_expression: Box<Expression<'input>>,
        right_expression: Box<Expression<'input>>,
        operator: BinaryOperator,
    },
    Empty,
}

#[derive(Clone, Debug)]
pub enum Printable<'input> {
    Expression(Expression<'input>),
    String(&'input str),
}

#[derive(Clone, Debug)]
pub enum Statement<'input> {
    AssignmentStatement {
        variable: VariableIdentifier<'input>,
        expression: Expression<'input>,
    },
    PrintStatement {
        parameter_list: Vec<Printable<'input>>,
    },
    ReadStatement {
        parameter_list: Vec<VariableIdentifier<'input>>,
    },
    ReturnStatement {
        expression: Expression<'input>,
    },
    ForStatement {
        init_variable: Variable<'input>,
        by_expression: Expression<'input>,
        to_expression: Expression<'input>,
        body: Vec<Statement<'input>>,
    },
    IfStatement {
        expression: Expression<'input>,
        if_body: Vec<Statement<'input>>,
        else_body: Vec<Statement<'input>>,
        use_else: bool,
    },
    WhileStatement {
        expression: Expression<'input>,
        body: Vec<Statement<'input>>,
    },
}