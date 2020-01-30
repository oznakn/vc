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
    IntDivision,
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
    pub function_list: Vec<Function<'input>>,
}

#[derive(Clone, Debug)]
pub struct Declaration<'input> {
    pub variable_list: Vec<Variable<'input>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum ValueType {
    Int,
    Real,
    Vector(Box<ValueType>, u64),
    String(u64),
}

impl ValueType {
    pub fn is_represents_bool(&self) -> bool {
        match self {
            ValueType::Int => true,
            _ => false,
        }
    }

    pub fn is_fits_into(&self, v: &ValueType) -> bool {
        if self.eq(v) {
            return true;
        }

        match self {
            ValueType::Int => v.eq(&ValueType::Real),
            _ => false,
        }
    }

    pub fn requires_promote(&self, v: &ValueType) -> bool {
        self.is_fits_into(v) && !self.eq(v)
    }

    pub fn requires_index(&self) -> bool {
        return match *self {
            ValueType::Vector(_, _) => true,
            _ => false,
        };
    }

    pub fn plain(&self) -> Self {
        return match self {
            ValueType::Vector(plain_type, _) => plain_type.as_ref().to_owned(),
            _ => self.to_owned(),
        };
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            ValueType::Int => write!(f, "int"),
            ValueType::Real => write!(f, "real"),
            ValueType::Vector(value_type, size) => write!(f, "vector_{}_{}", value_type, size),
            ValueType::String(_) => write!(f, "string"),
        };
    }
}

#[derive(Clone, Debug)]
pub struct Variable<'input> {
    pub name: &'input str,
    pub value_type: ValueType,
}

#[derive(Clone, Debug)]
pub struct Function<'input> {
    pub return_type: ValueType,
    pub name: &'input str,
    pub parameter_list: Vec<Variable<'input>>,
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
    IntExpression(u64),
    RealExpression(f64),
    VariableExpression(VariableIdentifier<'input>),
    FunctionCallExpression { name: &'input str, argument_list: Vec<Expression<'input>> },
    UnaryExpression { expression: Box<Expression<'input>>, operator: UnaryOperator },
    BinaryExpression { left_expression: Box<Expression<'input>>, right_expression: Box<Expression<'input>>, operator: BinaryOperator },
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
        init_variable: VariableIdentifier<'input>,
        start_expression: Expression<'input>,
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
