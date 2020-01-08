#[derive(Clone)]
pub enum UnaryOperator {
    Negative,
    // Not,
}

#[derive(Clone)]
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
    // Not,
}

#[derive(Clone)]
pub struct Program<'input> {
    pub declaration_list: Vec<Declaration<'input>>,
    pub function_list:  Vec<Function<'input>>,
}

#[derive(Clone)]
pub struct Declaration<'input> {
    pub variable_list: Vec<Variable<'input>>,
}

#[derive(Clone)]
pub enum VariableType {
    Int,
    IntVector {
        has_size: bool,
        size: i64,
    },
    Real,
    RealVector {
        has_size: bool,
        size: i64,
    },
}

#[derive(Clone)]
pub struct Variable<'input> {
    pub name: &'input str,
    pub variable_type: VariableType,
}

#[derive(Clone)]
pub struct Function<'input> {
    pub return_type: VariableType,
    pub name: &'input str,
    pub parameter_list: Vec<Variable<'input>>,
    pub declaration_list: Vec<Declaration<'input>>,
}

#[derive(Clone)]
pub enum Expression<'input> {
    IntExpression(i64),
    RealExpression(f64),
    VariableExpression {
        identifier: &'input str,
        use_index: bool,
        index: i64,
    },
    FunctionCallExpression {
        identifier: &'input str,
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
    // NotExpression, TODO: fix
}

#[derive(Clone)]
pub enum Printable<'input> {
    Expression(Expression<'input>),
    String(&'input str),
}

#[derive(Clone)]
pub struct Readable<'input> {
    pub identifier: &'input str,
    pub use_index: bool,
    pub index: i64,
}

#[derive(Clone)]
pub enum Statement<'input> {
    PrintStatement {
        parameter_list: Vec<Printable<'input>>,
    },
    ReadStatement {
        parameter_list: Vec<Readable<'input>>,
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
    }
}