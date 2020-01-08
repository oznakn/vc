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
