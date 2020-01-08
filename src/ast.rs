pub struct Declaration<'input> {
    pub variable_list: VariableList<'input>,
}

pub type VariableList<'input> = Vec<Variable<'input>>;

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

pub struct Variable<'input> {
    pub name: Identifier<'input>,
    pub variable_type: VariableType,
}

pub struct NumLiteral {
    pub value: f64,
}

pub struct Identifier<'input> {
    pub name: &'input str
}

pub struct StringLiteral<'input> {
    pub value: &'input str
}