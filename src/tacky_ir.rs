#[derive(Debug)]
pub struct TackyProgram {
    pub function_definition: TackyFunction,
}

#[derive(Debug)]
pub struct TackyFunction {
    pub identifier: String,
    pub body: Vec<TackyInstruction>,
}

#[derive(Debug)]
pub enum TackyInstruction {
    Return(Option<TackyVal>),
    Unary(TackyUnary)
}

#[derive(Debug, Clone)]
pub enum TackyVal {
    Constant(i64),
    Variable(String)
}

#[derive(Debug, Clone)]
pub struct TackyUnary {
    pub operator: TackyUnaryOperator,
    pub src: TackyVal,
    pub dest: TackyVal,
}

#[derive(Debug, Clone)]
pub enum TackyUnaryOperator {
    Complement,
    Negate,
}