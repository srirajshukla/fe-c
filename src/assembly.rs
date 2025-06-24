#[derive(Debug)]
pub struct AssemblyAst {
    pub function_def: FunctionAst,
}

#[derive(Debug)]
pub struct FunctionAst {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Ret,
    Mov(Operand, Operand),
}

#[derive(Debug)]
pub enum Operand {
    Imm(i64),
    Reg(Register),
}

#[derive(Debug)]
pub enum Register {
    EAX,
    ECX,
}
