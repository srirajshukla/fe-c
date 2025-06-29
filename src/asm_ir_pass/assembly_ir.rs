#[derive(Debug)]
pub struct AssemblyAst {
    pub function_def: AsmFunction,
}

#[derive(Debug)]
pub struct AsmFunction {
    pub name: String,
    pub instructions: Vec<AsmInstruction>,
}

#[derive(Debug, Clone)]
pub enum AsmInstruction {
    Ret,
    AsmUnary(AsmUnaryOperator, AsmOperand),
    AllocateStack(i64),
    Mov(AsmOperand, AsmOperand),
    Push(AsmRegister),
    Pop(AsmRegister),
}

#[derive(Debug, Clone)]
pub enum AsmUnaryOperator {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum AsmOperand {
    Imm(i64),
    Reg(AsmRegister),
    Pseudo(String),
    Stack(i64),
}

#[derive(Debug, Clone)]
pub enum AsmRegister {
    AX,
    ECX,
    R10,
    RBP,
    RSP,
}

