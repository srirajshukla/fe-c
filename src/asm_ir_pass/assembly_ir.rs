use std::fmt::{Display, Formatter};

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

impl Display for AssemblyAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.function_def)
    }
}

impl Display for AsmFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, ".globl {}", self.name)?;
        writeln!(f, "{}:", self.name)?;
        for instruction in &self.instructions {
            writeln!(f, "\t{}", instruction)?;
        }
        Ok(())
    }
}

impl Display for AsmInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AsmInstruction::Ret => write!(f, "ret"),
            AsmInstruction::Push(reg) => write!(f, "pushq {}", reg),
            AsmInstruction::Pop(reg) => write!(f, "popq {}", reg),
            AsmInstruction::AllocateStack(size) => write!(f, "subq ${}, %rsp", size),
            AsmInstruction::Mov(src, dest) => write!(f, "movq {}, {}", src, dest),
            AsmInstruction::AsmUnary(op, operand) => write!(f, "{} {}", op, operand),
        }
    }
}

impl Display for AsmUnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let v = match self { 
            AsmUnaryOperator::Neg => "negq",
            AsmUnaryOperator::Not => "notq",
        };
        write!(f, "{}", v)
    }
}

impl Display for AsmOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AsmOperand::Imm(v) => {
                write!(f, "${}", v)
            }
            AsmOperand::Reg(reg) => {
                write!(f, "{}", reg)
            }
            AsmOperand::Pseudo(id) => {
                write!(f, "{}", id)
            }
            AsmOperand::Stack(offset) => {
                write!(f, "{}(%rbp)", offset)
            }
        }
    }
}

impl Display for AsmRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self { 
            AsmRegister::AX => write!(f, "%rax"),
            AsmRegister::ECX => write!(f, "%rcx"),
            AsmRegister::R10 => write!(f, "%r10"),
            AsmRegister::RBP => write!(f, "%rsp"),
            AsmRegister::RSP => write!(f, "%rsp"),
        } 
    }
}