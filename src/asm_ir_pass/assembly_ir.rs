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
    AsmBinary(AsmBinaryOperator, AsmOperand, AsmOperand),
    AllocateStack(i64),
    Mov(AsmOperand, AsmOperand),
    Push(AsmRegister),
    Pop(AsmRegister),
    Idiv(AsmOperand),
    Cdq,
}

#[derive(Debug, Clone)]
pub enum AsmUnaryOperator {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmBinaryOperator {
    Add,
    Sub,
    Mult,
    Xor,
    And,
    Or,
    Shr, // shift right
    Shl, // shift left
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
    DX,
    R10,
    R11,
    CL,
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
            AsmInstruction::AsmBinary(op, op1, op2) => write!(f, "{} {} {}", op, op1, op2),
            AsmInstruction::Idiv(src) => write!(f, "idivq {}", src),
            AsmInstruction::Cdq => write!(f, "cqo"),
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

impl Display for AsmBinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AsmBinaryOperator::Add => write!(f, "addq"),
            AsmBinaryOperator::Sub => write!(f, "subq"),
            AsmBinaryOperator::Mult => write!(f, "imulq"),
            AsmBinaryOperator::Xor => write!(f, "xorq"),
            AsmBinaryOperator::And => write!(f, "andq"),
            AsmBinaryOperator::Or => write!(f, "orq"),
            AsmBinaryOperator::Shr => write!(f, "shrq"),
            AsmBinaryOperator::Shl => write!(f, "shlq"),
        }
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
            AsmRegister::DX => write!(f, "%rdx"),
            AsmRegister::ECX => write!(f, "%rcx"),
            AsmRegister::R10 => write!(f, "%r10"),
            AsmRegister::R11 => write!(f, "%r11"),
            AsmRegister::CL => write!(f, "%cl"),
            AsmRegister::RBP => write!(f, "%rbp"),
            AsmRegister::RSP => write!(f, "%rsp"),
        } 
    }
}
