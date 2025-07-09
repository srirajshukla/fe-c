use std::fmt::{Display, Formatter};

/// Represents the entire program as an Assembly Abstract Syntax Tree (AST).
/// This is a low-level representation of the program, close to the final assembly code.
#[derive(Debug)]
pub struct AssemblyAst {
    pub function_def: AsmFunction,
}

/// Represents a single function in the Assembly AST.
#[derive(Debug)]
pub struct AsmFunction {
    /// The name of the function.
    pub name: String,
    /// The list of assembly instructions in the function body.
    pub instructions: Vec<AsmInstruction>,
}

/// Represents a single assembly instruction.
#[derive(Debug, Clone)]
pub enum AsmInstruction {
    /// Return from the function.
    Ret,
    /// A unary operation (e.g., `negq rax`).
    AsmUnary(AsmUnaryOperator, AsmOperand),
    /// A binary operation (e.g., `addq rax, rbx`).
    AsmBinary(AsmBinaryOperator, AsmOperand, AsmOperand),
    /// Allocate space on the stack.
    AllocateStack(i64),
    /// Move data between operands.
    Mov(AsmOperand, AsmOperand),
    /// Push a register onto the stack.
    Push(AsmRegister),
    /// Pop a value from the stack into a register.
    Pop(AsmRegister),
    /// Signed division.
    Idiv(AsmOperand),
    /// Convert doubleword to quadword.
    Cdq,
}

/// Represents a unary operator in assembly.
#[derive(Debug, Clone)]
pub enum AsmUnaryOperator {
    Neg, // Negation
    Not, // Bitwise NOT
}

/// Represents a binary operator in assembly.
#[derive(Debug, Clone, PartialEq)]
pub enum AsmBinaryOperator {
    Add,
    Sub,
    Mult,
    Xor,
    And,
    Or,
    Shr, // Shift right
    Shl, // Shift left
}

/// Represents an operand in an assembly instruction.
#[derive(Debug, Clone)]
pub enum AsmOperand {
    /// An immediate value (a constant).
    Imm(i64),
    /// A hardware register.
    Reg(AsmRegister),
    /// A pseudo-register, which will be replaced by a stack location.
    /// This is an abstraction used before the final register allocation.
    Pseudo(String),
    /// A location on the stack, represented as an offset from the base pointer.
    Stack(i64),
}

/// Represents a hardware register in the x86-64 architecture.
/// The comments indicate the register's role in the System V ABI.
#[derive(Debug, Clone)]
pub enum AsmRegister {
    // The following registers are used for integer arguments and return values.
    AX,
    ECX,
    DX,

    // The following registers are temporary and can be used for intermediate calculations.
    R10, // Temporary register
    R11, // Temporary register

    // The following registers have special purposes.
    CL,  // Low byte of ECX, used for shifts
    RBP, // Base pointer, for stack frames
    RSP, // Stack pointer
}

// Display implementations for pretty-printing the Assembly IR.

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
            AsmInstruction::AsmBinary(op, op1, op2) => write!(f, "{} {}, {}", op, op1, op2),
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
            AsmOperand::Imm(v) => write!(f, "${}", v),
            AsmOperand::Reg(reg) => write!(f, "{}", reg),
            AsmOperand::Pseudo(id) => write!(f, "% {}(%rip)", id), // RIP-relative addressing for pseudos
            AsmOperand::Stack(offset) => write!(f, "{}(%rbp)", offset),
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
