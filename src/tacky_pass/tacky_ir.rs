/// Tacky IR (Three-Address Code) is a simplified, linear representation of the program.
/// It's an intermediate step between the AST and the final assembly code.
/// Each instruction has at most three operands (two sources and one destination),
/// which makes it easy to translate into assembly.

use std::fmt;

/// Represents the entire program in Tacky IR.
#[derive(Debug)]
pub struct TackyProgram {
    pub function_definition: TackyFunction,
}

/// Represents a single function in Tacky IR.
#[derive(Debug)]
pub struct TackyFunction {
    /// The name of the function.
    pub identifier: String,
    /// The body of the function, a list of Tacky instructions.
    pub body: Vec<TackyInstruction>,
}

/// Represents a single instruction in Tacky IR.
/// Each instruction is a simple operation, like return, or a unary/binary operation.
#[derive(Debug)]
pub enum TackyInstruction {
    /// A return instruction, with an optional return value.
    Return(Option<TackyVal>),
    /// A unary operation (e.g., negation).
    Unary(TackyUnary),
    /// A binary operation (e.g., addition).
    Binary(TackyBinary),
}

/// Represents a value in Tacky IR, which can be a constant or a temporary variable.
#[derive(Debug, Clone)]
pub enum TackyVal {
    /// A constant integer value.
    Constant(i64),
    /// A temporary variable, represented as a string (e.g., "tmp.0").
    /// These are created by the Tacky IR generator to hold intermediate results.
    Variable(String),
}

/// Represents a unary operation in Tacky IR (e.g., `dest = op src`).
#[derive(Debug, Clone)]
pub struct TackyUnary {
    pub operator: TackyUnaryOperator,
    pub src: TackyVal,
    pub dest: TackyVal,
}

/// Represents the different types of unary operators in Tacky IR.
#[derive(Debug, Clone)]
pub enum TackyUnaryOperator {
    Complement,
    Negate,
}

/// Represents a binary operation in Tacky IR (e.g., `dest = src1 op src2`).
#[derive(Debug, Clone)]
pub struct TackyBinary {
    pub operator: TackyBinaryOperator,
    pub src1: TackyVal,
    pub src2: TackyVal,
    pub dest: TackyVal,
}

/// Represents the different types of binary operators in Tacky IR.
#[derive(Debug, Clone)]
pub enum TackyBinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    And,
    Or,
    Xor,
    LeftShift,
    RightShift,
}

// Display implementations for pretty-printing the Tacky IR.

impl fmt::Display for TackyProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "FUNCTION {}", self.function_definition.identifier)?;
        write!(f, "{}", self.function_definition)
    }
}

impl fmt::Display for TackyFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, instr) in self.body.iter().enumerate() {
            writeln!(f, "{}: {}", i, instr)?;
        }
        Ok(())
    }
}

impl fmt::Display for TackyInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TackyInstruction::Return(v) => match v {
                None => write!(f, "return"),
                Some(v) => write!(f, "return {}", v),
            },
            TackyInstruction::Unary(unary) => {
                write!(f, "{} = {} {}", unary.dest, unary.operator, unary.src)
            }
            TackyInstruction::Binary(binary) => {
                write!(
                    f,
                    "{} = {} {} {}",
                    binary.dest, binary.src1, binary.operator, binary.src2
                )
            }
        }
    }
}

impl fmt::Display for TackyVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TackyVal::Constant(c) => write!(f, "{}", c),
            TackyVal::Variable(v) => write!(f, "{}", v),
        }
    }
}

impl fmt::Display for TackyUnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TackyUnaryOperator::Complement => write!(f, "~"),
            TackyUnaryOperator::Negate => write!(f, "-"),
        }
    }
}

impl fmt::Display for TackyBinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TackyBinaryOperator::Add => write!(f, "+"),
            TackyBinaryOperator::Subtract => write!(f, "-"),
            TackyBinaryOperator::Multiply => write!(f, "*"),
            TackyBinaryOperator::Divide => write!(f, "/"),
            TackyBinaryOperator::Remainder => write!(f, "%"),
            TackyBinaryOperator::And => write!(f, "&"),
            TackyBinaryOperator::Or => write!(f, "|"),
            TackyBinaryOperator::Xor => write!(f, "^"),
            TackyBinaryOperator::LeftShift => write!(f, "<<"),
            TackyBinaryOperator::RightShift => write!(f, ">>"),
        }
    }
}

impl TackyProgram {
    /// Pretty prints the Tacky IR with comments explaining each instruction.
    pub fn pretty_print_with_comments(&self) -> String {
        let mut result = String::new();
        result.push_str("# Three-Address Code (Tacky IR)\n");
        result.push_str(&format!(
            "# Function: {}\n\n",
            self.function_definition.identifier
        ));

        for (i, instruction) in self.function_definition.body.iter().enumerate() {
            result.push_str(&format!(
                "  {:2}: {}  # {}\n",
                i,
                instruction,
                instruction.description()
            ));
        }

        result
    }
}
impl TackyInstruction {
    /// Returns a human-readable description of the Tacky instruction.
    pub fn description(&self) -> &'static str {
        match self {
            TackyInstruction::Return(_) => "Return from function",
            TackyInstruction::Unary(unary) => match unary.operator {
                TackyUnaryOperator::Complement => "Bitwise complement",
                TackyUnaryOperator::Negate => "Arithmetic negation",
            },
            TackyInstruction::Binary(binary) => match binary.operator {
                TackyBinaryOperator::Add => "Addition",
                TackyBinaryOperator::Subtract => "Subtraction",
                TackyBinaryOperator::Multiply => "Multiplication",
                TackyBinaryOperator::Divide => "Division",
                TackyBinaryOperator::Remainder => "Remainder",
                TackyBinaryOperator::And => "Bitwise AND",
                TackyBinaryOperator::Or => "Bitwise OR",
                TackyBinaryOperator::Xor => "Bitwise XOR",
                TackyBinaryOperator::LeftShift => "Left shift",
                TackyBinaryOperator::RightShift => "Right shift",
            },
        }
    }
}
