use std::fmt;

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
    Unary(TackyUnary),
    Binary(TackyBinary),
}

#[derive(Debug, Clone)]
pub enum TackyVal {
    Constant(i64),
    Variable(String),
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

#[derive(Debug, Clone)]
pub struct TackyBinary {
    pub operator: TackyBinaryOperator,
    pub src1: TackyVal,
    pub src2: TackyVal,
    pub dest: TackyVal,
}

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
            },
            TackyInstruction::Binary(binary) => {
                write!(f, "{} = {} {} {}", binary.dest, binary.src1, binary.operator, binary.src2)
            }
        }
    }
}

impl fmt::Display for TackyVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TackyVal::Constant(c) => {
                write!(f, "{}", c)
            }
            TackyVal::Variable(v) => {
                write!(f, "{}", v)
            }
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
    pub fn pretty_print_with_comments(&self) -> String {
        let mut result = String::new();
        result.push_str(&"# Three-Address Code (Tacky IR)\n".to_string());
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
                TackyBinaryOperator::Multiply => "Multiply",
                TackyBinaryOperator::Divide => "Divide",
                TackyBinaryOperator::Remainder => "Remainder",
                TackyBinaryOperator::And => "Bitwise And",
                TackyBinaryOperator::Or => "Bitwise Or",
                TackyBinaryOperator::Xor => "Bitwise Xor",
                TackyBinaryOperator::LeftShift => "Leftshift Operator",
                TackyBinaryOperator::RightShift => "Rightshift operator",
            }
        }
    }
}
