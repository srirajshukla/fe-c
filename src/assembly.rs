#[derive(Debug)]
pub struct AssemblyAst {
    pub function_def: FunctionAst
}

#[derive(Debug)]
pub struct FunctionAst {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Ret,
    Mov(Operand, Operand)
}

impl Instruction {
    pub fn to_string(&self) -> String {
        match self {
            Instruction::Ret => "ret".to_string(),
            Instruction::Mov(src, dest) => {
                let mut ins = String::from("movl ");
                ins.push_str(src.to_string().as_str());
                ins.push_str(", ");
                ins.push_str(dest.to_string().as_str());
                return ins;
            }
        }
    }
}

#[derive(Debug)]
pub enum Operand {
    Imm(i64),
    Register
}

impl Operand {
    pub fn to_string(&self) -> String {
        match self {
            Operand::Imm(v) => {
                let mut o =  String::from("$");
                o.push_str(v.to_string().as_str());
                return o;
            }
            Operand::Register => {
                "%eax".to_string()
            }
        }
    }
}

