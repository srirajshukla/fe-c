use crate::parser;
use crate::tacky_pass::tacky_ir;

pub struct TackyIrGenerator {
    var_cnt: usize,
}

#[derive(Debug)]
pub enum TackyError {
    Error(String),
}

impl TackyIrGenerator {
    pub fn new() -> Self {
        Self { var_cnt: 0 }
    }

    fn make_temporary(&mut self) -> String {
        let var = format!("tmp.{}", self.var_cnt);
        self.var_cnt += 1;
        return var;
    }

    pub fn generate_program(
        &mut self,
        program: &parser::Program,
    ) -> Result<tacky_ir::TackyProgram, TackyError> {
        let function = program
            .functions
            .first()
            .ok_or(TackyError::Error("expected a function".to_string()))?;

        let function_ir = self.generate_function(&function)?;

        Ok(tacky_ir::TackyProgram {
            function_definition: function_ir,
        })
    }

    fn generate_function(
        &mut self,
        function: &parser::Function,
    ) -> Result<tacky_ir::TackyFunction, TackyError> {
        let mut instructions = Vec::new();

        for stmt in &function.body.stmt {
            let mut gen_instructions = self.generate_statement(&stmt)?;
            instructions.append(&mut gen_instructions);
        }

        Ok(tacky_ir::TackyFunction {
            identifier: function.name.clone(),
            body: instructions,
        })
    }

    fn generate_statement(
        &mut self,
        stmt: &parser::Statement,
    ) -> Result<Vec<tacky_ir::TackyInstruction>, TackyError> {
        match stmt {
            parser::Statement::RETURN(expr, _sp) => {
                let mut instructions = Vec::new();

                match expr {
                    None => {
                        let tacky = tacky_ir::TackyInstruction::Return(None);
                        instructions.push(tacky);
                    }
                    Some(expr) => {
                        let (mut expr_tacky, return_val) = self.emit_expression(&expr)?;
                        instructions.append(&mut expr_tacky);
                        instructions.push(tacky_ir::TackyInstruction::Return(Some(return_val)));
                    }
                }

                Ok(instructions)
            }
            _ => Err(TackyError::Error(format!(
                "unexpected statement {:?}",
                stmt
            ))),
        }
    }

    fn emit_expression(
        &mut self,
        expr: &parser::Expression,
    ) -> Result<(Vec<tacky_ir::TackyInstruction>, tacky_ir::TackyVal), TackyError> {
        match expr {
            parser::Expression::LITERAL(c, _) => Ok((Vec::new(), tacky_ir::TackyVal::Constant(*c))),
            parser::Expression::IDENTIFIER(s, _) => Err(TackyError::Error(format!(
                "tacky for identifier {} not yet implemented",
                &s
            ))),
            parser::Expression::UNARY(op, expr) => {
                let (mut instructions, operand) = self.emit_expression(expr)?;
                let result_var = self.make_temporary();
                let result_var = tacky_ir::TackyVal::Variable(result_var);

                let unary = tacky_ir::TackyInstruction::Unary(tacky_ir::TackyUnary {
                    operator: Self::convert_unary_operator(op),
                    src: operand,
                    dest: result_var.clone(),
                });

                instructions.push(unary);
                Ok((instructions, result_var))
            }
            parser::Expression::Binary(parser::BinaryExp{left, operator, right, span: _span}) => {
                let (mut instructions1, operand1) = self.emit_expression(left)?;
                let (mut instructions2, operand2) = self.emit_expression(right)?;

                let dst_var = self.make_temporary();
                let dst_var = tacky_ir::TackyVal::Variable(dst_var);

                let binary = tacky_ir::TackyInstruction::Binary(
                    tacky_ir::TackyBinary {
                        operator: Self::convert_binary_operator(operator),
                        src1: operand1,
                        src2: operand2,
                        dest: dst_var.clone(),
                    }
                );

                let mut instructions = Vec::new();
                instructions.append(&mut instructions1);
                instructions.append(&mut instructions2);
                instructions.push(binary);

                Ok((instructions, dst_var))
            }
        }
    }

    fn convert_unary_operator(op: &parser::UnaryOperator) -> tacky_ir::TackyUnaryOperator {
        match op {
            parser::UnaryOperator::Complement => tacky_ir::TackyUnaryOperator::Complement,
            parser::UnaryOperator::Negate => tacky_ir::TackyUnaryOperator::Negate,
        }
    }

    fn convert_binary_operator(op: &parser::BinaryOperator) -> tacky_ir::TackyBinaryOperator {
        match op {
            parser::BinaryOperator::Add => tacky_ir::TackyBinaryOperator::Add,
            parser::BinaryOperator::Subtract => tacky_ir::TackyBinaryOperator::Subtract,
            parser::BinaryOperator::Multiply => tacky_ir::TackyBinaryOperator::Multiply,
            parser::BinaryOperator::Divide => tacky_ir::TackyBinaryOperator::Divide,
            parser::BinaryOperator::Remainder => tacky_ir::TackyBinaryOperator::Remainder,
            parser::BinaryOperator::And => tacky_ir::TackyBinaryOperator::And,
            parser::BinaryOperator::Or => tacky_ir::TackyBinaryOperator::Or,
            parser::BinaryOperator::Xor => tacky_ir::TackyBinaryOperator::Xor,
            parser::BinaryOperator::LeftShift => tacky_ir::TackyBinaryOperator::LeftShift,
            parser::BinaryOperator::RightShift => tacky_ir::TackyBinaryOperator::RightShift,
        }
    }
}
