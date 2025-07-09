use crate::lexer::{Keyword, Span, Token, TokenType};
/// The root of the Abstract Syntax Tree (AST).
/// It represents the entire program, which is a collection of functions.
#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}



/// Represents a function declaration in the program.
#[derive(Debug)]
pub struct Function {
    /// The return type of the function.
    return_type: Type,
    /// The name of the function.
    pub name: String,
    /// The list of parameters the function accepts.
    pub parameters: Vec<Parameter>,
    /// The body of the function, which is a block of statements.
    pub body: Block,
    /// The span of the function in the source code.
    pub span: Span,
}

/// Represents a block of statements, enclosed in curly braces.
#[derive(Debug)]
pub struct Block {
    /// The list of statements within the block.
    pub stmt: Vec<Statement>,
    /// The span of the block in the source code.
    pub span: Span,
}

/// Represents a parameter in a function declaration.
#[derive(Debug)]
pub struct Parameter {
    /// The type of the parameter.
    param_type: Type,
    /// The name of the parameter.
    param_name: String,
    /// The span of the parameter in the source code.
    span: Span,
}

/// Represents a statement in the C language.
#[derive(Debug)]
pub enum Statement {
    /// A return statement, with an optional expression.
    RETURN(Option<Expression>, Span),
    /// An expression statement.
    EXPRESSION(Expression, Span),
}

/// Represents an expression in the C language.
#[derive(Debug)]
pub enum Expression {
    /// A literal value, such as a number.
    LITERAL(i64, Span),
    /// An identifier, such as a variable name.
    IDENTIFIER(String, Span),
    /// A unary operation, such as negation or complement.
    UNARY(UnaryOperator, Box<Expression>),
    /// A binary operation, such as addition or subtraction.
    Binary(BinaryExp),
}

impl Expression {
    /// Returns the span of the expression in the source code.
    pub fn span(&self) -> Span {
        match self {
            Self::LITERAL(_, span) => span.clone(),
            Self::IDENTIFIER(_, span) => span.clone(),
            Self::UNARY(_, expression) => expression.span(),
            Self::Binary(binary_exp) => binary_exp.span.clone(),
        }
    }

    /// Creates a new binary expression.
    pub fn binary(left: Expression, operator: BinaryOperator, right: Expression) -> Self {
        let span = left.span().start..right.span().end;
        Expression::Binary(BinaryExp {
            left: Box::new(left),
            operator: operator,
            right: Box::new(right),
            span: span,
        })
    }

    /// Creates a new unary expression.
    pub fn unary(operator: UnaryOperator, expression: Expression) -> Expression {
        Expression::UNARY(operator, Box::new(expression))
    }
}

/// Represents a binary expression, with a left-hand side, an operator, and a right-hand side.
#[derive(Debug)]
pub struct BinaryExp {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
    pub span: Span,
}

/// Represents the different types of binary operators.
#[derive(Debug)]
pub enum BinaryOperator {
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

/// Represents the different types of unary operators.
#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

/// Represents the data types in the C language.
#[derive(Debug)]
enum Type {
    INT,
    VOID,
}

/// Represents the different types of errors that can occur during parsing.
#[derive(Debug)]
pub enum ParserError {
    /// An unexpected token was found.
    UnexpectedToken { expected: String, found: Token },
    /// The end of the file was reached unexpectedly.
    UnexpectedEof { expected: String },
    /// An invalid number was encountered.
    InvalidNumber(String),
}

/// The Parser is responsible for constructing the AST from a stream of tokens.
/// It uses a recursive descent approach with Pratt parsing for expressions.
#[derive(Debug)]
pub struct Parser {
    /// The stream of tokens to be parsed.
    tokens: Vec<Token>,
    /// The current position in the token stream.
    pos: usize,
}

impl Parser {
    /// Creates a new Parser instance.
    pub fn new(tokens: Vec<Token>) -> Self {
        println!("[Parser] Initialized with {} tokens.", tokens.len());
        return Self { tokens, pos: 0 };
    }

    /// Parses the entire program and returns the AST.
    pub fn parse(&mut self) -> Result<Program, ParserError> {
        println!("[Parser] Starting to parse program.");
        let mut functions = Vec::new();

        while !self.is_at_end() {
            let function = self.parse_function()?;
            functions.push(function);
        }

        let program = Program { functions };
        println!("[Parser] Finished parsing program.");
        if std::env::var("CC_DEBUG").is_ok() {
            dbg!(&program);
        }
        return Ok(program);
    }

    /// Parses a function declaration.
    /// `Function ::= Type Identifier '(' ParameterList? ')' Block`
    fn parse_function(&mut self) -> Result<Function, ParserError> {
        println!("[Parser] Parsing function...");
        let start_pos = self.peek().span.start;
        let return_type = self.parse_type()?;
        let function_name = self.consume_identifier("Expected function name after type")?;
        println!("[Parser] Parsing function: {}", function_name);

        self.consume_token(TokenType::LPAREN, "Expected '(' after function name")?;
        let parameters = self.parse_parameter_list()?;
        self.consume_token(TokenType::RPAREN, "Expected ')' at end of function declaration")?;

        let body = self.parse_block()?;

        let end_pos = body.span.end;

        let function = Function {
            return_type: return_type,
            name: function_name,
            parameters: parameters,
            body: body,
            span: Span {
                start: start_pos,
                end: end_pos,
            },
        };
        println!("[Parser] Finished parsing function: {}", &function.name);
        return Ok(function);
    }

    /// Parses the list of parameters for a function.
    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, ParserError> {
        println!("[Parser] Parsing parameter list...");
        let mut parameters = Vec::new();

        let ct = &self.peek().ttype;

        if *ct == TokenType::RPAREN {
            return Ok(parameters);
        }
        if *ct == TokenType::KEYWORD(Keyword::VOID) {
            self.advance();
            return Ok(parameters);
        }

        let param_start = self.pos;
        let param_type = self.parse_type()?;
        let param_name = self.consume_identifier("Expected variable name after type")?;
        let param_end = self.pos;

        parameters.push(Parameter {
            param_type: param_type,
            param_name: param_name,
            span: Span {
                start: param_start,
                end: param_end,
            },
        });

        println!("[Parser] Parsed {} parameters.", parameters.len());
        Ok(parameters)
    }

    /// Parses a block of statements.
    /// `Block ::= '{' Statement* '}'`
    fn parse_block(&mut self) -> Result<Block, ParserError> {
        println!("[Parser] Parsing block...");
        let start_pos = self.peek().span.start;
        self.consume_token(TokenType::LBRACE, "Expected '{' at start of block")?;

        let mut stmts = Vec::new();

        while !self.check(TokenType::RBRACE) && !self.is_at_end() {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }

        let end_token = self.consume_token(TokenType::RBRACE, "Expected '}' at end of block")?;
        let end_pos = end_token.span.end;

        println!("[Parser] Finished parsing block with {} statements.", stmts.len());
        return Ok(Block {
            stmt: stmts,
            span: start_pos..end_pos,
        });
    }

    /// Parses a single statement.
    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        println!("[Parser] Parsing statement...");
        if self.match_token(TokenType::KEYWORD(Keyword::RETURN)) {
            self.parse_return_statement()
        } else {
            todo!("this token is not yet implemented")
        }
    }

    /// Parses a return statement.
    /// `ReturnStatement ::= 'return' Expression? ';'`
    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        println!("[Parser] Parsing return statement...");
        let start_pos = self.previous().span.start;

        let expr = if self.check(TokenType::SEMICOLON) {
            None
        } else {
            Some(self.parse_expression(0)?)
        };

        let end_token =
            self.consume_token(TokenType::SEMICOLON, "Expected ';' after return statement")?;
        let end_pos = end_token.span.end;

        println!("[Parser] Finished parsing return statement.");
        return Ok(Statement::RETURN(expr, start_pos..end_pos));
    }

    /// Parses an expression using the Pratt parsing algorithm (precedence climbing).
    /// This allows for handling operator precedence and associativity correctly.
    fn parse_expression(&mut self, min_precedence: u8) -> Result<Expression, ParserError> {
        println!("[Parser] Parsing expression with min_precedence: {}", min_precedence);
        // The left-hand side of the expression is parsed first.
        let mut lhs = self.parse_atom()?;

        // Loop as long as we find an operator with a higher precedence.
        loop {
            if self.is_at_end() {
                break;
            }

            let operator_token = self.peek();

            let operator = match self.token_to_binary_operator(&operator_token.ttype) {
                Some(op) => op,
                None => break,
            };

            let precedence = self.get_binary_operator_precedence(&operator);

            if precedence < min_precedence {
                break;
            }

            self.advance();

            // Recursively parse the right-hand side of the expression.
            let rhs = self.parse_expression(precedence + 1)?;

            lhs = Expression::binary(lhs, operator, rhs);
        }
        Ok(lhs)
    }

    /// Parses an atomic expression, which is the smallest unit of an expression.
    /// This can be a literal, an identifier, a parenthesized expression, or a unary operation.
    fn parse_atom(&mut self) -> Result<Expression, ParserError> {
        println!("[Parser] Parsing atom...");
        match self.peek().ttype {
            TokenType::BITWISE | TokenType::NEGATION => {
                let operator_token = self.advance().clone();

                // Unary operators have high precedence.
                let expr = self.parse_expression(70)?;

                let unary_op = match operator_token.ttype {
                    TokenType::BITWISE => UnaryOperator::Complement,
                    TokenType::NEGATION => UnaryOperator::Negate,
                    _ => unreachable!(),
                };

                Ok(Expression::unary(unary_op, expr))
            }
            TokenType::LPAREN => {
                self.advance(); // consume '('
                let expr = self.parse_expression(0)?;
                self.consume_token(
                    TokenType::RPAREN,
                    "Expected ')' after parenthesized expression",
                )?;

                Ok(expr)
            }
            _ => self.parse_primary(),
        }
    }

    /// Parses a primary expression, which is either a literal or an identifier.
    fn parse_primary(&mut self) -> Result<Expression, ParserError> {
        println!("[Parser] Parsing primary expression...");
        if self.is_at_end() {
            return Err(ParserError::UnexpectedEof {
                expected: "an expression".to_string(),
            });
        }

        match &self.peek().ttype {
            TokenType::NUMBER => {
                let token = self.advance().clone();
                let value = token
                    .lexeme
                    .parse::<i64>()
                    .map_err(|_| ParserError::InvalidNumber(token.lexeme.clone()))?;
                Ok(Expression::LITERAL(value, token.span))
            }
            TokenType::IDENTIFIER => {
                let token = self.advance().clone();
                Ok(Expression::IDENTIFIER(token.lexeme.clone(), token.span))
            }
            _ => Err(ParserError::UnexpectedToken {
                expected: "Expecting an expression".to_string(),
                found: self.peek().clone(),
            }),
        }
    }

    // Helper methods for token manipulation

    /// Consumes the current token if it matches the expected type, otherwise returns an error.
    fn consume_token(&mut self, tt: TokenType, msg: &str) -> Result<&Token, ParserError> {
        if self.check(tt) {
            return Ok(self.advance());
        } else if self.is_at_end() {
            return Err(ParserError::UnexpectedEof {
                expected: (msg.to_string()),
            });
        } else {
            return Err(ParserError::UnexpectedToken {
                expected: (msg.to_string()),
                found: (self.peek().clone()),
            });
        }
    }

    /// Consumes an identifier token and returns its lexeme.
    fn consume_identifier(&mut self, msg: &str) -> Result<String, ParserError> {
        if let TokenType::IDENTIFIER = self.peek().ttype {
            Ok(self.advance().lexeme.clone())
        } else if self.is_at_end() {
            Err(ParserError::UnexpectedEof {
                expected: msg.to_string(),
            })
        } else {
            Err(ParserError::UnexpectedToken {
                expected: msg.to_string(),
                found: self.peek().clone(),
            })
        }
    }

    /// Parses a type specifier.
    fn parse_type(&mut self) -> Result<Type, ParserError> {
        match self.peek().ttype {
            TokenType::KEYWORD(Keyword::INT) => {
                self.advance();
                Ok(Type::INT)
            }

            TokenType::KEYWORD(Keyword::VOID) => {
                self.advance();
                Ok(Type::VOID)
            }

            _ => Err(ParserError::UnexpectedToken {
                expected: "type".to_string(),
                found: self.peek().clone(),
            }),
        }
    }

    /// Peeks at the current token without consuming it.
    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    /// Returns the previous token.
    fn previous(&self) -> &Token {
        &self.tokens[self.pos - 1]
    }

    /// Advances to the next token and returns the previous one.
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.previous()
    }

    /// Checks if the parser has reached the end of the token stream.
    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len() || self.peek().ttype == TokenType::EOF
    }

    /// Checks if the current token matches the expected type.
    fn check(&self, expected: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        } else {
            return self.peek().ttype == expected;
        }
    }

    /// Matches the current token against an expected type and advances if it matches.
    fn match_token(&mut self, expected: TokenType) -> bool {
        if self.check(expected) {
            self.advance();
            return true;
        }
        return false;
    }
}

impl Parser {
    /// Converts a token type to a binary operator.
    fn token_to_binary_operator(&self, token_type: &TokenType) -> Option<BinaryOperator> {
        match token_type {
            TokenType::PLUS => Some(BinaryOperator::Add),
            TokenType::NEGATION => Some(BinaryOperator::Subtract), // '-' can be both unary and binary
            TokenType::ASTERISK => Some(BinaryOperator::Multiply),
            TokenType::ForwardSlash => Some(BinaryOperator::Divide),
            TokenType::PERCENT => Some(BinaryOperator::Remainder),
            TokenType::AND => Some(BinaryOperator::And),
            TokenType::OR => Some(BinaryOperator::Or),
            TokenType::XOR => Some(BinaryOperator::Xor),
            TokenType::LeftShift => Some(BinaryOperator::LeftShift),
            TokenType::RightShift => Some(BinaryOperator::RightShift),
            _ => None,
        }
    }

    /// Returns the precedence of a binary operator.
    fn get_binary_operator_precedence(&self, op: &BinaryOperator) -> u8 {
        match op {
            BinaryOperator::Or => 10,
            BinaryOperator::Xor => 20,
            BinaryOperator::And => 30,
            BinaryOperator::LeftShift | BinaryOperator::RightShift => 40,
            BinaryOperator::Add | BinaryOperator::Subtract => 50,
            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Remainder => 60,
        }
    }
}
