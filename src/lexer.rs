#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    BITWISE,
    NEGATION,
    DECREMENT,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    SEMICOLON,
    NUMBER,
    IDENTIFIER,
    KEYWORD(Keyword),
    EOF,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    INT,
    VOID,
    RETURN,
}

impl Keyword {
    #[allow(unused)]
    pub fn as_str(&self) -> &'static str {
        match self {
            Keyword::INT => "int",
            Keyword::VOID => "void",
            Keyword::RETURN => "return",
        }
    }

    pub fn to_keyword(s: &str) -> Option<Self> {
        match s {
            "int" => Some(Self::INT),
            "void" => Some(Self::VOID),
            "return" => Some(Self::RETURN),
            _ => None,
        }
    }

    pub fn is_keyword(s: &str) -> bool {
        match Keyword::to_keyword(s) {
            Some(_) => true,
            None => false,
        }
    }
}

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub ttype: TokenType,
    pub span: Span,
    pub lexeme: String,
}

impl Token {
    fn new(ttype: TokenType, start: usize, end: usize, value: String) -> Self {
        return Self {
            ttype: ttype,
            span: Span {
                start: start,
                end: end,
            },
            lexeme: value,
        };
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(program: String) -> Self {
        let input: Vec<char> = program.chars().collect();
        let current_char = input.first().copied();
        Self {
            input: input,
            pos: 0,
            current_char: current_char,
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.current_char = self.input.get(self.pos).copied();
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.pos + 1).copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) -> Result<(), String> {
        let cc = self.current_char;

        if cc.is_none() {
            return Err(format!("Unexpected EOF at position {}", self.pos));
        }

        let cc = cc.unwrap();

        if cc == '/' {
            // skip characters until new line is found
            self.advance(); // skip /

            while let Some(ch) = self.current_char {
                self.advance();
                if ch == '\n' {
                    break;
                }
            }
        } else if cc == '*' {
            self.advance();

            while let Some(ch) = self.current_char {
                self.advance();
                if ch == '*' {
                    if self.current_char.is_none() {
                        return Err(format!("Unexpected EOF at position {}", self.pos));
                    }

                    let cc = self.current_char.unwrap();
                    println!("this comment line: {}, cc = {}", self.pos, cc);
                    if cc == '/' {
                        self.advance();
                        break;
                    }
                }
            }
        }
        return Ok(());
    }

    fn parse_number(&mut self) -> Result<Token, String> {
        println!("parsing a number now");
        let start_pos = self.pos;
        let mut numbers = String::new();

        while let Some(ch) = self.current_char {
            if ch.is_ascii_digit() {
                numbers.push(ch);
                self.advance();
            } else {
                // only if this is a breaking character, the number is valid
                // if it a character for example, this number is probably invalid
                if ch.is_alphabetic() {
                    return Err(format!(
                        "A number cannot end with alphabets. Ends with {} at {}",
                        ch, self.pos
                    ));
                }
                break;
            }
        }

        if numbers.is_empty() {
            return Err("Not a valid number".to_string());
        }
        return Ok(Token::new(
            TokenType::NUMBER,
            start_pos,
            self.pos,
            numbers.to_string(),
        ));
    }

    fn parse_keyword_or_ident(&mut self) -> Result<Token, String> {
        let start_pos = self.pos;
        let mut word = String::new();

        if let Some(ch) = self.current_char {
            if ch.is_alphabetic() || ch == '_' {
                word.push(ch);
                self.advance();
            } else {
                return Err(format!(
                    "Identifier should start with alphabet or _, starts with {}",
                    ch
                ));
            }
        }

        while let Some(ch) = self.current_char {
            if ch.is_alphanumeric() || ch == '_' {
                word.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        if Keyword::is_keyword(&word) {
            let kw = Keyword::to_keyword(&word).unwrap();
            return Ok(Token::new(
                TokenType::KEYWORD(kw),
                start_pos,
                self.pos,
                word,
            ));
        } else {
            return Ok(Token::new(TokenType::IDENTIFIER, start_pos, self.pos, word));
        }
    }

    fn get_next_token(&mut self) -> Result<Token, String> {
        loop {
            self.skip_whitespace();

            let i = self.pos;
            println!("Lexing at position: {}", i);
            match self.current_char {
                None => return Ok(Token::new(TokenType::EOF, i, i, String::new())),
                Some('~') => {
                    self.advance();
                    return Ok(Token::new(TokenType::BITWISE, i, i + 1, "~".to_string()));
                }
                Some('-') => {
                    self.advance();
                    if let Some(x) = self.current_char {
                        if x == '-' {
                            self.advance();
                            return Ok(Token::new(
                                TokenType::DECREMENT,
                                i,
                                i + 2,
                                "--".to_string(),
                            ));
                        }
                    }
                    return Ok(Token::new(TokenType::NEGATION, i, i + 1, "-".to_string()));
                }
                Some('(') => {
                    self.advance();
                    return Ok(Token::new(TokenType::LPAREN, i, i + 1, "(".to_string()));
                }
                Some(')') => {
                    self.advance();
                    return Ok(Token::new(TokenType::RPAREN, i, i + 1, ")".to_string()));
                }
                Some('{') => {
                    self.advance();
                    return Ok(Token::new(TokenType::LBRACE, i, i + 1, "{".to_string()));
                }
                Some('}') => {
                    self.advance();
                    return Ok(Token::new(TokenType::RBRACE, i, i + 1, "}".to_string()));
                }
                Some(';') => {
                    self.advance();
                    return Ok(Token::new(TokenType::SEMICOLON, i, i + 1, ";".to_string()));
                }
                Some('/') => {
                    let next = self.peek();
                    if next.is_none() {
                        return Err(format!("Unexpected EOF after / at position {}", i));
                    }
                    let next = next.unwrap();

                    if next == '/' || next == '*' {
                        self.advance();
                        self.skip_comment()?;
                    }
                }
                Some(ch) if ch.is_ascii_digit() => {
                    return self.parse_number();
                }
                Some(ch) if ch.is_alphabetic() || ch == '_' => {
                    return self.parse_keyword_or_ident();
                }
                ch => {
                    return Err(format!(
                        "Unimplemented character: {:#?} at position: {}",
                        &ch, i
                    ));
                }
            }
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();

        loop {
            let token = self.get_next_token()?;
            let is_eof = token.ttype == TokenType::EOF;

            if is_eof {
                break;
            }

            tokens.push(token);
        }

        return Ok(tokens);
    }
}
