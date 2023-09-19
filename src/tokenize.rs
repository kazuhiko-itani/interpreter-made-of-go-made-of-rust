#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Let,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    Eq,
    NotEq,
    Identifier(String),
    Assign,
    IntLiteral(i64),
    Function,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Plus,
    MINUS,
    BANG,
    SLASH,
    ASTERISK,
    LT,
    GT,
    Semicolon,
    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    literal: String,
}

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: None,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = Some(self.input.chars().nth(self.read_position)).unwrap();
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            Some('=') => {
                if self.peek_char() == Some('=') {
                    let literal = "==".to_string();
                    self.read_char();
                    Token {
                        token_type: TokenType::Eq,
                        literal,
                    }
                } else {
                    Token {
                        token_type: TokenType::Assign,
                        literal: "=".to_string(),
                    }
                }
            }
            Some(';') => Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Some('(') => Token {
                token_type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Some(')') => Token {
                token_type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Some('{') => Token {
                token_type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Some('}') => Token {
                token_type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Some(',') => Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Some('+') => Token {
                token_type: TokenType::Plus,
                literal: "+".to_string(),
            },
            Some('-') => Token {
                token_type: TokenType::MINUS,
                literal: "-".to_string(),
            },
            Some('!') => {
                if self.peek_char() == Some('=') {
                    let literal = "!=".to_string();
                    self.read_char();
                    Token {
                        token_type: TokenType::NotEq,
                        literal,
                    }
                } else {
                    Token {
                        token_type: TokenType::BANG,
                        literal: "!".to_string(),
                    }
                }
            }
            Some('/') => Token {
                token_type: TokenType::SLASH,
                literal: "/".to_string(),
            },
            Some('*') => Token {
                token_type: TokenType::ASTERISK,
                literal: "*".to_string(),
            },
            Some('<') => Token {
                token_type: TokenType::LT,
                literal: "<".to_string(),
            },
            Some('>') => Token {
                token_type: TokenType::GT,
                literal: ">".to_string(),
            },
            Some(ch) if ch.is_alphabetic() => {
                let literal = self.read_identifier();
                let token_type = match literal.as_str() {
                    "let" => TokenType::Let,
                    "fn" => TokenType::Function,
                    "true" => TokenType::TRUE,
                    "false" => TokenType::FALSE,
                    "if" => TokenType::IF,
                    "else" => TokenType::ELSE,
                    "return" => TokenType::RETURN,
                    _ => TokenType::Identifier(literal.clone()),
                };

                return Token {
                    token_type,
                    literal,
                };
            }
            Some(ch) if ch.is_numeric() => {
                let literal = self.read_number();
                return Token {
                    token_type: TokenType::IntLiteral(literal.parse().unwrap()),
                    literal,
                };
            }
            None => {
                return Token {
                    token_type: TokenType::EOF,
                    literal: "".to_string(),
                };
            }
            _ => unimplemented!(),
        };

        self.read_char();
        token
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            if ch.is_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    fn read_identifier(&mut self) -> String {
        let start_position = self.position;
        while let Some(ch) = self.ch {
            if ch.is_alphabetic() || ch == '_' {
                self.read_char();
            } else {
                break;
            }
        }

        self.input[start_position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let start_position = self.position;
        while let Some(ch) = self.ch {
            if ch.is_numeric() {
                self.read_char();
            } else {
                break;
            }
        }

        self.input[start_position..self.position].to_string()
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input.chars().nth(self.read_position)).unwrap()
        }
    }
}

fn main() {
    let input = "
    let five = 5;
    let ten = 10;

    let add = fn(x, y) {
        x + y;
    };

    let result = add(five, ten);

    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
        return true;
    } else {
        return false;
    }

    10 == 10;
    10 != 9;
    ";

    let mut lexer = Lexer::new(input);
    loop {
        let token = lexer.next_token();
        if token.token_type == TokenType::EOF {
            break;
        }

        println!("{:?}", token);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "
        let five = 5;
        let ten = 10;
    
        let add = fn(x, y) {
            x + y;
        };
    
        let result = add(five, ten);

        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        ";

        let mut lexer = Lexer::new(input);

        let tests = vec![
            Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            Token {
                token_type: TokenType::Identifier("five".to_string()),
                literal: "five".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::IntLiteral(5),
                literal: "5".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            Token {
                token_type: TokenType::Identifier("ten".to_string()),
                literal: "ten".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::IntLiteral(10),
                literal: "10".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            Token {
                token_type: TokenType::Identifier("add".to_string()),
                literal: "add".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Function,
                literal: "fn".to_string(),
            },
            Token {
                token_type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                literal: "x".to_string(),
            },
            Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Token {
                token_type: TokenType::Identifier("y".to_string()),
                literal: "y".to_string(),
            },
            Token {
                token_type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Token {
                token_type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                literal: "x".to_string(),
            },
            Token {
                token_type: TokenType::Plus,
                literal: "+".to_string(),
            },
            Token {
                token_type: TokenType::Identifier("y".to_string()),
                literal: "y".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            Token {
                token_type: TokenType::Identifier("result".to_string()),
                literal: "result".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Identifier("add".to_string()),
                literal: "add".to_string(),
            },
            Token {
                token_type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Token {
                token_type: TokenType::Identifier("five".to_string()),
                literal: "five".to_string(),
            },
            Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Token {
                token_type: TokenType::Identifier("ten".to_string()),
                literal: "ten".to_string(),
            },
            Token {
                token_type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::BANG,
                literal: "!".to_string(),
            },
            Token {
                token_type: TokenType::MINUS,
                literal: "-".to_string(),
            },
            Token {
                token_type: TokenType::SLASH,
                literal: "/".to_string(),
            },
            Token {
                token_type: TokenType::ASTERISK,
                literal: "*".to_string(),
            },
            Token {
                token_type: TokenType::IntLiteral(5),
                literal: "5".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::IntLiteral(5),
                literal: "5".to_string(),
            },
            Token {
                token_type: TokenType::LT,
                literal: "<".to_string(),
            },
            Token {
                token_type: TokenType::IntLiteral(10),
                literal: "10".to_string(),
            },
            Token {
                token_type: TokenType::GT,
                literal: ">".to_string(),
            },
            Token {
                token_type: TokenType::IntLiteral(5),
                literal: "5".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::IF,
                literal: "if".to_string(),
            },
            Token {
                token_type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Token {
                token_type: TokenType::IntLiteral(5),
                literal: "5".to_string(),
            },
            Token {
                token_type: TokenType::LT,
                literal: "<".to_string(),
            },
            Token {
                token_type: TokenType::IntLiteral(10),
                literal: "10".to_string(),
            },
            Token {
                token_type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Token {
                token_type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Token {
                token_type: TokenType::RETURN,
                literal: "return".to_string(),
            },
            Token {
                token_type: TokenType::TRUE,
                literal: "true".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Token {
                token_type: TokenType::ELSE,
                literal: "else".to_string(),
            },
            Token {
                token_type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Token {
                token_type: TokenType::RETURN,
                literal: "return".to_string(),
            },
            Token {
                token_type: TokenType::FALSE,
                literal: "false".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Token {
                token_type: TokenType::IntLiteral(10),
                literal: "10".to_string(),
            },
            Token {
                token_type: TokenType::Eq,
                literal: "==".to_string(),
            },
            Token {
                token_type: TokenType::IntLiteral(10),
                literal: "10".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::IntLiteral(10),
                literal: "10".to_string(),
            },
            Token {
                token_type: TokenType::NotEq,
                literal: "!=".to_string(),
            },
            Token {
                token_type: TokenType::IntLiteral(9),
                literal: "9".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
        ];

        for test in tests {
            let token = lexer.next_token();
            assert_eq!(token.token_type, test.token_type);
            assert_eq!(token.literal, test.literal);
        }
    }
}
