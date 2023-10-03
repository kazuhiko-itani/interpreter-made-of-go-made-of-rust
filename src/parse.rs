use crate::ast::{Expression, Precedence, Program, Statement};
use crate::tokenize::{Lexer, Token, TokenType};

use std::collections::HashMap;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut parser = Parser {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        Self::setup_prefix_parse_fns(&mut parser);
        Self::setup_infix_parse_fns(&mut parser);

        parser
    }

    fn setup_prefix_parse_fns(&mut self) {
        self.register_prefix(TokenType::IDENT, Self::parse_ident);
        self.register_prefix(TokenType::INT, Self::parse_integer_literal);
        self.register_prefix(TokenType::BANG, Self::parse_prefix_expression);
        self.register_prefix(TokenType::MINUS, Self::parse_prefix_expression);
        self.register_prefix(TokenType::TRUE, Self::parse_boolean);
        self.register_prefix(TokenType::FALSE, Self::parse_boolean);
        self.register_prefix(TokenType::LPAREN, Self::parse_grouped_expression);
        self.register_prefix(TokenType::IF, Self::parse_if_expression);
        self.register_prefix(TokenType::FUNCTION, Self::parse_function_expression);
    }

    fn setup_infix_parse_fns(&mut self) {
        self.register_infix(TokenType::PLUS, Self::parse_infix_expression);
        self.register_infix(TokenType::MINUS, Self::parse_infix_expression);
        self.register_infix(TokenType::EQ, Self::parse_infix_expression);
        self.register_infix(TokenType::NOTEQ, Self::parse_infix_expression);
        self.register_infix(TokenType::SLASH, Self::parse_infix_expression);
        self.register_infix(TokenType::ASTERISK, Self::parse_infix_expression);
        self.register_infix(TokenType::GT, Self::parse_infix_expression);
        self.register_infix(TokenType::LT, Self::parse_infix_expression);
        self.register_infix(TokenType::LPAREN, Self::parse_call_expression);
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while !self.current_token_is((TokenType::EOF)) {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }

            self.next_token();
        }

        program
    }

    fn string(&self, program: &Program) -> Vec<String> {
        let mut program_string = vec![];

        for statement in &program.statements {
            match statement {
                Statement::Let(ident, expr) => {
                    program_string.push(format!("let {} = {};\n", ident, expr.to_string()))
                }
                Statement::Return(expr) => program_string.push(format!("return {};\n", expr)),
                Statement::Expression(expr) => program_string.push(format!("{};\n", expr)),
            }
        }

        program_string
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn current_token_is(&self, token: TokenType) -> bool {
        self.current_token.token_type == token
    }

    fn peek_token_is(&self, token: TokenType) -> bool {
        self.peek_token.token_type == token
    }

    fn current_precedence(&self) -> Precedence {
        match self.current_token.token_type {
            TokenType::EQ => Precedence::Equals,
            TokenType::NOTEQ => Precedence::Equals,
            TokenType::LT => Precedence::LessGreater,
            TokenType::GT => Precedence::LessGreater,
            TokenType::PLUS => Precedence::Sum,
            TokenType::MINUS => Precedence::Sum,
            TokenType::SLASH => Precedence::Product,
            TokenType::ASTERISK => Precedence::Product,
            TokenType::LPAREN => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        match self.peek_token.token_type {
            TokenType::EQ => Precedence::Equals,
            TokenType::NOTEQ => Precedence::Equals,
            TokenType::LT => Precedence::LessGreater,
            TokenType::GT => Precedence::LessGreater,
            TokenType::PLUS => Precedence::Sum,
            TokenType::MINUS => Precedence::Sum,
            TokenType::SLASH => Precedence::Product,
            TokenType::ASTERISK => Precedence::Product,
            TokenType::LPAREN => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn expect_peek(&mut self, token: TokenType) -> bool {
        if self.peek_token_is(token.clone()) {
            self.next_token();
            true
        } else {
            self.errors.push(format!(
                "expected next token to be {:?}, got {:?} instead",
                token.clone(),
                self.peek_token.token_type
            ));
            return false;
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token.token_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_block_statement(&mut self) -> Vec<Statement> {
        let mut statements = vec![];

        self.next_token();

        while !self.current_token_is(TokenType::RBRACE) && !self.current_token_is(TokenType::EOF) {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }

            self.next_token();
        }

        statements
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let peek_token_type = self.peek_token.token_type.clone();

        if let TokenType::IDENT = peek_token_type {
            self.next_token();

            let ident = self.current_token.literal.clone();

            self.expect_peek(TokenType::ASSIGN);

            if self.current_token_is(TokenType::ASSIGN) {
                self.next_token();

                // todo
                while !self.peek_token_is(TokenType::SEMICOLON) {
                    self.next_token();
                }

                self.next_token();

                return Some(Statement::Let(ident.clone(), Expression::IntegerLiteral(0)));
            }
        }

        None
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::Return(Expression::IntegerLiteral(0)))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        if let Some(expression) = self.parse_expression(Precedence::Lowest) {
            if self.peek_token_is(TokenType::SEMICOLON) {
                self.next_token();
            }

            Some(Statement::Expression(expression))
        } else {
            None
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.prefix_parse_fns.get(&self.current_token.token_type);
        if prefix.is_none() {
            self.errors.push(format!(
                "no prefix parse function for {:?} found",
                self.current_token.token_type
            ));
            return None;
        }

        let mut left = prefix.unwrap()(self)?;

        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            let infix_opt = {
                let infix = self.infix_parse_fns.get(&self.peek_token.token_type);
                infix.map(|f| *f)
            };

            if infix_opt.is_none() {
                return Some(left);
            }

            self.next_token();

            let infix = infix_opt.unwrap();
            left = infix(self, left)?;
        }

        Some(left)
    }

    fn parse_ident(&mut self) -> Option<Expression> {
        if let TokenType::IDENT = self.current_token.token_type {
            Some(Expression::Ident(self.current_token.literal.clone()))
        } else {
            None
        }
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        if let TokenType::INT = self.current_token.token_type {
            if let Ok(int) = self.current_token.literal.parse::<i64>() {
                Some(Expression::IntegerLiteral(int))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        if let TokenType::TRUE = self.current_token.token_type {
            Some(Expression::Boolean("true".to_string()))
        } else if let TokenType::FALSE = self.current_token.token_type {
            Some(Expression::Boolean("false".to_string()))
        } else {
            None
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        expression
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token_is(TokenType::ELSE) {
            self.next_token();

            if !self.expect_peek(TokenType::LBRACE) {
                return None;
            }

            Some(self.parse_block_statement())
        } else {
            None
        };

        Some(Expression::If(
            Box::new(condition.unwrap()),
            consequence,
            alternative,
        ))
    }

    fn parse_function_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Expression::Function(parameters, body))
    }

    fn parse_function_parameters(&mut self) -> Vec<String> {
        let mut identifiers = vec![];

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        identifiers.push(self.current_token.literal.clone());

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            identifiers.push(self.current_token.literal.clone());
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return vec![];
        }

        return identifiers;
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let arguments = self.parse_call_arguments();
        if let Some(args) = arguments {
            return Some(Expression::Call(Box::new(function), args));
        } else {
            None
        }
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut arguments = vec![];

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Some(arguments);
        }

        self.next_token();

        if let Some(argument) = self.parse_expression(Precedence::Lowest) {
            arguments.push(argument);
        }

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();

            if let Some(argument) = self.parse_expression(Precedence::Lowest) {
                arguments.push(argument);
            }
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        Some(arguments)
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = self.current_token.literal.clone();

        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;

        Some(Expression::Prefix(operator, Box::new(right)))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = self.current_token.literal.clone();
        let precedence = self.current_precedence();

        self.next_token();

        let right = self.parse_expression(precedence)?;

        Some(Expression::Infix(Box::new(left), operator, Box::new(right)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenize::Lexer;

    #[test]
    fn test_parser() {
        let input = "
            let x = 5;
            let y = 10;
            return 5;
            foobar;
            !foo;
            1 + 2;
            1 * 2 + 3;
            3 / 2 - 1;
            -1 * 2 + 3;
            10 > 5;
            5 < 10;
            3 * 2 + 1 > 1;
            3 * 2 + 1 == 7;
            3 * 2 + 1 != 10;
            1 + (2 + 3);
            (1 * (2 + 3));
            2 / (5 + 5);
            -(5 + 5);
            !(true == true);
            if (1 > 2) { 10 };
            if (1 > 2) { 10 } else { 5 };
            fn() { x + 1; 10; };
            fn(x, y, z) { x + 1; 10; };
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let program_string = parser.string(&program);

        let expected = vec![
            "let x = 0;\n".to_string(),
            "let y = 0;\n".to_string(),
            "return 0;\n".to_string(),
            "foobar;\n".to_string(),
            "(!foo);\n".to_string(),
            "(1 + 2);\n".to_string(),
            "((1 * 2) + 3);\n".to_string(),
            "((3 / 2) - 1);\n".to_string(),
            "(((-1) * 2) + 3);\n".to_string(),
            "(10 > 5);\n".to_string(),
            "(5 < 10);\n".to_string(),
            "(((3 * 2) + 1) > 1);\n".to_string(),
            "(((3 * 2) + 1) == 7);\n".to_string(),
            "(((3 * 2) + 1) != 10);\n".to_string(),
            "(1 + (2 + 3));\n".to_string(),
            "(1 * (2 + 3));\n".to_string(),
            "(2 / (5 + 5));\n".to_string(),
            "(-(5 + 5));\n".to_string(),
            "(!(true == true));\n".to_string(),
            "if ((1 > 2)) { 10 };\n".to_string(),
            "if ((1 > 2)) { 10 } else { 5 };\n".to_string(),
            "fn() { (x + 1);\n10;\n };\n".to_string(),
            "fn(x, y, z) { (x + 1);\n10;\n };\n".to_string(),
        ];

        assert_eq!(parser.errors.len(), 0, "Unvalid statements found");

        assert_eq!(
            program.statements.len(),
            23,
            "Unexpected number of statements"
        );

        assert_eq!(program_string, expected);
    }

    #[test]
    fn test_let_statement_parsing() {
        let input = "
            let x = 10;
            let y = 15;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            parser.errors.len(),
            0,
            "Unvalid statements found. {:?}",
            parser.errors
        );

        assert_eq!(
            program.statements.len(),
            2,
            "Unexpected number of statements"
        );

        let expected_idents = vec!["x", "y"];

        for (i, ident) in expected_idents.iter().enumerate() {
            match &program.statements[i] {
                Statement::Let(identifer, _) => {
                    assert_eq!(identifer, ident, "Unexpected IDENT name");
                }
                _ => panic!("Unexpected statement type"),
            }
        }
    }

    #[test]
    fn test_return_statement_parsing() {
        let input = "
            return 10;
            return 15;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            parser.errors.len(),
            0,
            "Unvalid statements found. {:?}",
            parser.errors
        );

        assert_eq!(
            program.statements.len(),
            2,
            "Unexpected number of statements"
        );

        let expected_values = vec![Expression::IntegerLiteral(0), Expression::IntegerLiteral(0)];

        for (i, v) in expected_values.iter().enumerate() {
            match &program.statements[i] {
                Statement::Return(value) => {
                    assert_eq!(value, v, "Unexpected IDENT name");
                }
                _ => panic!("Unexpected statement type"),
            }
        }
    }

    #[test]
    fn test_if_expression_parsing() {
        let input = "
            if (1 > 2) { 10 };
            if (1 > 2) { 10 } else { 5 };
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            parser.errors.len(),
            0,
            "Unvalid statements found. {:?}",
            parser.errors
        );

        assert_eq!(
            program.statements.len(),
            2,
            "Unexpected number of statements"
        );

        let expected_values = vec![
            Expression::If(
                Box::new(Expression::Infix(
                    Box::new(Expression::IntegerLiteral(1)),
                    ">".to_string(),
                    Box::new(Expression::IntegerLiteral(2)),
                )),
                vec![Statement::Expression(Expression::IntegerLiteral(10))],
                None,
            ),
            Expression::If(
                Box::new(Expression::Infix(
                    Box::new(Expression::IntegerLiteral(1)),
                    ">".to_string(),
                    Box::new(Expression::IntegerLiteral(2)),
                )),
                vec![Statement::Expression(Expression::IntegerLiteral(10))],
                Some(vec![Statement::Expression(Expression::IntegerLiteral(5))]),
            ),
        ];

        for (i, v) in expected_values.iter().enumerate() {
            match &program.statements[i] {
                Statement::Expression(value) => {
                    assert_eq!(value, v, "Unexpected IDENT name");
                }
                _ => panic!("Unexpected statement type"),
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "
            add(1, 2, 3);
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            parser.errors.len(),
            0,
            "Unvalid statements found. {:?}",
            parser.errors
        );

        assert_eq!(
            program.statements.len(),
            1,
            "Unexpected number of statements"
        );

        let expected_values = vec![Expression::Call(
            Box::new(Expression::Ident("add".to_string())),
            vec![
                Expression::IntegerLiteral(1),
                Expression::IntegerLiteral(2),
                Expression::IntegerLiteral(3),
            ],
        )];

        for (i, v) in expected_values.iter().enumerate() {
            match &program.statements[i] {
                Statement::Expression(value) => {
                    assert_eq!(value, v, "Unexpected function call");
                }
                _ => panic!("Unexpected statement type"),
            }
        }
    }

    #[test]
    fn test_expression_statement_parsing() {
        let input = "
            foo;
            bar
            5;
            10;
            !foo;
            -10;
            10 == 10;
            10 != 5;
            10 + 10;
            10 - 10;
            10 * 10;
            10 / 10
            10 > 5;
            10 < 15;
            true;
            false;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            parser.errors.len(),
            0,
            "Unvalid statements found. {:?}",
            parser.errors
        );

        assert_eq!(
            program.statements.len(),
            16,
            "Unexpected number of statements"
        );

        let expected_values = vec![
            Expression::Ident("foo".to_string()),
            Expression::Ident("bar".to_string()),
            Expression::IntegerLiteral(5),
            Expression::IntegerLiteral(10),
            Expression::Prefix(
                "!".to_string(),
                Box::new(Expression::Ident("foo".to_string())),
            ),
            Expression::Prefix("-".to_string(), Box::new(Expression::IntegerLiteral(10))),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "==".to_string(),
                Box::new(Expression::IntegerLiteral(10)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "!=".to_string(),
                Box::new(Expression::IntegerLiteral(5)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "+".to_string(),
                Box::new(Expression::IntegerLiteral(10)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "-".to_string(),
                Box::new(Expression::IntegerLiteral(10)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "*".to_string(),
                Box::new(Expression::IntegerLiteral(10)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "/".to_string(),
                Box::new(Expression::IntegerLiteral(10)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                ">".to_string(),
                Box::new(Expression::IntegerLiteral(5)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "<".to_string(),
                Box::new(Expression::IntegerLiteral(15)),
            ),
            Expression::Boolean("true".to_string()),
            Expression::Boolean("false".to_string()),
        ];

        for (i, v) in expected_values.iter().enumerate() {
            match &program.statements[i] {
                Statement::Expression(value) => {
                    assert_eq!(value, v, "Unexpected IDENT name");
                }
                _ => panic!("Unexpected statement type"),
            }
        }
    }
}
