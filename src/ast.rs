use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    Function(Vec<String>, Box<Statement>),
    Return(Box<Expression>),
    Infix(Box<Expression>, String, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::IntegerLiteral(int) => write!(f, "{}", int),
            Expression::Function(params, body) => {
                let mut params_str = String::new();

                for param in params {
                    params_str.push_str(&param);
                    params_str.push_str(", ");
                }

                write!(f, "fn({}) {{ {:?} }}", params_str, body)
            }
            Expression::Return(expr) => write!(f, "return {}", expr),
            Expression::Infix(left, op, right) => write!(f, "({} {} {})", left, op, right),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}
