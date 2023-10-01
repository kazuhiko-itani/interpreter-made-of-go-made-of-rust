use std::fmt;

#[derive(Debug, Clone, PartialEq)]

pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Ident(String),
    IntegerLiteral(i64),
    Function(Vec<String>, Box<Statement>),
    Return(Box<Expression>),
    Prefix(String, Box<Expression>),
    Infix(Box<Expression>, String, Box<Expression>),
    Boolean(String),
    If(Box<Expression>, Vec<Statement>, Option<Vec<Statement>>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(ident, expr) => write!(f, "let {} = {};", ident, expr),
            Statement::Return(expr) => write!(f, "return {};", expr),
            Statement::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(ident) => write!(f, "{}", ident),
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
            Expression::Prefix(op, expr) => write!(f, "({}{})", op, expr),
            Expression::Infix(left, op, right) => write!(f, "({} {} {})", left, op, right),
            Expression::Boolean(boolean) => write!(f, "{}", boolean),
            Expression::If(condition, consequence, alternative) => {
                let consequence_str = consequence
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<String>>()
                    .join("\n");

                if let Some(alt) = alternative {
                    let alternative_str = alt
                        .iter()
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>()
                        .join("\n");

                    write!(
                        f,
                        "if ({}) {{ {} }} else {{ {} }}",
                        condition, consequence_str, alternative_str
                    )
                } else {
                    write!(f, "if ({}) {{ {} }}", condition, consequence_str)
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    Equals = 1,
    LessGreater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}
