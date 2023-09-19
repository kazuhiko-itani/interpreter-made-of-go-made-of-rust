#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(String, Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    Function(Vec<String>, Box<Statement>),
    Return(Box<Expression>),
    Infix(Box<Expression>, String, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}
