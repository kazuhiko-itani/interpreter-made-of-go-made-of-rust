use std::fmt;
#[derive(Debug)]
pub struct BoolValue {
    pub value: bool,
}

impl fmt::Display for BoolValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BoolValue { value: true } => write!(f, "true"),
            BoolValue { value: false } => write!(f, "false"),
        }
    }
}

pub const TRUE: BoolValue = BoolValue { value: true };
pub const FALSE: BoolValue = BoolValue { value: false };
#[derive(Debug)]
pub struct Null {}

pub const NULL: Null = Null {};

impl fmt::Display for Null {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "null")
    }
}
#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(&'static BoolValue),
    ReturnValue(Box<Object>),
    Null(&'static Null),
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::ReturnValue(obj) => write!(f, "{}", obj),
            Object::Null(null) => write!(f, "{}", null),
            Object::Error(msg) => write!(f, "{}", msg),
        }
    }
}
