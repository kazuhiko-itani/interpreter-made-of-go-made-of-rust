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
struct Null {}

impl fmt::Display for Null {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "null")
    }
}
#[derive(Debug)]
pub enum Object {
    Integer(i64),
    Boolean(&'static BoolValue),
    Null(&'static Null),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Null(null) => write!(f, "{}", null),
        }
    }
}
