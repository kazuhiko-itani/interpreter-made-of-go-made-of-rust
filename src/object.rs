use std::fmt;

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

pub enum Object {
    Integer(i64),
    Boolean(&'static BoolValue),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
        }
    }
}
