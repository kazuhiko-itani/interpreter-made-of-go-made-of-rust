use crate::ast::Statement;
use std::collections::HashMap;
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
    String(String),
    Boolean(&'static BoolValue),
    ReturnValue(Box<Object>),
    Function(Vec<String>, Vec<Statement>, Environment),
    BuiltInFunction(fn(Vec<Object>) -> Object),
    Null(&'static Null),
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::String(string) => write!(f, "\"{}\"", string),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::ReturnValue(obj) => write!(f, "{}", obj),
            Object::Function(args, body, env) => {
                let mut args_str = String::new();

                for (i, arg) in args.iter().enumerate() {
                    args_str.push_str(&arg);
                    if i != args.len() - 1 {
                        args_str.push_str(", ");
                    }
                }

                let mut body_str = String::new();

                for statement in body {
                    body_str.push_str(&statement.to_string());
                    body_str.push_str(";\n");
                }

                write!(f, "fn({}) {{ {} }}", args_str, body_str)
            }
            Object::BuiltInFunction(_) => write!(f, "builtin function"),
            Object::Null(null) => write!(f, "{}", null),
            Object::Error(msg) => write!(f, "{}", msg),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    pub builtins: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            builtins: HashMap::new(),
        }
    }

    pub fn register_builtin(&mut self, name: &str, func: fn(Vec<Object>) -> Object) {
        self.builtins
            .insert(name.to_string(), Object::BuiltInFunction(func));
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.store.get(name).or_else(|| self.builtins.get(name))
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }
}
