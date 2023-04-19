use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub enum Value {
    I32(i32),
    U32(u32),
    None,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub enum BinaryOp {
    Add,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub enum UnaryOp {}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Expr {
    Value(Value),
    Const(String),
    Var(String),
    Arg(String),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
    Call(String, HashMap<String, Expr>),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Instruction {
    ConstAssign(String, Expr),
    VarAssign(String, Expr),
    Return(Expr),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Function(Vec<Instruction>);

impl Function {
    pub fn new(body: Vec<Instruction>) -> Self {
        Self(body)
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct State {
    constants: HashMap<String, Value>,
    functions: HashMap<String, Function>,
}

impl State {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Context {
    arguments: HashMap<String, Value>,
    variables: HashMap<String, Value>,
    constants: HashMap<String, Value>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            arguments: HashMap::new(),
            variables: HashMap::new(),
            constants: HashMap::new(),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct VM {
    state: State,
}

impl VM {
    pub fn new() -> Self {
        Self {
            state: State {
                constants: HashMap::new(),
                functions: HashMap::new(),
            },
        }
    }

    fn add_i32_and_u32(&self, a: i32, b: u32) -> Result<Value, &'static str> {
        let val = (a as i64) + (b as i64);
        if val < 0 {
            Ok(Value::I32(val as i32))
        } else {
            Ok(Value::U32(val as u32))
        }
    }

    pub fn eval_expression(
        &self,
        local_context: &Context,
        expr: &Expr,
    ) -> Result<Value, &'static str> {
        match expr {
            Expr::Value(value) => Ok(*value),
            Expr::Const(name) => match local_context.constants.get(name) {
                Some(value) => Ok(*value),
                None => match self.state.constants.get(name) {
                    Some(value) => Ok(*value),
                    None => Err("Cannot find constant"),
                },
            },
            Expr::Var(name) => match local_context.variables.get(name) {
                Some(value) => Ok(*value),
                None => Err("Cannot find variable"),
            },
            Expr::Arg(name) => match local_context.arguments.get(name) {
                Some(value) => Ok(*value),
                None => Err("Cannot find argument"),
            },
            Expr::If(comparison, if_true, if_false) => {
                let comparison = self.eval_expression(local_context, comparison)?;
                let is_true = match comparison {
                    Value::I32(value) => value != 0,
                    Value::U32(value) => value != 0,
                    Value::None => false,
                };
                if is_true {
                    self.eval_expression(local_context, if_true)
                } else {
                    self.eval_expression(local_context, if_false)
                }
            }
            Expr::BinaryOp(op, left, right) => {
                let left = self.eval_expression(local_context, left)?;
                let right = self.eval_expression(local_context, right)?;
                match op {
                    BinaryOp::Add => match left {
                        Value::I32(left_value) => match right {
                            Value::I32(right_value) => Ok(Value::I32(left_value + right_value)),
                            Value::U32(right_value) => {
                                self.add_i32_and_u32(left_value, right_value)
                            }
                            Value::None => Err("Cannot add None"),
                        },
                        Value::U32(left_value) => match right {
                            Value::I32(right_value) => {
                                self.add_i32_and_u32(right_value, left_value)
                            }
                            Value::U32(right_value) => Ok(Value::U32(left_value + right_value)),
                            Value::None => Err("Cannot add None"),
                        },
                        Value::None => Err("Cannot add None"),
                    },
                }
            }
            Expr::UnaryOp { .. } => Ok(Value::None),
            Expr::Call(name, args) => self.call_function(name, local_context, args),
        }
    }

    pub fn call_function(
        &self,
        name: &str,
        calling_context: &Context,
        args: &HashMap<String, Expr>,
    ) -> Result<Value, &'static str> {
        self.eval_function(
            calling_context,
            self.state
                .functions
                .get(name)
                .ok_or("function not found!")?,
            args,
        )
    }

    pub fn eval_function(
        &self,
        calling_context: &Context,
        Function(fun): &Function,
        args: &HashMap<String, Expr>,
    ) -> Result<Value, &'static str> {
        let mut local_context = Context::new();
        for (name, arg) in args {
            let value = self.eval_expression(&calling_context, &arg)?;
            local_context.arguments.insert(name.clone(), value);
        }

        for line in fun {
            match line {
                Instruction::ConstAssign(name, expr) => {
                    let value = self.eval_expression(&local_context, &expr)?;
                    local_context.constants.insert(name.clone(), value);
                }
                Instruction::VarAssign(name, expr) => {
                    let value = self.eval_expression(&local_context, &expr)?;
                    local_context.variables.insert(name.clone(), value);
                }
                Instruction::Return(expr) => return self.eval_expression(&local_context, expr),
            }
        }
        Ok(Value::None)
    }

    pub fn add_constant(&mut self, name: &str, value: Value) -> Result<(), &'static str> {
        if self.state.constants.contains_key(name) {
            return Err("constant already exists");
        }
        _ = self.state.constants.insert(name.to_owned(), value);
        Ok(())
    }

    pub fn add_function(&mut self, name: &str, func: Function) -> Result<(), &'static str> {
        if self.state.functions.contains_key(name) {
            return Err("function already exists");
        }
        _ = self.state.functions.insert(name.to_owned(), func);
        Ok(())
    }
}
