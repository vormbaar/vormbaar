use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

// todo: remove this one day
// maybe replace with thiserror
use anyhow::{anyhow, bail};

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Value {
    I32(i32),
    // U32(u32),
    None,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Lt,
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
    Call(String, BTreeMap<String, Expr>),
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

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct State {
    #[serde(default)]
    constants: BTreeMap<String, Value>,
    #[serde(default)]
    functions: BTreeMap<String, Function>,
}

impl State {
    pub fn new() -> Self {
        Self {
            constants: BTreeMap::new(),
            functions: BTreeMap::new(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct FunctionContext {
    #[serde(default)]
    arguments: BTreeMap<String, Value>,
    #[serde(default)]
    variables: BTreeMap<String, Value>,
    #[serde(default)]
    constants: BTreeMap<String, Value>,
}

impl FunctionContext {
    pub fn new() -> Self {
        Self {
            arguments: BTreeMap::new(),
            variables: BTreeMap::new(),
            constants: BTreeMap::new(),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct VM {
    #[serde(default)]
    state: State,
}

impl VM {
    pub fn new() -> Self {
        Self {
            state: State {
                constants: BTreeMap::new(),
                functions: BTreeMap::new(),
            },
        }
    }

    pub fn eval_expression(
        &self,
        local_context: &FunctionContext,
        expr: &Expr,
    ) -> anyhow::Result<Value> {
        match expr {
            Expr::Value(value) => Ok(*value),
            Expr::Const(name) => match local_context.constants.get(name) {
                Some(value) => Ok(*value),
                None => match self.state.constants.get(name) {
                    Some(value) => Ok(*value),
                    None => Err(anyhow!("Cannot find constant named {:?}", name)),
                },
            },
            Expr::Var(name) => match local_context.variables.get(name) {
                Some(value) => Ok(*value),
                None => Err(anyhow!("Cannot find variable named {:?}", name)),
            },
            Expr::Arg(name) => match local_context.arguments.get(name) {
                Some(value) => Ok(*value),
                None => Err(anyhow!("Cannot find argument named {:?}", name)),
            },
            Expr::If(comparison, if_true, if_false) => {
                let comparison = self.eval_expression(local_context, comparison)?;
                let is_true = match comparison {
                    Value::I32(value) => value != 0,
                    // Value::U32(value) => value != 0,
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
                    BinaryOp::Add => match (left, right) {
                        (Value::I32(left_value), Value::I32(right_value)) => {
                            Ok(Value::I32(left_value + right_value))
                        }
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(left_value + right_value))
                        // }
                        (Value::None, _) => Err(anyhow!("Cannot add number to None")),
                        (_, Value::None) => Err(anyhow!("Cannot add number to None")),
                        // _ => Err(anyhow!("Type mismatch")),
                    },
                    BinaryOp::Sub => match (left, right) {
                        (Value::I32(left_value), Value::I32(right_value)) => {
                            Ok(Value::I32(left_value - right_value))
                        }
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(left_value - right_value))
                        // }
                        (Value::None, _) => Err(anyhow!("Cannot subtract number to None")),
                        (_, Value::None) => Err(anyhow!("Cannot subtract number to None")),
                        // _ => Err(anyhow!("Type mismatch")),
                    },
                    BinaryOp::Mul => match (left, right) {
                        (Value::I32(left_value), Value::I32(right_value)) => {
                            Ok(Value::I32(left_value * right_value))
                        }
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(left_value * right_value))
                        // }
                        (Value::None, _) => Err(anyhow!("Cannot multiply number to None")),
                        (_, Value::None) => Err(anyhow!("Cannot multiply number to None")),
                        // _ => Err(anyhow!("Type mismatch")),
                    },
                    BinaryOp::Lt => match (left, right) {
                        (Value::I32(left_value), Value::I32(right_value)) => {
                            Ok(Value::I32(if left_value < right_value { 1 } else { 0 }))
                        }
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(if left_value < right_value { 1 } else { 0 }))
                        // }
                        (Value::None, _) => Err(anyhow!("Cannot compare to number to None")),
                        (_, Value::None) => Err(anyhow!("Cannot compare to number to None")),
                        // _ => Err(anyhow!("Type mismatch")),
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
        calling_context: &FunctionContext,
        args: &BTreeMap<String, Expr>,
    ) -> anyhow::Result<Value> {
        self.eval_function(
            calling_context,
            self.state
                .functions
                .get(name)
                .ok_or_else(|| anyhow!(r#"function "{:?}" not found!"#, name))?,
            args,
        )
    }

    pub fn eval_function(
        &self,
        calling_context: &FunctionContext,
        Function(fun): &Function,
        args: &BTreeMap<String, Expr>,
    ) -> anyhow::Result<Value> {
        let mut local_context = FunctionContext::new();
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

    pub fn add_constant(&mut self, name: &str, value: Value) -> anyhow::Result<()> {
        if self.state.constants.contains_key(name) {
            bail!(r#"constant "{:?}" already exists"#, name);
        }
        _ = self.state.constants.insert(name.to_owned(), value);
        Ok(())
    }

    pub fn add_function(&mut self, name: &str, func: Function) -> anyhow::Result<()> {
        if self.state.functions.contains_key(name) {
            bail!(r#"function "{:?}" already exists"#, name);
        }
        _ = self.state.functions.insert(name.to_owned(), func);
        Ok(())
    }
}
