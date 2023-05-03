use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

// todo: remove this one day
// maybe replace with thiserror
use anyhow::{anyhow, bail};

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Scalar {
    I32(i32),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Value {
    Scalar(Scalar),
    Vector(Vec<Value>),
    None,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Returned {
    Early(Value),
    Finished(Value),
}

impl Returned {
    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn into_inner(self) -> Value {
        match self {
            Returned::Early(value) | Returned::Finished(value) => value,
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Lt,
    Eq,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub enum UnaryOp {}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Expr {
    Value(Value),
    Const(String),
    Var(String),
    Arg(String),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
    Call(String, BTreeMap<String, Expr>),
}

impl Expr {
    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn get_arguments(&self) -> Vec<String> {
        let mut args = Vec::new();
        match self {
            Expr::Value(_) => (),
            Expr::Const(_) => (),
            Expr::Var(_) => (),
            Expr::Arg(name) => args.push(name.to_owned()),
            Expr::BinaryOp(_, left, right) => {
                args.extend(left.get_arguments().into_iter());
                args.extend(right.get_arguments().into_iter());
            }
            Expr::UnaryOp(_, expr) => {
                args.extend(expr.get_arguments().into_iter());
            }
            Expr::Call(_, arguments) => {
                for (_, value) in arguments {
                    args.extend(value.get_arguments().into_iter());
                }
            }
        };
        args.sort();
        args.dedup();
        args
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Instruction {
    If(Box<Expr>, Vec<Instruction>, Vec<Instruction>),
    ConstAssign(String, Expr),
    For(Range, Vec<Instruction>),
    VarAssign(String, Expr),
    Drop(Expr),
    Return(Expr),
}

impl Instruction {
    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn get_arguments(&self) -> Vec<String> {
        let mut args = Vec::new();
        match self {
            Instruction::If(expr, if_true, if_false) => {
                args.extend(expr.get_arguments());
                args.extend(
                    if_true
                        .into_iter()
                        .map(Instruction::get_arguments)
                        .flatten(),
                );
                args.extend(
                    if_false
                        .into_iter()
                        .map(Instruction::get_arguments)
                        .flatten(),
                );
            }
            Instruction::ConstAssign(_, expr) => args.extend(expr.get_arguments()),
            Instruction::For(expr, body) => {
                args.extend(expr.get_arguments());
                args.extend(body.into_iter().map(Instruction::get_arguments).flatten());
            }
            Instruction::VarAssign(_, expr) => {
                args.extend(expr.get_arguments());
            }
            Instruction::Drop(expr) => {
                args.extend(expr.get_arguments());
            }
            Instruction::Return(expr) => args.extend(expr.get_arguments()),
        }
        args.sort();
        args.dedup();
        args
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum RangeMode {
    Inclusive,
    Exclusive,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Range {
    Values {
        item: String,
        values: Vec<Value>,
    },
    Range {
        item: String,
        start: Expr,
        stop: Expr,
        step: Expr,
        mode: RangeMode,
    },
}

impl Range {
    pub fn get_arguments(&self) -> Vec<String> {
        let mut args = Vec::new();
        match self {
            Range::Values { .. } => (),
            Range::Range {
                start, stop, step, ..
            } => {
                args.extend(start.get_arguments().into_iter());
                args.extend(stop.get_arguments().into_iter());
                args.extend(step.get_arguments().into_iter());
            }
        }
        args.sort();
        args.dedup();
        args
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Function(Vec<Instruction>);

impl Function {
    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn new(body: Vec<Instruction>) -> Self {
        Self(body)
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn get_arguments(&self) -> Vec<String> {
        let Function(body) = self;
        let mut args = body
            .iter()
            .map(|i| i.get_arguments())
            .flatten()
            .collect::<Vec<_>>();
        args.sort();
        args.dedup();
        args
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
    #[cfg_attr(feature = "flame", tracing::instrument)]
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
    #[cfg_attr(feature = "flame", tracing::instrument)]
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
    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn new() -> Self {
        Self {
            state: State {
                constants: BTreeMap::new(),
                functions: BTreeMap::new(),
            },
        }
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn eval_expression(
        &self,
        local_context: &FunctionContext,
        expr: &Expr,
    ) -> anyhow::Result<Value> {
        match expr {
            Expr::Value(value) => Ok(value.clone()),
            Expr::Const(name) => match local_context.constants.get(name) {
                Some(value) => Ok(value.clone()),
                None => match self.state.constants.get(name) {
                    Some(value) => Ok(value.clone()),
                    None => Err(anyhow!("Cannot find constant named {:?}", name)),
                },
            },
            Expr::Var(name) => match local_context.variables.get(name) {
                Some(value) => Ok(value.clone()),
                None => Err(anyhow!("Cannot find variable named {:?}", name,)),
            },
            Expr::Arg(name) => match local_context.arguments.get(name) {
                Some(value) => Ok(value.clone()),
                None => Err(anyhow!("Cannot find argument named {:?}", name)),
            },
            Expr::BinaryOp(op, left, right) => {
                let left = self.eval_expression(local_context, left)?;
                let right = self.eval_expression(local_context, right)?;
                match op {
                    BinaryOp::Add => match (left, right) {
                        (
                            Value::Scalar(Scalar::I32(left_value)),
                            Value::Scalar(Scalar::I32(right_value)),
                        ) => Ok(Value::Scalar(Scalar::I32(left_value + right_value))),
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(left_value + right_value))
                        // }
                        (Value::None, _) => Err(anyhow!("Cannot add number to None")),
                        (_, Value::None) => Err(anyhow!("Cannot add number to None")),
                        _ => Err(anyhow!("Type mismatch")),
                    },
                    BinaryOp::Sub => match (left, right) {
                        (
                            Value::Scalar(Scalar::I32(left_value)),
                            Value::Scalar(Scalar::I32(right_value)),
                        ) => Ok(Value::Scalar(Scalar::I32(left_value - right_value))),
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(left_value - right_value))
                        // }
                        (Value::None, _) => Err(anyhow!("Cannot subtract number to None")),
                        (_, Value::None) => Err(anyhow!("Cannot subtract number to None")),
                        _ => Err(anyhow!("Type mismatch")),
                    },
                    BinaryOp::Mul => match (left, right) {
                        (
                            Value::Scalar(Scalar::I32(left_value)),
                            Value::Scalar(Scalar::I32(right_value)),
                        ) => Ok(Value::Scalar(Scalar::I32(left_value * right_value))),
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(left_value * right_value))
                        // }
                        (Value::None, _) => Err(anyhow!("Cannot multiply number to None")),
                        (_, Value::None) => Err(anyhow!("Cannot multiply number to None")),
                        _ => Err(anyhow!("Type mismatch")),
                    },
                    BinaryOp::Lt => match (left, right) {
                        (
                            Value::Scalar(Scalar::I32(left_value)),
                            Value::Scalar(Scalar::I32(right_value)),
                        ) => Ok(Value::Scalar(Scalar::I32(
                            (left_value < right_value).into(),
                        ))),
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(if left_value < right_value { 1 } else { 0 }))
                        // }
                        (Value::None, _) => Err(anyhow!("Cannot compare to number to None")),
                        (_, Value::None) => Err(anyhow!("Cannot compare to number to None")),
                        _ => Err(anyhow!("Type mismatch")),
                    },
                    BinaryOp::Eq => match (left, right) {
                        (
                            Value::Scalar(Scalar::I32(left_value)),
                            Value::Scalar(Scalar::I32(right_value)),
                        ) => Ok(Value::Scalar(Scalar::I32(
                            (left_value == right_value).into(),
                        ))),
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(if left_value < right_value { 1 } else { 0 }))
                        // }
                        (Value::None, _) => Err(anyhow!("Cannot compare to number to None")),
                        (_, Value::None) => Err(anyhow!("Cannot compare to number to None")),
                        _ => Err(anyhow!("Type mismatch")),
                    },
                }
            }
            Expr::UnaryOp { .. } => Ok(Value::None),
            Expr::Call(name, args) => self
                .call_function(name, local_context, args)
                .map(Returned::into_inner),
        }
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn call_function(
        &self,
        name: &str,
        calling_context: &FunctionContext,
        args: &BTreeMap<String, Expr>,
    ) -> anyhow::Result<Returned> {
        self.eval_instructions(
            calling_context,
            &self
                .state
                .functions
                .get(name)
                .ok_or_else(|| anyhow!(r#"function "{}" not found!"#, name))?
                .0,
            args,
        )
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn eval_instructions(
        &self,
        calling_context: &FunctionContext,
        instructions: &Vec<Instruction>,
        args: &BTreeMap<String, Expr>,
    ) -> anyhow::Result<Returned> {
        let mut local_context = calling_context.clone();
        for (name, arg) in args {
            let value = self.eval_expression(&calling_context, &arg)?;
            local_context.arguments.insert(name.clone(), value);
        }

        for instruction in instructions {
            match instruction {
                Instruction::If(comparison, if_true, if_false) => {
                    let comparison = self.eval_expression(&local_context, comparison)?;
                    let is_true = match comparison {
                        Value::Scalar(Scalar::I32(value)) => value != 0,
                        _ => false,
                    };
                    let body = if is_true { if_true } else { if_false };

                    let value = self.eval_instructions(
                        &local_context,
                        body,
                        &local_context
                            .arguments
                            .iter()
                            .map(|(name, expr)| (name.clone(), Expr::Value(expr.clone())))
                            .collect(),
                    )?;
                    if let Returned::Early(value) = value {
                        return Ok(Returned::Early(value));
                    }
                }
                Instruction::ConstAssign(name, expr) => {
                    let value = self.eval_expression(&local_context, &expr)?;
                    local_context.constants.insert(name.clone(), value);
                }
                Instruction::VarAssign(name, expr) => {
                    let value = self.eval_expression(&local_context, &expr)?;
                    local_context.variables.insert(name.clone(), value);
                }
                Instruction::Drop(_) => {}
                Instruction::Return(expr) => {
                    return self
                        .eval_expression(&local_context, expr)
                        .map(Returned::Early)
                }
                Instruction::For(range, instructions) => {
                    let mut loop_context = local_context.clone();
                    match range {
                        Range::Range {
                            item,
                            start,
                            stop,
                            step,
                            mode,
                        } => {
                            let start = self.eval_expression(&loop_context, start)?;
                            let stop = self.eval_expression(&loop_context, stop)?;
                            let step = self.eval_expression(&loop_context, step)?;
                            match (start, stop, step) {
                                (
                                    Value::Scalar(Scalar::I32(start)),
                                    Value::Scalar(Scalar::I32(stop)),
                                    Value::Scalar(Scalar::I32(step)),
                                ) => {
                                    for v in match &mode {
                                        RangeMode::Inclusive => ((start as usize)..(stop as usize))
                                            .step_by(step as usize),
                                        RangeMode::Exclusive => ((start as usize)..(stop as usize))
                                            .step_by(step as usize),
                                    } {
                                        loop_context.variables.insert(
                                            item.clone(),
                                            Value::Scalar(Scalar::I32(v as i32)),
                                        );
                                        let value = self.eval_instructions(
                                            &loop_context,
                                            instructions,
                                            &args,
                                        )?;
                                        if let Returned::Early(_) = value {
                                            return Ok(value);
                                        }
                                        if let Instruction::Return(_) = instruction {
                                            return Ok(Returned::Early(value.into_inner()));
                                        }
                                    }
                                }
                                (Value::Vector(_), _, _) | (Value::None, _, _) => {
                                    bail!(r#"Field "start" must be a vector!"#)
                                }
                                (_, Value::Vector(_), _) | (_, Value::None, _) => {
                                    bail!(r#"Field "stop" must be a vector!"#)
                                }
                                (_, _, Value::Vector(_)) | (_, _, Value::None) => {
                                    bail!(r#"Field "step" must be a vector!"#)
                                }
                            }
                        }
                        Range::Values { item, values } => {
                            for v in values {
                                loop_context.variables.insert(item.clone(), v.clone());
                                let value =
                                    self.eval_instructions(&loop_context, instructions, &args)?;
                                if let Returned::Early(_) = value {
                                    return Ok(value);
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(Returned::Finished(Value::None))
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn add_constant(&mut self, name: &str, value: Value) -> anyhow::Result<()> {
        if self.state.constants.contains_key(name) {
            bail!(r#"constant "{}" already exists"#, name);
        }
        _ = self.state.constants.insert(name.to_owned(), value);
        Ok(())
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn add_function(&mut self, name: &str, func: Function) -> anyhow::Result<()> {
        if self.state.functions.contains_key(name) {
            bail!(r#"function "{}" already exists"#, name);
        }
        _ = self.state.functions.insert(name.to_owned(), func);
        Ok(())
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn get_function(&self, name: &str) -> Option<&Function> {
        self.state.functions.get(name)
    }
}
