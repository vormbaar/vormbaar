use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum AddConstantError {
    #[error("Constant already exists: {name}")]
    ConstantAlreadyExists { name: String },
}

#[derive(Debug, Error)]
pub enum AddFunctionError {
    #[error("Function already exists: {name}")]
    FunctionAlreadyExists { name: String },
}

#[derive(Debug, Error)]
pub enum EvalInstructionsError {
    #[error("Evaluating expression failed with error: {0}")]
    EvalExpressionFailed(#[from] Box<EvalExpressionError>),
    #[error(r#"Only I32 is allowed for iteration!"#)]
    IteratorMustBeI32,
    #[error(r#"Field "start" must be an I32!"#)]
    StartMustBeI32,
    #[error(r#"Field "stop" must be an I32!"#)]
    StopMustBeI32,
    #[error(r#"Field "step" must be an I32!"#)]
    StepMustBeI32,
}

#[derive(Debug, Error)]
pub enum CallFunctionError {
    #[error("Evaluating expression failed with error: {0}")]
    EvalExpressionFailed(#[from] Box<EvalExpressionError>),
    #[error("Evaluating instructions failed with error: {0}")]
    EvalInstructionFailed(#[from] EvalInstructionsError),
    #[error(r#"function "{name}" not found!"#)]
    FunctionNotFound { name: String },
}

#[derive(Debug, Error)]
pub enum EvalExpressionError {
    #[error("Cannot find constant named {name}")]
    ConstantNotFound { name: String },
    #[error("Index is out of range")]
    IndexOutOfRange,
    #[error("Index must be I32!")]
    IndexMustBeI32,
    #[error("LHS is not indexable!")]
    LhsNotIndexable,
    #[error("Cannot find variable named {name}")]
    VariableNotFound { name: String },
    #[error("Cannot find argument named {name}")]
    ArgumentNotFound { name: String },
    #[error("Cannot find iteration item")]
    IterationItemNotFound,
    #[error("Cannot do arithmetic with None: {operation:?}")]
    NoArithmeticWithNone { operation: BinaryOp },
    #[error("Type mismatch")]
    TypeMismatch,
    #[error("Cannot compare with None: {comparison:?}")]
    NoComparisonWithNone { comparison: BinaryOp },
    #[error("Function call failed: {0}")]
    FunctionCallFailed(#[from] CallFunctionError),
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Scalar {
    I32(i32),
    Char(char),
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
    Index(Box<Expr>, Box<Expr>),
    Item,
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
            Expr::Index(left, right) => {
                args.extend(left.get_arguments().into_iter());
                args.extend(right.get_arguments().into_iter());
            }
            Expr::Item => (),
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
    Values(Vec<Value>),
    Range {
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
    pub fn value_equal(
        &self,
        local_context: &FunctionContext,
        iteration_item: &Option<Value>,
        left: &Value,
        right: &Value,
    ) -> bool {
        match (left, right) {
            (Value::Scalar(Scalar::I32(left_value)), Value::Scalar(Scalar::I32(right_value))) => {
                left_value == right_value
            }
            (Value::Scalar(Scalar::Char(left_value)), Value::Scalar(Scalar::Char(right_value))) => {
                left_value == right_value
            }
            (Value::Vector(vec), Value::Vector(vec2)) => vec
                .iter()
                .zip(vec2)
                .map(|(left, right)| self.value_equal(local_context, iteration_item, left, right))
                .all(|v| v),
            (Value::None, Value::None) => true,
            _ => false,
        }
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn eval_expression(
        &self,
        local_context: &FunctionContext,
        iteration_item: &Option<Value>,
        expr: &Expr,
    ) -> Result<Value, EvalExpressionError> {
        use EvalExpressionError::*;
        match expr {
            Expr::Value(value) => Ok(value.clone()),
            Expr::Const(name) => match local_context.constants.get(name) {
                Some(value) => Ok(value.clone()),
                None => match self.state.constants.get(name) {
                    Some(value) => Ok(value.clone()),
                    None => Err(ConstantNotFound {
                        name: name.to_owned(),
                    }),
                },
            },
            Expr::Index(container, index) => {
                let container = self.eval_expression(local_context, iteration_item, container)?;
                let index = self.eval_expression(local_context, iteration_item, index)?;
                match container {
                    Value::Vector(container) => match index {
                        Value::Scalar(Scalar::I32(index)) => match container.get(index as usize) {
                            Some(value) => Ok(value.clone()),
                            None => Err(IndexOutOfRange),
                        },
                        _ => Err(IndexMustBeI32),
                    },
                    _ => Err(LhsNotIndexable),
                }
            }
            Expr::Var(name) => match local_context.variables.get(name) {
                Some(value) => Ok(value.clone()),
                None => Err(VariableNotFound { name: name.clone() }),
            },
            Expr::Arg(name) => match local_context.arguments.get(name) {
                Some(value) => Ok(value.clone()),
                None => Err(ArgumentNotFound { name: name.clone() }),
            },
            Expr::Item => match iteration_item {
                Some(value) => Ok(value.clone()),
                None => Err(IterationItemNotFound),
            },
            Expr::BinaryOp(op, left, right) => {
                let left = self.eval_expression(local_context, iteration_item, left)?;
                let right = self.eval_expression(local_context, iteration_item, right)?;
                match op {
                    BinaryOp::Add => match (left, right) {
                        (
                            Value::Scalar(Scalar::I32(left_value)),
                            Value::Scalar(Scalar::I32(right_value)),
                        ) => Ok(Value::Scalar(Scalar::I32(left_value + right_value))),
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(left_value + right_value))
                        // }
                        (Value::None, _) => Err(NoArithmeticWithNone { operation: *op }),
                        (_, Value::None) => Err(NoArithmeticWithNone { operation: *op }),
                        _ => Err(TypeMismatch),
                    },
                    BinaryOp::Sub => match (left, right) {
                        (
                            Value::Scalar(Scalar::I32(left_value)),
                            Value::Scalar(Scalar::I32(right_value)),
                        ) => Ok(Value::Scalar(Scalar::I32(left_value - right_value))),
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(left_value - right_value))
                        // }
                        (Value::None, _) => Err(NoArithmeticWithNone { operation: *op }),
                        (_, Value::None) => Err(NoArithmeticWithNone { operation: *op }),
                        _ => Err(TypeMismatch),
                    },
                    BinaryOp::Mul => match (left, right) {
                        (
                            Value::Scalar(Scalar::I32(left_value)),
                            Value::Scalar(Scalar::I32(right_value)),
                        ) => Ok(Value::Scalar(Scalar::I32(left_value * right_value))),
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(left_value * right_value))
                        // }
                        (Value::None, _) => Err(NoArithmeticWithNone { operation: *op }),
                        (_, Value::None) => Err(NoArithmeticWithNone { operation: *op }),
                        _ => Err(TypeMismatch),
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
                        (Value::None, _) => Err(NoComparisonWithNone { comparison: *op }),
                        (_, Value::None) => Err(NoComparisonWithNone { comparison: *op }),
                        _ => Err(TypeMismatch),
                    },
                    BinaryOp::Eq => Ok(Value::Scalar(Scalar::I32(
                        self.value_equal(local_context, iteration_item, &left, &right)
                            .into(),
                    ))),
                }
            }
            Expr::UnaryOp { .. } => Ok(Value::None),
            Expr::Call(name, args) => self
                .call_function(name, local_context, iteration_item, args)
                .map(Returned::into_inner)
                .map_err(EvalExpressionError::from),
        }
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn call_function(
        &self,
        name: &str,
G        calling_context: &FunctionContext,
        iteration_item: &Option<Value>,
        args: &BTreeMap<String, Expr>,
    ) -> Result<Returned, CallFunctionError> {
        let mut local_context = calling_context.clone();
        for (name, arg) in args {
            let value = self
                .eval_expression(&calling_context, iteration_item, &arg)
                .map_err(Box::new)?;
            local_context.arguments.insert(name.clone(), value);
        }

        Ok(self.eval_instructions(
            calling_context,
            &None,
            &self
                .state
                .functions
                .get(name)
                .ok_or_else(|| CallFunctionError::FunctionNotFound {
                    name: name.to_owned(),
                })?
                .0,
            args,
        )?)
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn eval_instructions(
        &self,
        calling_context: &FunctionContext,
        iteration_item: &Option<Value>,
        instructions: &Vec<Instruction>,
        args: &BTreeMap<String, Expr>,
    ) -> Result<Returned, EvalInstructionsError> {
        let mut local_context = calling_context.clone();
        for (name, arg) in args {
            let value = self
                .eval_expression(&calling_context, iteration_item, &arg)
                .map_err(Box::new)?;
            local_context.arguments.insert(name.clone(), value);
        }

        for instruction in instructions {
            match instruction {
                Instruction::If(comparison, if_true, if_false) => {
                    let comparison = self
                        .eval_expression(&local_context, iteration_item, comparison)
                        .map_err(Box::new)?;
                    let is_true = match comparison {
                        Value::Scalar(Scalar::I32(value)) => value != 0,
                        _ => false,
                    };
                    let body = if is_true { if_true } else { if_false };

                    let args = local_context
                        .arguments
                        .iter()
                        .map(|(name, expr)| (name.clone(), Expr::Value(expr.clone())))
                        .collect();

                    let value =
                        self.eval_instructions(&local_context, iteration_item, body, &args)?;
                    if let Returned::Early(value) = value {
                        return Ok(Returned::Early(value));
                    }
                }
                Instruction::ConstAssign(name, expr) => {
                    let value = self
                        .eval_expression(&local_context, iteration_item, &expr)
                        .map_err(Box::new)?;
                    local_context.constants.insert(name.clone(), value);
                }
                Instruction::VarAssign(name, expr) => {
                    let value = self
                        .eval_expression(&local_context, iteration_item, &expr)
                        .map_err(Box::new)?;
                    local_context.variables.insert(name.clone(), value);
                }
                Instruction::Drop(_) => {}
                Instruction::Return(expr) => {
                    return Ok(self
                        .eval_expression(&local_context, iteration_item, expr)
                        .map(Returned::Early)
                        .map_err(Box::new)?)
                }
                Instruction::For(range, instructions) => {
                    match range {
                        Range::Range {
                            start,
                            stop,
                            step,
                            mode,
                        } => {
                            let start = self
                                .eval_expression(&local_context, iteration_item, start)
                                .map_err(Box::new)?;
                            let stop = self
                                .eval_expression(&local_context, iteration_item, stop)
                                .map_err(Box::new)?;
                            let step = self
                                .eval_expression(&local_context, iteration_item, step)
                                .map_err(Box::new)?;
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
                                        let value = self.eval_instructions(
                                            &local_context,
                                            &Some(Value::Scalar(Scalar::I32(v as i32))),
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
                                (Value::Scalar(_), Value::Scalar(_), Value::Scalar(_)) => {
                                    return Err(EvalInstructionsError::IteratorMustBeI32);
                                }
                                (Value::Vector(_), _, _) | (Value::None, _, _) => {
                                    return Err(EvalInstructionsError::StartMustBeI32);
                                }
                                (_, Value::Vector(_), _) | (_, Value::None, _) => {
                                    return Err(EvalInstructionsError::StopMustBeI32);
                                }
                                (_, _, Value::Vector(_)) | (_, _, Value::None) => {
                                    return Err(EvalInstructionsError::StepMustBeI32);
                                }
                            }
                        }
                        Range::Values(values) => {
                            for v in values {
                                let value = self.eval_instructions(
                                    &local_context,
                                    &Some(v.clone()),
                                    instructions,
                                    &args,
                                )?;
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
    pub fn add_constant(&mut self, name: &str, value: Value) -> Result<(), AddConstantError> {
        if self.state.constants.contains_key(name) {
            return Err(AddConstantError::ConstantAlreadyExists {
                name: name.to_owned(),
            });
        }
        _ = self.state.constants.insert(name.to_owned(), value);
        Ok(())
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn add_function(&mut self, name: &str, func: Function) -> Result<(), AddFunctionError> {
        if self.state.functions.contains_key(name) {
            return Err(AddFunctionError::FunctionAlreadyExists {
                name: name.to_owned(),
            });
        }
        _ = self.state.functions.insert(name.to_owned(), func);
        Ok(())
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn get_function(&self, name: &str) -> Option<&Function> {
        self.state.functions.get(name)
    }
}
