use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    ops::{Deref, DerefMut},
};

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
pub enum EvalExpressionError {
    #[error(r#"function "{name}" not found!"#)]
    FunctionNotFound { name: String },
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
    #[error("Expected argument but no arguments were given")]
    NoArgumentsGiven,
    #[error("Cannot find iteration item")]
    IterationItemNotFound,
    #[error("Cannot do arithmetic with None: {operation:?}")]
    NoArithmeticWithNone { operation: BinaryOp },
    #[error("Type mismatch")]
    TypeMismatch,
    #[error("Cannot compare with None: {comparison:?}")]
    NoComparisonWithNone { comparison: BinaryOp },
    #[error(r#"Only I32 is allowed for iteration!"#)]
    IteratorMustBeI32,
    #[error(r#"Field "start" must be an I32!"#)]
    StartMustBeI32,
    #[error(r#"Field "stop" must be an I32!"#)]
    StopMustBeI32,
    #[error(r#"Field "step" must be an I32!"#)]
    StepMustBeI32,
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

impl Value {
    fn early(self) -> Returned {
        Returned::Early(self)
    }
    fn done(self) -> Returned {
        Returned::Finished(self)
    }
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
pub struct Ref(usize);

impl Deref for Ref {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Ref {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum ExprTree {
    Value(Value),
    Const(String),
    Var(String),
    Arg(String),
    Index(Box<ExprTree>, Box<ExprTree>),
    Item,
    BinaryOp(BinaryOp, Box<ExprTree>, Box<ExprTree>),
    UnaryOp(UnaryOp, Box<ExprTree>),
    If(Box<ExprTree>, Vec<ExprTree>, Vec<ExprTree>),
    ConstAssign(String, Box<ExprTree>),
    For(Box<RangeTree>, Vec<ExprTree>),
    VarAssign(String, Box<ExprTree>),
    Drop(Box<ExprTree>),
    Return(Box<ExprTree>),
    Call(String, BTreeMap<String, ExprTree>),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Expr {
    Value(Value),
    Const(String),
    Var(String),
    Arg(String),
    Index(Ref, Ref),
    Item,
    BinaryOp(BinaryOp, Ref, Ref),
    UnaryOp(UnaryOp, Ref),
    If(Ref, Vec<Ref>, Vec<Ref>),
    ConstAssign(String, Ref),
    For(Range, Vec<Ref>),
    VarAssign(String, Ref),
    Drop(Ref),
    Return(Ref),
    Call(String, BTreeMap<String, Ref>),
}

impl Expr {
    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn get_arguments(&self, fun: &Function) -> Vec<String> {
        let mut args = Vec::new();
        match self {
            Expr::Value(_) => (),
            Expr::Const(_) => (),
            Expr::Var(_) => (),
            Expr::Index(left, right) => {
                let left = &fun[**left];
                let right = &fun[**right];
                args.extend(left.get_arguments(fun).into_iter());
                args.extend(right.get_arguments(fun).into_iter());
            }
            Expr::Item => (),
            Expr::Arg(name) => args.push(name.to_owned()),
            Expr::BinaryOp(_, left, right) => {
                let left = &fun[**left];
                let right = &fun[**right];
                args.extend(left.get_arguments(fun).into_iter());
                args.extend(right.get_arguments(fun).into_iter());
            }
            Expr::UnaryOp(_, expr) => {
                let expr = &fun[**expr];
                args.extend(expr.get_arguments(fun).into_iter());
            }
            Expr::Call(_, arguments) => {
                for (_, value) in arguments {
                    let value = &fun[**value];
                    args.extend(value.get_arguments(fun).into_iter());
                }
            }
            Expr::If(expr, if_true, if_false) => {
                let expr = &fun[**expr];
                args.extend(expr.get_arguments(fun));
                args.extend(
                    if_true
                        .into_iter()
                        .map(|i| &fun[**i])
                        .map(|v| v.get_arguments(fun))
                        .flatten(),
                );
                args.extend(
                    if_false
                        .into_iter()
                        .map(|i| &fun[**i])
                        .map(|v| v.get_arguments(fun))
                        .flatten(),
                );
            }
            Expr::ConstAssign(_, expr) => {
                let expr = &fun[**expr];
                args.extend(expr.get_arguments(fun))
            }
            Expr::For(expr, body) => {
                args.extend(expr.get_arguments(fun));
                args.extend(
                    body.into_iter()
                        .map(|i| &fun[**i])
                        .map(|v| v.get_arguments(fun))
                        .flatten(),
                );
            }
            Expr::VarAssign(_, expr) => {
                let expr = &fun[**expr];
                args.extend(expr.get_arguments(fun));
            }
            Expr::Drop(expr) => {
                let expr = &fun[**expr];
                args.extend(expr.get_arguments(fun));
            }
            Expr::Return(expr) => {
                let expr = &fun[**expr];
                args.extend(expr.get_arguments(fun));
            }
        };
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
        start: Ref,
        stop: Ref,
        step: Ref,
        mode: RangeMode,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum RangeTree {
    Values(Vec<Value>),
    Range {
        start: ExprTree,
        stop: ExprTree,
        step: ExprTree,
        mode: RangeMode,
    },
}

impl Range {
    pub fn get_arguments(&self, fun: &Function) -> Vec<String> {
        let mut args = Vec::new();
        match self {
            Range::Values { .. } => (),
            Range::Range {
                start, stop, step, ..
            } => {
                let start = &fun[**start];
                args.extend(start.get_arguments(fun).into_iter());
                let stop = &fun[**stop];
                args.extend(stop.get_arguments(fun).into_iter());
                let step = &fun[**step];
                args.extend(step.get_arguments(fun).into_iter());
            }
        }
        args.sort();
        args.dedup();
        args
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Function(Vec<Expr>);

impl Deref for Function {
    type Target = Vec<Expr>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Function {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Function {
    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn new(body: Vec<Expr>) -> Self {
        Self(body)
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn get_arguments(&self) -> Vec<String> {
        let Function(body) = self;
        let mut args = body
            .iter()
            .map(|i| i.get_arguments(self))
            .flatten()
            .collect::<Vec<_>>();
        args.sort();
        args.dedup();
        args
    }
}

#[derive(Debug, Default)]
pub struct FunctionTree(Vec<ExprTree>);

impl FunctionTree {
    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn new(body: Vec<ExprTree>) -> Self {
        Self(body)
    }

    pub fn flatten(self, container: &mut Vec<Expr>) {
        for expr in self.0 {
            match expr {
                ExprTree::Value(v) => container.push(Expr::Value(v)),
                ExprTree::Const(c) => container.push(Expr::Const(c)),
                ExprTree::Var(v) => container.push(Expr::Var(v)),
                ExprTree::Arg(a) => container.push(Expr::Arg(a)),
                ExprTree::Index(c, i) => {
                    let e = Expr::Index(c.flatten(container), i.flatten(container));
                    container.push(e);
                }
                ExprTree::Item => container.push(Expr::Item),
                ExprTree::BinaryOp(op, left, right) => {
                    let left = left.flatten(container);
                    let right = right.flatten(container);
                    container.push(Expr::BinaryOp(op, left, right));
                }
                ExprTree::UnaryOp(op, v) => {
                    let e = Expr::UnaryOp(op, v.flatten(container));
                    container.push(e);
                }
                ExprTree::If(comp, if_true, if_false) => {
                    let if_true = if_true.iter().map(|e| e.flatten(container)).collect();
                    let if_false = if_false.iter().map(|e| e.flatten(container)).collect();
                    let e = Expr::If(comp.flatten(container), if_true, if_false);
                    container.push(e);
                }
                ExprTree::ConstAssign(name, value) => {
                    let e = Expr::ConstAssign(name, value.flatten(container));
                    container.push(e);
                }
                ExprTree::For(range, body) => {
                    let e = Expr::For(
                        range.flatten(container),
                        body.iter().map(|v| v.flatten(container)).collect(),
                    );
                    container.push(e);
                }
                ExprTree::VarAssign(name, value) => {
                    let e = Expr::VarAssign(name, value.flatten(container));
                    container.push(e);
                }
                ExprTree::Drop(x) => {
                    let e = Expr::Drop(x.flatten(container));
                    container.push(e);
                }
                ExprTree::Return(r) => {
                    let e = Expr::Return(r.flatten(container));
                    container.push(e);
                }
                ExprTree::Call(name, args) => {
                    let e = Expr::Call(
                        name,
                        args.iter()
                            .map(|(name, value)| (name.to_owned(), value.flatten(container)))
                            .collect(),
                    );
                    container.push(e);
                }
            }
        }
    }
}

impl ExprTree {
    pub fn flatten(&self, container: &mut Vec<Expr>) -> Ref {
        match self {
            ExprTree::Value(v) => container.push(Expr::Value(v.clone())),
            ExprTree::Const(c) => container.push(Expr::Const(c.clone())),
            ExprTree::Var(v) => container.push(Expr::Var(v.clone())),
            ExprTree::Arg(a) => container.push(Expr::Arg(a.clone())),
            ExprTree::Index(v, i) => {
                let e = Expr::Index(v.flatten(container), i.flatten(container));
                container.push(e);
            }
            ExprTree::Item => container.push(Expr::Item),
            ExprTree::BinaryOp(op, left, right) => {
                let e = Expr::BinaryOp(
                    op.clone(),
                    left.flatten(container),
                    right.flatten(container),
                );
                container.push(e);
            }
            ExprTree::UnaryOp(op, e) => {
                let e = Expr::UnaryOp(*op, e.flatten(container));
                container.push(e);
            }
            ExprTree::If(comp, if_true, if_false) => {
                let e = Expr::If(
                    comp.flatten(container),
                    if_true.iter().map(|e| e.flatten(container)).collect(),
                    if_false.iter().map(|e| e.flatten(container)).collect(),
                );
                container.push(e);
            }
            ExprTree::ConstAssign(name, e) => {
                let e = Expr::ConstAssign(name.clone(), e.flatten(container));
                container.push(e);
            }
            ExprTree::For(range, body) => {
                let e = Expr::For(
                    range.flatten(container),
                    body.iter().map(|e| e.flatten(container)).collect(),
                );
                container.push(e);
            }
            ExprTree::VarAssign(name, e) => {
                let e = Expr::VarAssign(name.clone(), e.flatten(container));
                container.push(e);
            }
            ExprTree::Drop(e) => {
                let e = Expr::Drop(e.flatten(container));
                container.push(e);
            }
            ExprTree::Return(e) => {
                let e = Expr::Return(e.flatten(container));
                container.push(e);
            }
            ExprTree::Call(name, args) => {
                let e = Expr::Call(
                    name.clone(),
                    args.iter()
                        .map(|(name, e)| (name.to_owned(), e.flatten(container)))
                        .collect(),
                );
                container.push(e);
            }
        }
        Ref(container.len() - 1)
    }
}

impl RangeTree {
    pub fn flatten(&self, container: &mut Vec<Expr>) -> Range {
        match self {
            RangeTree::Values(v) => Range::Values(v.clone()),
            RangeTree::Range {
                start,
                stop,
                step,
                mode,
            } => Range::Range {
                start: start.flatten(container),
                stop: stop.flatten(container),
                step: step.flatten(container),
                mode: mode.clone(),
            },
        }
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

#[derive(Clone, Debug)]
pub struct FunctionContext {
    variables: BTreeMap<String, Value>,
    constants: BTreeMap<String, Value>,
}

impl FunctionContext {
    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn new() -> Self {
        Self {
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
        fun: &Function,
        local_context: &mut FunctionContext,
        args: Option<&BTreeMap<String, &Value>>,
        iteration_item: &Option<Value>,
        expr: &Expr,
    ) -> Result<Returned, EvalExpressionError> {
        use EvalExpressionError::*;
        match expr {
            Expr::Value(value) => Ok(value.clone().done()),
            Expr::Const(name) => match local_context.constants.get(name) {
                Some(value) => Ok(value.clone().done()),
                None => match self.state.constants.get(name) {
                    Some(value) => Ok(value.clone().done()),
                    None => Err(ConstantNotFound {
                        name: name.to_owned(),
                    }),
                },
            },
            Expr::Index(container, index) => {
                let container = self.eval_expression(
                    fun,
                    local_context,
                    args,
                    iteration_item,
                    &fun[**container],
                )?;
                let index =
                    self.eval_expression(fun, local_context, args, iteration_item, &fun[**index])?;
                match container.into_inner() {
                    Value::Vector(container) => match index.into_inner() {
                        Value::Scalar(Scalar::I32(index)) => match container.get(index as usize) {
                            Some(value) => Ok(value.clone().done()),
                            None => Err(IndexOutOfRange),
                        },
                        _ => Err(IndexMustBeI32),
                    },
                    _ => Err(LhsNotIndexable),
                }
            }
            Expr::Var(name) => match local_context.variables.get(name) {
                Some(value) => Ok(value.clone().done()),
                None => Err(VariableNotFound { name: name.clone() }),
            },
            Expr::Arg(name) => {
                if let Some(args) = args {
                    match args.get(name) {
                        Some(value) => Ok(value.to_owned().clone().done()),
                        None => Err(ArgumentNotFound { name: name.clone() }),
                    }
                } else {
                    Err(NoArgumentsGiven)
                }
            }
            Expr::Item => match iteration_item {
                Some(value) => Ok(value.clone().done()),
                None => Err(IterationItemNotFound),
            },
            Expr::BinaryOp(op, left, right) => {
                let left =
                    self.eval_expression(fun, local_context, args, iteration_item, &fun[**left])?;
                let right =
                    self.eval_expression(fun, local_context, args, iteration_item, &fun[**right])?;
                match op {
                    BinaryOp::Add => match (left.into_inner(), right.into_inner()) {
                        (
                            Value::Scalar(Scalar::I32(left_value)),
                            Value::Scalar(Scalar::I32(right_value)),
                        ) => Ok(Value::Scalar(Scalar::I32(left_value + right_value)).done()),
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(left_value + right_value))
                        // }
                        (Value::None, _) => Err(NoArithmeticWithNone { operation: *op }),
                        (_, Value::None) => Err(NoArithmeticWithNone { operation: *op }),
                        _ => Err(TypeMismatch),
                    },
                    BinaryOp::Sub => match (left.into_inner(), right.into_inner()) {
                        (
                            Value::Scalar(Scalar::I32(left_value)),
                            Value::Scalar(Scalar::I32(right_value)),
                        ) => Ok(Value::Scalar(Scalar::I32(left_value - right_value)).done()),
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(left_value - right_value))
                        // }
                        (Value::None, _) => Err(NoArithmeticWithNone { operation: *op }),
                        (_, Value::None) => Err(NoArithmeticWithNone { operation: *op }),
                        _ => Err(TypeMismatch),
                    },
                    BinaryOp::Mul => match (left.into_inner(), right.into_inner()) {
                        (
                            Value::Scalar(Scalar::I32(left_value)),
                            Value::Scalar(Scalar::I32(right_value)),
                        ) => Ok(Value::Scalar(Scalar::I32(left_value * right_value)).done()),
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(left_value * right_value))
                        // }
                        (Value::None, _) => Err(NoArithmeticWithNone { operation: *op }),
                        (_, Value::None) => Err(NoArithmeticWithNone { operation: *op }),
                        _ => Err(TypeMismatch),
                    },
                    BinaryOp::Lt => match (left.into_inner(), right.into_inner()) {
                        (
                            Value::Scalar(Scalar::I32(left_value)),
                            Value::Scalar(Scalar::I32(right_value)),
                        ) => Ok(
                            Value::Scalar(Scalar::I32((left_value < right_value).into())).done(),
                        ),
                        // (Value::U32(left_value), Value::U32(right_value)) => {
                        //     Ok(Value::U32(if left_value < right_value { 1 } else { 0 }))
                        // }
                        (Value::None, _) => Err(NoComparisonWithNone { comparison: *op }),
                        (_, Value::None) => Err(NoComparisonWithNone { comparison: *op }),
                        _ => Err(TypeMismatch),
                    },
                    BinaryOp::Eq => Ok(Value::Scalar(Scalar::I32(
                        self.value_equal(
                            local_context,
                            iteration_item,
                            &left.into_inner(),
                            &right.into_inner(),
                        )
                        .into(),
                    ))
                    .done()),
                }
            }
            Expr::UnaryOp { .. } => Ok(Value::None.done()),
            Expr::Call(name, args) => {
                let args = args
                    .iter()
                    .map(|(name, value)| {
                        self.eval_expression(
                            fun,
                            local_context,
                            None,
                            iteration_item,
                            &fun[**value],
                        )
                        .map(|v| (name, v.into_inner()))
                    })
                    .collect::<Result<BTreeMap<_, _>, _>>()?;
                self.call_function(
                    name,
                    local_context,
                    iteration_item,
                    Some(
                        &args
                            .iter()
                            .map(|(name, arg)| (name.to_owned().clone(), arg))
                            .collect(),
                    ),
                )
                .map_err(EvalExpressionError::from)
            }

            Expr::If(comparison, if_true, if_false) => {
                let comparison = self.eval_expression(
                    fun,
                    local_context,
                    args,
                    iteration_item,
                    &fun[**comparison],
                )?;
                let is_true = match comparison.into_inner() {
                    Value::Scalar(Scalar::I32(value)) => value != 0,
                    _ => false,
                };
                let body = if is_true { if_true } else { if_false };

                let value =
                    self.eval_expressions(fun, local_context, iteration_item, body, args)?;
                Ok(value)
            }
            Expr::ConstAssign(name, expr) => {
                let value =
                    self.eval_expression(fun, local_context, args, iteration_item, &fun[**expr])?;
                local_context
                    .constants
                    .insert(name.clone(), value.into_inner());
                Ok(Value::None.done())
            }
            Expr::VarAssign(name, expr) => {
                let value =
                    self.eval_expression(fun, local_context, args, iteration_item, &fun[**expr])?;
                if let Some(value) = local_context
                    .variables
                    .insert(name.clone(), value.into_inner())
                {
                    Ok(value.done())
                } else {
                    Ok(Value::None.done())
                }
            }
            Expr::Drop(_) => Ok(Value::None.done()),
            Expr::Return(expr) => {
                return Ok(self.eval_expression(
                    fun,
                    local_context,
                    args,
                    iteration_item,
                    &fun[**expr],
                )?)
            }
            Expr::For(range, expressions) => match range {
                Range::Range {
                    start,
                    stop,
                    step,
                    mode,
                } => {
                    let start = self.eval_expression(
                        fun,
                        local_context,
                        args,
                        iteration_item,
                        &fun[**start],
                    )?;
                    let stop = self.eval_expression(
                        fun,
                        local_context,
                        args,
                        iteration_item,
                        &fun[**stop],
                    )?;
                    let step = self.eval_expression(
                        fun,
                        local_context,
                        args,
                        iteration_item,
                        &fun[**step],
                    )?;
                    match (start.into_inner(), stop.into_inner(), step.into_inner()) {
                        (
                            Value::Scalar(Scalar::I32(start)),
                            Value::Scalar(Scalar::I32(stop)),
                            Value::Scalar(Scalar::I32(step)),
                        ) => {
                            let mut last = Returned::Finished(Value::None);
                            for v in match &mode {
                                RangeMode::Inclusive => {
                                    ((start as usize)..(stop as usize)).step_by(step as usize)
                                }
                                RangeMode::Exclusive => {
                                    ((start as usize)..(stop as usize)).step_by(step as usize)
                                }
                            } {
                                let value = self.eval_expressions(
                                    fun,
                                    &local_context,
                                    &Some(Value::Scalar(Scalar::I32(v as i32))),
                                    expressions,
                                    args,
                                )?;
                                if let Returned::Early(_) = value {
                                    return Ok(value);
                                }
                                if let Expr::Return(_) = expr {
                                    return Ok(value.into_inner().early());
                                }
                                last = value;
                            }
                            Ok(last)
                        }
                        (Value::Scalar(_), Value::Scalar(_), Value::Scalar(_)) => {
                            return Err(EvalExpressionError::IteratorMustBeI32);
                        }
                        (Value::Vector(_), _, _) | (Value::None, _, _) => {
                            return Err(EvalExpressionError::StartMustBeI32);
                        }
                        (_, Value::Vector(_), _) | (_, Value::None, _) => {
                            return Err(EvalExpressionError::StopMustBeI32);
                        }
                        (_, _, Value::Vector(_)) | (_, _, Value::None) => {
                            return Err(EvalExpressionError::StepMustBeI32);
                        }
                    }
                }
                Range::Values(values) => {
                    let mut last = Returned::Finished(Value::None);
                    for v in values {
                        let value = self.eval_expressions(
                            fun,
                            &local_context,
                            &Some(v.clone()),
                            expressions,
                            args,
                        )?;
                        if let Returned::Early(_) = value {
                            return Ok(value);
                        }
                        last = value;
                    }
                    Ok(last)
                }
            },
        }
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn call_function(
        &self,
        name: &str,
        calling_context: &FunctionContext,
        iteration_item: &Option<Value>,
        args: Option<&BTreeMap<String, &Value>>,
    ) -> Result<Returned, EvalExpressionError> {
        let fun = self.state.functions.get(name).ok_or_else(|| {
            EvalExpressionError::FunctionNotFound {
                name: name.to_owned(),
            }
        })?;

        let body = fun
            .iter()
            .enumerate()
            .map(|(i, _)| Ref(i))
            .collect::<Vec<_>>();

        Ok(self.eval_expressions(fun, calling_context, iteration_item, &body, args)?)
    }

    #[cfg_attr(feature = "flame", tracing::instrument)]
    pub fn eval_expressions(
        &self,
        fun: &Function,
        calling_context: &FunctionContext,
        iteration_item: &Option<Value>,
        expressions: &Vec<Ref>,
        args: Option<&BTreeMap<String, &Value>>,
    ) -> Result<Returned, EvalExpressionError> {
        let mut local_context = calling_context.clone();

        for expr in expressions {
            match self.eval_expression(
                fun,
                &mut local_context,
                args,
                iteration_item,
                &fun[**expr],
            )? {
                Returned::Early(v) => return Ok(v.early()),
                _ => (),
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
