use std::collections::HashMap;
use crate::common::Span;
use crate::syntax::*;

pub struct Env<'a> {
    parent: Option<&'a Env<'a>>,
    vars: HashMap<&'a str, Value>,
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Unit,
}

#[derive(Debug)]
pub enum EvalError {
    InvalidStatement(Span),
    InvalidExpression(Span),
    UnknownVariable(String, Span),
    InvalidType(Value, Value, Span),
}

pub fn interpret_program<'a>(ast: &'a ParsedAst) -> (HashMap<&'a str, Value>, Vec<EvalError>) {
    let mut env = Env { parent: None, vars: HashMap::new() };
    let mut errors = Vec::new();

    for stmt in &ast.stmts {
        match eval_stmt(stmt, &mut env) {
            Err(e) => errors.push(e),
            Ok(_) => {},
        }
    }

    (env.vars, errors)
}

fn eval_stmt<'a>(stmt: &Stmt<'a>, env: &mut Env<'a>) -> Result<(), EvalError> {
    match &stmt.kind {
        StmtKind::Let { name, value } => {
            let v = eval_expr(value, env)?;
            env.vars.insert(*name, v);
            Ok(())
        }
        StmtKind::Expr(expr) => {
            eval_expr(expr, env)?;
            Ok(())
        }
        StmtKind::Empty => Ok(()),
        StmtKind::Error(_) => Err(EvalError::InvalidStatement(stmt.span)),
    }
}

fn eval_expr<'a>(expr: &Expr<'a>, env: &Env<'a>) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Literal(Literal::Number { value, .. }) => Ok(Value::Number(*value)),
        ExprKind::Literal(Literal::Bool(b)) => Ok(Value::Bool(*b)),

        ExprKind::Identifier { name } => {
            let mut current = env;
            loop {
                let found = current.vars.get(name).copied();

                if let Some(val) = found {
                    return Ok(val)
                }

                match current.parent {
                    Some(p) => current = p,
                    None => return Err(EvalError::UnknownVariable(name.to_string(), expr.span)),
                }
            }
        }

        ExprKind::UnaryOp { op, expr: inner_expr } => {
            let v = eval_expr(inner_expr, env)?;

            match (op, v) {
                (UnaryOp::Neg, Value::Number(v)) => Ok(Value::Number(-v)),
                _ => return Err(EvalError::InvalidType(v, Value::Number(0.0), expr.span))
            }
        }

        ExprKind::BinaryOp { op, left, right } => {
            let l = eval_expr(left, env)?;
            let r = eval_expr(right, env)?;

            match (op, l, r) {
                (BinaryOp::Add, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (BinaryOp::Sub, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                (BinaryOp::Mul, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                (BinaryOp::Div, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),

                (BinaryOp::Gt, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l > r)),
                (BinaryOp::Ge, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l >= r)),
                (BinaryOp::Lt, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l < r)),
                (BinaryOp::Le, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l <= r)),

                (BinaryOp::Eq, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l == r)),
                (BinaryOp::Ne, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l != r)),
                (BinaryOp::Eq, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l != r)),
                (BinaryOp::Ne, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l != r)),
                _ => return Err(EvalError::InvalidType(l, r, expr.span)),
            }
        }

        ExprKind::Block { stmts, tail_expr } => {
            let mut inner_env = Env { parent: Some(env), vars: HashMap::new() };

            for stmt in stmts {
                eval_stmt(stmt, &mut inner_env)?;
            }

            tail_expr.as_ref().map(|expr| eval_expr(expr, &inner_env)).unwrap_or(Ok(Value::Unit))
        }

        ExprKind::Error(_) => Err(EvalError::InvalidExpression(expr.span)),
    }
}
