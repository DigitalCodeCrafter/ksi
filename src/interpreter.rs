use std::collections::HashMap;
use crate::common::Span;
use crate::common::diagnostics::sinks::Diagnostics;
use crate::syntax::*;

pub struct Env<'a> {
    parent: Option<&'a Env<'a>>,
    vars: HashMap<&'a str, f64>,
}

#[derive(Debug)]
pub enum EvalError {
    InvalidStatement(Span),
    InvalidExpression(Span),
    UnknownVariable(String, Span),
}

pub fn interpret_program<'a>(src: &'a str) -> (HashMap<&'a str, f64>, Vec<EvalError>) {
    let mut diagnostics = Diagnostics::empty();
    let ast = parse(src, &mut diagnostics);

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
        StmtKind::Error => Err(EvalError::InvalidStatement(stmt.span)),
    }
}

fn eval_expr<'a>(expr: &Expr<'a>, env: &Env<'a>) -> Result<f64, EvalError> {
    match &expr.kind {
        ExprKind::Number { value, .. } => Ok(*value),

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

        ExprKind::BinaryOp { op, left, right } => {
            let l = eval_expr(left, env)?;
            let r = eval_expr(right, env)?;

            match op {
                BinaryOp::Add => Ok(l + r),
                BinaryOp::Sub => Ok(l - r),
                BinaryOp::Mul => Ok(l * r),
                BinaryOp::Div => Ok(l / r),
            }
        }

        ExprKind::Block { stmts, tail_expr } => {
            let mut inner_env = Env { parent: Some(env), vars: HashMap::new() };

            for stmt in stmts {
                eval_stmt(stmt, &mut inner_env)?;
            }

            tail_expr.map(|expr| eval_expr(expr, &inner_env)).unwrap_or(Ok(0.0))
        }

        ExprKind::Error => Err(EvalError::InvalidExpression(expr.span)),
    }
}
