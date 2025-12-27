use std::collections::HashMap;
use crate::{parsed_ast::{BinaryOp, Expr, ExprKind, Stmt, StmtKind}, lexer::TokenStream, parser::Parser, Span};

pub struct Env<'a> {
    vars: HashMap<&'a str, f64>,
}

#[derive(Debug)]
pub enum EvalError {
    InvalidStatement(Span),
    InvalidExpression(Span),
    UnknownVariable(String, Span),
}

pub fn interpret_program<'a>(src: &'a str) -> (HashMap<&'a str, f64>, Vec<EvalError>) {
    let tokens = TokenStream::new(src);
    let mut parser = Parser::new(tokens);

    let ast = parser.parse_program();

    let mut env = Env { vars: HashMap::new() };
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

        ExprKind::Identifier { name } => env.vars
            .get(name)
            .copied()
            .ok_or(EvalError::UnknownVariable(name.to_string(), expr.span)),

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

        ExprKind::Error => Err(EvalError::InvalidExpression(expr.span)),
    }
}
