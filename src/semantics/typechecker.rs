use crate::common::{Span, diagnostics::*};
use crate::semantics::{
    resolver::SymbolTable,
    resolved_ast as r,
    typed_ast as t,
};

pub fn check(resolved_ast: r::ResolvedAst, symbols: &mut SymbolTable, diagnostics: &mut impl DiagnosticSink) -> t::TypedAst {
    let mut tc = TypeChecker::new(symbols, diagnostics);
    tc.type_check(resolved_ast)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Number,
    Bool,
    Unit,

    Never,
    Error(ErrorGuaranteed),
}

struct TypeChecker<'a, 'd, D: DiagnosticSink> {
    symbols: &'a mut SymbolTable,
    diags: &'d mut D,
}

impl<'a, 'd, D: DiagnosticSink> TypeChecker<'a, 'd, D> {
    fn new(symbols: &'a mut SymbolTable, diags: &'d mut D) -> Self {
        Self {
            symbols,
            diags,
        }
    }

    fn type_check(&mut self, ast: r::ResolvedAst) -> t::TypedAst {
        let typed_stmts = ast.stmts
            .into_iter()
            .map(|stmt| self.check_stmt(stmt));
        
        t::TypedAst {
            stmts: typed_stmts.collect(),
            span: ast.span
        }
    }
}

impl<D: DiagnosticSink> TypeChecker<'_, '_, D> {
    fn check_stmt(&mut self, stmt: r::Stmt) -> t::Stmt {
        let kind = match stmt.kind {
            r::StmtKind::Let { sym, value } => {
                let  value = self.infer(value);
                self.symbols.get_mut(sym).ty = Some(value.ty.clone());
                t::StmtKind::Let { sym, value }
            }
            r::StmtKind::Expr(expr) => t::StmtKind::Expr(self.infer(expr)),
            r::StmtKind::Empty => t::StmtKind::Empty,
            r::StmtKind::Error(e) => t::StmtKind::Error(e),
        };

        t::Stmt { kind, span: stmt.span }
    }

    fn infer(&mut self, expr: r::Expr) -> t::Expr {
       match expr.kind {
            r::ExprKind::Literal(lit) => {
                let ty = match lit {
                    r::Literal::Number { .. } => Type::Number,
                    r::Literal::Bool(_) => Type::Bool,
                };
                t::Expr {
                    kind: t::ExprKind::Literal(lit),
                    span: expr.span,
                    ty,
                }
            }

            r::ExprKind::Identifier { sym } => {
                let ty = self.symbols
                    .get(sym).ty
                    .clone()
                    .expect(&format!("[Typechecker] Internal error: tried to get type of the untyped symbol {:?}", sym));
                
                t::Expr {
                    kind: t::ExprKind::Identifier { sym },
                    span: expr.span,
                    ty,
                }
            }

            r::ExprKind::BinaryOp { op, left, right } => self.infer_binary(expr.span, op, *left, *right),

            r::ExprKind::UnaryOp { op, expr: inner_expr } => {
                let typed_expr = Box::new(self.check(*inner_expr, Type::Number));

                t::Expr {
                    kind: t::ExprKind::UnaryOp { op, expr: typed_expr },
                    span: expr.span,
                    ty: Type::Number,
                }
            }

            r::ExprKind::Block { stmts, tail_expr } => {
                let typed_stmts: Vec<t::Stmt> = stmts
                    .into_iter()
                    .map(|stmt| self.check_stmt(stmt))
                    .collect();
                
                let typed_expr = tail_expr.map(|expr| Box::new(self.infer(*expr)));

                let ty = typed_expr.as_ref().map(|e| e.ty.clone()).unwrap_or(Type::Unit);

                t::Expr {
                    kind: t::ExprKind::Block {
                        stmts: typed_stmts,
                        tail_expr: typed_expr,
                    },
                    span: expr.span,
                    ty,
                }
            }

            r::ExprKind::If { cond, then_branch, else_branch: Some(else_branch) } => {
                let typed_cond = self.check(*cond, Type::Bool);
                let typed_then = self.infer(*then_branch);
                let typed_else = self.check(*else_branch, typed_then.ty.clone());
                let ty = typed_then.ty.clone();

                t::Expr {
                    kind: t::ExprKind::If {
                        cond: Box::new(typed_cond),
                        then_branch: Box::new(typed_then),
                        else_branch: Some(Box::new(typed_else))
                    }, 
                    span: expr.span, 
                    ty
                }
            }

            r::ExprKind::If { cond, then_branch, else_branch: None } => {
                let typed_cond = self.check(*cond, Type::Bool);
                let typed_then = self.check(*then_branch, Type::Unit);

                t::Expr {
                    kind: t::ExprKind::If {
                        cond: Box::new(typed_cond),
                        then_branch: Box::new(typed_then),
                        else_branch: None
                    }, 
                    span: expr.span, 
                    ty: Type::Unit
                }
            }

            r::ExprKind::Error(e) => t::Expr {
                kind: t::ExprKind::Error(e),
                span: expr.span,
                ty: Type::Never
            },
        }
    }

    fn check(&mut self, expr: r::Expr, expected: Type) -> t::Expr {
        match expr.kind {
            r::ExprKind::Block { stmts, tail_expr: Some(tail_expr) } => {
                let typed_stmts: Vec<t::Stmt> = stmts
                    .into_iter()
                    .map(|stmt| self.check_stmt(stmt))
                    .collect();
                
                let typed_expr = self.check(*tail_expr, expected.clone());
                let ty = typed_expr.ty.clone();

                t::Expr {
                    kind: t::ExprKind::Block {
                        stmts: typed_stmts,
                        tail_expr: Some(Box::new(typed_expr)),
                    },
                    span: expr.span,
                    ty,
                }
            }

            r::ExprKind::If { cond, then_branch, else_branch: Some(else_branch) } => {
                let typed_cond = self.check(*cond, Type::Bool);
                let typed_then = self.check(*then_branch, expected.clone());
                let typed_else = self.check(*else_branch, expected.clone());
                
                let ty = self.unify_or_error(expr.span, typed_then.ty.clone(), typed_else.ty.clone());

                t::Expr {
                    kind: t::ExprKind::If {
                        cond: Box::new(typed_cond),
                        then_branch: Box::new(typed_then),
                        else_branch: Some(Box::new(typed_else))
                    },
                    span: expr.span,
                    ty,
                }
            }

            _ => {
                let mut t_expr = self.infer(expr);
                t_expr.ty = self.unify_or_error(t_expr.span, expected, t_expr.ty);
                t_expr
            }
        }
    }
}

impl<D: DiagnosticSink> TypeChecker<'_, '_, D> {
    fn infer_binary(&mut self, span: Span, op: r::BinaryOp, left: r::Expr, right: r::Expr) -> t::Expr {
        match op {
            r::BinaryOp::Add
            | r::BinaryOp::Sub
            | r::BinaryOp::Mul
            | r::BinaryOp::Div => {
                let left = Box::new(self.check(left, Type::Number));
                let right = Box::new(self.check(right, Type::Number));
        
                t::Expr {
                    kind: t::ExprKind::BinaryOp { op, left, right },
                    span,
                    ty: Type::Number,
                }
            }

            r::BinaryOp::Ge
            | r::BinaryOp::Gt
            | r::BinaryOp::Le
            | r::BinaryOp::Lt => {
                let left = Box::new(self.check(left, Type::Number));
                let right = Box::new(self.check(right, Type::Number));
        
                t::Expr {
                    kind: t::ExprKind::BinaryOp { op, left, right },
                    span,
                    ty: Type::Bool,
                }
            }

            r::BinaryOp::Eq
            | r::BinaryOp::Ne => {
                let left = Box::new(self.infer(left));
                let right = Box::new(self.check(right, left.ty.clone()));
        
                t::Expr {
                    kind: t::ExprKind::BinaryOp { op, left, right },
                    span,
                    ty: Type::Bool,
                }
            },
        }

    }
}

impl<D: DiagnosticSink> TypeChecker<'_, '_, D> {
    fn unify(a: Type, b: Type) -> Option<Type> {
        use Type::*;
        match (a, b) {
            (Error(_), t) | (t, Error(_)) => Some(t),
            (Never, t) | (t, Never) => Some(t),

            (Number, Number) => Some(Number),
            (Bool, Bool) => Some(Bool),

            _ => None,
        }
    }

    fn unify_or_error(&mut self, span: Span, expected: Type, actual: Type) -> Type {
        match Self::unify(expected.clone(), actual.clone()) {
            Some(ty) => ty,
            None => {
                let e = self.diags.emit(
                    Diagnostic::error("type mismatch")
                    .with_span(span)
                    .note(format!("expected type: {:?}", expected))
                    .note(format!("actual type: {:?}", actual))
                );
                Type::Error(e)
            }
        }
    }
}

