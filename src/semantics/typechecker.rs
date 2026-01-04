use crate::common::diagnostics::*;
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
    Unit,
    Never,
    Error,
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
            r::StmtKind::Error => t::StmtKind::Error,
        };

        t::Stmt { kind, span: stmt.span }
    }

    fn infer(&mut self, expr: r::Expr) -> t::Expr {
       match expr.kind {
            r::ExprKind::Number { value, unit } => t::Expr {
                kind: t::ExprKind::Number { value, unit },
                span: expr.span,
                ty: Type::Number
            },

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
            r::ExprKind::BinaryOp { op, left, right } => {
                let left = Box::new(self.check(*left, Type::Number));
                let right = Box::new(self.check(*right, Type::Number));

                t::Expr {
                    kind: t::ExprKind::BinaryOp { op, left, right },
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

            r::ExprKind::Error => t::Expr {
                kind: t::ExprKind::Error,
                span: expr.span,
                ty: Type::Never
            },
        }
    }

    fn check(&mut self, expr: r::Expr, expected: Type) -> t::Expr {
        let mut t_expr = self.infer(expr);

        if t_expr.ty != expected && !matches!(t_expr.ty, Type::Error | Type::Never) {
            self.diags.emit(
                Diagnostic::error("type mismatch")
                .with_span(t_expr.span)
                .note(format!("expected type: {:?}", expected))
                .note(format!("actual type: {:?}", t_expr.ty))
            );
            t_expr.ty = Type::Error
        }

        t_expr
    }
}

