use crate::common::Span;
use crate::common::diagnostics::*;
use crate::semantics::{
    resolver::SymbolTable,
    resolved_ast as r,
    typed_ast as t,
};

pub fn check(resolved_ast: r::ResolvedAst, symbols: &mut SymbolTable, _diagnostics: &mut impl DiagnosticSink) -> t::TypedAst {
    let mut tc = TypeChecker::new(symbols);
    tc.type_check(resolved_ast)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Number,
    Unit,
    Never,
    Error,
}

pub enum TypeError {
    Mismatch(Type, Type, Span),
    UntypedSymbol(Span),
}

pub struct TypeChecker<'a> {
    symbols: &'a mut SymbolTable,
    errors: Vec<TypeError>
}

impl<'a> TypeChecker<'a> {
    pub fn new(symbols: &'a mut SymbolTable) -> Self {
        Self {
            symbols,
            errors: Vec::new(),
        }
    }

    pub fn type_check(&mut self, ast: r::ResolvedAst) -> t::TypedAst {
        let typed_stmts = ast.stmts
            .into_iter()
            .map(|stmt| self.check_stmt(stmt));
        
        t::TypedAst {
            stmts: typed_stmts.collect(),
            span: ast.span
        }
    }
}


impl TypeChecker<'_> {
    fn check_stmt(&mut self, stmt: r::Stmt) -> t::Stmt {
        let kind = match stmt.kind {
            r::StmtKind::Let { sym, value } => {
                let  value = self.infer(value);
                self.symbols.get_mut(sym).ty = Some(value.ty.clone());
                t::StmtKind::Let { sym, value }
            }
            r::StmtKind::Expr { expr, terminated } => t::StmtKind::Expr {
                expr: self.infer(expr),
                terminated
            },
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
                    .unwrap_or_else(|| {
                        self.errors.push(TypeError::UntypedSymbol(expr.span));
                        Type::Error
                    });
                
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

            r::ExprKind::Block { stmts } => {
                let typed_stmts: Vec<t::Stmt> = stmts
                    .into_iter()
                    .map(|stmt| self.check_stmt(stmt))
                    .collect();
                
                let ty = match typed_stmts.first() {
                    Some(t::Stmt {
                        kind: t::StmtKind::Expr { expr, terminated: false }, 
                        .. 
                    }) => expr.ty.clone(),
                    _ => Type::Unit,
                };

                t::Expr {
                    kind: t::ExprKind::Block { stmts: typed_stmts },
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
            self.errors.push(TypeError::Mismatch(t_expr.ty.clone(), expected, t_expr.span));
            t_expr.ty = Type::Error
        }

        t_expr
    }
}


