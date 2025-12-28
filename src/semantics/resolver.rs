use std::collections::HashMap;
use crate::common::Span;
use crate::common::diagnostics::*;
use crate::syntax as p;
use crate::semantics::{
    resolved_ast as r,
    typechecker::Type,
};

pub fn resolve(parsed_ast: p::ParsedAst, diagnostics: &mut impl DiagnosticSink) -> (r::ResolvedAst, SymbolTable) {
    let mut resolver = Resolver::new(diagnostics);
    let resolved_ast = resolver.resolve_program(parsed_ast);
    let symbols = resolver.symbols;
    (resolved_ast, symbols)
}

// IDs

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ScopeId(u32);

// Symbols

pub struct Symbol {
    kind: SymbolKind,
    scope: ScopeId,
    def_span: Span,
    pub ty: Option<Type>
}
enum SymbolKind {
    Local,
}

pub struct SymbolTable {
    symbols: Vec<Symbol>,
}
impl SymbolTable {
    fn insert(&mut self, symbol: Symbol) -> SymbolId {
        let id = SymbolId(self.symbols.len() as u32);
        self.symbols.push(symbol);
        id
    }

    pub fn get(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.0 as usize]
    }

    pub fn get_mut(&mut self, id: SymbolId) -> &mut Symbol {
        &mut self.symbols[id.0 as usize]
    }
}

// Scopes

struct Scope {
    parent: Option<ScopeId>,
    bindings: HashMap<String, SymbolId>,
}

struct ScopeGraph {
    scopes: Vec<Scope>,
}
impl ScopeGraph {
    fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(Scope {
            parent,
            bindings: HashMap::new(),
        });
        id
    }

    fn parent(&self, scope: ScopeId) -> Option<ScopeId> {
        self.scopes[scope.0 as usize].parent
    }

    fn insert(&mut self, scope: ScopeId, name: String, symbol: SymbolId) -> Result<(), ()> {
        let scope = &mut self.scopes[scope.0 as usize];
        if scope.bindings.contains_key(&name) {
            return Err(());
        }
        scope.bindings.insert(name, symbol);
        Ok(())
    }

    fn lookup(&self, mut scope: ScopeId, name: &str) -> Option<SymbolId> {
        loop {
            let s = &self.scopes[scope.0 as usize];
            if let Some(sym) = s.bindings.get(name) {
                return Some(*sym);
            }
            scope = s.parent?;
        }
    }
}

// Resolution

pub enum ResolveError {
    Redeclaration(String, Span),
    Unresolved(String, Span),
    UseBeforeDefinintion(String, Span, Span),
}

pub struct Resolver<'d, D: DiagnosticSink> {
    scopes: ScopeGraph,
    symbols: SymbolTable,
    errors: Vec<ResolveError>,
    diags: &'d mut D,

    current_scope: ScopeId,
}

impl<'d, D: DiagnosticSink> Resolver<'d, D> {
    pub fn new(diags: &'d mut D) -> Self {
        let mut scopes = ScopeGraph { scopes: Vec::new() };
        let global = scopes.new_scope(None);

        Self {
            scopes,
            symbols: SymbolTable { symbols: Vec::new() },
            errors: Vec::new(),
            diags,
            current_scope: global,
        }
    }

    pub fn resolve_program(&mut self, ast: p::ParsedAst) -> r::ResolvedAst {
        for stmt in &ast.stmts {
            self.declare_stmt(stmt);
        }

        let resolved_stmts = ast.stmts
            .into_iter()
            .map(|stmt| self.resolve_stmt(stmt));
        
        r::ResolvedAst { stmts: resolved_stmts.collect(), span: ast.span }
    }
}

impl<'d, D: DiagnosticSink> Resolver<'d, D> {
    fn declare_stmt(&mut self, stmt: &p::Stmt) {
        match stmt.kind {
            p::StmtKind::Let { name, .. } => {
                let symbol = self.symbols.insert(Symbol {
                    kind: SymbolKind::Local,
                    scope: self.current_scope,
                    def_span: stmt.span,
                    ty: None,
                });

                if self.scopes.insert(self.current_scope, name.to_string(), symbol).is_err() {
                    self.errors.push(ResolveError::Redeclaration(name.to_string(), stmt.span));
                }
            }

            _ => {}
        }
    }

    fn resolve_stmt(&mut self, stmt: p::Stmt) -> r::Stmt {
        let kind = match stmt.kind {
            p::StmtKind::Let { name, value } => {
                let value = self.resolve_expr(value);
                let sym = self.scopes.lookup(self.current_scope, name).unwrap();
                r::StmtKind::Let { sym, value }
            }
            p::StmtKind::Expr(expr) => r::StmtKind::Expr(self.resolve_expr(expr)),
            p::StmtKind::Empty => r::StmtKind::Empty,
            p::StmtKind::Error => r::StmtKind::Error,
        };

        r::Stmt { kind, span: stmt.span }
    }

    fn resolve_expr(&mut self, expr: p::Expr) -> r::Expr {
        let kind = match expr.kind {
            p::ExprKind::Number { value, unit } => r::ExprKind::Number { value, unit },
            p::ExprKind::Identifier { name } => {
                match self.resolve_ident(name, expr.span) {
                    Some(sym) => r::ExprKind::Identifier { sym },
                    None => r::ExprKind::Error,
                }
            }
            p::ExprKind::BinaryOp { op, left, right } => {
                let left = Box::new(self.resolve_expr(*left));
                let right = Box::new(self.resolve_expr(*right));
                r::ExprKind::BinaryOp { op, left, right }
            },
            p::ExprKind::Error => r::ExprKind::Error,
        };

        r::Expr { kind, span: expr.span }
    }

    fn resolve_ident(&mut self, name: &str, span: Span) -> Option<SymbolId> {
        let mut scope = self.current_scope;
        let mut first = None;
        loop {
            match self.scopes.lookup(scope, name) {
                Some(sym) => {
                    if first.is_none() { first = Some(sym) }
                    let symbol = self.symbols.get(sym);

                    if matches!(symbol.kind, SymbolKind::Local)
                        && span.start < symbol.def_span.end
                    {
                        // try to re-lookup in the symbols scopes parent scope.
                        scope = match self.scopes.parent(symbol.scope) {
                            Some(p) => p,
                            None => {
                                let sym_span = self.symbols.get(first.unwrap()).def_span;
                                self.errors.push(ResolveError::UseBeforeDefinintion(name.to_string(), span, sym_span));
                                return None;
                            }
                        };
                        continue;
                    }

                    return Some(sym);
                }
                None => {
                    self.errors.push(ResolveError::Unresolved(name.to_string(), span));
                    return None;
                }
            }
        }
    }
}

// Tests

#[cfg(test)]
mod tests {
    use crate::common::Span;

    use super::*;

    #[test]
    fn simple_test() {
        let src = p::ParsedAst {
            stmts: vec![
                p::Stmt {
                    kind: p::StmtKind::Let {
                        name: "x",
                        value: p::Expr {
                            kind: p::ExprKind::Number { value: 2.0, unit: None },
                            span: Span::new(8, 9)
                        }
                    },
                    span: Span { start: 0, end: 10 }
                },
                p::Stmt {
                    kind: p::StmtKind::Expr(p::Expr {
                        kind: p::ExprKind::BinaryOp {
                            op: p::BinaryOp::Add,
                            left: Box::new(p::Expr { kind: p::ExprKind::Identifier { name: "x" }, span: Span::new(11, 12) }),
                            right: Box::new(p::Expr { kind: p::ExprKind::Identifier { name: "x" }, span: Span::new(15, 16) }),
                        },
                        span: Span::new(11, 16)
                    }),
                    span: Span::new(11, 17)
                }
            ],
            span: Span::new(0, 17)
        };

        let expected = r::ResolvedAst {
            stmts: vec![
                r::Stmt {
                    kind: r::StmtKind::Let {
                        sym: SymbolId(0),
                        value: r::Expr {
                            kind: r::ExprKind::Number { value: 2.0, unit: None },
                            span: Span::new(8, 9)
                        }
                    },
                    span: Span { start: 0, end: 10 }
                },
                r::Stmt {
                    kind: r::StmtKind::Expr(r::Expr {
                        kind: r::ExprKind::BinaryOp {
                            op: r::BinaryOp::Add,
                            left: Box::new(r::Expr { kind: r::ExprKind::Identifier { sym: SymbolId(0) }, span: Span::new(11, 12) }),
                            right: Box::new(r::Expr { kind: r::ExprKind::Identifier { sym: SymbolId(0) }, span: Span::new(15, 16) }),
                        },
                        span: Span::new(11, 16)
                    }),
                    span: Span::new(11, 17)
                }
            ],
            span: Span::new(0, 17)
        };

        let mut diags = AssertErrors;
        let mut resolver = Resolver::new(&mut diags);
        assert_eq!(resolver.resolve_program(src), expected);
    }
}
