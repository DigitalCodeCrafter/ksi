use std::collections::HashMap;
use crate::common::diagnostics::*;
use crate::semantics::{Type, SymbolTable, SymbolId, typed_ast as t};
use crate::mir::mir::*;

pub fn lower(typed_ast: t::TypedAst, symbols: &SymbolTable, _diagnostics: &mut impl DiagnosticSink) -> Body {
    let mut builder = MirBuilder::new(symbols);
    builder.lower_function(typed_ast);
    builder.finish()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

impl LocalId {
    pub fn index(&self) -> u32 { self.0 }
}
impl BlockId {
    pub fn index(&self) -> u32 { self.0 }
}


struct MirBuilder<'a> {
    locals: Vec<Local>,
    blocks: Vec<Block>,

    current_block: BlockId,
    
    symbols: &'a SymbolTable,
    env: HashMap<SymbolId, LocalId>,
}
impl<'a> MirBuilder<'a> {
    fn new(symbols: &'a SymbolTable) -> Self {
        Self {
            locals: Vec::new(),
            blocks: vec![Block {
                instrs: Vec::new(),
                terminator: Terminator::Unreachable
            }],

            current_block: BlockId(0),

            env: HashMap::new(),
            symbols,
        }
    }

    fn lower_function(&mut self, ast: t::TypedAst) {
        for stmt in ast.stmts {
            self.lower_stmt(stmt);
        }
        self.blocks[self.current_block.0 as usize].terminator = Terminator::Return(Operand::Const(Const::Unit));
    }

    fn finish(self) -> Body {
        Body {
            blocks: self.blocks,
            locals: self.locals,
        }
    }
}

impl MirBuilder<'_> {
    fn lower_stmt(&mut self, stmt: t::Stmt) {
        match stmt.kind {
            t::StmtKind::Let { sym, value } => {
                let symbol = self.symbols.get(sym);
                let local = self.new_local(symbol.ty.clone().expect("[Lowerer] Internal Error: local var symbol has no type annotation"));
                self.env.insert(sym, local);
                self.lower_expr(Place { local, projection: Vec::new() }, value);
                self.emit(Instr::Debug(DebugInfo { span: stmt.span, kind: DebugKind::DeclareLocal { local, name: symbol.name.clone() } }));
            }
            t::StmtKind::Expr(expr) => {
                let unused = Place { local: self.new_local(expr.ty.clone()), projection: Vec::new() };
                self.lower_expr(unused, expr);
            }
            t::StmtKind::Empty => {},
            t::StmtKind::Error(_) => {},
        }
    }

    fn lower_expr(&mut self, dest: Place, expr: t::Expr) {
        let rval = match expr.kind {
            t::ExprKind::Literal(t::Literal::Number { value, .. }) => RValue::Use(Operand::Const(Const::Number(value))),
            t::ExprKind::Literal(t::Literal::Bool(b)) => RValue::Use(Operand::Const(Const::Bool(b))),

            t::ExprKind::Identifier { sym } => match self.env.get(&sym) {
                Some(&local) => RValue::Use(Operand::Copy(Place { local, projection: Vec::new() })),
                None => RValue::Poison,
            }

            t::ExprKind::BinaryOp { op, left, right } => {
                let l_place = Place { local: self.new_local(left.ty.clone()), projection: Vec::new() };
                let r_place = Place { local: self.new_local(right.ty.clone()), projection: Vec::new() };
                self.lower_expr(l_place.clone(), *left);
                self.lower_expr(r_place.clone(), *right);

                RValue::Binary(op, Operand::Move(l_place), Operand::Move(r_place))
            }

            t::ExprKind::UnaryOp { op, expr } => {
                let e_place = Place { local: self.new_local(expr.ty.clone()), projection: Vec::new() };
                self.lower_expr(e_place.clone(), *expr);
                RValue::Unary(op, Operand::Move(e_place))
            }

            t::ExprKind::Block { stmts, tail_expr } => {
                self.emit(Instr::Debug(DebugInfo { span: expr.span, kind: DebugKind::EnterScope }));
                for stmt in stmts {
                    self.lower_stmt(stmt);
                }

                self.emit(Instr::Debug(DebugInfo { span: expr.span, kind: DebugKind::ExitScope }));
                
                match tail_expr {
                    Some(expr) => return self.lower_expr(dest, *expr),
                    None => RValue::Use(Operand::Const(Const::Unit))
                }
            }

            t::ExprKind::If { cond, then_branch, else_branch: Some(else_branch) } => {
                let cond_place = Place { local: self.new_local(expr.ty.clone()), projection: Vec::new() };
                self.lower_expr(cond_place.clone(), *cond);
                let then_block = self.new_block();
                let else_block = self.new_block();
                let join_block = self.new_block();
                self.set_terminator(Terminator::Branch(Operand::Move(cond_place), then_block, else_block));

                self.switch_block(then_block);
                self.lower_expr(dest.clone(), *then_branch);
                self.set_terminator(Terminator::Goto(join_block));

                self.switch_block(else_block);
                self.lower_expr(dest, *else_branch);
                self.set_terminator(Terminator::Goto(join_block));

                self.switch_block(join_block);
                return;
            }

            t::ExprKind::If { cond, then_branch, else_branch: None } => {
                let cond_place = Place { local: self.new_local(expr.ty.clone()), projection: Vec::new() };
                self.lower_expr(cond_place.clone(), *cond);
                let then_block = self.new_block();
                let join_block = self.new_block();
                self.set_terminator(Terminator::Branch(Operand::Move(cond_place), then_block, join_block));

                self.switch_block(then_block);
                let fresh = Place { local: self.new_local(then_branch.ty.clone()), projection: Vec::new() };
                self.lower_expr(fresh, *then_branch);
                self.set_terminator(Terminator::Goto(join_block));
                
                self.switch_block(join_block);
                RValue::Use(Operand::Const(Const::Unit))
            }

            t::ExprKind::Error(_) => RValue::Poison,
        };

        self.emit(Instr::Assign(dest, rval));
    }
}

impl MirBuilder<'_> {
    fn new_local(&mut self, ty: Type) -> LocalId {
        let id = LocalId(self.locals.len() as u32);
        self.locals.push(Local { ty });
        id
    }

    fn emit(&mut self, instr: Instr) {
        let block = &mut self.blocks[self.current_block.0 as usize];
        block.instrs.push(instr);
    }

    fn set_terminator(&mut self, terminator: Terminator) {
        let block = &mut self.blocks[self.current_block.0 as usize];
        block.terminator = terminator;
    }

    fn switch_block(&mut self, block: BlockId) {
        assert!((block.0 as usize) < self.blocks.len(), "[Lowerer] Internal error: switched to non-existing block");
        self.current_block = block;
    }

    fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(Block { instrs: vec![], terminator: Terminator::Unreachable });
        id
    }
}


#[cfg(test)]
mod tests {
    use crate::{common::{Span, diagnostics::sinks::AssertErrors}, mir::pretty, semantics, syntax};
    use super::*;

    #[test]
    fn simple_test() {
        let src = "
let x = 1 + 2
let y = x - 3
        ";
        
        let mut diagnostics = AssertErrors;
        let parsed_ast = syntax::parse(&src, &mut diagnostics);
        let (typed_ast, symbols) = semantics::analyze(parsed_ast, &mut diagnostics);
        let prog_ir = lower(typed_ast, &symbols, &mut diagnostics);

        let expected = Body {
            locals: vec![
                Local { ty: Type::Number },
                Local { ty: Type::Number },
                Local { ty: Type::Number },
                Local { ty: Type::Number },
                Local { ty: Type::Number },
                Local { ty: Type::Number },
            ],
            blocks: vec![Block {
                instrs: vec![
                    Instr::Assign(Place { local: LocalId(1), projection: vec![] }, RValue::Use(Operand::Const(Const::Number(1.0)))),
                    Instr::Assign(Place { local: LocalId(2), projection: vec![] }, RValue::Use(Operand::Const(Const::Number(2.0)))),
                    Instr::Assign(Place { local: LocalId(0), projection: vec![] }, RValue::Binary(BinaryOp::Add, 
                        Operand::Move(Place { local: LocalId(1), projection: vec![] }), 
                        Operand::Move(Place { local: LocalId(2), projection: vec![] })
                    )),
                    Instr::Debug(DebugInfo { span: Span::new(1, 15), kind: DebugKind::DeclareLocal { local: LocalId(0), name: "x".to_string() } }),
                    Instr::Assign(Place { local: LocalId(4), projection: vec![] }, RValue::Use(Operand::Copy(Place { local: LocalId(0), projection: vec![] }))),
                    Instr::Assign(Place { local: LocalId(5), projection: vec![] }, RValue::Use(Operand::Const(Const::Number(3.0)))),
                    Instr::Assign(Place { local: LocalId(3), projection: vec![] }, RValue::Binary(BinaryOp::Sub, 
                        Operand::Move(Place { local: LocalId(4), projection: vec![] }), 
                        Operand::Move(Place { local: LocalId(5), projection: vec![] })
                    )),
                    Instr::Debug(DebugInfo { span: Span::new(15, 29), kind: DebugKind::DeclareLocal { local: LocalId(3), name: "y".to_string() } }),
                ],
                terminator: Terminator::Return(Operand::Const(Const::Unit)),
            }]
        };

        assert_eq!(expected, prog_ir, "{}\n\n{}", pretty::format_body(&expected, "expected"), pretty::format_body(&prog_ir, "actual"));
    }
}
