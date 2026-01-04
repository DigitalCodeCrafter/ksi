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
pub struct LocalId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(u32);

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
                let local = self.new_local(self.symbols.get(sym).ty.clone().expect("[Lowerer] Internal Error: local var symbol has no type annotation"));
                self.env.insert(sym, local);
                self.lower_expr(Place { local, projection: Vec::new() }, value);
            }
            t::StmtKind::Expr(expr) => {
                let unused = Place { local: self.new_local(expr.ty.clone()), projection: Vec::new() };
                self.lower_expr(unused, expr);
            }
            t::StmtKind::Empty => {},
            t::StmtKind::Error => {},
        }
    }

    fn lower_expr(&mut self, dest: Place, expr: t::Expr) {
        let rval = match expr.kind {
            t::ExprKind::Number { value, .. } => RValue::Use(Operand::Const(Const::Number(value))),

            t::ExprKind::Identifier { sym } => match self.env.get(&sym) {
                Some(&local) => RValue::Use(Operand::Copy(Place { local, projection: Vec::new() })),
                None => RValue::Poison,
            },

            t::ExprKind::BinaryOp { op, left, right } => {
                let l_place = Place { local: self.new_local(left.ty.clone()), projection: Vec::new() };
                let r_place = Place { local: self.new_local(right.ty.clone()), projection: Vec::new() };
                self.lower_expr(l_place.clone(), *left);
                self.lower_expr(r_place.clone(), *right);

                RValue::Binary(op, Operand::Move(l_place), Operand::Move(r_place))
            }

            t::ExprKind::Block { stmts, tail_expr } => {
                for stmt in stmts {
                    self.lower_stmt(stmt);
                }

                match tail_expr {
                    Some(expr) => return self.lower_expr(dest, *expr),
                    None => RValue::Use(Operand::Const(Const::Unit)),
                }
            }

            t::ExprKind::Error => RValue::Poison,
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
}


#[cfg(test)]
mod tests {
    use crate::{common::diagnostics::sinks::AssertErrors, mir::pretty, semantics, syntax};
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
                    Instr::Assign(Place { local: LocalId(4), projection: vec![] }, RValue::Use(Operand::Copy(Place { local: LocalId(0), projection: vec![] }))),
                    Instr::Assign(Place { local: LocalId(5), projection: vec![] }, RValue::Use(Operand::Const(Const::Number(3.0)))),
                    Instr::Assign(Place { local: LocalId(3), projection: vec![] }, RValue::Binary(BinaryOp::Sub, 
                        Operand::Move(Place { local: LocalId(4), projection: vec![] }), 
                        Operand::Move(Place { local: LocalId(5), projection: vec![] })
                    )),
                ],
                terminator: Terminator::Return(Operand::Const(Const::Unit)),
            }]
        };

        assert_eq!(expected, prog_ir, "{}\n\n{}", pretty::format_body(&expected, "expected"), pretty::format_body(&prog_ir, "actual"));
    }
}
