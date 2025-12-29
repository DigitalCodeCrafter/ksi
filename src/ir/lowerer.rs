use std::collections::HashMap;
use crate::common::diagnostics::*;
use crate::semantics::{Type, SymbolTable, SymbolId, typed_ast as t};
use crate::ir::ir::*;

pub fn lower(typed_ast: t::TypedAst, symbols: &SymbolTable, _diagnostics: &mut impl DiagnosticSink) -> ProgramIR {
    let mut builder = FunctionIRBuilder::new(symbols);
    builder.lower_function(typed_ast);
    let func_ir = builder.finish();
    ProgramIR { functions: vec![func_ir] }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TempId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(u32);

impl TempId {
    pub fn index(&self) -> u32 { self.0 }
}
impl LocalId {
    pub fn index(&self) -> u32 { self.0 }
}
impl BlockId {
    pub fn index(&self) -> u32 { self.0 }
}

pub struct FunctionIRBuilder<'a> {
    locals: Vec<Local>,
    temps: Vec<Temp>,
    blocks: Vec<Block>,

    current_block: BlockId,
    
    symbols: &'a SymbolTable,
    env: HashMap<SymbolId, LocalId>,
}
impl<'a> FunctionIRBuilder<'a> {
    pub fn new(symbols: &'a SymbolTable) -> Self {
        Self {
            locals: Vec::new(),
            temps: Vec::new(),
            blocks: vec![Block {
                instrs: Vec::new(),
                terminator: Terminator::Unreachable
            }],

            current_block: BlockId(0),

            env: HashMap::new(),
            symbols,
        }
    }

    pub fn lower_function(&mut self, ast: t::TypedAst) {
        for stmt in ast.stmts {
            self.lower_stmt(stmt);
        }
    }

    pub fn finish(mut self) -> FunctionIR {
        self.blocks[self.current_block.0 as usize].terminator = Terminator::Return(Value::ConstNumber(0.0));
        FunctionIR {
            blocks: self.blocks,
            locals: self.locals,
            temps: self.temps
        }
    }
}

impl FunctionIRBuilder<'_> {
    fn lower_stmt(&mut self, stmt: t::Stmt) {
        match stmt.kind {
            t::StmtKind::Let { sym, value } => {
                let local = self.new_local(sym);
                let temp = self.lower_expr(value);
                self.emit(Instr::Store {
                    place: Place::Local(local),
                    value: Value::Temp(temp),
                });
            }
            t::StmtKind::Expr(expr) => {
                let _ = self.lower_expr(expr);
            }
            t::StmtKind::Empty => {},
            t::StmtKind::Error => {
                self.emit(Instr::Poision { dst: None });
            }
        }
    }

    fn lower_expr(&mut self, expr: t::Expr) -> TempId {
        match expr.kind {
            t::ExprKind::Number { value, .. } => {
                let dst = self.new_temp(expr.ty);
                self.emit(Instr::LoadConst {
                    dst,
                    value
                });
                dst
            }
            t::ExprKind::Identifier { sym } => {
                let dst = self.new_temp(expr.ty);
                match self.env.get(&sym) {
                    Some(&local) => self.emit(
                        Instr::Load {
                            dst,
                            place: Place::Local(local)
                        }
                    ),
                    None => self.emit(
                        Instr::Poision { dst: Some(dst) }
                    ),
                }
                dst
            }
            t::ExprKind::BinaryOp { op, left, right } => {
                let l_tmp = self.lower_expr(*left);
                let r_tmp = self.lower_expr(*right);
                let dst = self.new_temp(expr.ty);
                self.emit(Instr::Binary {
                    dst,
                    op,
                    lhs: Value::Temp(l_tmp),
                    rhs: Value::Temp(r_tmp)
                });
                dst
            }
            t::ExprKind::Error => {
                let dst = self.new_temp(expr.ty);
                self.emit(Instr::Poision { dst: Some(dst) });
                dst
            }
        }
    }
}

impl FunctionIRBuilder<'_> {
    fn new_temp(&mut self, ty: Type) -> TempId {
        let id = TempId(self.temps.len() as u32);
        self.temps.push(Temp { ty });
        id
    }

    fn new_local(&mut self, sym: SymbolId) -> LocalId {
        let id = LocalId(self.locals.len() as u32);
        let ty = self.symbols.get(sym).ty.clone().expect("[Lowerer] Interal Error: Untyped local symbol");
        self.locals.push(Local { ty });
        self.env.insert(sym, id);
        id
    }

    fn emit(&mut self, instr: Instr) {
        let block = &mut self.blocks[self.current_block.0 as usize];
        block.instrs.push(instr);
    }    
}

/*
#[cfg(test)]
mod tests {
    use crate::{lexer::TokenStream, parser::Parser, resolver::Resolver, typechecker::TypeChecker};
    use super::*;

    #[test]
    fn simple_test() {
        let src = "
let x = 1 + 2
let y = x - 3
        ";
        
        let tokens = TokenStream::new(src);
        let ast = Parser::new(tokens).parse_program();

        let mut resolver = Resolver::new();

        let resolved_ast = resolver.resolve_program(ast);
        let mut symbols = resolver.into_table();

        let typed_ast = TypeChecker::new(&mut symbols).type_check(resolved_ast);

        let mut builder = FunctionIRBuilder::new(&symbols);
        builder.lower_program(typed_ast);
        let ir = builder.finish();

        let expected = FunctionIR {
            locals: vec![
                Local { ty: Type::Number },
                Local { ty: Type::Number }
            ],
            temps: vec![
                Temp { ty: Type::Number },
                Temp { ty: Type::Number },
                Temp { ty: Type::Number },
                Temp { ty: Type::Number },
                Temp { ty: Type::Number },
                Temp { ty: Type::Number },
            ],
            blocks: vec![Block {
                instrs: vec![
                    Instr::LoadConst { dst: TempId(0), value: 1.0 },
                    Instr::LoadConst { dst: TempId(1), value: 2.0 },
                    Instr::Binary { dst: TempId(2), op: BinaryOp::Add, lhs: Value::Temp(TempId(0)), rhs: Value::Temp(TempId(1)) },
                    Instr::Store { place: Place::Local(LocalId(0)), value: Value::Temp(TempId(2)) },

                    Instr::Load { dst: TempId(3), place: Place::Local(LocalId(0)) },
                    Instr::LoadConst { dst: TempId(4), value: 3.0 },
                    Instr::Binary { dst: TempId(5), op: BinaryOp::Sub, lhs: Value::Temp(TempId(3)), rhs: Value::Temp(TempId(4)) },
                    Instr::Store { place: Place::Local(LocalId(1)), value: Value::Temp(TempId(5)) },
                ],
                terminator: Terminator::Return(Value::ConstNumber(0.0))
            }]
        };

        assert_eq!(expected, ir);
    }
}
*/