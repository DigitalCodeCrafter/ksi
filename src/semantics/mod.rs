mod resolver;
mod resolved_ast;

mod typechecker;
pub mod typed_ast;

pub use typechecker::Type;
pub use resolver::SymbolTable;
pub use resolver::SymbolId;



use crate::common::diagnostics::DiagnosticSink;
use crate::syntax::ParsedAst;

pub fn analyze(parsed_ast: ParsedAst, diagnostics: &mut impl DiagnosticSink) -> (typed_ast::TypedAst, SymbolTable) {
    let (resolved_ast, mut symbols) = resolver::resolve(parsed_ast, diagnostics);
    let typed_ast = typechecker::check(resolved_ast, &mut symbols, diagnostics);
    (typed_ast, symbols)
}
