pub mod lexer;
pub mod parsed_ast;
pub mod parser;
pub mod interpreter;
pub mod resolved_ast;
pub mod resolver;
pub mod typed_ast;
pub mod typechecker;

/// Span of a Token / AST Node in bytes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,   // inclusive
    end: usize,     // exclusive
}
impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        assert!(end >= start, "Internal Error: tried to create span of negative length");
        Span { start, end }
    }

    pub fn concat(&self, rhs: &Span) -> Span {
        Span { start: self.start.min(rhs.start), end: self.end.max(rhs.end) }
    }
}
