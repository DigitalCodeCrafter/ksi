pub mod diagnostics;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,   // inclusive
    pub end: usize,     // exclusive
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
