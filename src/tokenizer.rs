
#[derive(Debug, Clone, Copy)]
pub struct Span {
    start: usize,
    end: usize,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Literals
    Identifier,
    Number,

    // Symbols
    Plus,
    Minus,
    Star,
    Slash,
    Caret,

    // groups
    LParen,
    RParen,

    // Delimiters
    Dot,
    Semicolon,

    // Keywords
    Let,

    // Other
    EOF,
    Unknown,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub struct Tokenizer<'a> {
    char_stream: std::str::Chars<'a>,
    pos: usize,
    current_char: Option<char>,
    eof_omitted: bool,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut t = Tokenizer {
            char_stream: input.chars(),
            pos: 0,
            current_char: None,
            eof_omitted: false,
        };
        t.current_char = t.char_stream.next();
        t
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.current_char = self.char_stream.next();
    }
}

impl Tokenizer<'_> {
    fn is_whitespace(c: char) -> bool {
        c.is_whitespace()
    }

    fn is_digit(c: char) -> bool {
        c.is_ascii_digit()
    }

    fn is_letter(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }
}

impl Tokenizer<'_> {
    fn next_token(&mut self) -> TokenKind {
        while let Some(c) = self.current_char {
            if Self::is_whitespace(c) {
                self.skip_whitespace();
                continue;
            }

            return match c {
                '+' => { self.advance(); TokenKind::Plus }
                '-' => { self.advance(); TokenKind::Minus }
                '*' => { self.advance(); TokenKind::Star }
                '/' => { self.advance(); TokenKind::Slash }
                '^' => { self.advance(); TokenKind::Caret }
                '(' => { self.advance(); TokenKind::LParen }
                ')' => { self.advance(); TokenKind::RParen }
                '.' => { self.advance(); TokenKind::Dot }
                ';' => { self.advance(); TokenKind::Semicolon }

                '0'..='9' => self.number(),
                'a'..='z' | 'A'..='Z' | '_' => self.identifier(),

                _ => { self.advance(); TokenKind::Unknown }
            };
        }

        TokenKind::EOF
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char {
            if !Self::is_whitespace(c) { break; }
            self.advance();
        }
    }

    fn number(&mut self) -> TokenKind {
        while let Some(c) = self.current_char {
            if Self::is_digit(c) || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        TokenKind::Number
    }

    fn identifier(&mut self) -> TokenKind {
        let mut id = String::new();

        while let Some(c) = self.current_char {
            if Self::is_letter(c) || Self::is_digit(c) || c == '_' {
                id.push(c);
                self.advance();
            } else {
                break;
            }
        }

        match id.as_str() {
            "let" => TokenKind::Let,
            _ => TokenKind::Identifier,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof_omitted { return None; }

        self.skip_whitespace();

        let start = self.pos;

        let kind = self.next_token();
        if kind == TokenKind::EOF { self.eof_omitted = true; }

        Some(Token { kind, span: Span { start, end: self.pos } })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_test() {
        let src = "Ident 20 + - * / ^ ().; let!";
        let mut tokenizer = Tokenizer::new(src);
        let expected = [
            Token { kind: TokenKind::Identifier,    span: Span { start:  0, end:  5 } },
            Token { kind: TokenKind::Number,        span: Span { start:  6, end:  8 }},
            Token { kind: TokenKind::Plus,          span: Span { start:  9, end: 10 }},
            Token { kind: TokenKind::Minus,         span: Span { start: 11, end: 12 }},
            Token { kind: TokenKind::Star,          span: Span { start: 13, end: 14 }},
            Token { kind: TokenKind::Slash,         span: Span { start: 15, end: 16 }},
            Token { kind: TokenKind::Caret,         span: Span { start: 17, end: 18 }},
            Token { kind: TokenKind::LParen,        span: Span { start: 19, end: 20 }},
            Token { kind: TokenKind::RParen,        span: Span { start: 20, end: 21 }},
            Token { kind: TokenKind::Dot,           span: Span { start: 21, end: 22 }},
            Token { kind: TokenKind::Semicolon,     span: Span { start: 22, end: 23 }},
            Token { kind: TokenKind::Let,           span: Span { start: 24, end: 27 }},
            Token { kind: TokenKind::Unknown,       span: Span { start: 27, end: 28 }},
            Token { kind: TokenKind::EOF,           span: Span { start: 28, end: 28 }},
        ];

        let mut i = 0;
        while let Some(tok) = tokenizer.next() {
            assert_eq!(expected[i].kind, tok.kind, "expected kind to be {:?}, actual was {:?}", expected[i].kind, tok.kind);
            assert_eq!(expected[i].span.start, tok.span.start, "expected span start to be {}, actual was {}", expected[i].span.start, tok.span.start);
            assert_eq!(expected[i].span.end, tok.span.end, "expected span end to be {}, actual was {}", expected[i].span.end, tok.span.end);
            i += 1;
        }
        assert_eq!(expected.len(), i, "expected {} tokens, got {} instead", expected.len(), i);
    }
}
