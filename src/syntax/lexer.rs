use crate::common::Span;

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
    Assign,
    Dot,
    Semicolon,

    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Let,

    // Other
    Newline,
    EOF,
    Unknown,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum LexHint {
    Any,        // default
    // DigitsOnly, // just digits, no '.' or 'e'
}

// Lexing

struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
        }
    }

    fn advance_char(&mut self) -> Option<char> {
        let c = self.peek_char()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    fn peek_char(&self) -> Option<char> {
        if self.pos >= self.input.len() { return None; }
        self.input[self.pos..].chars().next()
    }
}

impl Lexer<'_> {
    fn is_ident_start(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_ident_continue(c: char) -> bool {
        Self::is_ident_start(c) || c.is_ascii_digit()
    }
}

impl Lexer<'_> {
    fn next_token(&mut self, hint: LexHint) -> Token {
       self.skip_whitespace();

        let Some(c) = self.peek_char() else {
            return self.emit_eof();
        };

        match c {
            '+' => return self.single(TokenKind::Plus),
            '-' => return self.single(TokenKind::Minus),
            '*' => return self.single(TokenKind::Star),
            '/' => return self.single(TokenKind::Slash),
            '=' => return self.single(TokenKind::Assign),
            '.' => return self.single(TokenKind::Dot),
            ';' => return self.single(TokenKind::Semicolon),
            '(' => return self.single(TokenKind::LParen),
            ')' => return self.single(TokenKind::RParen),
            '{' => return self.single(TokenKind::LBrace),
            '}' => return self.single(TokenKind::RBrace),
            '\n' => return self.single(TokenKind::Newline),

            _ => {}
        }

        if c.is_ascii_digit() {
            return self.lex_number(hint);
        }

        if Self::is_ident_start(c) {
            return self.lex_identifier();
        }
        
        // unknown fallback
        self.single(TokenKind::Unknown)
    }

    fn emit_eof(&mut self) -> Token {
        Token { kind: TokenKind::EOF, span: Span { start: self.pos, end: self.pos } }
    }

    fn single(&mut self, kind: TokenKind) -> Token {
        let start = self.pos;
        self.advance_char();
        Token { kind, span: Span { start, end: self.pos } }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek_char(), Some(c) if c.is_whitespace() && c != '\n') {
            self.advance_char();
        }
    }

    fn lex_number(&mut self, hint: LexHint) -> Token {
        let start = self.pos;

        while matches!(self.peek_char(), Some(c) if c.is_ascii_digit()) {
            self.advance_char();
        }

        if matches!(hint, LexHint::Any) {
            if self.peek_char() == Some('.') {
                let save = self.pos;
                self.advance_char();

                if matches!(self.peek_char(), Some(c) if c.is_ascii_digit()) {
                    while matches!(self.peek_char(), Some(c) if c.is_ascii_digit()) {
                        self.advance_char();
                    }
                } else {
                    self.pos = save;
                }
            }
        }

        if matches!(hint, LexHint::Any) {
            if matches!(self.peek_char(), Some('e' | 'E')) {
                let save = self.pos;
                self.advance_char();

                if matches!(self.peek_char(), Some('+' | '-')) {
                    self.advance_char();
                }

                if matches!(self.peek_char(), Some(c) if c.is_ascii_digit()) {
                    while matches!(self.peek_char(), Some(c) if c.is_ascii_digit()) {
                        self.advance_char();
                    }
                } else {
                    self.pos = save;
                }
            }
        }

        Token { kind: TokenKind::Number, span: Span { start, end: self.pos } }
    }

    fn lex_identifier(&mut self) -> Token {
        let start = self.pos;

        while matches!(self.peek_char(), Some(c) if Self::is_ident_continue(c)) {
            self.advance_char();
        }

        let span = Span { start, end: self.pos };
        let text = &self.input[span.start..span.end];

        let kind = match text {
            "let" => TokenKind::Let,
            _ => TokenKind::Identifier,
        };

        Token { kind, span }
    }
}

// Streaming

#[derive(Debug, Clone, Copy)]
pub struct LexerPosition(usize);

pub struct TokenStream<'a> {
    lexer: Lexer<'a>,
    done: bool,

    peek_info: Option<(LexHint, Token)>,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { lexer: Lexer::new(input), done: false, peek_info: None }
    }

    pub fn get_src(&self) -> &'a str {
        self.lexer.input
    }

    pub fn get_position(&self) -> LexerPosition {
        LexerPosition(self.lexer.pos)
    }

    pub fn set_position(&mut self, pos: LexerPosition) {
        self.lexer.pos = pos.0;
        self.done = false;
    }

    pub fn next(&mut self) -> Option<Token> {
        self.next_with(LexHint::Any)
    }

    pub fn next_with(&mut self, hint: LexHint) -> Option<Token> {
        if self.done { return None; }

        let tok = self.lexer.next_token(hint);

        if tok.kind == TokenKind::EOF { self.done = true; }
        Some(tok)
    }

    pub fn peek_with(&mut self, hint: LexHint) -> Option<Token> {
        let save = self.lexer.pos;
        
        let tok = self.lexer.next_token(hint);

        self.peek_info = Some((hint, tok));
        self.lexer.pos = save;

        Some(tok)
    }
}

// Tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_test() {
        let src = "Ident 20 + - * /   ().; let!ä";
        let mut tokens = TokenStream::new(src);
        let expected = [
            Token { kind: TokenKind::Identifier,    span: Span { start:  0, end:  5 } },
            Token { kind: TokenKind::Number,        span: Span { start:  6, end:  8 }},
            Token { kind: TokenKind::Plus,          span: Span { start:  9, end: 10 }},
            Token { kind: TokenKind::Minus,         span: Span { start: 11, end: 12 }},
            Token { kind: TokenKind::Star,          span: Span { start: 13, end: 14 }},
            Token { kind: TokenKind::Slash,         span: Span { start: 15, end: 16 }},
            Token { kind: TokenKind::LParen,        span: Span { start: 19, end: 20 }},
            Token { kind: TokenKind::RParen,        span: Span { start: 20, end: 21 }},
            Token { kind: TokenKind::Dot,           span: Span { start: 21, end: 22 }},
            Token { kind: TokenKind::Semicolon,     span: Span { start: 22, end: 23 }},
            Token { kind: TokenKind::Let,           span: Span { start: 24, end: 27 }},
            Token { kind: TokenKind::Unknown,       span: Span { start: 27, end: 28 }},
            Token { kind: TokenKind::Identifier,    span: Span { start: 28, end: 30 }},
            Token { kind: TokenKind::EOF,           span: Span { start: 30, end: 30 }},
        ];

        let mut i = 0;
        while let Some(tok) = tokens.next() {
            assert_eq!(expected[i].kind, tok.kind, "expected kind to be {:?}, actual was {:?}", expected[i].kind, tok.kind);
            assert_eq!(expected[i].span.start, tok.span.start, "expected span start to be {}, actual was {}", expected[i].span.start, tok.span.start);
            assert_eq!(expected[i].span.end, tok.span.end, "expected span end to be {}, actual was {}", expected[i].span.end, tok.span.end);
            i += 1;
        }
        assert_eq!(expected.len(), i, "expected {} tokens, got {} instead", expected.len(), i);
    }
}
