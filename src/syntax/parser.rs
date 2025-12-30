use crate::common::Span;
use crate::common::diagnostics::*;
use crate::syntax::{
    lexer::*,
    parsed_ast::*,
};

pub fn parse<'src>(src: &'src str, diagnostics: &mut impl DiagnosticSink) -> ParsedAst<'src> {
    let tokens = TokenStream::new(src);
    let mut parser = Parser::new(tokens, diagnostics);
    parser.parse_program()
}

type NudFn<'a, 'd, D> = fn(&mut Parser<'a, 'd, D>, Token) -> Expr<'a>;
type LedFn<'a, 'd, D> = fn(&mut Parser<'a, 'd, D>, Expr<'a>, Token) -> Expr<'a>;

struct Operator<'a, 'd, D: DiagnosticSink> {
    lbp: u8,
    nud: Option<NudFn<'a, 'd, D>>,
    led: Option<LedFn<'a, 'd, D>>,
}
impl<'a, 'd, D: DiagnosticSink> Operator<'a, 'd, D> {
    fn nud_op(nud: NudFn<'a, 'd, D>) -> Self {
        Self { lbp: 0, nud: Some(nud), led: None }
    }

    fn led_op(lbp: u8, led: LedFn<'a, 'd, D>) -> Self {
        Self { lbp, nud: None, led: Some(led) }
    }

    fn not_an_op() -> Self {
        Self { lbp: 0, nud: None, led: None }
    }
}

pub struct Parser<'a, 'd, D: DiagnosticSink> {
    stream: TokenStream<'a>,
    diags: &'d mut D,
}

impl<'a, 'd, D: DiagnosticSink> Parser<'a, 'd, D> {
    pub fn new(stream: TokenStream<'a>, diags: &'d mut D) -> Self {
        Self {
            stream,
            diags,
        }
    }

    pub fn parse_program(&mut self) -> ParsedAst<'a> {
        let mut stmts = Vec::new();

        while !matches!(self.stream.peek_with(LexHint::Any).map(|t| t.kind), Some(TokenKind::EOF) | None) {
            stmts.push(self.parse_statement());
        }

        let span = stmts.first()
            .and_then(|f| stmts.last()
                .map(|l| f.span.concat(&l.span)))
            .unwrap_or(Span::new(0, 0));

        ParsedAst {
            stmts,
            span,
        }
    }
}

// Helpers

impl<'a, 'd, D: DiagnosticSink> Parser<'a, 'd, D> {
    fn next_token(&mut self) -> Token {
        self.stream.next().expect("[Parser] Internal error: Unexpected end of token stream")
    }

    fn peek_token(&mut self) -> Token {
        self.stream.peek_with(LexHint::Any).expect("[Parser] Internal error: Unexpected end of token stream")
    }

    fn hard_expect(&mut self, kind: TokenKind) -> Token {
        let tok = self.next_token();
        assert_eq!(tok.kind, kind, "[Parser] Internal error: Expected token of kind {:?}, got {:?} instead", kind, tok.kind);
        tok
    }
}

// Statements

impl<'a, 'd, D: DiagnosticSink> Parser<'a, 'd, D> {
    fn parse_statement(&mut self) -> Stmt<'a> {
        self.skip_newlines();
        match self.peek_token().kind {
            TokenKind::Let => self.parse_let_stmt(),
            tk if Self::get_op(tk).nud.is_some() => self.parse_expr_stmt(),
            TokenKind::Semicolon => Stmt { kind: StmtKind::Empty, span: self.next_token().span },
            _ => {
                let tok = self.next_token();
                self.diags.emit(
                    Diagnostic::error("invalid start of statement")
                    .with_span(tok.span)
                    .note("expected 'let' or an expression")
                );
                return Stmt { kind: StmtKind::Error, span: tok.span };
            }
        }
    }

    fn parse_let_stmt(&mut self) -> Stmt<'a> {
        let let_kw = self.hard_expect(TokenKind::Let);

        let ident_token = match self.next_token() {
            ident @ Token { kind: TokenKind::Identifier, .. }=> ident,
            other => {
                self.diags.emit(
                    Diagnostic::error("expected an identifier after 'let'")
                    .with_span(other.span)
                    .with_label(Label::secondary(let_kw.span, "'let' starts a variable declaration"))
                );

                self.recover_to_stmt_start();
                return Stmt { kind: StmtKind::Error, span: let_kw.span }
            }
        };

        let name = &self.stream.get_src()[ident_token.span.start..ident_token.span.end];

        match self.next_token() {
            Token { kind: TokenKind::Assign, .. } => {}
            other => {
                self.diags.emit(
                    Diagnostic::error("expected '=' after variable name")
                    .with_span(other.span)
                    .note("a 'let' statement must assign an initial value")
                    .with_label(Label::secondary(ident_token.span, "variable declared here"))
                );
                self.recover_to_stmt_start();
                return Stmt { kind: StmtKind::Error, span: let_kw.span.concat(&ident_token.span) }
            }
        }

        let value = self.parse_expression(0);

        let terminator = self.expect_terminator();

        let span = terminator.map(|t| t.span.concat(&let_kw.span)).unwrap_or(let_kw.span);

        Stmt { kind: StmtKind::Let { name, value }, span }
    }

    fn parse_expr_stmt(&mut self) -> Stmt<'a> {
        let expr = self.parse_expression(0);
        let terminator = self.expect_terminator();

        let span = terminator.map(|t| t.span.concat(&expr.span)).unwrap_or(expr.span);

        Stmt { kind: StmtKind::Expr(expr), span }
    }

    fn expect_terminator(&mut self) -> Option<Token> {
        let save = self.stream.get_position();
        self.skip_newlines();
        match self.peek_token() {
            Token { kind: TokenKind::Semicolon, .. } => self.stream.next(),
            Token { kind: TokenKind::RBrace | TokenKind::EOF, .. } => None,
            other => {
                // skipped at least one newline
                if save != self.stream.get_position() {
                    self.stream.set_position(save);
                    return self.stream.next();
                }

                self.diags.emit(
                    Diagnostic::error("missing statement terminator")
                    .with_span(other.span)
                    .note("statements must end with ';' or a new line")
                );
                None
            }
        }
    }

    // FIXME: This might recover beyond a scope termination.
    fn recover_to_stmt_start(&mut self) {
        let mut save = self.stream.get_position();
        while let Some(tok) = self.stream.next() {
            match tok.kind {
                TokenKind::Let |
                TokenKind::RBrace |
                TokenKind::EOF => {
                    self.stream.set_position(save);
                    break;
                }
                k if Self::get_op(k).nud.is_some() => {
                    self.stream.set_position(save);
                    break;
                }
                _ => save = self.stream.get_position(),
            }
        }
    }
}

// Expressions

impl<'a, 'd, D: DiagnosticSink> Parser<'a, 'd, D> {
    fn parse_expression(&mut self, rbp: u8) -> Expr<'a> {
        let revert_point = self.stream.get_position();
        self.skip_newlines();

        let token = self.next_token();

        let mut lhs = {
            let op = Self::get_op(token.kind);
            let nud = match op.nud {
                Some(nud) => nud,
                None => {
                    self.stream.set_position(revert_point);
                    self.diags.emit(
                        Diagnostic::error("Expected expression")
                        .with_span(token.span)
                        .note("expected a literal, identifier, or '('")
                    );
                    return Expr { kind: ExprKind::Error, span: token.span };
                }
            };

            nud(self, token)
        };

        loop {
            let revert_point = self.stream.get_position();
            self.skip_newlines();

            let next = self.next_token();

            let op = Self::get_op(next.kind);

            if op.lbp <= rbp { self.stream.set_position(revert_point); break; }

            let led = op.led.expect("token has l_bp but no LED");

            lhs = led(self, lhs, next);
        }

        lhs
    }

    fn skip_newlines(&mut self) {
        while matches!(self.stream.peek_with(LexHint::Any), Some(tok) if tok.kind == TokenKind::Newline) {
            self.stream.next();
        }
    }

    fn get_op(kind: TokenKind) -> Operator<'a, 'd, D> {
        use TokenKind::*;
        match kind {
            Identifier  => Operator::nud_op(Self::parse_var),
            Number      => Operator::nud_op(Self::parse_number),
            Plus        => Operator::led_op(2, Self::parse_binary_op),
            Minus       => Operator { lbp: 2, nud: None, led: Some(Self::parse_binary_op) },
            Star        => Operator::led_op(4, Self::parse_binary_op),
            Slash       => Operator::led_op(4, Self::parse_binary_op),
            Assign      => Operator::not_an_op(), // TODO
            Dot         => Operator::not_an_op(), // TODO
            Semicolon   => Operator::not_an_op(),
            LParen      => Operator::nud_op(Self::parse_parathesised),
            RParen      => Operator::not_an_op(),
            LBrace      => Operator::nud_op(Self::parse_block),
            RBrace      => Operator::not_an_op(),
            Let         => Operator::not_an_op(),
            Newline     => Operator::not_an_op(),
            Unknown     => Operator::not_an_op(),
            EOF         => Operator::not_an_op(),
        }
    }

    fn parse_var(&mut self, tok: Token) -> Expr<'a> {
        let text = &self.stream.get_src()[tok.span.start..tok.span.end];

        Expr { kind: ExprKind::Identifier { name: text }, span: tok.span }
    }

    fn parse_number(&mut self, tok: Token) -> Expr<'a> {
        let num_str = &self.stream.get_src()[tok.span.start..tok.span.end];

        Expr { kind: ExprKind::Number { value: num_str.parse().unwrap(), unit: None }, span: tok.span }
    }

    fn parse_binary_op(&mut self, lhs: Expr<'a>, tok: Token) -> Expr<'a> {
        let (op, rbp) = match tok.kind {
            // TokenKind::Assign   => (BinaryOp::Assign, 0),
            TokenKind::Plus     => (BinaryOp::Add, 3),
            TokenKind::Minus    => (BinaryOp::Sub, 3),
            TokenKind::Star     => (BinaryOp::Mul, 5),
            TokenKind::Slash    => (BinaryOp::Div, 5),
            _ => unimplemented!("Unsupported binary operator")
        };

        let rhs = self.parse_expression(rbp);
        let span = lhs.span.concat(&rhs.span);

        Expr { kind: ExprKind::BinaryOp { op, left: Box::new(lhs), right: Box::new(rhs) }, span }
    }

    fn parse_parathesised(&mut self, tok: Token) -> Expr<'a> {
        let expr = self.parse_expression(0);

        let closing_tok = match self.peek_token() {
            token @ Token { kind: TokenKind::RParen, .. } => {
                self.stream.next();
                Some(token)
            }
            other => {
                self.diags.emit(
                    Diagnostic::error("missing closing ')'")
                    .with_span(other.span)
                    .with_label(Label::secondary(tok.span, "this '(' is not closed"))
                );
                None
            }
        };

        let span = closing_tok.map(|t| t.span.concat(&tok.span)).unwrap_or(tok.span);

        Expr { span, ..expr }
    }

    fn parse_block(&mut self, tok: Token) -> Expr<'a> {
        let mut stmts = Vec::new();

        self.skip_newlines();
        while !matches!(self.peek_token().kind, TokenKind::RBrace | TokenKind::EOF) {
            stmts.push(self.parse_statement());
            self.skip_newlines();
        }

        let closing_tok = match self.peek_token() {
            token @ Token { kind: TokenKind::RBrace, .. } => {
                self.stream.next();
                Some(token)
            }
            other => {
                self.diags.emit(
                    Diagnostic::error("missing closing '}'")
                    .with_span(other.span)
                    .with_label(Label::secondary(tok.span, "this '(' is not closed"))
                );
                None
            }
        };

        let span = closing_tok.map(|t| t.span.concat(&tok.span)).unwrap_or(tok.span);

        let tail_expr = match stmts.pop() {
            Some(Stmt { kind: StmtKind::Expr(expr), span }) if expr.span == span => Some(Box::new(expr)),
            Some(other) => { stmts.push(other); None }
            _ => None,
        };

        Expr {
            kind: ExprKind::Block { stmts, tail_expr },
            span,
        }
    }
}

// Tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expression_test() {
        let src = "2 + 2 * (4 - 2)";

        let tokens = TokenStream::new(src);
        let mut diags = sinks::AssertErrors;
        let mut parser = Parser::new(tokens, &mut diags);

        let expected = Expr {
            kind: ExprKind::BinaryOp {
                op: BinaryOp::Add,
                left: Box::new(Expr { kind: ExprKind::Number { value: 2.0, unit: None }, span: Span::new(0, 1) }),
                right: Box::new(Expr {
                    kind: ExprKind::BinaryOp {
                        op: BinaryOp::Mul,
                        left: Box::new(Expr { kind: ExprKind::Number { value: 2.0, unit: None }, span: Span::new(4, 5) }),
                        right: Box::new(Expr {
                            kind: ExprKind::BinaryOp {
                                op: BinaryOp::Sub,
                                left: Box::new(Expr { kind: ExprKind::Number { value: 4.0, unit: None }, span: Span::new(9, 10) }),
                                right: Box::new(Expr { kind: ExprKind::Number { value: 2.0, unit: None }, span: Span::new(13, 14) })
                            },
                            span: Span::new(8, 15)
                        })
                    },
                    span: Span::new(4, 15)
                })
            },
            span: Span::new(0, 15)
        };

        assert_eq!(parser.parse_expression(0), expected);
    }

    #[test]
    fn statement_test() {
        let src = "\r
let x = 0; 2 + 2\r
+ 4\r
let a = x + 2\r
2 +\r
2\r
        ";

        let tokens = TokenStream::new(src);
        let mut diags = sinks::AssertErrors;
        let mut parser = Parser::new(tokens, &mut diags);

        let expected = ParsedAst {
            stmts: vec![
                Stmt {
                    kind: StmtKind::Let {
                        name: "x",
                        value: Expr { kind: ExprKind::Number { value: 0.0, unit: None }, span: Span::new(10, 11) }
                    },
                    span: Span::new(2, 12)
                },
                Stmt {
                    kind: StmtKind::Expr(Expr {
                        kind: ExprKind::BinaryOp {
                            op: BinaryOp::Add,
                            left: Box::new(Expr {
                                kind: ExprKind::BinaryOp {
                                    op: BinaryOp::Add,
                                    left: Box::new(Expr { kind: ExprKind::Number { value: 2.0, unit: None }, span: Span::new(13, 14) }),
                                    right: Box::new(Expr { kind: ExprKind::Number { value: 2.0, unit: None }, span: Span::new(17, 18) }),
                                },
                                span: Span::new(13, 18)
                            }),
                            right: Box::new(Expr { kind: ExprKind::Number { value: 4.0, unit: None }, span: Span::new(22, 23) })
                        },
                        span: Span::new(13, 23),
                    }),
                    span: Span::new(13, 25),
                },
                Stmt {
                    kind: StmtKind::Let {
                        name: "a",
                        value: Expr {
                            kind: ExprKind::BinaryOp {
                                op: BinaryOp::Add,
                                left: Box::new(Expr { kind: ExprKind::Identifier { name: "x" }, span: Span::new(33, 34) }),
                                right: Box::new(Expr { kind: ExprKind::Number { value: 2.0, unit: None }, span: Span::new(37, 38) })
                            },
                            span: Span::new(33, 38)
                        }
                    },
                    span: Span::new(25, 40)
                },
                Stmt {
                    kind: StmtKind::Expr(Expr {
                        kind: ExprKind::BinaryOp {
                            op: BinaryOp::Add,
                            left: Box::new(Expr { kind: ExprKind::Number { value: 2.0, unit: None }, span: Span::new(40, 41) }),
                            right: Box::new(Expr { kind: ExprKind::Number { value: 2.0, unit: None }, span: Span::new(45, 46) })
                        },
                        span: Span::new(40, 46)
                    }),
                    span: Span::new(40, 46)
                }
            ],
            span: Span::new(2, 46),
        };

        assert_eq!(parser.parse_program(), expected);
    }
}
