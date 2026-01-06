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

struct Parser<'a, 'd, D: DiagnosticSink> {
    stream: TokenStream<'a>,
    diags: &'d mut D,
}

impl<'a, 'd, D: DiagnosticSink> Parser<'a, 'd, D> {
    fn new(stream: TokenStream<'a>, diags: &'d mut D) -> Self {
        Self {
            stream,
            diags,
        }
    }

    fn parse_program(&mut self) -> ParsedAst<'a> {
        let mut stmts = Vec::new();

        self.skip_newlines();
        while !matches!(self.stream.peek_with(LexHint::Any).map(|t| t.kind), Some(TokenKind::EOF) | None) {
            stmts.push(self.parse_statement());
            self.skip_newlines();
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
        let tok = self.stream.next().expect("[Parser] Internal error: Unexpected end of token stream");
        if tok.kind == TokenKind::Unknown {
            self.diags.emit(
                Diagnostic::error("unknown token")
                .with_span(tok.span)
            );
        };
        tok
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
                let e = self.diags.emit(
                    Diagnostic::error("invalid start of statement")
                    .with_span(tok.span)
                    .note("expected 'let' or an expression")
                );
                return Stmt { kind: StmtKind::Error(e), span: tok.span };
            }
        }
    }

    fn parse_let_stmt(&mut self) -> Stmt<'a> {
        let let_kw = self.hard_expect(TokenKind::Let);

        let ident_token = match self.next_token() {
            ident @ Token { kind: TokenKind::Identifier, .. }=> ident,
            other => {
                let e = self.diags.emit(
                    Diagnostic::error("expected an identifier after 'let'")
                    .with_span(other.span)
                    .with_label(Label::secondary(let_kw.span, "'let' starts a variable declaration"))
                );

                self.recover_to_stmt_start();
                return Stmt { kind: StmtKind::Error(e), span: let_kw.span }
            }
        };

        let name = &self.stream.get_src()[ident_token.span.start..ident_token.span.end];

        match self.next_token() {
            Token { kind: TokenKind::Assign, .. } => {}
            other => {
                let e = self.diags.emit(
                    Diagnostic::error("expected '=' after variable name")
                    .with_span(other.span)
                    .note("a 'let' statement must assign an initial value")
                    .with_label(Label::secondary(ident_token.span, "variable declared here"))
                );
                self.recover_to_stmt_start();
                return Stmt { kind: StmtKind::Error(e), span: let_kw.span.concat(&ident_token.span) }
            }
        }

        let value = self.parse_expression(0);

        let terminator = self.expect_terminator();

        let span = let_kw.span.concat(&terminator.map(|t| t.span).unwrap_or(value.span));

        Stmt { kind: StmtKind::Let { name, value }, span }
    }

    fn parse_expr_stmt(&mut self) -> Stmt<'a> {
        let expr = self.parse_expression(0);
        let terminator = self.expect_terminator();

        let span = expr.span.concat(&terminator.map(|t| t.span).unwrap_or(expr.span));

        Stmt { kind: StmtKind::Expr(expr), span }
    }

    fn expect_terminator(&mut self) -> Option<Token> {
        let save = self.stream.get_position();
        self.skip_newlines();
        match self.peek_token() {
            Token { kind: TokenKind::Semicolon, .. } => self.stream.next(),
            Token { kind: TokenKind::RBrace, .. } => None,
            _ if save != self.stream.get_position() => {
                self.stream.set_position(save);
                self.stream.next()
            }
            Token { kind: TokenKind::EOF, .. } => self.stream.next(),
            other => {
                self.diags.emit(
                    Diagnostic::error("missing statement terminator")
                    .with_span(other.span)
                    .note("statements must end with ';' or a new line")
                );
                None
            }
        }
    }

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

// led power table
// ==, !=       => 2, 3
// <, <=, >, >= => 4, 5
// +, -         => 6, 7
// *, /         => 8, 9

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
                    let e = self.diags.emit(
                        Diagnostic::error("Expected expression")
                        .with_span(token.span)
                        .note("expected a literal, identifier, or '('")
                    );
                    return Expr { kind: ExprKind::Error(e), span: token.span };
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
        use TokenKind as k;
        match kind {
            k::Identifier  => Operator::nud_op(Self::parse_var),
            k::Number      => Operator::nud_op(Self::parse_number),
            k::True        => Operator::nud_op(|_, t| Expr { kind: ExprKind::Literal(Literal::Bool(true)), span: t.span }),
            k::False       => Operator::nud_op(|_, t| Expr { kind: ExprKind::Literal(Literal::Bool(false)), span: t.span }),

            k::Plus        => Operator::led_op(4, Self::parse_binary_op),
            k::Minus       => Operator { lbp: 4, nud: Some(Self::parse_unary_op), led: Some(Self::parse_binary_op) },
            k::Star        => Operator::led_op(8, Self::parse_binary_op),
            k::Slash       => Operator::led_op(8, Self::parse_binary_op),

            k::Gt          => Operator::led_op(4, Self::parse_binary_op),
            k::Lt          => Operator::led_op(4, Self::parse_binary_op),
            k::GtEq        => Operator::led_op(4, Self::parse_binary_op),
            k::LtEq        => Operator::led_op(4, Self::parse_binary_op),
            k::Eq          => Operator::led_op(2, Self::parse_binary_op),
            k::NotEq       => Operator::led_op(2, Self::parse_binary_op),
            // And         => Operator::led_op(0, Self::parse_binary_op),
            // Or          => Operator::led_op(0, Self::parse_binary_op),

            k::Assign      => Operator::not_an_op(), // TODO
            k::Dot         => Operator::not_an_op(), // TODO
            k::Semicolon   => Operator::not_an_op(),
            k::LParen      => Operator::nud_op(Self::parse_parathesised),
            k::RParen      => Operator::not_an_op(),
            k::LBrace      => Operator::nud_op(Self::parse_block),
            k::RBrace      => Operator::not_an_op(),
            k::Let         => Operator::not_an_op(),
            k::Newline     => Operator::not_an_op(),
            k::Unknown     => Operator::not_an_op(),
            k::EOF         => Operator::not_an_op(),
        }
    }

    fn parse_var(&mut self, tok: Token) -> Expr<'a> {
        let text = &self.stream.get_src()[tok.span.start..tok.span.end];

        Expr { kind: ExprKind::Identifier { name: text }, span: tok.span }
    }

    fn parse_number(&mut self, tok: Token) -> Expr<'a> {
        let num_str = &self.stream.get_src()[tok.span.start..tok.span.end];

        Expr { kind: ExprKind::Literal(Literal::Number { value: num_str.parse().unwrap(), unit: None }), span: tok.span }
    }

    fn parse_binary_op(&mut self, lhs: Expr<'a>, tok: Token) -> Expr<'a> {
        let (op, rbp) = match tok.kind {
            // TokenKind::Assign   => (BinaryOp::Assign, 0),
            TokenKind::Plus     => (BinaryOp::Add, 7),
            TokenKind::Minus    => (BinaryOp::Sub, 7),
            TokenKind::Star     => (BinaryOp::Mul, 9),
            TokenKind::Slash    => (BinaryOp::Div, 9),

            TokenKind::Gt       => (BinaryOp::Gt, 5),
            TokenKind::GtEq     => (BinaryOp::Ge, 5),
            TokenKind::Lt       => (BinaryOp::Lt, 5),
            TokenKind::LtEq     => (BinaryOp::Le, 5),
            TokenKind::Eq       => (BinaryOp::Eq, 3),
            TokenKind::NotEq    => (BinaryOp::Ne, 3),

            _ => unimplemented!("Unsupported binary operator")
        };

        let rhs = self.parse_expression(rbp);
        let span = lhs.span.concat(&rhs.span);

        Expr { kind: ExprKind::BinaryOp { op, left: Box::new(lhs), right: Box::new(rhs) }, span }
    }

    fn parse_unary_op(&mut self, tok: Token) -> Expr<'a> {
        let (op, rbp) = match tok.kind {
            TokenKind::Minus    => (UnaryOp::Neg, 10),
            _ => unimplemented!("Unsupported unary operator")
        };

        let expr = self.parse_expression(rbp);
        let span = tok.span.concat(&expr.span);

        Expr { kind: ExprKind::UnaryOp { op, expr: Box::new(expr) }, span }
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

        let span = tok.span.concat(&closing_tok.map(|t| t.span).unwrap_or(expr.span));

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

        let tail_expr = match stmts.last() {
            Some(Stmt { kind: StmtKind::Expr(expr), span }) if expr.span == *span => {
                let Stmt { kind: StmtKind::Expr(expr), .. } = stmts.pop().unwrap() else { unreachable!() };
                Some(Box::new(expr))
            }
            _ => None,
        };

        // yes, I know this is a lot but it makes sense. ( "{" -> last stmt -> tail expr -> "}" )
        let span = tok.span.concat(&closing_tok.map(|t| t.span)
            .or(tail_expr.as_ref().map(|e| e.span))
            .or(stmts.last().map(|s| s.span))
            .unwrap_or(tok.span)
        );

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
                left: Box::new(Expr { kind: ExprKind::Literal(Literal::Number { value: 2.0, unit: None }), span: Span::new(0, 1) }),
                right: Box::new(Expr {
                    kind: ExprKind::BinaryOp {
                        op: BinaryOp::Mul,
                        left: Box::new(Expr { kind: ExprKind::Literal(Literal::Number { value: 2.0, unit: None }), span: Span::new(4, 5) }),
                        right: Box::new(Expr {
                            kind: ExprKind::BinaryOp {
                                op: BinaryOp::Sub,
                                left: Box::new(Expr { kind: ExprKind::Literal(Literal::Number { value: 4.0, unit: None }), span: Span::new(9, 10) }),
                                right: Box::new(Expr { kind: ExprKind::Literal(Literal::Number { value: 2.0, unit: None }), span: Span::new(13, 14) })
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
                        value: Expr { kind: ExprKind::Literal(Literal::Number { value: 0.0, unit: None }), span: Span::new(10, 11) }
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
                                    left: Box::new(Expr { kind: ExprKind::Literal(Literal::Number { value: 2.0, unit: None }), span: Span::new(13, 14) }),
                                    right: Box::new(Expr { kind: ExprKind::Literal(Literal::Number { value: 2.0, unit: None }), span: Span::new(17, 18) }),
                                },
                                span: Span::new(13, 18)
                            }),
                            right: Box::new(Expr { kind: ExprKind::Literal(Literal::Number { value: 4.0, unit: None }), span: Span::new(22, 23) })
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
                                right: Box::new(Expr { kind: ExprKind::Literal(Literal::Number { value: 2.0, unit: None }), span: Span::new(37, 38) })
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
                            left: Box::new(Expr { kind: ExprKind::Literal(Literal::Number { value: 2.0, unit: None }), span: Span::new(40, 41) }),
                            right: Box::new(Expr { kind: ExprKind::Literal(Literal::Number { value: 2.0, unit: None }), span: Span::new(45, 46) })
                        },
                        span: Span::new(40, 46)
                    }),
                    span: Span::new(40, 48)
                }
            ],
            span: Span::new(2, 48),
        };

        assert_eq!(parser.parse_program(), expected);
    }
}
