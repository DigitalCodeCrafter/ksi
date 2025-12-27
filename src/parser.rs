use crate::lexer::{LexHint, Token, TokenKind, TokenStream};
use crate::parsed_ast::*;
use crate::Span;

type NudFn<'a> = fn(&mut Parser<'a>, Token) -> Expr<'a>;
type LedFn<'a> = fn(&mut Parser<'a>, Expr<'a>, Token) -> Expr<'a>;

struct Operator<'a> {
    lbp: u8,
    nud: Option<NudFn<'a>>,
    led: Option<LedFn<'a>>,
}
impl<'a> Operator<'a> {
    fn nud_op(nud: NudFn<'a>) -> Self {
        Self { lbp: 0, nud: Some(nud), led: None }
    }

    fn led_op(lbp: u8, led: LedFn<'a>) -> Self {
        Self { lbp, nud: None, led: Some(led) }
    }

    fn not_an_op() -> Self {
        Self { lbp: 0, nud: None, led: None }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub span: Span,
    pub msg: String,
}

pub struct Parser<'a> {
    stream: TokenStream<'a>,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(stream: TokenStream<'a>) -> Self {
        Self {
            stream,
            errors: Vec::new(),
        }
    }

    pub fn parse_program(&mut self) -> Ast<'a> {
        let mut stmts = Vec::new();

        while !matches!(self.stream.peek_with(LexHint::Any).map(|t| t.kind), Some(TokenKind::EOF) | None) {
            stmts.push(self.parse_statement());
        }

        let span = stmts.first()
            .and_then(|f| stmts.last()
                .map(|l| f.span.concat(&l.span)))
            .unwrap_or(Span::new(0, 0));

        Ast {
            stmts,
            span,
        }
    }
}

// Helpers

impl<'a> Parser<'a> {
    fn next_token(&mut self) -> Token {
        self.stream.next().expect("[Parser] Internal error: Unexpected end of token stream")
    }

    fn peek_token(&mut self) -> Token {
        self.stream.peek_with(LexHint::Any).expect("[Parser] Internal error: Unexpected end of token stream")
    }

    fn expect(&mut self, kind: TokenKind, msg: String) -> Option<Token> {
        let tok = self.peek_token();
        if tok.kind != kind {
            self.error(tok.span, msg);
            None
        } else {
            self.stream.next()
        }
    }

    fn hard_expect(&mut self, kind: TokenKind) -> Token {
        let tok = self.next_token();
        assert_eq!(tok.kind, kind, "[Parser] Internal error: Expected token of kind {:?}, got {:?} instead", kind, tok.kind);
        tok
    }
}

// Errors

impl<'a> Parser<'a> {
    fn error(&mut self, span: Span, msg: String) {
        self.errors.push(ParseError { span, msg });
    }

    fn recover_to_stmt_start(&mut self) {
        let mut save = self.stream.get_position();
        while let Some(tok) = self.stream.next() {
            if tok.kind == TokenKind::Let || Self::get_op(tok.kind).nud.is_some() {
                self.stream.set_position(save);
                break;
            }
            save = self.stream.get_position();
        }
    }
}

// Statements

impl<'a> Parser<'a> {
    fn parse_statement(&mut self) -> Stmt<'a> {
        self.skip_newlines();
        match self.peek_token().kind {
            TokenKind::Let => self.parse_let_stmt(),
            tk if Self::get_op(tk).nud.is_some() => self.parse_expr_stmt(),
            TokenKind::Semicolon => Stmt { kind: StmtKind::Empty, span: self.next_token().span },
            _ => {
                let tok = self.next_token();
                self.error(tok.span, format!("Expected Let Statement or Expression, found {:?}", tok.kind));
                return Stmt { kind: StmtKind::Error, span: tok.span };
            }
        }
    }

    fn parse_let_stmt(&mut self) -> Stmt<'a> {
        let let_kw = self.hard_expect(TokenKind::Let);

        let ident_token = match self.expect(TokenKind::Identifier, "Expected Identifier".to_string()) {
            Some(ident) => ident,
            None => {
                self.recover_to_stmt_start();
                return Stmt { kind: StmtKind::Error, span: let_kw.span }
            }
        };

        let name = &self.stream.get_src()[ident_token.span.start..ident_token.span.end];

        match self.expect(TokenKind::Assign, "Expected '='".to_string()) {
            None => {
                self.recover_to_stmt_start();
                return Stmt { kind: StmtKind::Error, span: let_kw.span.concat(&ident_token.span) }
            }
            _ => {},
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
        let tok = self.peek_token();
        if !matches!(tok.kind, TokenKind::Newline | TokenKind::EOF | TokenKind::Semicolon ) {
            self.error(tok.span, "Expected new line or ';' terminator".to_string());
            None
        } else {
            self.stream.next()
        }
    }
}

// Expressions

impl<'a> Parser<'a> {
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
                    self.error(token.span, "Expected expression".to_string());
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

    fn get_op(kind: TokenKind) -> Operator<'a> {
        use TokenKind::*;
        match kind {
            Identifier  => Operator::nud_op(Self::parse_var),
            Number      => Operator::nud_op(Self::parse_number),
            Plus        => Operator::led_op(2, Self::parse_binary_op),
            Minus       => Operator { lbp: 2, nud: None, led: Some(Self::parse_binary_op) },
            Star        => Operator::led_op(4, Self::parse_binary_op),
            Slash       => Operator::led_op(4, Self::parse_binary_op),
            Assign      => Operator::not_an_op(),
            LParen      => Operator::nud_op(Self::parse_parathesised),
            RParen      => Operator::not_an_op(),
            Dot         => todo!("Member access"),
            Semicolon   => Operator::not_an_op(),
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
        let closing_tok = self.expect(TokenKind::RParen, "Expected ')'".to_string());

        let span = closing_tok.map(|t| t.span.concat(&tok.span)).unwrap_or(tok.span);

        Expr { span, ..expr }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expression_test() {
        let src = "2 + 2 * (4 - 2)";

        let tokens = TokenStream::new(src);
        let mut parser = Parser::new(tokens);

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
        assert_eq!(parser.errors.len(), 0);
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
        let mut parser = Parser::new(tokens);

        let expected = Ast {
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
                    span: Span::new(40, 48)
                }
            ],
            span: Span::new(2, 48),
        };

        assert_eq!(parser.parse_program(), expected);
        assert_eq!(parser.errors.len(), 0);
    }
}
