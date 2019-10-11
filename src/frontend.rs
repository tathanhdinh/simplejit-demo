/// The AST node for expressions.
pub enum Expr {
    Literal(String),
    Identifier(String),
    Assign(String, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    IfElse(Box<Expr>, Vec<Expr>, Vec<Expr>),
    WhileLoop(Box<Expr>, Vec<Expr>),
    Call(String, Vec<Expr>),
    GlobalDataAddr(String),
}

peg::parser!{pub grammar parser() for str {
    use super::Expr;

    pub rule function() -> (String, Vec<String>, String, Vec<Expr>)
        = [' ' | '\t' | '\n']* "fn" blanks() name:identifier() blanks()
        "(" params:((blanks() i:identifier() blanks() {i}) ** ",") ")" blanks()
        "->" blanks()
        "(" returns:(blanks() i:identifier() blanks() {i}) ")" blanks()
        "{" blanks() "\n"
        stmts:statements()
        blanks() "}" blanks() "\n" blanks()
        { (name, params, returns, stmts) }

    rule statements() -> Vec<Expr>
        = s:(statement()*) { s }

    rule statement() -> Expr
        = blanks() e:expression() blanks() "\n" { e }

    rule expression() -> Expr
        = if_else()
        / while_loop()
        / i:identifier() blanks() "=" blanks() e:expression() { Expr::Assign(i, Box::new(e)) }
        / compare()

    rule if_else() -> Expr
        = "if" blanks() e:expression() blanks() "{" blanks() "\n"
        then_body:statements() blanks() "}" blanks() "else" blanks() "{" blanks() "\n"
        else_body:statements() blanks() "}"
        { Expr::IfElse(Box::new(e), then_body, else_body) }

    rule while_loop() -> Expr
        = "while" blanks() e:expression() blanks() "{" blanks() "\n"
        loop_body:statements() blanks() "}"
        { Expr::WhileLoop(Box::new(e), loop_body) }

    rule compare() -> Expr
        = a:sum() blanks() "==" blanks() b:compare() { Expr::Eq(Box::new(a), Box::new(b)) }
        / a:sum() blanks() "!=" blanks() b:compare() { Expr::Ne(Box::new(a), Box::new(b)) }
        / a:sum() blanks() "<"  blanks() b:compare() { Expr::Lt(Box::new(a), Box::new(b)) }
        / a:sum() blanks() "<=" blanks() b:compare() { Expr::Le(Box::new(a), Box::new(b)) }
        / a:sum() blanks() ">"  blanks() b:compare() { Expr::Gt(Box::new(a), Box::new(b)) }
        / a:sum() blanks() ">=" blanks() b:compare() { Expr::Ge(Box::new(a), Box::new(b)) }
        / sum()

    rule sum() -> Expr
        = a:product() blanks() "+" blanks() b:sum() { Expr::Add(Box::new(a), Box::new(b)) }
        / a:product() blanks() "-" blanks() b:sum() { Expr::Sub(Box::new(a), Box::new(b)) }
        / product()

    rule product() -> Expr
        = a:call_or_identifier_or_literal() blanks() "*" blanks() b:product() { Expr::Mul(Box::new(a), Box::new(b)) }
        / a:call_or_identifier_or_literal() blanks() "/" blanks() b:product() { Expr::Div(Box::new(a), Box::new(b)) }
        / call_or_identifier_or_literal()

    rule call_or_identifier_or_literal() -> Expr
        = i:identifier() blanks() "(" args:((blanks() e:expression() blanks() {e}) ** ",") ")" { Expr::Call(i, args) }
        / i:identifier() { Expr::Identifier(i) }
        / literal()

    rule identifier() -> String
        = n:$(['a'..='z'|'A'..='Z'|'_']['a'..='z'|'A'..='Z'|'0'..='9'|'_']*) { n.to_owned() }

    rule literal() -> Expr
        = n:$(['0'..='9']+) { Expr::Literal(n.to_owned()) }
        / "&" i:identifier() { Expr::GlobalDataAddr(i) }

    rule blanks() = [' '|'\t']*
}}
