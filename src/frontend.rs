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
    pub rule function() -> (String, Vec<String>, String, Vec<Expr>)
        = [' '|'\t'|'\n']* "fn" blanks() name:identifier() blanks()
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
        / binary_op()

    rule if_else() -> Expr
        = "if" blanks() e:expression() blanks() "{" blanks() "\n"
        then_body:statements() blanks() "}" blanks() "else" blanks() "{" blanks() "\n"
        else_body:statements() blanks() "}"
        { Expr::IfElse(Box::new(e), then_body, else_body) }

    rule while_loop() -> Expr
        = "while" blanks() e:expression() blanks() "{" blanks() "\n"
        loop_body:statements() blanks() "}"
        { Expr::WhileLoop(Box::new(e), loop_body) }

     rule binary_op() -> Expr = precedence!{
        a:@ blanks() "==" blanks() b:(@) { Expr::Eq(Box::new(a), Box::new(b)) }
        a:@ blanks() "!=" blanks() b:(@) { Expr::Ne(Box::new(a), Box::new(b)) }
        a:@ blanks() "<"  blanks() b:(@) { Expr::Lt(Box::new(a), Box::new(b)) }
        a:@ blanks() "<=" blanks() b:(@) { Expr::Le(Box::new(a), Box::new(b)) }
        a:@ blanks() ">"  blanks() b:(@) { Expr::Gt(Box::new(a), Box::new(b)) }
        a:@ blanks() ">=" blanks() b:(@) { Expr::Ge(Box::new(a), Box::new(b)) }
        --
        a:@ blanks() "+" blanks() b:(@) { Expr::Add(Box::new(a), Box::new(b)) }
        a:@ blanks() "-" blanks() b:(@) { Expr::Sub(Box::new(a), Box::new(b)) }
        --
        a:@ blanks() "*" blanks() b:(@) { Expr::Mul(Box::new(a), Box::new(b)) }
        a:@ blanks() "/" blanks() b:(@) { Expr::Div(Box::new(a), Box::new(b)) }
        --
        i:identifier() blanks() "(" args:((blanks() e:expression() blanks() {e}) ** ",") ")" { Expr::Call(i, args) }
        i:identifier() { Expr::Identifier(i) }
        l:literal() { l }
    }

    rule identifier() -> String
        = n:$(['a'..='z'|'A'..='Z'|'_']['a'..='z'|'A'..='Z'|'0'..='9'|'_']*) { n.to_owned() }

    rule literal() -> Expr
        = n:$(['0'..='9']+) { Expr::Literal(n.to_owned()) }
        / "&" i:identifier() { Expr::GlobalDataAddr(i) }

    rule blanks() = [' '|'\t']*
}}
