use std::fmt::{self, Display, Formatter, Write};

type NodeId = usize;

#[derive(Clone)]
struct LocalVar {
    count: usize,
}

impl LocalVar {
    fn next(&mut self) -> &mut Self {
        self.count += 1;
        self
    }
}

impl Codegen for LocalVar {
    fn emit(&self, ctx: &mut Context) -> String {
        format!("_{}", self.count)
    }
}

struct Context {
    buf: String,
    local_var: LocalVar,
    callee: Vec<String>,
}

impl Context {
    fn new() -> Self {
        Self {
            buf: String::new(),
            local_var: LocalVar { count: 0 },
            callee: Vec::new(),
        }
    }
}

trait Codegen {
    fn emit(&self, ctx: &mut Context) -> String;
}

enum Postfix {
    FunCall(Vec<Expr>),
    Index(Box<Expr>),
}

impl Codegen for Postfix {
    fn emit(&self, mut ctx: &mut Context) -> String {
        match self {
            Postfix::FunCall(ve) => {
                let mut buf = String::new();
                write!(&mut buf, "(").unwrap();
                for (i, e) in ve.iter().enumerate() {
                    write!(&mut buf, "{}", e.emit(&mut ctx)).unwrap();
                    if i != ve.len() - 1 {
                        write!(&mut buf, ", ").unwrap();
                    }
                }
                write!(&mut buf, ")").unwrap();
                buf
            }
            Postfix::Index(e) => format!("[{}]", e.emit(&mut ctx)),
        }
    }
}

enum Prefix {
    Neg,
}

impl Codegen for Prefix {
    fn emit(&self, _ctx: &mut Context) -> String {
        match self {
            Neg => "-".to_owned(),
        }
    }
}

enum Binary {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    ShiftLeft,
    ShiftRight,
    BitAnd,
    BitOr,
    BitXor,
    //Range,
    //ArrowLeft,
    //ArrowRight,
}

impl Codegen for Binary {
    fn emit(&self, _ctx: &mut Context) -> String {
        match self {
            Binary::Add => "+".to_owned(),
            Binary::Sub => "-".to_owned(),
            Binary::Mul => "*".to_owned(),
            Binary::Div => "/".to_owned(),
            Binary::Mod => "%".to_owned(),
            Binary::Equal => "==".to_owned(),
            Binary::Greater => ">".to_owned(),
            Binary::Less => "<".to_owned(),
            Binary::GreaterEq => ">=".to_owned(),
            Binary::LessEq => "<=".to_owned(),
            Binary::ShiftLeft => "<<".to_owned(),
            Binary::ShiftRight => ">>".to_owned(),
            Binary::BitAnd => "&".to_owned(),
            Binary::BitOr => "|".to_owned(),
            Binary::BitXor => "^".to_owned(),
        }
    }
}

enum Expr {
    BinOp(Box<Expr>, Binary, Box<Expr>),
    PostOp(Box<Expr>, Postfix),
    PreOp(Box<Expr>, Prefix),
    FunTree(Args, Vec<Expr>),
    Constant(Constant),
    Identifier(String),
    If(Box<Expr>, Vec<Expr>, Vec<Expr>),
    Callee,
}

impl Codegen for Expr {
    fn emit(&self, mut ctx: &mut Context) -> String {
        match self {
            Expr::BinOp(l, op, r) => format!(
                "{} {} {}",
                l.emit(&mut ctx),
                op.emit(&mut ctx),
                r.emit(&mut ctx)
            ),
            Expr::PostOp(e, op) => format!("{}{}", e.emit(&mut ctx), op.emit(&mut ctx)),
            Expr::PreOp(e, op) => format!("{}{}", op.emit(&mut ctx), e.emit(&mut ctx)),
            Expr::FunTree(args, procs) => {
                let mut buf = String::new();
                write!(&mut buf, "[]{}{{\n", args.emit(&mut ctx)).unwrap();
                write!(&mut buf, "}}\n").unwrap();
                buf
            }
            Expr::Constant(c) => c.emit(&mut ctx),
            Expr::Identifier(i) => i.to_owned(),
            Expr::Callee => ctx.callee.last().unwrap().clone(),
            Expr::If(c, t, e) => {
                if t.len() == 1 && e.len() == 1 {
                    format!(
                        "({}?{}:{})",
                        c.emit(&mut ctx),
                        t[0].emit(&mut ctx),
                        e[0].emit(&mut ctx)
                    )
                } else {
                    let mut buf = String::new();
                    // let then_var = ctx.local_var.next();
                    // let else_var = ctx.local_var.next();
                    // write!(&mut ctx.buf, "auto {};", then_var.emit(&mut ctx));
                    // write!(&mut ctx.buf, "auto {};", else_var.emit(&mut ctx));
                    let tf_var = ctx.local_var.next().clone();
                    let tf_var_ident = tf_var.emit(&mut ctx);
                    write!(&mut ctx.buf, "bool {};", tf_var_ident).unwrap();
                    write!(&mut buf, "if ({}) {{\n", c.emit(&mut ctx)).unwrap();
                    write!(&mut buf, "{} = true;", tf_var.emit(&mut ctx)).unwrap();
                    for (i, e) in t.iter().enumerate() {
                        if i == t.len() - 1 {
                            // write!(
                            //     &mut ctx.buf,
                            //     "{} = {}\n",
                            //     then_var.emit(&mut ctx),
                            //     e.emit(&mut ctx)
                            // )
                            // .unwrap();
                            //// for if EXPRESSION
                            write!(&mut buf, "{}\n", e.emit(&mut ctx)).unwrap();
                        } else {
                            write!(&mut buf, "{}\n", e.emit(&mut ctx)).unwrap();
                        }
                    }
                    write!(&mut buf, "}}").unwrap();
                    if e.len() > 0 {
                        write!(&mut buf, " else {{\n").unwrap();
                        for (i, e) in e.iter().enumerate() {
                            if i == t.len() - 1 {
                                // write!(
                                //     &mut ctx.buf,
                                //     "{} = {}\n",
                                //     else_var.emit(&mut ctx),
                                //     e.emit(&mut ctx)
                                // )
                                // .unwrap();
                                //// for if EXPRESSION
                                write!(&mut buf, "{}\n", e.emit(&mut ctx)).unwrap();
                            } else {
                                write!(&mut buf, "{}\n", e.emit(&mut ctx)).unwrap();
                            }
                        }
                        write!(&mut buf, "}}\n").unwrap();
                    }
                    format!("({}?{}:{})", tf_var.emit(&mut ctx), "nullptr", "nullptr")
                }
            }
        }
    }
}

enum TypeDef {
    Void,
    Int32,
    Int64,
    Float32,
    Float64,
    Char,
    String,
    Boolean,
    Array(Box<TypeDef>, usize),
    Vector(Box<TypeDef>),
    Tuple(Vec<TypeDef>),
}

impl Codegen for TypeDef {
    fn emit(&self, mut ctx: &mut Context) -> String {
        match self {
            TypeDef::Void => "void".to_owned(),
            TypeDef::Int32 => "int32_t".to_owned(),
            TypeDef::Int64 => "int64_t".to_owned(),
            TypeDef::Float32 => "float".to_owned(),
            TypeDef::Float64 => "double".to_owned(),
            TypeDef::Char => "char".to_owned(),
            TypeDef::String => "std::string".to_owned(),
            TypeDef::Boolean => "bool".to_owned(),
            TypeDef::Array(t, size) => format!("{}[{}]", t.emit(&mut ctx), size),
            TypeDef::Vector(t) => format!("std::vector< {} >", t.emit(&mut ctx)),
            TypeDef::Tuple(vt) => {
                let mut buf = String::new();
                write!(&mut buf, "std::tuple< ").unwrap();
                for (i, t) in vt.iter().enumerate() {
                    write!(&mut buf, "{}", t.emit(&mut ctx)).unwrap();
                    if i != vt.len() - 1 {
                        write!(&mut buf, ", ").unwrap();
                    }
                }
                write!(&mut buf, " >").unwrap();
                buf
            }
        }
    }
}

struct Args {
    args: Vec<(String, TypeDef)>,
}

impl Codegen for Args {
    fn emit(&self, mut ctx: &mut Context) -> String {
        let mut buf = String::new();
        write!(&mut buf, "(").unwrap();
        for (i, arg) in self.args.iter().enumerate() {
            write!(&mut buf, "{} {}", arg.1.emit(&mut ctx), arg.0).unwrap();
            if i != self.args.len() - 1 {
                write!(&mut buf, ", ").unwrap();
            }
        }
        write!(&mut buf, ")").unwrap();
        buf
    }
}

struct FunDef {
    label: String,
    ret: TypeDef,
    args: Args,
    procs: Vec<Expr>,
}

impl Codegen for FunDef {
    fn emit(&self, mut ctx: &mut Context) -> String {
        let mut buf = String::new();
        write!(
            &mut buf,
            "{} _fn_{}{} {{\n",
            self.ret.emit(&mut ctx),
            self.label,
            self.args.emit(&mut ctx)
        )
        .unwrap();
        ctx.callee.push(self.label.clone());
        for (i, proc) in self.procs.iter().enumerate() {
            if i == self.procs.len() - 1 {
                write!(&mut buf, "return ").unwrap();
            }
            write!(&mut buf, "{};\n", proc.emit(&mut ctx)).unwrap();
        }
        ctx.callee.pop();
        write!(&mut buf, "}}\n").unwrap();
        buf
    }
}

enum Constant {
    Integer(i64),
    Floating(f64),
    String(String),
    Boolean(bool),
    Char(char),
    Array(Vec<Constant>),
}

impl Codegen for Constant {
    fn emit(&self, mut ctx: &mut Context) -> String {
        match self {
            Constant::Integer(v) => v.to_string(),
            Constant::Floating(v) => v.to_string(),
            Constant::String(v) => format!("\"{}\"", v),
            Constant::Boolean(v) => {
                if *v {
                    "true".to_owned()
                } else {
                    "false".to_owned()
                }
            }
            Constant::Char(v) => v.to_string(),
            Constant::Array(v) => {
                let mut buf = String::new();
                write!(&mut buf, "{{").unwrap();
                for (i, val) in v.iter().enumerate() {
                    write!(&mut buf, "{}", val.emit(&mut ctx)).unwrap();
                    if i != v.len() - 1 {
                        write!(&mut buf, ", ").unwrap();
                    }
                }
                write!(&mut buf, "}}").unwrap();
                buf
            }
        }
    }
}

struct Node {
    dependencies: Vec<NodeId>,
    exprs: Vec<Expr>,
}

fn main() {
    let mut ctx = Context::new();
    let fib = FunDef {
        label: "fib".to_owned(),
        ret: TypeDef::Int32,
        args: Args {
            args: vec![("n".to_owned(), TypeDef::Int32)],
        },
        procs: vec![Expr::If(
            Box::new(Expr::BinOp(
                Box::new(Expr::Identifier("n".to_owned())),
                Binary::LessEq,
                Box::new(Expr::Constant(Constant::Integer(1))),
            )),
            vec![Expr::Identifier("n".to_owned())],
            vec![Expr::BinOp(
                Box::new(Expr::PostOp(
                    Box::new(Expr::Callee),
                    Postfix::FunCall(vec![Expr::BinOp(
                        Box::new(Expr::Identifier("n".to_owned())),
                        Binary::Sub,
                        Box::new(Expr::Constant(Constant::Integer(2))),
                    )]),
                )),
                Binary::Add,
                Box::new(Expr::PostOp(
                    Box::new(Expr::Callee),
                    Postfix::FunCall(vec![Expr::BinOp(
                        Box::new(Expr::Identifier("n".to_owned())),
                        Binary::Sub,
                        Box::new(Expr::Constant(Constant::Integer(1))),
                    )]),
                )),
            )],
        )],
    };
    let main = FunDef {
        label: "main".to_owned(),
        ret: TypeDef::Int32,
        args: Args { args: vec![] },
        procs: vec![Expr::PostOp(
            Box::new(Expr::Identifier("fib".to_owned())),
            Postfix::FunCall(vec![Expr::Constant(Constant::Integer(42))]),
        )],
    };
    println!("{}", fib.emit(&mut ctx));
    println!("{}", main.emit(&mut ctx));
}
