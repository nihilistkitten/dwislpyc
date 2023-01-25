use crate::check::Ty;
use crate::ir::{Cond, InstBuf, Ir, Reg, SymT};
use crate::Error;

use parsel::{
    ast::{
        Any, Brace, Ident, LeftAssoc, LitBool, LitInt, LitStr, Many, Maybe, Paren, Punctuated,
        RightAssoc, Token,
    },
    FromStr, Parse, ToTokens,
};

mod kw {
    parsel::custom_keyword!(pass);
    parsel::custom_keyword!(print);
    parsel::custom_keyword!(input);
    parsel::custom_keyword!(int);
    parsel::custom_keyword!(def);
    parsel::custom_keyword!(str);
    parsel::custom_keyword!(not);
    parsel::custom_keyword!(and);
    parsel::custom_keyword!(or);
    parsel::custom_keyword!(None);
    parsel::custom_keyword!(bool);
}

/// <prgm> ::= <blck>
#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub struct Prgm {
    pub defns: Any<Defn>,
    pub main: Blck,
}

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub struct Defn {
    pub def: kw::def,
    pub name: Ident,
    pub params: Paren<Punctuated<TypedIdent, Token!(,)>>,
    pub ret: Maybe<ReturnType>,
    pub rule: Nest,
}

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub struct Nest {
    pub block: Brace<Blck>,
}

/// <blck> ::= <stmt> EOLN <stmt> OLN
#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub struct Blck {
    pub stmts: Many<Stmt>,
}

// <stmt> ::= <name> = <expn>
//          | pass
//          | print ( <expn> )
#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub enum Stmt {
    Decl {
        typed_ident: TypedIdent,
        equals: Token!(=),
        expn: Expn,
        end: Token!(;),
    },
    Assgn {
        ident: Ident,
        equals: Token!(=),
        expn: Expn,
        end: Token!(;),
    },
    Updt {
        ident: Ident,
        op: Updt,
        expn: Expn,
        end: Token!(;),
    },
    Pass(kw::pass, Token!(;)),
    Print(kw::print, Paren<Punctuated<Expn, Token!(,)>>, Token!(;)),
    If {
        if_: Token!(if),
        cond: Expn,
        #[parsel(recursive)]
        if_nest: Box<Nest>,

        else_: Token!(else),
        #[parsel(recursive)]
        else_nest: Box<Nest>,
    },
    While {
        while_: Token!(while),
        cond: Expn,
        #[parsel(recursive)]
        nest: Box<Nest>,
    },
    ReturnExpn {
        return_: Token!(return),
        expn: Expn,
        end: Token!(;),
    },
    Return {
        return_: Token!(return),
        end: Token!(;),
    },
    FuncCall {
        name: Ident,
        args: Paren<Punctuated<Expn, Token!(,)>>,
        end: Token!(;),
    },
}

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub enum Updt {
    Plus(Token!(+=)),
    Minus(Token!(-=)),
}

// <expn> ::= <addn>
#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub struct Expn(
    #[allow(clippy::type_complexity)]
    pub  LeftAssoc<
        And,
        LeftAssoc<
            Or,
            RightAssoc<Comp, LeftAssoc<Add, LeftAssoc<Mult, LeftAssoc<Expt, UnExp<Not, Leaf>>>>>,
        >,
    >,
);

pub trait Binop {
    fn check(&self, lhs: Ty, rhs: Ty) -> Result<Ty, Error>;
    fn gen(&self, target: Reg, buf: &mut InstBuf, symt: &mut SymT, lhs: Reg, rhs: Reg);
    // fn eval(&self, lhs: Value, rhs: Value) -> Result<Value, Error>;
}

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub enum Add {
    Plus(Token!(+)),
    Minus(Token!(-)),
}

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub enum Mult {
    Times(Token!(*)),
    Div(Token!(/)),
    Mod(Token!(%)),
}

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub struct Expt(Token!(^));

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub enum Comp {
    Lt(Token!(<)),
    Leq(Token!(<=)),
    Eq(Token!(==)),
}

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub struct And(kw::and);

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub struct Or(kw::or);

impl Binop for Add {
    // fn eval(&self, lhs: Value, rhs: Value) -> Result<Value, Error> {
    //     let left = lhs.expect_int()?;
    //     let right = rhs.expect_int()?;
    //     Ok(match self {
    //         Self::Plus(_) => left + right,
    //         Self::Minus(_) => left - right,
    //     }
    //     .into())
    // }

    fn check(&self, lhs: Ty, rhs: Ty) -> Result<Ty, Error> {
        lhs.expect_int()?;
        rhs.expect_int()?;
        Ok(Ty::Int)
    }

    fn gen(&self, target: Reg, buf: &mut InstBuf, _: &mut SymT, lhs: Reg, rhs: Reg) {
        buf.push(match self {
            Self::Plus(_) => Ir::Add {
                dest: target,
                src1: lhs,
                src2: rhs,
            },
            Self::Minus(_) => Ir::Sub {
                dest: target,
                src1: lhs,
                src2: rhs,
            },
        });
    }
}

impl Binop for Mult {
    // fn eval(&self, lhs: Value, rhs: Value) -> Result<Value, Error> {
    //     let left = lhs.expect_int()?;
    //     let right = rhs.expect_int()?;
    //     Ok(match self {
    //         Self::Times(_) => left * right,
    //         Self::Div(_) => {
    //             if right == 0 {
    //                 return Err("cannot divide by zero");
    //             }
    //             left / right
    //         }
    //         Self::Mod(_) => {
    //             if right == 0 {
    //                 return Err("cannot mod by zero");
    //             }
    //             left % right
    //         }
    //     }
    //     .into())
    // }

    fn check(&self, lhs: Ty, rhs: Ty) -> Result<Ty, Error> {
        lhs.expect_int()?;
        rhs.expect_int()?;
        Ok(Ty::Int)
    }

    fn gen(&self, target: Reg, buf: &mut InstBuf, _: &mut SymT, lhs: Reg, rhs: Reg) {
        match self {
            Self::Times(_) => buf.push(Ir::Mul {
                dest: target,
                src1: lhs,
                src2: rhs,
            }),
            Self::Div(_) | Self::Mod(_) => unimplemented!(),
        }
    }
}

impl Binop for Expt {
    // fn eval(&self, lhs: Value, rhs: Value) -> Result<Value, Error> {
    //     let left = lhs.expect_int()?;
    //     let right = rhs.expect_int()?;
    //     if let Ok(exp) = right.try_into() {
    //         Ok(left.pow(exp).into())
    //     } else {
    //         Err("negative powers are not supported since there are no floats in dwislpy")
    //     }
    // }

    fn check(&self, lhs: Ty, rhs: Ty) -> Result<Ty, Error> {
        lhs.expect_int()?;
        rhs.expect_int()?;
        Ok(Ty::Int)
    }

    fn gen(&self, _: Reg, _: &mut InstBuf, _: &mut SymT, _: Reg, _: Reg) {
        unimplemented!();
    }
}

impl Binop for Comp {
    // fn eval(&self, lhs: Value, rhs: Value) -> Result<Value, Error> {
    //     let left = lhs.expect_int()?;
    //     let right = rhs.expect_int()?;
    //     Ok(match self {
    //         Self::Lt(_) => left < right,
    //         Self::Leq(_) => left <= right,
    //         Self::Eq(_) => left == right,
    //     }
    //     .into())
    // }

    fn check(&self, lhs: Ty, rhs: Ty) -> Result<Ty, Error> {
        lhs.expect_int()?;
        rhs.expect_int()?;
        Ok(Ty::Bool)
    }

    fn gen(&self, target: Reg, buf: &mut InstBuf, symt: &mut SymT, lhs: Reg, rhs: Reg) {
        let cnd = match self {
            Self::Lt(_) => Cond::Lt,
            Self::Leq(_) => Cond::Le,
            Self::Eq(_) => Cond::Eq,
        };

        let lbl_true = symt.new_label();
        let lbl_false = symt.new_label();
        let lbl_done = symt.new_label();

        buf.push(Ir::Bcn {
            cnd,
            src1: lhs,
            src2: rhs,
            lblt: lbl_true,
            lblf: lbl_false,
        });

        buf.push(Ir::Lbl(lbl_true));
        buf.push(Ir::Set {
            dest: target,
            val: 1,
        });
        buf.push(Ir::Jmp(lbl_done));
        buf.push(Ir::Lbl(lbl_false));
        buf.push(Ir::Set {
            dest: target,
            val: 0,
        });
        buf.push(Ir::Lbl(lbl_done));
    }
}

impl Binop for And {
    // fn eval(&self, lhs: Value, rhs: Value) -> Result<Value, Error> {
    //     let left = lhs.expect_bool()?;
    //     let right = rhs.expect_bool()?;
    //     Ok((left && right).into())
    // }

    fn check(&self, lhs: Ty, rhs: Ty) -> Result<Ty, Error> {
        lhs.expect_bool()?;
        rhs.expect_bool()?;
        Ok(Ty::Bool)
    }

    fn gen(&self, target: Reg, buf: &mut InstBuf, _: &mut SymT, lhs: Reg, rhs: Reg) {
        buf.push(Ir::And {
            dest: target,
            src1: lhs,
            src2: rhs,
        });
    }
}

impl Binop for Or {
    // fn eval(&self, lhs: Value, rhs: Value) -> Result<Value, Error> {
    //     let left = lhs.expect_bool()?;
    //     let right = rhs.expect_bool()?;
    //     Ok((left || right).into())
    // }

    fn check(&self, lhs: Ty, rhs: Ty) -> Result<Ty, Error> {
        lhs.expect_bool()?;
        rhs.expect_bool()?;
        Ok(Ty::Bool)
    }

    fn gen(&self, target: Reg, buf: &mut InstBuf, _: &mut SymT, lhs: Reg, rhs: Reg) {
        buf.push(Ir::Or {
            dest: target,
            src1: lhs,
            src2: rhs,
        });
    }
}

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub enum UnExp<U: Unop, C> {
    Op(U, #[parsel(recursive)] Box<Self>),
    Child(C),
}

pub trait Unop {
    fn check(&self, on: Ty) -> Result<Ty, Error>;
    fn gen(&self, target: Reg, buf: &mut InstBuf, symt: &mut SymT, src: Reg);
    // fn eval(&self, on: Value) -> Result<Value, Error>;
}

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub struct Not(kw::not);

impl Unop for Not {
    // fn eval(&self, on: Value) -> Result<Value, Error> {
    //     let on = on.expect_bool()?;
    //     Ok((!on).into())
    // }

    fn check(&self, on: Ty) -> Result<Ty, Error> {
        on.expect_bool()?;
        Ok(Ty::Bool)
    }

    fn gen(&self, target: Reg, buf: &mut InstBuf, symt: &mut SymT, src: Reg) {
        let tmp = symt.new_reg();
        buf.push(Ir::Set { dest: tmp, val: 1 });
        buf.push(Ir::Sub {
            dest: target,
            src1: tmp,
            src2: src,
        });
    }
}

// <leaf> ::= <name> | <nmbr> | input ( <strg> ) | ( <expn> )
// <name> ::= x | count | _special | y0 | camelWalk | snake_slither | ...
// <nmbr> ::= 0 | 1 | 2 | 3 | ...
// <strg> ::= "hello" | "" | "say \"yo!\n\tyo.\"" | ...
#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub enum Leaf {
    Inpt(kw::input, #[parsel(recursive)] Paren<Box<Expn>>),
    Int(kw::int, #[parsel(recursive)] Paren<Box<Expn>>),
    Str(kw::str, #[parsel(recursive)] Paren<Box<Expn>>),
    FuncCall {
        name: Ident,
        #[parsel(recursive)]
        args: Paren<Punctuated<Box<Expn>, Token!(,)>>,
    },
    Nmbr(LitInt),
    Strg(LitStr),
    Bool(LitBool),
    Name(Ident),
    Unit(kw::None),
    Expn(#[parsel(recursive)] Paren<Box<Expn>>),
}

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub struct ReturnType {
    arrow: Token!(->),
    pub ty: Type,
}

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub struct TypedIdent {
    pub ident: Ident,
    colon: Token!(:),
    pub ty: Type,
}

#[derive(PartialEq, Eq, Debug, Parse, ToTokens, FromStr, Clone)]
pub enum Type {
    Int(kw::int),
    Bool(kw::bool),
    Str(kw::str),
    Unit(kw::None),
}
