#![allow(dead_code)]
use std::{collections::HashMap, fmt::Display};

use parsel::{
    ast::{LeftAssoc, RightAssoc},
    syn::Ident,
};

#[allow(clippy::wildcard_imports)]
use crate::ast::*;
use crate::Error;

#[derive(Clone, Copy)]
pub struct Reg(u32);

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

#[derive(Clone, Copy)]
pub struct Lbl(u32);
impl Display for Lbl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "l{}", self.0)
    }
}

type Val = isize;
pub type InstBuf = Vec<Ir>;

pub enum Cond {
    Lt,
    Eq,
    Le,
}

pub enum Ir {
    Set {
        dest: Reg,
        val: Val,
    },
    Mov {
        dest: Reg,
        src: Reg,
    },
    Add {
        dest: Reg,
        src1: Reg,
        src2: Reg,
    },
    Sub {
        dest: Reg,
        src1: Reg,
        src2: Reg,
    },
    Mul {
        dest: Reg,
        src1: Reg,
        src2: Reg,
    },
    And {
        dest: Reg,
        src1: Reg,
        src2: Reg,
    },
    Or {
        dest: Reg,
        src1: Reg,
        src2: Reg,
    },
    Nop,
    Lbl(Lbl),
    Bcn {
        cnd: Cond,
        src1: Reg,
        src2: Reg,
        lblt: Lbl,
        lblf: Lbl,
    },
    Bcz {
        cnd: Cond,
        src: Reg,
        lblt: Lbl,
        lblf: Lbl,
    },
    Jmp(Lbl),
    Enter,
    Rtn(Reg),
    Leave,
    Arg {
        num: usize,
        src: Reg,
    },
    Cll(Lbl),
    Gti(Reg),
    Pti(Reg),
}

impl Display for Ir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Set { dest, val } => write!(f, "SET {dest} {val}"),
            Self::Mov { dest, src } => write!(f, "MOV {dest} {src}"),
            Self::Add { dest, src1, src2 } => write!(f, "ADD {dest} {src1} {src2}"),
            Self::Sub { dest, src1, src2 } => write!(f, "SUB {dest} {src1} {src2}"),
            Self::Mul { dest, src1, src2 } => write!(f, "MUL {dest} {src1} {src2}"),
            Self::And { dest, src1, src2 } => write!(f, "AND {dest} {src1} {src2}"),
            Self::Or { dest, src1, src2 } => write!(f, "OR {dest} {src1} {src2}"),
            Self::Nop => write!(f, "NOP"),
            Self::Lbl(l) => write!(f, "LBL {l}"),
            Self::Bcn {
                cnd,
                src1,
                src2,
                lblt,
                lblf,
            } => match cnd {
                Cond::Lt => write!(f, "BLT {src1} {src2} {lblt} {lblf}"),
                Cond::Eq => write!(f, "BEQ {src1} {src2} {lblt} {lblf}"),
                Cond::Le => write!(f, "BLE {src1} {src2} {lblt} {lblf}"),
            },
            Self::Bcz {
                cnd,
                src,
                lblt,
                lblf,
            } => match cnd {
                Cond::Lt => write!(f, "BLTZ {src} {lblt} {lblf}"),
                Cond::Eq => write!(f, "BEQZ {src} {lblt} {lblf}"),
                Cond::Le => write!(f, "BLEZ {src} {lblt} {lblf}"),
            },
            Self::Jmp(l) => write!(f, "JMP {l}"),
            Self::Enter => write!(f, "ENTER"),
            Self::Rtn(r) => write!(f, "RTN {r}"),
            Self::Leave => write!(f, "LEAVE"),
            Self::Arg { num, src } => write!(f, "ARG {num} {src}"),
            Self::Cll(l) => write!(f, "CLL {l}"),
            Self::Gti(r) => write!(f, "GTI {r}"),
            Self::Pti(r) => write!(f, "PTI {r}"),
        }
    }
}

#[derive(Default)]
pub struct SymT {
    syms: HashMap<Ident, u32>,
    counter: u32,
}

impl SymT {
    pub fn new_reg(&mut self) -> Reg {
        let ret = self.counter;
        self.counter += 1;
        Reg(ret)
    }

    pub fn new_label(&mut self) -> Lbl {
        Lbl(self.new_reg().0)
    }

    fn get(&self, name: &Ident) -> u32 {
        *self
            .syms
            .get(name)
            .expect("reference is live because of typechecking")
    }

    /// Associate the name to a register.
    ///
    /// You can only associate names to registers using this method, to guarantee that we never
    /// have multiple names mapping to the same register.
    fn new_named_reg(&mut self, name: Ident) -> Reg {
        let reg = self.new_reg();
        self.syms.insert(name, reg.0);
        reg
    }

    fn new_named_label(&mut self, name: Ident) -> Lbl {
        Lbl(self.new_named_reg(name).0)
    }
}

impl Prgm {
    pub fn gen(self) -> Result<InstBuf, Error> {
        let mut symt = SymT::default();
        let lbl = symt.new_label();
        let mut buf = InstBuf::default();
        for defn in self.defns {
            defn.gen(&mut symt, &mut buf)?;
        }
        self.main.gen(&mut symt, &mut buf, lbl)?;
        Ok(buf)
    }
}

impl Defn {
    fn gen(self, symt: &mut SymT, buf: &mut InstBuf) -> Result<(), Error> {
        let lbl_def = symt.new_named_label(self.name);
        let lbl_done = symt.new_label();
        buf.push(Ir::Lbl(lbl_def));
        buf.push(Ir::Enter);
        self.rule.gen(symt, buf, lbl_done)?;
        buf.push(Ir::Lbl(lbl_done));
        buf.push(Ir::Leave);
        Ok(())
    }
}

impl Nest {
    fn gen(self, symt: &mut SymT, buf: &mut InstBuf, exit: Lbl) -> Result<(), Error> {
        self.block.into_inner().gen(symt, buf, exit)
    }
}

impl Blck {
    fn gen(self, symt: &mut SymT, buf: &mut InstBuf, exit: Lbl) -> Result<(), Error> {
        for stmt in self.stmts {
            stmt.gen(symt, buf, exit)?;
        }
        Ok(())
    }
}

impl Stmt {
    fn gen(self, symt: &mut SymT, buf: &mut InstBuf, exit: Lbl) -> Result<(), Error> {
        match self {
            Self::Decl {
                typed_ident: TypedIdent { ident, .. },
                expn,
                ..
            }
            | Self::Assgn { ident, expn, .. } => {
                let target = symt.new_named_reg(ident);
                expn.gen(target, symt, buf)?;
            }
            Self::Updt { .. } => {
                unimplemented!();
            }
            Self::Pass(_, _) => buf.push(Ir::Nop),
            Self::Print(_, expns, _) => {
                let tmp = symt.new_reg();
                for expn in expns.into_inner() {
                    expn.gen(tmp, symt, buf)?;
                    buf.push(Ir::Pti(tmp));
                }
            }
            Self::If {
                cond,
                if_nest,
                else_nest,
                ..
            } => {
                let tmp = symt.new_reg();
                let lbl_if = symt.new_label();
                let lbl_else = symt.new_label();
                let lbl_done = symt.new_label();
                cond.gen(tmp, symt, buf)?;
                buf.push(Ir::Bcz {
                    cnd: Cond::Eq,
                    src: tmp,
                    lblt: lbl_else,
                    lblf: lbl_if,
                });
                buf.push(Ir::Lbl(lbl_if));
                if_nest.gen(symt, buf, exit)?;
                buf.push(Ir::Jmp(lbl_done));
                buf.push(Ir::Lbl(lbl_else));
                else_nest.gen(symt, buf, exit)?;
                buf.push(Ir::Lbl(lbl_done));
            }
            Self::While { cond, nest, .. } => {
                let tmp = symt.new_reg();
                let lbl_start = symt.new_label();
                let lbl_body = symt.new_label();
                let lbl_done = symt.new_label();
                buf.push(Ir::Lbl(lbl_start));
                cond.gen(tmp, symt, buf)?;
                buf.push(Ir::Bcz {
                    cnd: Cond::Eq,
                    src: tmp,
                    lblt: lbl_done,
                    lblf: lbl_body,
                });
                buf.push(Ir::Lbl(lbl_body));
                nest.gen(symt, buf, exit)?;
                buf.push(Ir::Jmp(lbl_start));
                buf.push(Ir::Lbl(lbl_done));
            }
            Self::ReturnExpn { expn, .. } => {
                let tmp = symt.new_reg();
                expn.gen(tmp, symt, buf)?;
                buf.push(Ir::Rtn(tmp));
                buf.push(Ir::Jmp(exit));
            }
            Self::Return { .. } => {
                let tmp = symt.new_reg();
                buf.push(Ir::Set { dest: tmp, val: 0 });
                buf.push(Ir::Rtn(tmp));
                buf.push(Ir::Jmp(exit));
            }
            Self::FuncCall { name, args, .. } => {
                let arg_regs = args
                    .into_inner()
                    .into_iter()
                    .map(|a| {
                        let tmp = symt.new_reg();
                        a.gen(tmp, symt, buf)?;
                        Ok(tmp)
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                for (num, src) in arg_regs.into_iter().enumerate() {
                    buf.push(Ir::Arg { num, src });
                }
                buf.push(Ir::Cll(Lbl(symt.get(&name))));
            }
        }
        Ok(())
    }
}

trait ExpnGen {
    fn gen(self, target: Reg, symt: &mut SymT, buf: &mut InstBuf) -> Result<(), Error>;
}

impl ExpnGen for Expn {
    fn gen(self, target: Reg, symt: &mut SymT, buf: &mut InstBuf) -> Result<(), Error> {
        self.0.gen(target, symt, buf)
    }
}

impl<B: Binop, C: ExpnGen> ExpnGen for LeftAssoc<B, C> {
    fn gen(self, target: Reg, symt: &mut SymT, buf: &mut InstBuf) -> Result<(), Error> {
        match self {
            Self::Binary { lhs, op, rhs } => {
                let tmp1 = symt.new_reg();
                let tmp2 = symt.new_reg();
                lhs.gen(tmp1, symt, buf)?;
                rhs.gen(tmp2, symt, buf)?;
                op.gen(target, buf, symt, tmp1, tmp2);
            }
            Self::Rhs(expn) => expn.gen(target, symt, buf)?,
        }
        Ok(())
    }
}

impl<B: Binop, C: ExpnGen> ExpnGen for RightAssoc<B, C> {
    fn gen(self, target: Reg, symt: &mut SymT, buf: &mut InstBuf) -> Result<(), Error> {
        match self {
            Self::Binary { lhs, op, rhs } => {
                let tmp1 = symt.new_reg();
                let tmp2 = symt.new_reg();
                lhs.gen(tmp1, symt, buf)?;
                rhs.gen(tmp2, symt, buf)?;
                op.gen(target, buf, symt, tmp1, tmp2);
            }
            Self::Lhs(expn) => expn.gen(target, symt, buf)?,
        }
        Ok(())
    }
}

impl<U: Unop, C: ExpnGen> ExpnGen for UnExp<U, C> {
    fn gen(self, target: Reg, symt: &mut SymT, buf: &mut InstBuf) -> Result<(), Error> {
        match self {
            Self::Op(op, child) => {
                let tmp = symt.new_reg();
                child.gen(tmp, symt, buf)?;
                op.gen(target, buf, symt, tmp);
            }
            Self::Child(expn) => expn.gen(target, symt, buf)?,
        }
        Ok(())
    }
}

impl ExpnGen for Leaf {
    fn gen(self, target: Reg, symt: &mut SymT, buf: &mut InstBuf) -> Result<(), Error> {
        match self {
            Self::Str(_, _) | Self::Strg(_) | Self::Unit(_) => {
                return Err(Error::UnsupportedConstruct)
            }
            Self::Inpt(_, e) => {
                let tmp = symt.new_reg();
                e.into_inner().gen(tmp, symt, buf)?;
                buf.push(Ir::Pti(tmp));
                buf.push(Ir::Gti(target));
            }
            Self::Int(_, e) | Self::Expn(e) => e.into_inner().gen(target, symt, buf)?,
            Self::FuncCall { name, args } => {
                let arg_regs = args
                    .into_inner()
                    .into_iter()
                    .map(|a| {
                        let tmp = symt.new_reg();
                        a.gen(tmp, symt, buf)?;
                        Ok(tmp)
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                for (num, src) in arg_regs.into_iter().enumerate() {
                    buf.push(Ir::Arg { num, src });
                }
                buf.push(Ir::Cll(Lbl(symt.get(&name))));
                buf.push(Ir::Rtn(target));
            }
            Self::Nmbr(n) => buf.push(Ir::Set {
                dest: target,
                val: n.into_inner() as isize,
            }),
            Self::Bool(b) => {
                let val = isize::from(b.into_inner());
                buf.push(Ir::Set { dest: target, val });
            }
            Self::Name(n) => buf.push(Ir::Mov {
                dest: target,
                src: Reg(symt.get(&n)),
            }),
        }
        Ok(())
    }
}
