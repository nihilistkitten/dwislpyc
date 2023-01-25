use std::collections::HashMap;
use std::fmt::Display;

use parsel::ast::{LeftAssoc, RightAssoc};
use parsel::syn::Ident;
use parsel::Spanned;

#[allow(clippy::wildcard_imports)]
use crate::ast::*;
use crate::Error;

trait Check {
    type Info;

    fn check(&mut self, defs: &mut DefTypes, syms: &mut SymTab) -> Result<Self::Info, Error>;
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Ty {
    Int,
    Bool,
    Str,
    Unit,
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Int => "int",
                Self::Bool => "bool",
                Self::Str => "str",
                Self::Unit => "unit",
            }
        )
    }
}

impl From<&Type> for Ty {
    fn from(ty: &Type) -> Self {
        match ty {
            Type::Int(_) => Self::Int,
            Type::Bool(_) => Self::Bool,
            Type::Str(_) => Self::Str,
            Type::Unit(_) => Self::Unit,
        }
    }
}

impl Ty {
    fn expect(self, ty: Self) -> Result<(), Error> {
        if ty == self {
            Ok(())
        } else {
            Err(Error::TypeError {
                expected: ty,
                got: self,
            })
        }
    }
    pub fn expect_str(self) -> Result<(), Error> {
        self.expect(Self::Str)
    }

    pub fn expect_int(self) -> Result<(), Error> {
        self.expect(Self::Int)
    }

    pub fn expect_bool(self) -> Result<(), Error> {
        self.expect(Self::Bool)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Rtns {
    Fallthrough,
    MightReturn(Ty),
    Returns(Ty),
}

impl Rtns {
    /// Determine which type to return if one branch returns self and the other returns other
    fn reconcile(self, other: Self) -> Result<Self, Error> {
        Ok(match (self, other) {
            (Self::Fallthrough, Self::Fallthrough) => Self::Fallthrough,
            (Self::MightReturn(t), Self::MightReturn(q)) if t == q => Self::MightReturn(t),
            (Self::Returns(t), Self::Returns(q)) if t == q => Self::Returns(t),
            (Self::Returns(t) | Self::MightReturn(t), Self::Returns(q) | Self::MightReturn(q))
                if t == q =>
            {
                Self::MightReturn(t)
            }
            (Self::Returns(t) | Self::MightReturn(t), Self::MightReturn(q) | Self::Returns(q)) => {
                return Err(Error::ReturnTypeMismatch {
                    first: t,
                    second: q,
                });
            }
            (Self::Fallthrough, Self::MightReturn(t) | Self::Returns(t))
            | (Self::Returns(t) | Self::MightReturn(t), Self::Fallthrough) => Self::MightReturn(t),
        })
    }
}

#[derive(Clone)]
pub struct ArrowType {
    pub return_type: Ty, // Unit if no return
    pub params: Vec<Ty>,
}

impl ArrowType {
    fn check_call<'a, I>(
        &self,
        args: I,
        defs: &mut DefTypes,
        syms: &mut SymTab,
    ) -> Result<(), Error>
    where
        I: ExactSizeIterator<Item = &'a mut Expn>,
    {
        if args.len() != self.params.len() {
            return Err(Error::ArgumentLen {
                expected: self.params.len(),
                got: args.len(),
            });
        }
        for (arg, expected) in args.zip(&self.params) {
            let got = arg.check(defs, syms)?;
            got.expect(*expected)?;
        }
        Ok(())
    }
}

pub struct DefTypes(HashMap<Ident, ArrowType>);

impl DefTypes {
    fn get(&mut self, name: &Ident) -> Option<ArrowType> {
        self.0.get(name).cloned()
    }

    fn get_or(&mut self, name: &Ident) -> Result<ArrowType, Error> {
        self.get(name)
            .ok_or_else(|| Error::UndefinedVariable(name.to_string()))
    }

    fn set(&mut self, name: Ident, val: ArrowType) {
        self.0.insert(name, val);
    }
}

pub struct SymTab {
    table: HashMap<Ident, Ty>,
}

impl SymTab {
    fn get(&mut self, name: &Ident) -> Option<Ty> {
        self.table.get(name).copied()
    }

    fn get_or(&mut self, name: &Ident) -> Result<Ty, Error> {
        self.get(name)
            .ok_or_else(|| Error::UndefinedVariable(name.to_string()))
    }

    fn set(&mut self, name: Ident, val: impl Into<Ty>) {
        self.table.insert(name, val.into());
    }
}

impl Check for Prgm {
    type Info = ();

    fn check(&mut self, defs: &mut DefTypes, syms: &mut SymTab) -> Result<Self::Info, Error> {
        for def in &mut self.defns {
            def.check(defs, syms)?;
        }

        if self.main.check(defs, syms)? == Rtns::Fallthrough {
            Ok(())
        } else {
            Err(Error::ReturnFromMain)
        }
    }
}

impl Check for Defn {
    type Info = ();

    fn check(&mut self, defs: &mut DefTypes, syms: &mut SymTab) -> Result<Self::Info, Error> {
        let got = match self.rule.check(defs, syms)? {
            Rtns::Fallthrough => Ty::Unit,
            Rtns::MightReturn(_) => {
                return Err(Error::FunctionIndefiniteReturn);
            }
            Rtns::Returns(ty) => ty,
        };

        #[allow(clippy::option_if_let_else)]
        let expected = match &*self.ret {
            Some(ret) => (&ret.0.ty).into(),
            None => Ty::Unit,
        };

        if got != expected {
            return Err(Error::TypeError { expected, got });
        }

        let params = self.params.iter_mut().map(|p| (&p.ty).into()).collect();
        let arrow = ArrowType {
            params,
            return_type: got,
        };

        defs.set(self.name.clone(), arrow);

        Ok(())
    }
}

impl Check for Nest {
    type Info = Rtns;

    fn check(&mut self, defs: &mut DefTypes, syms: &mut SymTab) -> Result<Self::Info, Error> {
        self.block.check(defs, syms)
    }
}

impl Check for Blck {
    type Info = Rtns;

    fn check(&mut self, defs: &mut DefTypes, syms: &mut SymTab) -> Result<Self::Info, Error> {
        let mut rtns = Rtns::Fallthrough;
        for stmt in &mut self.stmts {
            if let Rtns::Returns(_) = rtns {
                // already returned, but we have more code
                return Err(Error::DeadCode);
            }
            rtns = rtns.reconcile(stmt.check(defs, syms)?)?;
        }
        Ok(rtns)
    }
}

impl Check for Stmt {
    type Info = Rtns;

    fn check(&mut self, defs: &mut DefTypes, syms: &mut SymTab) -> Result<Self::Info, Error> {
        Ok(match self {
            Self::Decl {
                typed_ident, expn, ..
            } => {
                let expected: Ty = (&typed_ident.ty).into();
                let got = expn.check(defs, syms)?;
                if expected == got {
                    syms.set(typed_ident.ident.clone(), got);
                    Rtns::Fallthrough
                } else {
                    return Err(Error::TypeError { expected, got });
                }
            }
            Self::Assgn { ident, expn, .. } => {
                let expected = syms.get_or(ident)?;
                let got = expn.check(defs, syms)?;
                if expected == got {
                    Rtns::Fallthrough
                } else {
                    return Err(Error::TypeError { expected, got });
                }
            }
            Self::Updt { ident, expn, .. } => {
                syms.get_or(ident)?.expect_int()?;
                expn.check(defs, syms)?.expect_int()?;
                Rtns::Fallthrough
            }
            Self::Pass(_, _) => Rtns::Fallthrough,
            Self::Print(_, args, _) => {
                for arg in args.iter_mut() {
                    arg.check(defs, syms)?;
                }
                Rtns::Fallthrough
            }
            Self::If {
                cond,
                if_nest,
                else_nest,
                ..
            } => {
                cond.check(defs, syms)?.expect_bool()?;
                let if_ret = if_nest.check(defs, syms)?;
                let else_ret = else_nest.check(defs, syms)?;
                if_ret.reconcile(else_ret)?
            }
            Self::While { cond, nest, .. } => {
                cond.check(defs, syms)?.expect_bool()?;
                nest.check(defs, syms)?.reconcile(Rtns::Fallthrough)?
            }
            Self::ReturnExpn { expn, .. } => Rtns::Returns(expn.check(defs, syms)?),
            Self::Return { .. } => Rtns::Returns(Ty::Unit),
            Self::FuncCall { name, args, .. } => {
                let arrow = defs.get_or(name)?;
                arrow.check_call(args.iter_mut(), defs, syms)?;
                Rtns::Fallthrough
            }
        })
    }
}

impl Check for Expn {
    type Info = Ty;

    fn check(&mut self, defs: &mut DefTypes, syms: &mut SymTab) -> Result<Self::Info, Error> {
        self.0.check(defs, syms)
    }
}

impl<B: Binop, C: Check<Info = Ty>> Check for LeftAssoc<B, C>
where
    Self: Spanned,
{
    type Info = Ty;

    fn check(&mut self, defs: &mut DefTypes, syms: &mut SymTab) -> Result<Self::Info, Error> {
        match self {
            Self::Binary { lhs, op, rhs } => {
                let lhs = lhs.check(defs, syms)?;
                let rhs = rhs.check(defs, syms)?;
                op.check(lhs, rhs)
            }
            Self::Rhs(expn) => expn.check(defs, syms),
        }
    }
}

impl<B: Binop, C: Check<Info = Ty>> Check for RightAssoc<B, C>
where
    Self: Spanned,
{
    type Info = Ty;

    fn check(&mut self, defs: &mut DefTypes, syms: &mut SymTab) -> Result<Self::Info, Error> {
        match self {
            Self::Binary { lhs, op, rhs } => {
                let rhs = rhs.check(defs, syms)?;
                let lhs = lhs.check(defs, syms)?;
                op.check(lhs, rhs)
            }
            Self::Lhs(expn) => expn.check(defs, syms),
        }
    }
}

impl<U: Unop, C: Check<Info = Ty> + parsel::ToTokens> Check for UnExp<U, C>
where
    Self: Spanned,
{
    type Info = Ty;

    fn check(&mut self, defs: &mut DefTypes, syms: &mut SymTab) -> Result<Self::Info, Error> {
        match self {
            Self::Op(op, child) => op.check(child.check(defs, syms)?),
            Self::Child(expn) => expn.check(defs, syms),
        }
    }
}

impl Check for Leaf {
    type Info = Ty;

    fn check(&mut self, defs: &mut DefTypes, syms: &mut SymTab) -> Result<Self::Info, Error> {
        Ok(match self {
            Self::Inpt(_, e) => {
                e.check(defs, syms)?.expect_str()?;
                Ty::Str
            }
            Self::Int(_, e) => {
                let got = e.check(defs, syms)?;
                if matches!(got, Ty::Int | Ty::Bool) {
                    // can convert to int
                    Ty::Int
                } else {
                    return Err(Error::TypeError {
                        expected: Ty::Bool,
                        got,
                    });
                }
            }
            Self::Str(_, e) => {
                // can convert anything to a str
                e.check(defs, syms)?;
                Ty::Str
            }
            Self::FuncCall { name, args } => {
                let arrow = defs.get_or(name)?;
                arrow.check_call(args.iter_mut().map(Box::as_mut), defs, syms)?;
                arrow.return_type
            }
            Self::Nmbr(_) => Ty::Int,
            Self::Strg(_) => Ty::Str,
            Self::Bool(_) => Ty::Bool,
            Self::Unit(_) => Ty::Unit,
            Self::Name(name) => syms.get_or(name)?,
            Self::Expn(e) => e.check(defs, syms)?,
        })
    }
}
