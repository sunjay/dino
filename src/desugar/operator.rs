use crate::ast;
use crate::hir;

/// Helper trait for operator desugaring
pub trait Operator: Copy {
    /// Returns the method name that should be called for this operator
    fn method_name(self) -> hir::Ident;
}

impl Operator for ast::CompareOp {
    fn method_name(self) -> hir::Ident {
        use ast::CompareOp::*;
        match self {
            Eq(span) => hir::Ident {
                value: "eq".into(),
                span,
            },
            Ne(span) => hir::Ident {
                value: "ne".into(),
                span,
            },

            //TODO: Replace this desugaring to use `Ord` and `Ordering`
            Lt(span) => hir::Ident {
                value: "lt".into(),
                span,
            },
            Le(span) => hir::Ident {
                value: "le".into(),
                span,
            },
            Gt(span) => hir::Ident {
                value: "gt".into(),
                span,
            },
            Ge(span) => hir::Ident {
                value: "ge".into(),
                span,
            },
        }
    }
}

impl Operator for ast::BitwiseOp {
    fn method_name(self) -> hir::Ident {
        use ast::BitwiseOp::*;
        match self {
            Or(span) => hir::Ident {
                value: "bit_or".into(),
                span,
            },
            Xor(span) => hir::Ident {
                value: "bit_xor".into(),
                span,
            },
            And(span) => hir::Ident {
                value: "bit_and".into(),
                span,
            },
            Shl(span) => hir::Ident {
                value: "shl".into(),
                span,
            },
            Shr(span) => hir::Ident {
                value: "shr".into(),
                span,
            },
        }
    }
}

impl Operator for ast::NumericOp {
    fn method_name(self) -> hir::Ident {
        use ast::NumericOp::*;
        match self {
            Add(span) => hir::Ident {
                value: "add".into(),
                span,
            },
            Sub(span) => hir::Ident {
                value: "sub".into(),
                span,
            },
            Mul(span) => hir::Ident {
                value: "mul".into(),
                span,
            },
            Div(span) => hir::Ident {
                value: "div".into(),
                span,
            },
            Rem(span) => hir::Ident {
                value: "rem".into(),
                span,
            },
            Pow(span) => hir::Ident {
                value: "pow".into(),
                span,
            },
        }
    }
}

impl Operator for ast::UnaryOp {
    fn method_name(self) -> hir::Ident {
        use ast::UnaryOp::*;
        match self {
            Pos(span) => hir::Ident {
                value: "pos".into(),
                span,
            },
            Neg(span) => hir::Ident {
                value: "neg".into(),
                span,
            },
            Not(span) => hir::Ident {
                value: "not".into(),
                span,
            },
        }
    }
}
