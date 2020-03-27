use crate::ast;
use crate::hir;

/// Helper trait for operator desugaring
pub trait Operator: Copy {
    /// Returns the method name that should be called for this operator
    fn method_name(self) -> hir::Ident;
}

impl Operator for ast::BoolOp {
    fn method_name(self) -> hir::Ident {
        use ast::BoolOp::*;
        match self {
            _ => todo!(),
        }
    }
}

impl Operator for ast::CompareOp {
    fn method_name(self) -> hir::Ident {
        use ast::CompareOp::*;
        match self {
            _ => todo!()
        }
    }
}

impl Operator for ast::BitwiseOp {
    fn method_name(self) -> hir::Ident {
        use ast::BitwiseOp::*;
        match self {
            _ => todo!()
        }
    }
}

impl Operator for ast::NumericOp {
    fn method_name(self) -> hir::Ident {
        use ast::NumericOp::*;
        match self {
            _ => todo!()
        }
    }
}

impl Operator for ast::UnaryOp {
    fn method_name(self) -> hir::Ident {
        use ast::UnaryOp::*;
        match self {
            _ => todo!()
        }
    }
}
