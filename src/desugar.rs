mod operator;

use crate::ast;
use crate::hir;
use crate::diagnostics::Diagnostics;

use operator::Operator;

pub trait Desugar {
    type Output;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output;
}

impl<T: Desugar> Desugar for Option<T> {
    type Output = Option<T::Output>;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        self.as_ref().map(|value| value.desugar(diag))
    }
}

impl<T: Desugar> Desugar for Vec<T> {
    type Output = Vec<T::Output>;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        self.iter().map(|value| value.desugar(diag)).collect()
    }
}

impl<T: Desugar, U: Desugar> Desugar for (T, U) {
    type Output = (T::Output, U::Output);

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let (x, y) = self;
        (x.desugar(diag), y.desugar(diag))
    }
}

impl Desugar for ast::Module {
    type Output = hir::Module;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {decls} = self;

        hir::Module {
            decls: decls.iter().map(|decl| decl.desugar(diag)).collect(),
        }
    }
}

impl Desugar for ast::Decl {
    type Output = hir::Decl;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        match self {
            ast::Decl::Import(import_path) => hir::Decl::Import(import_path.desugar(diag)),
            ast::Decl::Struct(struct_decl) => hir::Decl::Struct(struct_decl.desugar(diag)),
            ast::Decl::Impl(impl_decl) => hir::Decl::Impl(impl_decl.desugar(diag)),
            ast::Decl::Function(func) => hir::Decl::Function(func.desugar(diag)),
        }
    }
}

impl Desugar for ast::ImportPath {
    type Output = hir::ImportPath;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {prefix, path, selection} = self;

        hir::ImportPath {
            prefix: prefix.desugar(diag),
            path: path.desugar(diag),
            selection: selection.desugar(diag),
        }
    }
}

impl Desugar for ast::ImportSelection {
    type Output = hir::ImportSelection;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        match self {
            ast::ImportSelection::Names(names) => hir::ImportSelection::Names(names.desugar(diag)),
            &ast::ImportSelection::All(span) => hir::ImportSelection::All(span),
        }
    }
}

impl Desugar for ast::ImportName {
    type Output = hir::ImportName;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        match self {
            ast::ImportName::Name {name, alias} => hir::ImportName::Name {
                name: name.desugar(diag),
                alias: alias.desugar(diag),
            },
            ast::ImportName::SelfValue {alias} => hir::ImportName::SelfValue {
                alias: alias.desugar(diag),
            },
        }
    }
}

impl Desugar for ast::Struct {
    type Output = hir::Struct;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {name, fields} = self;

        hir::Struct {
            name: name.desugar(diag),
            fields: fields.desugar(diag),
        }
    }
}

impl Desugar for ast::StructField {
    type Output = hir::StructField;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {name, ty} = self;

        hir::StructField {
            name: name.desugar(diag),
            ty: ty.desugar(diag),
        }
    }
}

impl Desugar for ast::Impl {
    type Output = hir::Impl;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {self_ty, methods} = self;

        hir::Impl {
            self_ty: self_ty.desugar(diag),
            methods: methods.desugar(diag),
        }
    }
}

impl Desugar for ast::Function {
    type Output = hir::Function;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {name, sig, body} = self;

        hir::Function {
            name: name.desugar(diag),
            sig: sig.desugar(diag),
            body: body.desugar(diag),
        }
    }
}

impl Desugar for ast::FuncSig {
    type Output = hir::FuncSig;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {params, return_type} = self;

        let (self_param, params) = match &params[..] {
            [ast::FuncParam::SelfValue(span), rest @ ..] => (Some(*span), rest),
            params => (None, params),
        };

        let mut errors = 0;
        let params = params.iter().map(|param| match param {
            &ast::FuncParam::SelfValue(span) => {
                diag.error("unexpected `self` parameter in function")
                    .span_error(span, "must be the first function parameter")
                    .emit();

                errors += 1;
                let fresh_name = hir::Ident {
                    value: format!("self$p{}", errors).into(),
                    span,
                };

                hir::FuncParam {
                    name: fresh_name,
                    ty: hir::Ty::SelfType(span),
                }
            },

            ast::FuncParam::Named {name, ty} => hir::FuncParam {
                name: name.desugar(diag),
                ty: ty.desugar(diag),
            },
        }).collect();

        let return_type = return_type.desugar(diag);

        hir::FuncSig {self_param, params, return_type}
    }
}

impl Desugar for ast::Block {
    type Output = hir::Block;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let &Self {ref decls, ref stmts, ref ret, span} = self;

        hir::Block {
            decls: decls.desugar(diag),
            stmts: stmts.desugar(diag),
            ret: ret.desugar(diag),
            span,
        }
    }
}

impl Desugar for ast::Stmt {
    type Output = hir::Stmt;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        match self {
            ast::Stmt::Cond(cond) => hir::Stmt::Cond(cond.desugar(diag)),
            ast::Stmt::WhileLoop(wloop) => hir::Stmt::WhileLoop(wloop.desugar(diag)),
            ast::Stmt::VarDecl(var_decl) => hir::Stmt::VarDecl(var_decl.desugar(diag)),
            ast::Stmt::Expr(expr) => hir::Stmt::Expr(expr.desugar(diag)),
        }
    }
}

impl Desugar for ast::WhileLoop {
    type Output = hir::WhileLoop;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {cond, body} = self;

        hir::WhileLoop {
            cond: cond.desugar(diag),
            body: body.desugar(diag),
        }
    }
}

impl Desugar for ast::VarDecl {
    type Output = hir::VarDecl;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {name, ty, expr} = self;

        hir::VarDecl {
            name: name.desugar(diag),
            ty: ty.desugar(diag),
            expr: expr.desugar(diag),
        }
    }
}

impl Desugar for ast::Expr {
    type Output = hir::Expr;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        match self {
            ast::Expr::Assign(assign) => hir::Expr::Assign(Box::new(assign.desugar(diag))),
            ast::Expr::Range(range) => range.desugar(diag),
            ast::Expr::BoolOp(bin) => bin.desugar(diag),
            ast::Expr::CompareOp(bin) => bin.desugar(diag),
            ast::Expr::BitwiseOp(bin) => bin.desugar(diag),
            ast::Expr::NumericOp(bin) => bin.desugar(diag),
            ast::Expr::UnaryOp(unary) => unary.desugar(diag),
            ast::Expr::CastAs(_cast) => todo!(), // See issue #59,
            ast::Expr::MethodCall(call) => hir::Expr::MethodCall(Box::new(call.desugar(diag))),
            ast::Expr::FieldAccess(access) => hir::Expr::FieldAccess(Box::new(access.desugar(diag))),
            ast::Expr::Cond(cond) => hir::Expr::Cond(Box::new(cond.desugar(diag))),
            ast::Expr::Call(call) => hir::Expr::Call(Box::new(call.desugar(diag))),
            ast::Expr::Index(index) => index.desugar(diag),
            ast::Expr::Return(ret) => hir::Expr::Return(Box::new(ret.desugar(diag))),
            &ast::Expr::Break(span) => hir::Expr::Break(span),
            &ast::Expr::Continue(span) => hir::Expr::Continue(span),
            ast::Expr::Block(_block) => todo!(), // See issue #60
            ast::Expr::StructLiteral(lit) => hir::Expr::StructLiteral(lit.desugar(diag)),
            ast::Expr::BStrLiteral(ast::Literal {value, span}) => hir::Expr::BStrLiteral(value.clone(), *span),
            ast::Expr::IntegerLiteral(lit) => hir::Expr::IntegerLiteral(lit.desugar(diag)),
            &ast::Expr::RealLiteral(ast::Literal {value, span}) => hir::Expr::RealLiteral(value, span),
            &ast::Expr::ComplexLiteral(ast::Literal {value, span}) => hir::Expr::ComplexLiteral(value, span),
            &ast::Expr::BoolLiteral(ast::Literal {value, span}) => hir::Expr::BoolLiteral(value, span),
            &ast::Expr::UnitLiteral(span) => hir::Expr::UnitLiteral(span),
            &ast::Expr::SelfValue(span) => hir::Expr::SelfValue(span),
            ast::Expr::Path(path) => hir::Expr::Path(path.desugar(diag)),
        }
    }
}

impl Desugar for ast::Range {
    type Output = hir::Expr;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {lhs, op, rhs} = self;

        let span = op.span();

        use ast::RangeOp::*;
        match (lhs.desugar(diag), rhs.desugar(diag)) {
            (None, None) => hir::Expr::Path(hir::Path::hardcoded(
                vec!["std", "ops", "RangeFull"],
                span,
            )),

            (Some(lhs), None) => hir::Expr::StructLiteral(hir::StructLiteral {
                name: hir::NamedTy::Named(hir::Path::hardcoded(
                    vec!["std", "ops", "RangeFrom"],
                    span,
                )),
                field_values: vec![
                    hir::StructFieldValue {
                        name: hir::Ident {
                            value: "start".into(),
                            span,
                        },
                        value: lhs,
                    },
                ],
                span: self.span(),
            }),

            (None, Some(rhs)) => hir::Expr::StructLiteral(hir::StructLiteral {
                name: hir::NamedTy::Named(hir::Path::hardcoded(
                    vec![
                        "std",
                        "ops",
                        match op {
                            Exclusive(_) => "RangeTo",
                            Inclusive(_) => "RangeToInclusive",
                        },
                    ],
                    span,
                )),
                field_values: vec![
                    hir::StructFieldValue {
                        name: hir::Ident {
                            value: "end".into(),
                            span,
                        },
                        value: rhs,
                    },
                ],
                span: self.span(),
            }),

            (Some(lhs), Some(rhs)) => hir::Expr::StructLiteral(hir::StructLiteral {
                name: hir::NamedTy::Named(hir::Path::hardcoded(
                    vec![
                        "std",
                        "ops",
                        match op {
                            Exclusive(_) => "Range",
                            Inclusive(_) => "RangeInclusive",
                        },
                    ],
                    span,
                )),
                field_values: vec![
                    hir::StructFieldValue {
                        name: hir::Ident {
                            value: "start".into(),
                            span,
                        },
                        value: lhs,
                    },
                    hir::StructFieldValue {
                        name: hir::Ident {
                            value: "end".into(),
                            span,
                        },
                        value: rhs,
                    },
                ],
                span: self.span(),
            }),
        }
    }
}

impl Desugar for ast::Binary<ast::BoolOp> {
    type Output = hir::Expr;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {lhs, op, rhs} = self;

        let lhs = lhs.desugar(diag);
        let rhs = rhs.desugar(diag);

        match op {
            &ast::BoolOp::Or(span) => hir::Expr::BoolOr(Box::new(hir::BoolOr {lhs, span, rhs})),
            &ast::BoolOp::And(span) => hir::Expr::BoolAnd(Box::new(hir::BoolAnd {lhs, span, rhs})),
        }
    }
}

impl<Op: Operator> Desugar for ast::Binary<Op> {
    type Output = hir::Expr;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {lhs, op, rhs} = self;

        hir::Expr::MethodCall(Box::new(hir::MethodCall {
            lhs: lhs.desugar(diag),
            method_name: op.method_name(),
            args: vec![rhs.desugar(diag)],
            span: self.span(),
        }))
    }
}

impl Desugar for ast::Unary {
    type Output = hir::Expr;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {op, expr} = self;

        hir::Expr::MethodCall(Box::new(hir::MethodCall {
            lhs: expr.desugar(diag),
            method_name: op.method_name(),
            args: Vec::new(),
            span: self.span(),
        }))
    }
}

impl Desugar for ast::Index {
    type Output = hir::Expr;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let &Self {ref value, ref expr, span} = self;

        hir::Expr::MethodCall(Box::new(hir::MethodCall {
            lhs: value.desugar(diag),
            method_name: hir::Ident {
                value: "index".into(),
                span,
            },
            args: vec![expr.desugar(diag)],
            span,
        }))
    }
}

impl Desugar for ast::Assign {
    type Output = hir::Assign;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {lhs, rhs} = self;

        let lvalue = match lhs {
            ast::Expr::FieldAccess(access) => hir::LValue::FieldAccess(access.desugar(diag)),
            ast::Expr::Path(path) => hir::LValue::Path(path.desugar(diag)),
            _ => {
                let span = lhs.span();
                diag.error("invalid left-hand side of assignment")
                    .span_info(span, "cannot assign to this expression")
                    .emit();

                // Make a fake lvalue so desugaring can continue
                hir::LValue::Path(hir::Path::from(hir::Ident {
                    value: "$error".into(),
                    span,
                }))
            },
        };

        hir::Assign {
            lvalue,
            expr: rhs.desugar(diag),
        }
    }
}

impl Desugar for ast::MethodCall {
    type Output = hir::MethodCall;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let &Self {ref lhs, ref method_name, ref args, span} = self;

        hir::MethodCall {
            lhs: lhs.desugar(diag),
            method_name: method_name.desugar(diag),
            args: args.desugar(diag),
            span,
        }
    }
}

impl Desugar for ast::FieldAccess {
    type Output = hir::FieldAccess;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {lhs, field} = self;

        hir::FieldAccess {
            lhs: lhs.desugar(diag),
            field: field.desugar(diag),
        }
    }
}

impl Desugar for ast::Cond {
    type Output = hir::Cond;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let &Self {ref conds, ref else_body, span} = self;

        hir::Cond {
            conds: conds.desugar(diag),
            else_body: else_body.desugar(diag),
            span,
        }
    }
}

impl Desugar for ast::FuncCall {
    type Output = hir::FuncCall;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let &Self {ref value, ref args, span} = self;

        hir::FuncCall {
            value: value.desugar(diag),
            args: args.desugar(diag),
            span,
        }
    }
}

impl Desugar for ast::Return {
    type Output = hir::Return;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let &Self {return_span, ref expr} = self;

        hir::Return {
            return_span,
            expr: expr.desugar(diag),
        }
    }
}

impl Desugar for ast::StructLiteral {
    type Output = hir::StructLiteral;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let &Self {ref name, ref field_values, span} = self;

        hir::StructLiteral {
            name: name.desugar(diag),
            field_values: field_values.desugar(diag),
            span,
        }
    }
}

impl Desugar for ast::StructFieldValue {
    type Output = hir::StructFieldValue;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let Self {name, value} = self;

        let name = name.desugar(diag);
        // Field with no value desugars to the field name as a variable
        let value = value.desugar(diag)
            .unwrap_or_else(|| hir::Expr::Path(hir::Path::from(name.clone())));

        hir::StructFieldValue {name, value}
    }
}

impl Desugar for ast::IntegerLiteral {
    type Output = hir::IntegerLiteral;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        let &Self {value, suffix, span} = self;

        hir::IntegerLiteral {
            value,
            suffix: suffix.desugar(diag),
            span,
        }
    }
}

impl Desugar for ast::LiteralSuffix {
    type Output = hir::LiteralSuffix;

    fn desugar(&self, _diag: &Diagnostics) -> Self::Output {
        match self {
            ast::LiteralSuffix::Int => hir::LiteralSuffix::Int,
            ast::LiteralSuffix::Real => hir::LiteralSuffix::Real,
        }
    }
}

impl Desugar for ast::Ty {
    type Output = hir::Ty;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        match self {
            &ast::Ty::Unit(span) => hir::Ty::Unit(span),
            &ast::Ty::SelfType(span) => hir::Ty::SelfType(span),
            ast::Ty::Named(path) => hir::Ty::Named(path.desugar(diag)),
        }
    }
}

impl Desugar for ast::NamedTy {
    type Output = hir::NamedTy;

    fn desugar(&self, diag: &Diagnostics) -> Self::Output {
        match self {
            &ast::NamedTy::SelfType(span) => hir::NamedTy::SelfType(span),
            ast::NamedTy::Named(path) => hir::NamedTy::Named(path.desugar(diag)),
        }
    }
}

impl Desugar for ast::Path {
    type Output = hir::Path;

    fn desugar(&self, _diag: &Diagnostics) -> Self::Output {
        self.clone()
    }
}

impl Desugar for ast::PathPrefix {
    type Output = hir::PathPrefix;

    fn desugar(&self, _diag: &Diagnostics) -> Self::Output {
        *self
    }
}

impl Desugar for ast::Ident {
    type Output = hir::Ident;

    fn desugar(&self, _diag: &Diagnostics) -> Self::Output {
        self.clone()
    }
}
