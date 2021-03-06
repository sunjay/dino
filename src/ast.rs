//! The abstract syntax tree of the program.
//!
//! This is the closest representation to the actual syntax.

use std::fmt;
use std::sync::Arc;

use crate::span::Span;

/// Represents a single module within the current package
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Import(ImportPath),
    Struct(Struct),
    Impl(Impl),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportPath {
    /// The prefix of the path (if any)
    pub prefix: Option<PathPrefix>,
    /// The path (within the prefix) to import from
    pub path: Vec<Ident>,
    /// The items selected from the path
    pub selection: ImportSelection,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportSelection {
    /// A specific list of names being imported
    Names(Vec<ImportName>),
    /// A wildcard import (all items)
    All(Span),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportName {
    Name {name: Ident, alias: Option<Ident>},
    SelfValue {alias: Option<Ident>},
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    /// The name of the struct
    pub name: Ident,
    /// The fields of the struct
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Impl {
    /// The Self type of this impl block
    pub self_ty: Ty,
    /// The method decls of this impl block
    pub methods: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Ident,
    pub sig: FuncSig,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncSig {
    pub params: Vec<FuncParam>,
    pub return_type: Option<Ty>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FuncParam {
    SelfValue(Span),
    Named {
        name: Ident,
        ty: Ty,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub decls: Vec<Decl>,
    pub stmts: Vec<Stmt>,
    /// The final statement of the block, used as the return value of the block
    pub ret: Option<Expr>,
    /// The span of the entire block
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Cond(Cond),
    WhileLoop(WhileLoop),
    VarDecl(VarDecl),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileLoop {
    /// The condition for which the loop is expected to continue
    pub cond: Expr,
    /// The body of the loop, executed until the condition is false
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    /// The identifier to assign a value to
    pub name: Ident,
    /// The type of the variable (or None if the type is to be inferred)
    pub ty: Option<Ty>,
    /// The expression for the value to assign to the variable
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assign(Box<Assign>),
    Range(Box<Range>),
    BoolOp(Box<Binary<BoolOp>>),
    CompareOp(Box<Binary<CompareOp>>),
    BitwiseOp(Box<Binary<BitwiseOp>>),
    NumericOp(Box<Binary<NumericOp>>),
    UnaryOp(Box<Unary>),
    CastAs(Box<CastAs>),
    MethodCall(Box<MethodCall>),
    FieldAccess(Box<FieldAccess>),
    Cond(Box<Cond>),
    Call(Box<FuncCall>),
    Index(Box<Index>),
    Return(Box<Return>),
    Break(Span),
    Continue(Span),
    Block(Box<Block>),
    StructLiteral(StructLiteral),
    BStrLiteral(Literal<Arc<[u8]>>),
    IntegerLiteral(IntegerLiteral),
    RealLiteral(Literal<f64>),
    ComplexLiteral(Literal<f64>),
    BoolLiteral(Literal<bool>),
    UnitLiteral(Span),
    SelfValue(Span),
    Path(Path),
}

impl Expr {
    /// Returns the span encompassing the entire expression
    pub fn span(&self) -> Span {
        use Expr::*;
        match self {
            Assign(assign) => assign.span(),
            Range(range) => range.span(),
            BoolOp(bin) => bin.span(),
            CompareOp(bin) => bin.span(),
            BitwiseOp(bin) => bin.span(),
            NumericOp(bin) => bin.span(),
            UnaryOp(unary) => unary.span(),
            CastAs(cast_as) => cast_as.span(),
            MethodCall(method_call) => method_call.span,
            FieldAccess(access) => access.span(),
            Cond(cond) => cond.span,
            Call(call) => call.span,
            Index(index) => index.span,
            Return(ret) => ret.span(),
            &Break(span) => span,
            &Continue(span) => span,
            Block(block) => block.span,
            StructLiteral(lit) => lit.span,
            BStrLiteral(lit) => lit.span,
            IntegerLiteral(lit) => lit.span,
            RealLiteral(lit) => lit.span,
            ComplexLiteral(lit) => lit.span,
            BoolLiteral(lit) => lit.span,
            &UnitLiteral(span) => span,
            &SelfValue(span) => span,
            Path(path) => path.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    pub lhs: Option<Expr>,
    pub op: RangeOp,
    pub rhs: Option<Expr>,
}

impl Range {
    pub fn span(&self) -> Span {
        let Self {lhs, op, rhs} = self;

        match (lhs, rhs) {
            (None, None) => op.span(),
            (Some(lhs), None) => lhs.span().to(op.span()),
            (None, Some(rhs)) => op.span().to(rhs.span()),
            (Some(lhs), Some(rhs)) => lhs.span().to(rhs.span()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary<Op> {
    pub lhs: Expr,
    pub op: Op,
    pub rhs: Expr,
}

impl<Op> Binary<Op> {
    pub fn span(&self) -> Span {
        let Self {lhs, op: _, rhs} = self;

        lhs.span().to(rhs.span())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub op: UnaryOp,
    pub expr: Expr,
}

impl Unary {
    pub fn span(&self) -> Span {
        let Self {op, expr} = self;

        op.span().to(expr.span())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RangeOp {
    /// The `..` operator
    Exclusive(Span),
    /// The `..=` operator
    Inclusive(Span),
}

impl RangeOp {
    pub fn span(self) -> Span {
        use RangeOp::*;
        match self {
            Exclusive(span) | Inclusive(span) => span,
        }
    }
}

/// All boolean binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoolOp {
    /// The `||` operator
    Or(Span),
    /// The `&&` operator
    And(Span),
}

impl BoolOp {
    pub fn span(self) -> Span {
        use BoolOp::*;
        match self {
            Or(span) | And(span) => span,
        }
    }
}

/// All comparison operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompareOp {
    /// The `==` operator
    Eq(Span),
    /// The `!=` operator
    Ne(Span),
    /// The `<` operator
    Lt(Span),
    /// The `<=` operator
    Le(Span),
    /// The `>` operator
    Gt(Span),
    /// The `>=` operator
    Ge(Span),
}

impl CompareOp {
    pub fn span(self) -> Span {
        use CompareOp::*;
        match self {
            Eq(span) | Ne(span) | Lt(span) | Le(span) | Gt(span) | Ge(span) => span,
        }
    }
}

/// All bitwise operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BitwiseOp {
    /// The `|` operator (bitwise OR)
    Or(Span),
    /// The `~` operator (bitwise XOR)
    Xor(Span),
    /// The `&` operator (bitwise AND)
    And(Span),
    /// The `<<` operator (shift left)
    Shl(Span),
    /// The `>>` operator (shift right)
    Shr(Span),
}

impl BitwiseOp {
    pub fn span(self) -> Span {
        use BitwiseOp::*;
        match self {
            Or(span) | Xor(span) | And(span) | Shl(span) | Shr(span) => span,
        }
    }
}

/// All numeric operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumericOp {
    /// The `+` operator
    Add(Span),
    /// The `-` operator
    Sub(Span),
    /// The `*` operator
    Mul(Span),
    /// The `/` operator
    Div(Span),
    /// The `%` operator
    Rem(Span),
    /// The `^` operator
    Pow(Span),
}

impl NumericOp {
    pub fn span(self) -> Span {
        use NumericOp::*;
        match self {
            Add(span) | Sub(span) | Mul(span) | Div(span) | Rem(span) | Pow(span) => span,
        }
    }
}

/// All unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// The `+` operator
    Pos(Span),
    /// The `-` operator
    Neg(Span),
    /// The `!` operator
    Not(Span),
}

impl UnaryOp {
    pub fn span(self) -> Span {
        use UnaryOp::*;
        match self {
            Pos(span) | Neg(span) | Not(span) => span,
        }
    }
}

/// An assignment expression in the form `<lvalue> = <value>`
#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    /// The left-hand expression to assign a value to
    pub lhs: Expr,
    /// The expression for the value to assign to the left-hand side
    pub rhs: Expr,
}

impl Assign {
    pub fn span(&self) -> Span {
        let Self {lhs, rhs} = self;

        lhs.span().to(rhs.span())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastAs {
    pub expr: Expr,
    pub ty: Ty,
}

impl CastAs {
    pub fn span(&self) -> Span {
        let Self {expr, ty} = self;

        expr.span().to(ty.span())
    }
}

/// A method call in the form `<expr> . <call-expr>`
#[derive(Debug, Clone, PartialEq)]
pub struct MethodCall {
    /// The expression of the left-hand side of the method call
    pub lhs: Expr,
    /// The method being called
    pub method_name: Ident,
    /// The arguments to the method call
    pub args: Vec<Expr>,
    /// The span of the entire method call
    pub span: Span,
}

/// A field access in the form `<expr> . <ident>`
#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    /// The expression of the left-hand side of the field access
    pub lhs: Expr,
    /// The field being accessed
    pub field: Ident,
}

impl FieldAccess {
    pub fn span(&self) -> Span {
        let Self {lhs, field} = self;

        lhs.span().to(field.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cond {
    /// A list of (condition, body) that corresponds to:
    /// if cond1 { body1 } else if cond2 { body2 } ...
    ///
    /// This must be non-empty (or else there would be no condition).
    pub conds: Vec<(Expr, Block)>,
    /// The `else` clause (if any)
    pub else_body: Option<Block>,
    /// The span of the entire conditional expression
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    /// The value being called
    pub value: Expr,
    /// The arguments passed to the value
    pub args: Vec<Expr>,
    /// The span of the entire call expression
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Index {
    /// The value being indexed
    pub value: Expr,
    /// The index expression
    pub expr: Expr,
    /// The span of the entire index expression
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    /// The span of the `return` keyword
    pub return_span: Span,
    /// The expression being returned (optional)
    pub expr: Option<Expr>
}

impl Return {
    pub fn span(&self) -> Span {
        let &Self {return_span, ref expr} = self;

        match expr {
            Some(expr) => return_span.to(expr.span()),
            None => return_span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    pub name: NamedTy,
    pub field_values: Vec<StructFieldValue>,
    /// The span of the entire struct literal
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldValue {
    /// The name of the field
    pub name: Ident,
    /// The expression being assigned to the field
    ///
    /// If `None`, the expression is a variable with the same name as the field
    pub value: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub value: i64,
    /// You can append "int" or "real" to help disambiguate the literal
    /// e.g. 132int or 32real
    pub suffix: Option<LiteralSuffix>,
    /// The span for the entire integer literal, including its suffix
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LiteralSuffix {
    /// The suffix `int`
    Int,
    /// The suffix `real`
    Real,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Unit(Span),
    SelfType(Span),
    Named(Path),
}

impl Ty {
    pub fn span(&self) -> Span {
        use Ty::*;
        match self {
            &Unit(span) => span,
            &SelfType(span) => span,
            Named(path) => path.span(),
        }
    }
}

/// A type explicitly named with an identifier or path (as opposed to (), [T], etc.)
#[derive(Debug, Clone, PartialEq)]
pub enum NamedTy {
    SelfType(Span),
    Named(Path),
}

impl From<NamedTy> for Ty {
    fn from(ty: NamedTy) -> Self {
        match ty {
            NamedTy::SelfType(span) => Ty::SelfType(span),
            NamedTy::Named(path) => Ty::Named(path),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    /// The prefix of the path (if any)
    pub prefix: Option<PathPrefix>,
    /// The components of the path (allowed to be empty only if `prefix` is not `None`)
    pub components: Vec<Ident>,
}

impl Path {
    pub fn hardcoded(components: Vec<&str>, span: Span) -> Self {
        Self {
            prefix: None,
            components: components.into_iter().map(|name| Ident {
                value: name.into(),
                span,
            }).collect(),
        }
    }

    pub fn span(&self) -> Span {
        let Self {prefix, components} = self;

        match (prefix, &components[..]) {
            (Some(prefix), []) => prefix.span(),
            (Some(prefix), [.., last]) => prefix.span().to(last.span),
            (None, [comp]) => comp.span,
            (None, [first, .., last]) => first.span.to(last.span),
            (None, []) => unreachable!("bug: path should never be empty"),
        }
    }
}

impl From<Ident> for Path {
    fn from(component: Ident) -> Self {
        Self {
            prefix: None,
            components: vec![component],
        }
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {prefix, components} = self;

        let remaining_components = match prefix {
            Some(prefix) => {
                write!(f, "{}", prefix)?;
                components
            },
            None => {
                write!(f, "{}", components[0])?;
                &components[1..]
            },
        };
        for comp in remaining_components {
            write!(f, "::{}", comp)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PathPrefix {
    /// The `package` keyword
    Package(Span),
    /// The `Self` keyword
    SelfType(Span),
    /// The `self` keyword
    SelfValue(Span),
    /// The `super` keyword
    Super(Span),
}

impl PathPrefix {
    pub fn span(self) -> Span {
        use PathPrefix::*;
        match self {
            Package(span) | SelfType(span) | SelfValue(span) | Super(span) => span,
        }
    }
}

impl fmt::Display for PathPrefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PathPrefix::*;
        match self {
            Package(_) => write!(f, "package"),
            SelfType(_) => write!(f, "Self"),
            SelfValue(_) => write!(f, "self"),
            Super(_) => write!(f, "super"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal<T> {
    pub value: T,
    pub span: Span,
}

impl<T: fmt::Display> fmt::Display for Literal<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub value: Arc<str>,
    pub span: Span,
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
