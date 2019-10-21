use std::iter::once;

use snafu::Snafu;
use nom::{
    error::VerboseError,
    branch::alt,
    number::complete::double,
    character::complete::{char, digit1, one_of},
    combinator::{all_consuming, map, map_res, recognize, opt, not},
    bytes::complete::{tag, take_while1, take_while, take_till, take_till1, escaped_transform},
    sequence::{tuple, pair, delimited, terminated, preceded},
    multi::{many0, fold_many0, separated_list},
};

use super::*;

type Input<'a> = &'a str;
type IResult<'a, O> = nom::IResult<Input<'a>, O, VerboseError<Input<'a>>>;

/// Represents errors that can occur during parsing
#[derive(Debug, Snafu)]
pub enum Error {
    //TODO: Figure out what different error cases we can return here
}

impl From<nom::Err<VerboseError<Input<'_>>>> for Error {
    fn from(err: nom::Err<VerboseError<Input>>) -> Self {
        panic!("Error: {:?}", err);
    }
}

/// Attempts to parse the given input module
pub fn parse_module(input: &str) -> Result<Module, Error> {
    let (inp, module) = match all_consuming(module)(input) {
        Ok((inp, module)) => (inp, module),
        Err(nom::Err::Error(err)) | Err(nom::Err::Failure(err)) => {
            //TODO: Convert to Error type above
            panic!("Error: {}", nom::error::convert_error(input, err));
        },
        // This should not be reachable because we are using the 'complete' versions of all parsers
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    };

    Ok(match inp {
        "" => module,
        // Should never reach this case because the parser should ensure that all input is consumed
        _ => unreachable!(),
    })
}

fn module(input: Input) -> IResult<Module> {
    map(
        preceded(wsc0, many0(terminated(decl, wsc0))),
        |decls| Module {decls},
    )(input)
}

fn decl(input: Input) -> IResult<Decl> {
    alt((
        map(struct_decl, Decl::Struct),
        map(impl_block, Decl::Impl),
        map(function(FuncType::Function), Decl::Function),
    ))(input)
}

fn struct_decl(input: Input) -> IResult<Struct> {
    map(tuple((
        kw_struct,
        wsc0,
        ident,
        wsc0,
        struct_fields,
    )), |(_, _, name, _, fields)| Struct {name, fields})(input)
}

fn struct_fields(input: Input) -> IResult<Vec<StructField>> {
    delimited(
        tuple((char('{'), wsc0)),
        comma_separated(struct_field),
        tuple((wsc0, char('}'))),
    )(input)
}

fn struct_field(input: Input) -> IResult<StructField> {
    map(tuple((
        ident,
        wsc0,
        char(':'),
        wsc0,
        ty,
    )), |(name, _, _, _, ty)| StructField {name, ty})(input)
}

fn impl_block(input: Input) -> IResult<Impl> {
    map(
        tuple((
            kw_impl,
            wsc0,
            ty,
            wsc0,
            delimited(
                char('{'),
                preceded(wsc0, many0(terminated(function(FuncType::Method), wsc0))),
                char('}'),
            ),
        )),
        |(_, _, self_ty, _, methods)| Impl {self_ty, methods},
    )(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum FuncType {
    Function,
    Method,
}

fn function(func_type: FuncType) -> impl Fn(Input) -> IResult<Function> {
    move |input| map(tuple((
        kw_fn,
        wsc0,
        ident,
        wsc0,
        match func_type {
            FuncType::Function => function_params,
            FuncType::Method => method_params,
        },
        opt(tuple((wsc0, tag("->"), wsc0, ty))),
        wsc0,
        block,
    )), |(_, _, name, _, params, return_ty, _, body)| Function {
        name,
        sig: FuncSig {
            // The default return type is unit
            return_type: return_ty.map(|(_, _, _, ty)| ty).unwrap_or(Ty::Unit),
            params,
        },
        body,
        is_extern: false,
    })(input)
}

fn method_params(input: Input) -> IResult<Vec<FuncParam>> {
    static SELF_PARAM: FuncParam = FuncParam {
        name: "self",
        ty: Ty::SelfType,
    };

    delimited(
        tuple((char('('), wsc0)),
        alt((
            // `self` + `,` + param_list
            map(
                tuple((kw_selfvalue, wsc0, char(','), wsc0, param_list)),
                |(_, _, _, _, params)| once(SELF_PARAM.clone()).chain(params).collect(),
            ),
            // `self`
            map(
                kw_selfvalue,
                |_| vec![SELF_PARAM.clone()],
            ),
            // param_list
            param_list,
        )),
        tuple((wsc0, char(')'))),
    )(input)
}

fn function_params(input: Input) -> IResult<Vec<FuncParam>> {
    delimited(
        tuple((char('('), wsc0)),
        param_list,
        tuple((wsc0, char(')'))),
    )(input)
}

fn param_list(input: Input) -> IResult<Vec<FuncParam>> {
    comma_separated(map(
        tuple((ident, wsc0, char(':'), wsc0, ty)),
        |(name, _, _, _, ty)| FuncParam {name, ty},
    ))(input)
}

fn block(input: Input) -> IResult<Block> {
    map(
        delimited(
            char('{'),
            tuple((
                preceded(wsc0, many0(terminated(stmt, wsc0))),
                opt(tuple((expr, wsc0))),
            )),
            char('}'),
        ),
        |(mut stmts, expr)| {
            let ret = expr.map(|(expr, _)| expr);

            // There is an ambiguity here because certain expressions can also be written in
            // statment position. When that is the case, we need to be sure to pull those
            // statements into the return expression instead of leaving them in the statements
            // list.
            let ret = if ret.is_none() && !stmts.is_empty() {
                match stmts.pop().unwrap() {
                    // Since semi-colons after conditionals are optional even in statement
                    // position, they may sometimes be seen as statements when the user actually
                    // intended them to be the return expression
                    Stmt::Cond(cond) => Some(Expr::Cond(Box::new(cond))),
                    // Cannot currently promote a while loop to an expression
                    stmt@Stmt::WhileLoop(_) |
                    // Cannot promote var declaration to expression
                    stmt@Stmt::VarDecl(_) |
                    // Expressions cannot be promoted because for them to be parsed as statements
                    // they must have ended in a semi-colon. That means that the user explicitly
                    // indended the expression's value to *not* be returned.
                    stmt@Stmt::Expr(_) => {
                        // Need to push statement back. Likely won't cause any re-allocation, since
                        // the collection shouldn't have freed the space we just pop'd this from.
                        // At most we pay a bit for a move, but that's not too big of a price to
                        // pay for simpler code.
                        stmts.push(stmt);
                        None
                    },
                }
            } else {
                ret
            };

            Block {stmts, ret}
        },
    )(input)
}

fn stmt(input: Input) -> IResult<Stmt> {
    alt((
        // Need to explicitly ensure that there is so semi-colon following this since that means it
        // was intended as an expression, not a statement
        map(tuple((cond, not(char(';')))), |(cond, _)| Stmt::Cond(cond)),
        map(while_loop, Stmt::WhileLoop),
        map(var_decl, Stmt::VarDecl),
        map(tuple((expr, wsc0, char(';'))), |(expr, _, _)| Stmt::Expr(expr)),
    ))(input)
}

fn while_loop(input: Input) -> IResult<WhileLoop> {
    map(
        tuple((kw_while, wsc0, expr, wsc0, block)),
        |(_, _, cond, _, body)| WhileLoop {cond, body},
    )(input)
}

fn var_decl(input: Input) -> IResult<VarDecl> {
    map(
        tuple((
            kw_let,
            wsc0,
            ident,
            wsc0,
            opt(tuple((
                char(':'),
                wsc0,
                ty,
                wsc0,
            ))),
            char('='),
            wsc0,
            expr,
            wsc0,
            char(';'),
        )),
        |(_, _, ident, _, ty, _, _, expr, _, _)| VarDecl {
            ident,
            ty: ty.map(|(_, _, ty, _)| ty),
            expr,
        },
    )(input)
}

fn expr(input: Input) -> IResult<Expr> {
    // Precedence of binary operators is defined implicitly by the structure of this code. Each
    // level is encoded in a function precedenceN where N is the level of precedence.
    precedence0(input)
}

fn precedence0(input: Input) -> IResult<Expr> {
    alt((
        // Assignment (=) has the lowest precedence and is right associative
        // This is a special operator because its left-hand-side is limited to a subset of all
        // possible expressions
        map(
            // The '=' operator is right associative, so we use right-recursion here
            infix(ident, char('='), expr),
            |(ident, expr)| Expr::VarAssign(Box::new(VarAssign {ident, expr})),
        ),

        // If nothing above parses, we can use the next upper level of precedence
        precedence1,
    ))(input)
}

fn precedence1(input: Input) -> IResult<Expr> {
    bin_op_opt1(
        precedence2,
        alt((
            tag("=="),
            tag("!="),
            tag("<"),
            tag(">"),
            tag("<="),
            tag(">="),
        )),
        precedence2,
        |lhs, op, rhs| Expr::MethodCall(Box::new(MethodCall {
            lhs,
            //TODO: Should be using trait methods
            call: CallExpr {
                func_name: match op {
                    "==" => "eq",
                    "!=" => "ne",
                    "<" => "lt",
                    ">" => "gt",
                    "<=" => "le",
                    ">=" => "ge",
                    _ => unreachable!(),
                },
                args: vec![rhs],
            },
        })),
    )(input)
}

fn precedence2(input: Input) -> IResult<Expr> {
    bin_op_opt1(
        precedence3,
        one_of("+-"),
        precedence3,
        |lhs, op, rhs| Expr::MethodCall(Box::new(MethodCall {
            lhs,
            //TODO: Should be using trait methods
            call: CallExpr {
                func_name: match op {
                    '+' => "add",
                    '-' => "sub",
                    _ => unreachable!(),
                },
                args: vec![rhs],
            },
        })),
    )(input)
}

fn precedence3(input: Input) -> IResult<Expr> {
    bin_op_opt1(
        precedence4,
        one_of("*/%"),
        precedence4,
        |lhs, op, rhs| Expr::MethodCall(Box::new(MethodCall {
            lhs,
            //TODO: Should be using trait methods
            call: CallExpr {
                func_name: match op {
                    '*' => "mul",
                    '/' => "div",
                    '%' => "rem",
                    _ => unreachable!(),
                },
                args: vec![rhs],
            },
        })),
    )(input)
}

fn precedence4(input: Input) -> IResult<Expr> {
    alt((
        map(
            tuple((one_of("-!"), wsc0, precedence5)),
            |(op, _, lhs)| Expr::MethodCall(Box::new(MethodCall {
                //TODO: Should be using trait methods for operators
                call: CallExpr {
                    func_name: match op {
                        // HACK: we can make type inference a bit easier for ourselves if we allow
                        // numeric literals to just include their negative sign directly
                        '-' => match lhs {
                            Expr::IntegerLiteral(lit) => {
                                return Expr::IntegerLiteral(IntegerLiteral {
                                    value: -lit.value,
                                    ..lit
                                });
                            },
                            Expr::RealLiteral(value) => return Expr::RealLiteral(-value),
                            Expr::ComplexLiteral(value) => return Expr::ComplexLiteral(-value),
                            _ => "neg",
                        },
                        '!' => "not",
                        _ => unreachable!(),
                    },
                    args: Vec::new(),
                },
                lhs,
            })),
        ),

        // If nothing above parses, we can use the next upper level of precedence
        precedence5,
    ))(input)
}

fn precedence5(input: Input) -> IResult<Expr> {
    // The dot (.) operator has very high precedence and is left associative
    // Using the next level of precedence like this is a technique for left recursion
    // This operator is special because it has a limited number of things that can be used as its
    // right-hand side.
    bin_op_opt1(precedence6, char('.'), func_call,
        |lhs, _, call| Expr::MethodCall(Box::new(MethodCall {lhs, call})))(input)
}

fn precedence6(input: Input) -> IResult<Expr> {
    alt((
        group,
        map(cond, |cond| Expr::Cond(Box::new(cond))),
        map(func_call, Expr::Call),
        map(return_expr, |ret_expr| Expr::Return(ret_expr.map(Box::new))),
        map(bstr_literal, Expr::BStrLiteral),
        // Integer literal must be parsed before real_literal because that parser also accepts all
        // valid integer literals
        map(integer_literal, Expr::IntegerLiteral),
        real_or_complex_literal,
        map(bool_literal, Expr::BoolLiteral),
        map(unit_literal, |_| Expr::UnitLiteral),
        map(ident, Expr::Var),
    ))(input)
}

fn group(input: Input) -> IResult<Expr> {
    map(
        tuple((char('('), wsc0, expr, wsc0, char(')'))),
        |(_, _, expr, _, _)| expr,
    )(input)
}

fn cond(input: Input) -> IResult<Cond> {
    map(
        tuple((
            tuple((kw_if, wsc0, expr, wsc0, block)),
            many0(tuple((wsc0, kw_else, wsc0, kw_if, wsc0, expr, wsc0, block))),
            opt(tuple((wsc0, kw_else, wsc0, block))),
        )),
        |(top_if, else_ifs, else_body)| {
            let (_, _, top_if_cond, _, top_if_body) = top_if;
            let else_ifs = else_ifs.into_iter()
                .map(|(_, _, _, _, _, else_if_cond, _, else_if_body)| (else_if_cond, else_if_body));
            let else_body = else_body.map(|(_, _, _, else_body)| else_body);

            let conds = once((top_if_cond, top_if_body)).chain(else_ifs).collect();
            Cond {conds, else_body}
        },
    )(input)
}

fn func_call(input: Input) -> IResult<CallExpr> {
    map(
        tuple((ident, wsc0, func_args)),
        |(func_name, _, args)| CallExpr {func_name, args},
    )(input)
}

fn func_args(input: Input) -> IResult<Vec<Expr>> {
    delimited(char('('), comma_separated(expr), char(')'))(input)
}

fn return_expr(input: Input) -> IResult<Option<Expr>> {
    map(
        tuple((kw_return, wsc0, opt(expr))),
        |(_, _, expr)| expr,
    )(input)
}

fn bstr_literal(input: Input) -> IResult<Vec<u8>> {
    map(delimited(
        tag("b\""),
        opt(escaped_transform(take_till1(|c| c == '"' || c == '\\'), '\\', |inp| alt((
            map(char('\\'), |_| "\\"),
            map(char('"'), |_| "\""),
            map(char('n'), |_| "\n"),
            map(char('r'), |_| "\r"),
            map(char('t'), |_| "\t"),
        ))(inp))),
        char('"'),
    ), |s| s.unwrap_or_default().into_bytes())(input)
}

fn ty(input: Input) -> IResult<Ty> {
    alt((
        map(tag("()"), |_| Ty::Unit),
        map(kw_selftype, |_| Ty::SelfType),
        map(ident, |name| Ty::Named(name)),
    ))(input)
}

fn ident(input: Input) -> IResult<&str> {
    preceded(
        // Identifier must not be a keyword
        not(keyword),
        ident_raw,
    )(input)
}

/// The raw identifier, without checking for whether it is a keyword
fn ident_raw(input: Input) -> IResult<&str> {
    recognize(pair(
        // Must be at least non-number
        take_while1(|c: char| c.is_alphabetic() || c == '_'),
        // Followed by any other identifier characters
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    ))(input)
}

fn integer_literal(input: Input) -> IResult<IntegerLiteral> {
    use nom::ParseTo;
    use nom::error::{ParseError, ErrorKind};

    map_res(
        tuple((
            recognize(tuple((
                opt(alt((char('+'), char('-')))),
                digit1,
                // Cannot end in something that would result in a real number literal
                not(one_of("eEjJI")),
                // 'i' is a special case because 'i' isn't allowed, but "int" is fine
                //
                // This reads as "you're not allowed i when it isn't followed by 'nt'"
                not(pair(char('i'), not(tag("nt")))),
                // '.' is a special case because method calls are allowed but `12.` is still a
                // valid real number literal
                //
                // This reads as "you're not allowed '.' when it isn't about to be interpreted
                // as a method call or field access"
                not(pair(char('.'), not(ident))),
            ))),
            opt(alt((tag("int"), tag("real")))),
        )),
        |(val, type_hint): (Input, _)| match val.parse_to() {
            Some(value) => Ok(IntegerLiteral {value, type_hint}),
            None => Err(nom::Err::Error(VerboseError::from_error_kind(input, ErrorKind::ParseTo))),
        },
    )(input)
}

fn real_or_complex_literal(input: Input) -> IResult<Expr> {
    map(
        tuple((double, opt(alt((char('j'), char('J'), char('i'), char('I')))))),
        |(value, complex)| if complex.is_some() {
            Expr::ComplexLiteral(value)
        } else {
            Expr::RealLiteral(value)
        }
    )(input)
}

fn bool_literal(input: Input) -> IResult<bool> {
    alt((
        map(kw_true, |_| true),
        map(kw_false, |_| false),
    ))(input)
}

fn unit_literal(input: Input) -> IResult<()> {
    map(tag("()"), |_| ())(input)
}

/// Parses any amount of whitespace or comment
fn wsc0(input: Input) -> IResult<()> {
    // Using an inner function to avoid allowing anyone to accidentally use this instead of wsc1
    /// Parses at least one whitespace character
    fn ws1(input: Input) -> IResult<()> {
        map(take_while1(|c: char| c.is_whitespace()), |_| ())(input)
    }

    map(many0(alt((comment, ws1))), |_| ())(input)
}

/// Parses a comment
fn comment(input: Input) -> IResult<()> {
    map(
        tuple((tag("//"), take_till(|c| c == '\n'), char('\n'))),
        |_| (),
    )(input)
}

/// Parses comma separated values, allowing for a trailing comma at the end
///
/// The returned Vec is allowed to be empty
fn comma_separated<'r, T: Clone + 'r, F>(parser: F) -> impl Fn(Input<'r>) -> IResult<Vec<T>>
    where F: Fn(Input<'r>) -> IResult<T> {
    terminated(
        separated_list(tuple((wsc0, char(','), wsc0)), parser),
        opt(tuple((wsc0, char(',')))),
    )
}

/// Parses the left-hand side of a binary operator, then optionally parses the operator and the
/// right-hand side repeatedly until there is no more to parse. This is useful for parsing left
/// associative operators that would normally need to use left recursion. To use this for this
/// purpose, make sure that `lhs` and `rhs` are of greater precedence than the operator. Use the
/// folding function `merge` to join the results together.
///
/// To parse multiple operators at the same precedence level, use `alt` in `op` and then match on
/// the result in `merge`.
fn bin_op_opt1<'r, L, Op, R, LHS, OpR, RHS, F>(
    lhs: L,
    op: Op,
    rhs: R,
    merge: F,
) -> impl Fn(Input<'r>) -> IResult<LHS>
    where L: Fn(Input<'r>) -> IResult<LHS>,
          Op: Fn(Input<'r>) -> IResult<OpR>,
          R: Fn(Input<'r>) -> IResult<RHS>,
          LHS: Clone,
          F: Fn(LHS, OpR, RHS) -> LHS,
{
    move |input| {
        // Prevent moving these captured variables when they get used below
        let op = &op;
        let rhs = &rhs;

        // Parse the initial value
        let (input, first) = lhs(input)?;

        // Parse any remaining instances of the binary operator being used (or just return the
        // initial value)
        fold_many0(
            tuple((wsc0, op, wsc0, rhs)),
            first,
            |lhs, (_, op, _, rhs)| merge(lhs, op, rhs),
        )(input)
    }
}

/// Parses a binary infix operator. This can be used to parse right-associative binary operators
/// using right recursion. To do this, make sure the `lhs` is of higher precedence and the `rhs` is
/// of lower/same precedence than the operator.
fn infix<'r, L, Op, R, LHS, OpR, RHS>(lhs: L, op: Op, rhs: R) -> impl Fn(Input<'r>) -> IResult<(LHS, RHS)>
    where L: Fn(Input<'r>) -> IResult<LHS>,
          Op: Fn(Input<'r>) -> IResult<OpR>,
          R: Fn(Input<'r>) -> IResult<RHS>,
{
    map(
        tuple((
            lhs,
            wsc0,
            op,
            wsc0,
            rhs,
            // No semi-colon because this is an expression
        )),
        |(left, _, _, _, right)| (left, right),
    )
}

/// Generates keyword parsers
macro_rules! keywords {
    ($($parser:ident : $kw:ident)*) => {
        /// Parses any keyword
        fn keyword(input: Input) -> IResult<Input> {
            // alt() could not handle this many values, so we had to hand roll it ourself
            let (inp, parsed) = ident_raw(input)?;

            // Optimization: do not match if input too long
            let mut max_len = 0;
            $(
                let $parser = stringify!($kw);
                max_len = max_len.max($parser.len());
            )*
            if parsed.len() > max_len {
                // Return the same error returned by tag()
                return Err(nom::Err::Error(nom::error_position!(input, nom::error::ErrorKind::TagBits)));
            }

            match parsed {
                $(stringify!($kw) => Ok((inp, parsed))),*,
                // Return the same error returned by tag()
                _ => Err(nom::Err::Error(nom::error_position!(input, nom::error::ErrorKind::TagBits))),
            }
        }

        $(
            // Keywords are reserved for the future, so the parser may not be used immediately
            #[allow(dead_code)]
            fn $parser(input: Input) -> IResult<Input> {
                // Keyword must be followed by non-identifier characters
                terminated(tag(stringify!($kw)), not(ident))(input)
            }
        )*
    };
}

keywords! {
    kw_as : as
    kw_break : break
    kw_const : const
    kw_continue : continue
    kw_crate : crate
    kw_else : else
    kw_enum : enum
    kw_extern : extern
    kw_false : false
    kw_fn : fn
    kw_for : for
    kw_if : if
    kw_impl : impl
    kw_in : in
    kw_let : let
    kw_loop : loop
    kw_match : match
    kw_mod : mod
    kw_move : move
    kw_mut : mut
    kw_pub : pub
    kw_ref : ref
    kw_return : return
    kw_selfvalue : self
    kw_selftype : Self
    kw_static : static
    kw_struct : struct
    kw_super : super
    kw_trait : trait
    kw_true : true
    kw_type : type
    kw_unsafe : unsafe
    kw_use : use
    kw_where : where
    kw_while : while
    kw_abstract : abstract
    kw_become : become
    kw_box : box
    kw_do : do
    kw_final : final
    kw_macro : macro
    kw_override : override
    kw_priv : priv
    kw_typeof : typeof
    kw_unsized : unsized
    kw_virtual : virtual
    kw_yield : yield
    kw_async : async
    kw_await : await
    kw_try : try
    kw_union : union
    kw_dyn : dyn
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::convert_error;

    macro_rules! test_parser {
        ($parser:ident($input:expr) -> ok) => {
            {
                let input = $input;
                match $parser(input) {
                    Ok((inp, _)) => if !inp.is_empty() {
                        panic!("Parser did not consume entire input")
                    },
                    Err(nom::Err::Error(err)) | Err(nom::Err::Failure(err)) => {
                        panic!("Parse error: {}", convert_error(input, err));
                    },
                    Err(nom::Err::Incomplete(_)) => unreachable!("Parser returned incomplete"),
                }
            }
        };
        ($parser:ident($input:expr) -> err) => {
            {
                let input = $input;
                match $parser(input) {
                    Ok((inp, val)) => panic!("Parser should not have succeeded.\n\tRemaining Input: `{}`\n\tValue: `{:?}`", inp, val),
                    Err(_) => {},
                }
            }
        };
    }

    #[test]
    fn function_parser() {
        // Valid cases with different whitespace
        let func = function(FuncType::Function);
        test_parser!(func("fn foo() {}") -> ok);
        test_parser!(func("fn foo(){}") -> ok);
        test_parser!(func("fn foo (){}") -> ok);
        test_parser!(func("fn
            foo () {}") -> ok);

        // No space between `fn` and `foo`
        test_parser!(func("fnfoo(){}") -> err);
    }
}
