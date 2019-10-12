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
    multi::{many0, separated_list},
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
    map(function, Decl::Function)(input)
}

fn function(input: Input) -> IResult<Function> {
    map(tuple((
        kw_fn,
        wsc0,
        ident,
        wsc0,
        function_params,
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

fn function_params(input: Input) -> IResult<Vec<FuncParam>> {
    delimited(
        char('('),
        comma_separated(map(
            tuple((ident, wsc0, char(':'), wsc0, ty)),
            |(name, _, _, _, ty)| FuncParam {name, ty},
        )),
        char(')'),
    )(input)
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
    alt((
        map(cond, |cond| Expr::Cond(Box::new(cond))),
        map(func_call, Expr::Call),
        // Must be above `ident` or else this will never parse
        map(var_assign, |assign| Expr::VarAssign(Box::new(assign))),
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

fn comma_separated<'r, T: Clone + 'r, F>(parser: F) -> impl Fn(Input<'r>) -> IResult<Vec<T>>
    where F: Fn(Input<'r>) -> IResult<T> {
    separated_list(tuple((wsc0, char(','), wsc0)), parser)
}

fn var_assign(input: Input) -> IResult<VarAssign> {
    map(
        tuple((
            ident,
            wsc0,
            char('='),
            wsc0,
            expr,
            // No semi-colon because this is an expression
        )),
        |(ident, _, _, _, expr)| VarAssign {
            ident,
            expr,
        },
    )(input)
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
                not(one_of(".eEjJI")),
                // 'i' is a special case because 'i' isn't allowed, but "int" is fine
                //
                // This reads as "you're not allowed i when it isn't followed by 'nt'"
                not(pair(char('i'), not(tag("nt")))),
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
        test_parser!(function("fn foo() {}") -> ok);
        test_parser!(function("fn foo(){}") -> ok);
        test_parser!(function("fn foo (){}") -> ok);
        test_parser!(function("fn
            foo () {}") -> ok);

        // No space between `fn` and `foo`
        test_parser!(function("fnfoo(){}") -> err);
    }
}
