//! Translates IR into generated code

use super::*;

use std::collections::HashMap;

use snafu::Snafu;
use rand::{Rng, SeedableRng, rngs::SmallRng};

use crate::ir;
use crate::resolve::{TyId, ProgramDecls, DeclMap, ExternType};

/// Represents a single level of local scope and maps the names of variables to their mangled
/// equivalent
struct NameMangler {
    rng: SmallRng,
    mangled_names: HashMap<String, String>,
}

impl NameMangler {
    pub fn new() -> Self {
        Self {
            // Want names to be deterministic across builds
            rng: SmallRng::seed_from_u64(2194920),
            mangled_names: HashMap::new(),
        }
    }

    /// Mangles the given name, overwriting any mangled name previously stored for the same name
    pub fn mangle_name(&mut self, name: &str) -> &str {
        // Append some random bytes to the end of the name to differentiate this name from any
        // other shadowed variables with the same name
        //TODO: Base the random characters off of the name so they aren't the same in every function
        let mut mangled_name = name.to_string();
        mangled_name.reserve_exact(9);
        mangled_name.push('_');
        for _ in 0..8 {
            mangled_name.push(self.rng.gen_range(b'a', b'z') as char);
        }

        self.mangled_names.insert(name.to_string(), mangled_name);
        self.get(name)
    }

    /// Returns the mangled name of the given name or panics
    pub fn get(&self, name: &str) -> &str {
        self.mangled_names.get(name).expect("bug: unresolved name was allowed to get to codegen")
    }
}

/// Code generation errors
#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("`main` function not found"))]
    NoEntryPoint,
    #[snafu(display("`main` function has wrong type"))]
    InvalidEntryPointType,
}

/// Generates an executable program from the given IR
pub fn executable(prog: &ir::Program, program_scope: &ProgramDecls) -> Result<CExecutableProgram, Error> {
    let ir::Program {top_level_module} = prog;
    let ir::Module {decls} = top_level_module;

    let ProgramDecls {top_level_decls: mod_scope, prims} = program_scope;

    let mut entry_point = None;
    let mut functions = Vec::new();

    for decl in decls {
        match decl {
            ir::Decl::Function(ir::Function {name, sig, body}) if *name == "main" => {
                // The main function must have no return type and no arguments
                if sig.return_type != prims.unit() || !sig.params.is_empty() {
                    return Err(Error::InvalidEntryPointType);
                }

                // Note that it is guaranteed that `entry_point` will only be assigned once since
                // the IR assumes that all declaration names have been checked to be unique within
                // a given module.
                debug_assert!(entry_point.is_none(), "bug: allowed multiple entry points");

                entry_point = Some(CEntryPoint {
                    body: gen_function_body(body, mod_scope)?,
                });
            },
            ir::Decl::Function(func) => functions.push(gen_function(func, mod_scope)?),
        }
    }

    let entry_point = match entry_point {
        Some(entry_point) => entry_point,
        None => return Err(Error::NoEntryPoint),
    };

    Ok(CExecutableProgram {functions, entry_point})
}

fn gen_function(
    func: &ir::Function,
    mod_scope: &DeclMap,
) -> Result<CFunction, Error> {
    unimplemented!() //TODO
}

fn gen_function_body(
    block: &ir::Block,
    mod_scope: &DeclMap,
) -> Result<CFunctionBody, Error> {
    let ir::Block {stmts} = block;

    let mut mangler = NameMangler::new();
    // Statements must be traversed in order for this name mangling mechanism to work
    let stmts = stmts.iter().map(|stmt| Ok(match stmt {
        ir::Stmt::VarDecl(var_decl) => CStmt::VarDecl(gen_var_decl(var_decl, &mut mangler, mod_scope)?),
        ir::Stmt::Expr(expr) => CStmt::Expr(gen_expr(expr, &mangler, mod_scope)?),
    })).collect::<Result<_, _>>()?;

    Ok(CFunctionBody {stmts})
}

fn gen_var_decl(
    var_decl: &ir::VarDecl,
    mangler: &mut NameMangler,
    mod_scope: &DeclMap,
) -> Result<CVarDecl, Error> {
    let ir::VarDecl {ident, ty, expr} = var_decl;

    Ok(CVarDecl {
        mangled_name: mangler.mangle_name(ident).to_string(),
        ty: lookup_type(ty, mod_scope).extern_name.clone(),
        init_expr: CInitializerExpr::Expr(gen_expr(expr, mangler, mod_scope)?)
    })
}

fn gen_expr(
    expr: &ir::Expr,
    mangler: &NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    Ok(match expr {
        ir::Expr::Call(call, _) => CExpr::Call(gen_call_expr(call, mangler, mod_scope)?),
        &ir::Expr::IntegerLiteral(value, ty) => gen_int_literal(value, ty, mangler, mod_scope)?,
        &ir::Expr::RealLiteral(value, ty) => gen_real_literal(value, ty, mangler, mod_scope)?,
        &ir::Expr::ComplexLiteral(value, ty) => gen_complex_literal(value, ty, mangler, mod_scope)?,
        &ir::Expr::BoolLiteral(value, ty) => gen_bool_literal(value, ty, mangler, mod_scope)?,
        &ir::Expr::Var(name, _) => CExpr::Var(mangler.get(name).to_string()),
    })
}

fn gen_call_expr(
    expr: &ir::CallExpr,
    mangler: &NameMangler,
    mod_scope: &DeclMap,
) -> Result<CCallExpr, Error> {
    let ir::CallExpr {func_name, args} = expr;

    assert!(
        mod_scope.is_func_extern(func_name).expect("bug: unknown function went through to codegen"),
        "Only extern functions are currently supported",
    );

    Ok(CCallExpr {
        func_name: func_name.to_string(),
        args: args.iter()
            .map(|expr| gen_expr(expr, mangler, mod_scope))
            .collect::<Result<Vec<_>, _>>()?,
    })
}

fn gen_int_literal(
    value: i64,
    ty: TyId,
    _mangler: &NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    let extern_type = lookup_type(&ty, mod_scope);
    Ok(CExpr::Call(CCallExpr {
        func_name: extern_type.int_literal_constructor
            .as_ref()
            .expect("bug: no integer literal constructor defined for type that type checked to int")
            .clone(),
        args: vec![CExpr::IntegerLiteral(value)],
    }))
}

fn gen_real_literal(
    value: f64,
    ty: TyId,
    _mangler: &NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    let extern_type = lookup_type(&ty, mod_scope);
    Ok(CExpr::Call(CCallExpr {
        func_name: extern_type.real_literal_constructor
            .as_ref()
            .expect("bug: no real literal constructor defined for type that type checked to real")
            .clone(),
        args: vec![CExpr::DoubleLiteral(value)],
    }))
}

fn gen_complex_literal(
    value: f64,
    ty: TyId,
    _mangler: &NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    let extern_type = lookup_type(&ty, mod_scope);
    Ok(CExpr::Call(CCallExpr {
        func_name: extern_type.complex_literal_constructor
            .as_ref()
            .expect("bug: no complex literal constructor defined for type that type checked to complex")
            .clone(),
        args: vec![CExpr::DoubleLiteral(value)],
    }))
}

fn gen_bool_literal(
    value: bool,
    ty: TyId,
    _mangler: &NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    let extern_type = lookup_type(&ty, mod_scope);
    Ok(CExpr::Call(CCallExpr {
        func_name: extern_type.bool_literal_constructor
            .as_ref()
            .expect("bug: no bool literal constructor defined for type that type checked to bool")
            .clone(),
        args: vec![CExpr::BoolLiteral(value)],
    }))
}

fn lookup_type<'a>(ty: &TyId, mod_scope: &'a DeclMap) -> &'a ExternType {
    mod_scope.type_extern_info(ty)
        .expect("bug: unknown type was allowed to get through to codegen")
}
