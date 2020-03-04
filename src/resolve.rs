//! Name resolution - translates High-level IR (HIR) to Nameless IR (NIR)

use std::collections::{VecDeque, HashMap, HashSet};

use crate::hir;
use crate::nir::{self, DefId};
use crate::primitives::Primitives;
use crate::diagnostics::Diagnostics;
use crate::package::Packages;

pub fn resolve_names(
    pkg: &hir::Package,
    def_store: &nir::DefStoreSync,
    packages: &Packages,
    prims: &Primitives,
    diag: &Diagnostics,
) -> nir::Package {
    let mut walker = HIRWalker {
        scope_stack: VecDeque::new(),
        field_names: HashMap::new(),
        def_store,
        packages,
        prims,
        diag,
    };

    walker.resolve_package(pkg)
}

/// The different kinds of scopes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ScopeKind {
    /// Module scope
    ///
    /// Type and function lookups stop here. This is to prevent modules from accessing parent
    /// modules.
    Module,
    /// Impl scope
    ///
    /// The methods in an impl are allowed to have the same names as functions in module scope
    /// because they are namespaced by the type being implemented.
    Impl,
    /// Function scope
    ///
    /// Variable lookups stop here. This is to prevent inner functions from accessing the variables
    /// of outer functions.
    Function,
    /// Block scope
    Block,
}

/// Represents a single level of scope
///
/// This is used during name resolution to figure out what is currently in scope as we walk through
/// the program. Non-wildcard imported names must be unique throughout the entire stack. Most decls
/// can be referenced out of order, so they are grouped together on the stack. Variable names must
/// be defined before they are used.
///
/// Thus, name resolution proceeds in two steps:
/// 1. At each level of scope, collect all imports and decls
/// 2. Walk through the scope, adding one variable at a time, ensuring that variables are only used
///    after their declarations
#[derive(Debug)]
struct Scope {
    /// The kind of scope this is
    pub kind: ScopeKind,
    /// All symbols from all of these modules are available in the scope
    /// The name is looked up
    pub wildcard_imports: Vec<DefId>,
    /// All the types in the current scope
    pub types: nir::DefTable,
    /// All the functions (*not* methods) in the current scope
    pub functions: nir::DefTable,
    /// All the variables in the current scope
    ///
    /// To support shadowing, variables can be arbitrarily overwritten, at which point they will be
    /// given a fresh `DefId`.
    pub variables: nir::DefTable,
}

struct HIRWalker<'a> {
    /// The back of the `VecDeque` is the top of the stack
    scope_stack: VecDeque<Scope>,
    /// A mapping from struct ID to the names of its fields
    field_names: HashMap<DefId, HashMap<String, DefId>>,
    /// The definitions of all `DefId`s
    def_store: &'a nir::DefStoreSync,
    /// The packages available in the root scope
    packages: &'a Packages,
    /// Compiler built-in primitive types
    prims: &'a Primitives,
    /// For outputting diagnostics
    diag: &'a Diagnostics,
}

impl<'a> HIRWalker<'a> {
    fn resolve_package(&mut self, pkg: &hir::Package) -> nir::Package {
        let hir::Package {root} = pkg;

        assert!(self.scope_stack.is_empty(),
            "bug: attempt to add a root module scope onto a non-empty scope stack");
        let root = self.resolve_module(root);
        assert!(self.scope_stack.is_empty(), "bug: mismatched push and pop calls");

        nir::Package {root}
    }

    fn resolve_module(&mut self, module: &hir::Module) -> nir::Module {
        let hir::Module {decls} = module;

        self.push_scope(ScopeKind::Module);
        let decls = self.resolve_decls(decls, None);
        self.pop_scope();

        nir::Module {decls}
    }

    fn resolve_decls(&mut self, decls: &[hir::Decl], self_ty: Option<DefId>) -> nir::Decls {
        // Add the decl names to the scope so they are available during resolution
        self.insert_decls(decls);

        let structs = self.resolve_structs(decls, self_ty);

        let functions = decls.iter().filter_map(|decl| match decl {
            hir::Decl::Function(func) => Some(self.resolve_function(func, self_ty)),
            _ => None,
        }).collect();

        nir::Decls {structs, functions}
    }

    /// Inserts all the decl names into the scope
    fn insert_decls(&mut self, decls: &[hir::Decl]) {
        for decl in decls {
            match decl {
                hir::Decl::Import(import) => {
                    let hir::ImportPath {path, selection} = import;

                    //TODO: Lookup the module at `path`
                    use hir::ImportSelection::*;
                    match selection {
                        //TODO: Lookup each name in the `path` module
                        Names(names) => todo!(),
                        //TODO: Add `path` module DefId to `wildcard_imports`
                        All => todo!(),
                    }
                },

                hir::Decl::Struct(struct_decl) => {
                    let &hir::Struct {name, fields: _} = struct_decl;

                    // You're allowed to redefine structs that are already at higher levels of
                    // scope as long as the same level doesn't define the same name more than once
                    let insert_res = self.top_scope().types.insert_with(name.to_string(), nir::DefData::new_struct());
                    if let Err(_) = insert_res {
                        self.diag.emit_error(format!("the name `{}` is defined multiple times", name));
                    }
                },

                hir::Decl::Function(func) => {
                    let &hir::Function {name, sig: _, body: _} = func;

                    //HACK: Using a default `FuncSig` until we can insert it properly. Is there a better way?
                    let sig = nir::FuncSig {params: Vec::new(), return_type: self.prims.unit()};
                    // You're allowed to redefine functions that are already at higher levels of
                    // scope as long as the same level doesn't define the same name more than once
                    let insert_res = self.top_scope().functions.insert_with(name.to_string(), nir::DefData::new_func(sig));
                    if let Err(_) = insert_res {
                        self.diag.emit_error(format!("the name `{}` is defined multiple times", name));
                    }
                },

                // Ignored until we get all the types that are in scope
                hir::Decl::Impl(_) => {},
            }
        }
    }

    fn resolve_structs(
        &mut self,
        decls: &[hir::Decl],
        self_ty: Option<DefId>,
    ) -> HashMap<DefId, nir::Struct> {
        let mut structs = HashMap::default();

        // Insert struct fields now that all types have been inserted
        for decl in decls {
            match decl {
                // Already handled
                hir::Decl::Import(_) => {},

                hir::Decl::Struct(struct_decl) => {
                    let hir::Struct {name, fields} = struct_decl;
                    // Looking in the top scope because that's exactly where we expect the decl to
                    // be given that we just inserted it
                    let struct_id = self.top_scope().types.id(*name)
                        .expect("bug: all structs should be inserted in the scope at this point");
                    let struct_data = structs.entry(struct_id)
                        .or_insert_with(|| nir::Struct::new(struct_id));

                    // We don't have to worry about overwriting fields because we've already
                    // checked that there is only one decl of this name in this scope
                    debug_assert!(struct_data.fields.is_empty(), "bug: should be only unique decls");
                    struct_data.fields = self.resolve_fields(struct_id, fields);
                },

                // Ignored in this pass
                hir::Decl::Impl(_) |
                hir::Decl::Function(_) => {},
            }
        }

        // Need to do impls after all the fields have been inserted in case those fields are used
        for decl in decls {
            match decl {
                // Already handled
                hir::Decl::Import(_) |
                hir::Decl::Struct(_) => {},

                hir::Decl::Impl(impl_decl) => {
                    // The type name must be in scope at this point
                    let hir::Impl {self_ty: impl_self_ty, methods} = impl_decl;

                    // The `Self` type changes to the type specified in this impl
                    // We resolve the type properly instead of just checking the top scope because
                    // impls don't have to be in the same scope as the struct itself
                    let self_ty = self.resolve_ty(impl_self_ty, self_ty);

                    // Push a scope so that method names do not clash with free function names
                    self.push_scope(ScopeKind::Impl);
                    let methods: Vec<nir::Function> = methods.iter().map(|method| {
                        let func = self.resolve_function(method, Some(self_ty));

                        // Insert into type info so the methods can be looked up by name later
                        let mut store = self.def_store.lock().expect("bug: lock poisoned");
                        let ty_info = store.data_mut(self_ty).unwrap_type_mut();
                        if ty_info.methods.contains_key(method.name) {
                            self.diag.emit_error(format!("duplicate definitions with name `{}`", method.name));
                        } else {
                            // Only insert the method if a method with that name hasn't been
                            // inserted yet. This means that the first decl of every method name
                            // will be kept in the type info.
                            ty_info.methods.insert(method.name.to_string(), func.name);
                        }

                        func
                    }).collect();
                    self.pop_scope();

                    let struct_data = structs.entry(self_ty)
                        .or_insert_with(|| nir::Struct::new(self_ty));
                    struct_data.methods.extend(methods);
                },

                // Ignored in this pass
                hir::Decl::Function(_) => {},
            }
        }

        structs
    }

    fn resolve_fields(&mut self, self_ty: DefId, fields: &[hir::StructField]) -> Vec<nir::NamedField> {
        // Need to check that field names are unique
        let mut field_names = nir::DefTable::new(self.def_store.clone());

        fields.iter().filter_map(|field| {
            let hir::StructField {name: name_ident, ty} = field;

            let name = match field_names.insert_with(name_ident.to_string(), nir::DefData::Field) {
                Ok(name) => name,
                Err(_) => {
                    self.diag.emit_error(format!("field `{}` is already declared", name_ident));
                    return None;
                },
            };
            let ty = self.resolve_ty(ty, Some(self_ty));

            // Insert into `field_names` so we can lookup fields by name
            self.field_names.entry(self_ty).or_default().insert(name_ident.to_string(), name);

            // Insert into type info so the fields are available by ID
            let mut store = self.def_store.lock().expect("bug: lock poisoned");
            let ty_info = store.data_mut(self_ty).unwrap_type_mut();
            ty_info.push_struct_field(name, ty);

            Some(nir::NamedField {name, ty})
        }).collect()
    }

    fn resolve_function(&mut self, func: &hir::Function, self_ty: Option<DefId>) -> nir::Function {
        let hir::Function {name, sig, body} = func;

        // Name should be in the top scope at this point
        let name = self.top_scope().functions.id(*name)
            .expect("bug: all functions should be inserted in the scope at this point");

        // Push a new scope for this function so that after we're done with it the parameters are
        // not in scope anymore
        self.push_scope(ScopeKind::Function);

        // Resolve function parameters before the body based on the current scope up until now
        let sig = self.resolve_sig(sig, self_ty);
        let body = self.resolve_block(body, self_ty);

        self.pop_scope();

        nir::Function {name, sig, body}
    }

    fn resolve_sig(&mut self, sig: &hir::FuncSig, self_ty: Option<DefId>) -> nir::FuncSig {
        let hir::FuncSig {params, return_type} = sig;

        // Make sure there are no duplicate parameters
        let mut duplicate_count = 0;
        let params = params.iter().map(|param| {
            let hir::FuncParam {name, ty} = param;

            let name = match self.top_scope().variables.insert_with(name.to_string(), nir::DefData::FuncParam) {
                Ok(id) => id,
                Err(_) => {
                    self.diag.emit_error(format!("identifier `{}` is bound more than once in this parameter list", name));

                    // Generate a fresh name so there is at least the right number of params
                    duplicate_count += 1;
                    let fresh_name = format!("{}$p{}", name, duplicate_count);
                    self.top_scope().variables.insert_with(fresh_name, nir::DefData::Error)
                        .expect("bug: fresh variables should not collide")
                },
            };

            let ty = self.resolve_ty(ty, self_ty);

            nir::FuncParam {name, ty}
        }).collect();

        let return_type = self.resolve_ty(return_type, self_ty);

        nir::FuncSig {params, return_type}
    }

    fn resolve_block(&mut self, block: &hir::Block, self_ty: Option<DefId>) -> nir::Block {
        let hir::Block {decls, stmts, ret} = block;

        self.push_scope(ScopeKind::Block);

        let decls = self.resolve_decls(decls, self_ty);
        let stmts = stmts.iter()
            .map(|stmt| self.resolve_stmt(stmt, self_ty))
            .collect();
        let ret = ret.as_ref().map(|ret| self.resolve_expr(ret, self_ty));

        self.pop_scope();

        nir::Block {decls, stmts, ret}
    }

    fn resolve_stmt(&mut self, stmt: &hir::Stmt, self_ty: Option<DefId>) -> nir::Stmt {
        match stmt {
            hir::Stmt::Cond(cond) => nir::Stmt::Cond(self.resolve_cond(cond, self_ty)),
            hir::Stmt::WhileLoop(wloop) => nir::Stmt::WhileLoop(self.resolve_while_loop(wloop, self_ty)),
            hir::Stmt::VarDecl(var_decl) => nir::Stmt::VarDecl(self.resolve_var_decl(var_decl, self_ty)),
            hir::Stmt::Expr(expr) => nir::Stmt::Expr(self.resolve_expr(expr, self_ty)),
        }
    }

    fn resolve_while_loop(&mut self, wloop: &hir::WhileLoop, self_ty: Option<DefId>) -> nir::WhileLoop {
        let hir::WhileLoop {cond, body} = wloop;

        let cond = self.resolve_expr(cond, self_ty);
        let body = self.resolve_block(body, self_ty);

        nir::WhileLoop {cond, body}
    }

    fn resolve_var_decl(&mut self, var_decl: &hir::VarDecl, self_ty: Option<DefId>) -> nir::VarDecl {
        let hir::VarDecl {name, ty, expr} = var_decl;

        // Using `insert_overwrite` is how we support variable shadowing
        let name = self.top_scope().variables.insert_overwrite_with(name.to_string(), nir::DefData::Variable);
        let ty = ty.as_ref().map(|ty| self.resolve_ty(ty, self_ty));
        let expr = self.resolve_expr(expr, self_ty);

        nir::VarDecl {name, ty, expr}
    }

    fn resolve_expr(&mut self, expr: &hir::Expr, self_ty: Option<DefId>) -> nir::Expr {
        use hir::Expr::*;
        match expr {
            Assign(assign) => nir::Expr::Assign(Box::new(self.resolve_assign(assign, self_ty))),
            MethodCall(call) => nir::Expr::MethodCall(Box::new(self.resolve_method_call(call, self_ty))),
            FieldAccess(access) => nir::Expr::FieldAccess(Box::new(self.resolve_field_access(access, self_ty))),
            Cond(cond) => nir::Expr::Cond(Box::new(self.resolve_cond(cond, self_ty))),
            Call(call) => nir::Expr::Call(self.resolve_func_call(call, self_ty)),
            Return(ret) => nir::Expr::Return(ret.as_ref().map(|ret| Box::new(self.resolve_expr(ret, self_ty)))),
            StructLiteral(struct_lit) => nir::Expr::StructLiteral(self.resolve_struct_lit(struct_lit, self_ty)),
            BStrLiteral(lit) => nir::Expr::BStrLiteral(lit.clone()),
            IntegerLiteral(int_lit) => nir::Expr::IntegerLiteral(self.resolve_int_lit(int_lit)),
            &RealLiteral(value) => nir::Expr::RealLiteral(value),
            &ComplexLiteral(value) => nir::Expr::ComplexLiteral(value),
            &BoolLiteral(value) => nir::Expr::BoolLiteral(value),
            UnitLiteral => nir::Expr::UnitLiteral,
            SelfLiteral => nir::Expr::SelfLiteral,
            Path(path) => nir::Expr::Var(self.resolve_func_name(path)),
            Var(var) => nir::Expr::Var(self.resolve_var(var)),
        }
    }

    fn resolve_assign(&mut self, assign: &hir::Assign, self_ty: Option<DefId>) -> nir::Assign {
        let hir::Assign {lhs, expr} = assign;

        let lhs = match lhs {
            hir::LValue::FieldAccess(access) => {
                nir::LValue::FieldAccess(self.resolve_field_access(access, self_ty))
            },

            hir::LValue::Var(var) => nir::LValue::Var(self.resolve_var(var)),
        };
        let expr = self.resolve_expr(expr, self_ty);

        nir::Assign {lhs, expr}
    }

    fn resolve_method_call(&mut self, call: &hir::MethodCall, self_ty: Option<DefId>) -> nir::MethodCall {
        let hir::MethodCall {lhs, method_name, args} = call;

        let lhs = self.resolve_expr(lhs, self_ty);
        let method_name = method_name.to_string();
        let args = args.into_iter().map(|expr| self.resolve_expr(expr, self_ty)).collect();

        nir::MethodCall {lhs, method_name, args}
    }

    fn resolve_field_access(&mut self, access: &hir::FieldAccess, self_ty: Option<DefId>) -> nir::FieldAccess {
        let hir::FieldAccess {lhs, field} = access;

        let lhs = self.resolve_expr(lhs, self_ty);
        let field = field.to_string();

        nir::FieldAccess {lhs, field}
    }

    fn resolve_cond(&mut self, cond: &hir::Cond, self_ty: Option<DefId>) -> nir::Cond {
        let hir::Cond {conds, else_body} = cond;

        let conds = conds.iter().map(|(if_cond, if_body)| {
            let if_cond = self.resolve_expr(if_cond, self_ty);
            let if_body = self.resolve_block(if_body, self_ty);
            (if_cond, if_body)
        }).collect();
        let else_body = else_body.as_ref().map(|body| self.resolve_block(body, self_ty));

        nir::Cond {conds, else_body}
    }

    fn resolve_func_call(&mut self, call: &hir::FuncCall, self_ty: Option<DefId>) -> nir::FuncCall {
        let hir::FuncCall {func_name, args} = call;

        let func_name = self.resolve_func_name(func_name);
        let args = args.into_iter().map(|expr| self.resolve_expr(expr, self_ty)).collect();

        nir::FuncCall {func_name, args}
    }

    fn resolve_struct_lit(&mut self, struct_lit: &hir::StructLiteral, self_ty: Option<DefId>) -> nir::StructLiteral {
        let hir::StructLiteral {name, field_values} = struct_lit;

        let mut fields = HashSet::new();
        let name = self.resolve_named_ty(name, self_ty);
        let field_values = field_values.iter().map(|field_value| {
            let hir::StructFieldValue {name: field_name, value} = field_value;

            let name = self.resolve_field_name(name, field_name);
            let value = self.resolve_expr(value, self_ty);

            // Make sure none of the names are specified more than once
            if fields.contains(&name) {
                self.diag.emit_error(format!("field `{}` specified more than once", field_name));
            }
            fields.insert(name);

            (name, value)
        }).collect();

        nir::StructLiteral {name, field_values}
    }

    fn resolve_int_lit(&mut self, int_lit: &hir::IntegerLiteral) -> nir::IntegerLiteral {
        let &hir::IntegerLiteral {value, type_hint} = int_lit;

        let type_hint = type_hint.as_ref().and_then(|hint| match self.lookup_name(hint) {
            Some(ty) => Some(ty),
            None => {
                self.diag.emit_error(format!("invalid suffix `{}` for integer literal", hint));
                // Just default to there being no type hint if an error occurs
                None
            },
        });

        nir::IntegerLiteral {value, type_hint}
    }

    /// Resolves a field name. Returns something even if the field wasn't found so that name
    /// resolution may continue.
    fn resolve_field_name(&mut self, self_ty: DefId, field_name: hir::Ident) -> DefId {
        let field = self.field_names.get(&self_ty).and_then(|fields| fields.get(field_name)).copied();

        match field {
            Some(field) => field,
            None => {
                //HACK: Use a fake variable so name resolution may continue
                self.top_scope().variables.insert_overwrite_with("$error".to_string(), nir::DefData::Error)
            },
        }
    }

    /// Resolves a function name. Returns something even if the function wasn't found so that name
    /// resolution may continue.
    fn resolve_func_name(&mut self, path: &hir::IdentPath) -> DefId {
        match self.lookup_path(path) {
            Some(func) => func,
            None => {
                // Insert a fake function so name resolution may continue
                self.top_scope().functions.insert_overwrite_with("$error".to_string(), nir::DefData::Error)
            },
        }
    }

    /// Resolves a variable. Returns something even if the variable wasn't found so that name
    /// resolution may continue.
    fn resolve_var(&mut self, name: hir::Ident) -> DefId {
        match self.lookup_name(name) {
            Some(var) => var,
            None => {
                // Insert a fake variable so name resolution may continue
                self.top_scope().variables.insert_overwrite_with("$error".to_string(), nir::DefData::Error)
            },
        }
    }

    /// Attempts to resolve a named type and emits an error if the type could not be resolved
    fn resolve_named_ty(&mut self, ty: &hir::NamedTy, self_ty: Option<DefId>) -> DefId {
        use hir::NamedTy::*;
        let ty = match ty {
            SelfType => match self_ty {
                Some(ty) => Some(ty),
                None => {
                    self.diag.emit_error(format!("cannot find type `Self` in this scope"));
                    None
                },
            },
            Named(ty_name) => match self.lookup_path(ty_name) {
                Some(ty) => Some(ty),
                None => {
                    self.diag.emit_error(format!("cannot find type `{}` in this scope", ty_name));
                    None
                },
            },
        };

        match ty {
            Some(ty) => ty,
            None => {
                // Insert a fake type so name resolution may continue
                self.top_scope().types.insert_overwrite_with("$error".to_string(), nir::DefData::Error)
            },
        }
    }

    /// Attempts to resolve a type and emits an error if the type could not be resolved
    fn resolve_ty(&mut self, ty: &hir::Ty, self_ty: Option<DefId>) -> DefId {
        use hir::Ty::*;
        let ty = match ty {
            Unit => Some(self.prims.unit()),
            SelfType => match self_ty {
                Some(ty) => Some(ty),
                None => {
                    self.diag.emit_error(format!("cannot find type `Self` in this scope"));
                    None
                },
            },
            Named(ty_name) => match self.lookup_path(ty_name) {
                Some(ty) => Some(ty),
                None => {
                    self.diag.emit_error(format!("cannot find type `{}` in this scope", ty_name));
                    None
                },
            },
        };

        match ty {
            Some(ty) => ty,
            None => {
                // Insert a fake type so name resolution may continue
                self.top_scope().types.insert_overwrite_with("$error".to_string(), nir::DefData::Error)
            },
        }
    }

    /// Attempts to lookup a path
    fn lookup_path(&self, path: &hir::IdentPath) -> Option<DefId> {
        let hir::IdentPath {components, root} = path;

        if *root {
            //TODO: Lookup the module (starting from the root) and lookup the name in its decls
            todo!()
        } else {
            match &components[..] {
                [name] => self.lookup_name(name),
                //TODO: Lookup the module and lookup the name in its decls
                _ => todo!(),
            }
        }
    }

    /// Attempts to lookup a name by walking up the scope stack
    fn lookup_name(&self, name: hir::Ident) -> Option<DefId> {
        // Search variables
        for scope in self.scope_stack.iter().rev() {
            use ScopeKind::*;
            match scope.kind {
                Module => break,
                Impl => break,
                Function => match scope.variables.id(name) {
                    Some(id) => return Some(id),
                    // Stop searching for variables once we reach a function boundary
                    None => break,
                },
                Block => if let Some(id) = scope.variables.id(name) {
                    return Some(id);
                },
            }
        }

        // Search functions
        for scope in self.scope_stack.iter().rev() {
            use ScopeKind::*;
            match scope.kind {
                Module => break,
                Impl => if let Some(id) = scope.functions.id(name) {
                    return Some(id);
                },
                Function => if let Some(id) = scope.functions.id(name) {
                    return Some(id);
                },
                Block => if let Some(id) = scope.functions.id(name) {
                    return Some(id);
                },
            }
        }

        // Search types
        for scope in self.scope_stack.iter().rev() {
            use ScopeKind::*;
            match scope.kind {
                Module => break,
                Impl => if let Some(id) = scope.types.id(name) {
                    return Some(id);
                },
                Function => if let Some(id) = scope.types.id(name) {
                    return Some(id);
                },
                Block => if let Some(id) = scope.types.id(name) {
                    return Some(id);
                },
            }
        }

        None
    }

    /// Returns the scope at the top of the stack (the "current" scope)
    fn top_scope(&mut self) -> &mut Scope {
        self.scope_stack.back_mut()
            .expect("bug: scope stack should not be empty")
    }

    /// Pushes a new level onto the scope stack
    fn push_scope(&mut self, kind: ScopeKind) {
        self.scope_stack.push_back(Scope {
            kind,
            wildcard_imports: Vec::new(),
            types: nir::DefTable::new(self.def_store.clone()),
            functions: nir::DefTable::new(self.def_store.clone()),
            variables: nir::DefTable::new(self.def_store.clone()),
        });
    }

    /// Removes the upper-most level of the scope stack
    fn pop_scope(&mut self) -> Scope {
        self.scope_stack.pop_back()
            .expect("bug: scope stack was empty when it shouldn't have been")
    }
}
