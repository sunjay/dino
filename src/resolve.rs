//! Name resolution - translates High-level IR (HIR) to Nameless IR (NIR)

use std::collections::{VecDeque, HashMap};

use crate::hir;
use crate::nir::{self, DefId};
use crate::primitives::Primitives;
use crate::diagnostics::Diagnostics;
use crate::package::Packages;

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

/// Resolves the declarations for a single module
pub struct ModuleWalker<'a> {
    /// The back of the `VecDeque` is the top of the stack
    scope_stack: VecDeque<Scope>,
    /// All functions found throughout the module
    functions: Vec<nir::Function>,
    /// The definitions of all `DefId`s
    def_store: &'a nir::DefStoreSync,
    /// The packages available in the root scope
    packages: &'a Packages,
    /// Compiler built-in primitive types
    prims: &'a Primitives,
    /// For outputting diagnostics
    diag: &'a Diagnostics,
}

impl<'a> ModuleWalker<'a> {
    pub fn resolve(
        module: &hir::Module,
        def_store: &'a nir::DefStoreSync,
        packages: &'a Packages,
        prims: &'a Primitives,
        diag: &'a Diagnostics,
    ) -> nir::Module {
        let walker = Self {
            scope_stack: VecDeque::new(),
            functions: Vec::new(),
            def_store,
            packages,
            prims,
            diag,
        };

        walker.resolve_module(module)
    }

    fn resolve_module(mut self, module: &hir::Module) -> nir::Module {
        assert!(self.scope_stack.is_empty(),
            "bug: attempt to add a root module scope onto a non-empty scope stack");

        let hir::Module {decls} = module;

        self.push_scope(ScopeKind::Module);
        self.insert_decls(decls, None);
        self.pop_scope();

        assert!(self.scope_stack.is_empty(), "bug: mismatched push and pop calls");

        nir::Module {functions: self.functions}
    }

    fn insert_decls(&mut self, decls: &[hir::Decl], self_ty: Option<DefId>) {
        // Add imports/types first, so they can be available for everything else
        self.insert_imports_types(decls, self_ty);
        // Insert fields of each type now that all declared types/imports have been inserted
        self.insert_type_fields(decls);
        // Insert all function names
        self.insert_function_names(decls);

        // Add each function and the methods from each impl in the set of decls
        self.insert_funcs_impls(decls, self_ty);
    }

    fn insert_imports_types(&mut self, decls: &[hir::Decl], self_ty: Option<DefId>) {
        for decl in decls {
            match decl {
                hir::Decl::Import(import) => {
                    let hir::ImportPath {prefix, path, selection} = import;

                    //TODO: Lookup the module at `path`
                    //TODO: Allow imports from `Self`
                    use hir::ImportSelection::*;
                    match selection {
                        //TODO: Lookup each name in the `path` module
                        Names(names) => todo!(),
                        //TODO: Add `path` module DefId to `wildcard_imports`
                        All(span) => todo!(),
                    }
                },

                hir::Decl::Struct(struct_decl) => {
                    let hir::Struct {name, fields: _} = struct_decl;

                    // Create an empty table for fields for now since we can't insert them until
                    // we've walked all the types
                    let field_names = nir::DefTable::new(self.def_store.clone());
                    let struct_data = nir::DefData::new_struct(field_names);
                    // You're allowed to redefine structs that are already at higher levels of
                    // scope as long as the same level doesn't define the same name more than once
                    let insert_res = self.top_scope().types.insert_with(name.value.clone(), struct_data);
                    if let Err(_) = insert_res {
                        self.diag.emit_error(format!("the name `{}` is defined multiple times", name));
                    }
                },

                // Ignored until we get all the types that are in scope
                hir::Decl::Function(_) |
                hir::Decl::Impl(_) => {},
            }
        }
    }

    fn insert_type_fields(&mut self, decls: &[hir::Decl]) {
        for decl in decls {
            match decl {
                // Already handled
                hir::Decl::Import(_) => {},

                hir::Decl::Struct(struct_decl) => {
                    let hir::Struct {name, fields} = struct_decl;
                    // Looking in the top scope because that's exactly where we expect the decl to
                    // be given that we just inserted it
                    let struct_id = self.top_scope().types.id(&name.value)
                        .expect("bug: all structs should be inserted in the scope at this point");

                    // Insert into type info so the fields are available by ID
                    // We don't have to worry about overwriting fields because we've already
                    // checked that there is only one decl of this name in this scope
                    self.insert_struct_fields(struct_id, fields);
                },

                // Ignored in this pass
                hir::Decl::Impl(_) |
                hir::Decl::Function(_) => {},
            }
        }
    }

    fn insert_struct_fields(&mut self, self_ty: DefId, fields: &[hir::StructField]) {
        // If the struct fields are not currently empty, we must be walking a duplicate decl
        let mut store = self.def_store.lock();
        let ty_info = store.data_mut(self_ty).unwrap_type_mut();
        if !ty_info.fields.is_empty() {
            // Ignore this decl
            return;
        }

        for field in fields {
            let hir::StructField {name: name_ident, ty} = field;
            let ty = self.resolve_ty(ty, Some(self_ty));

            let mut store = self.def_store.lock();
            let ty_info = store.data_mut(self_ty).unwrap_type_mut();
            let struct_fields = ty_info.fields.struct_fields_mut();
            // Need to check that field names are unique
            match struct_fields.insert_with(name_ident.value.clone(), nir::DefData::Field {ty}) {
                Ok(_) => {},
                Err(_) => {
                    self.diag.emit_error(format!("field `{}` is already declared", name_ident));
                },
            };
        }
    }

    fn insert_function_names(&mut self, decls: &[hir::Decl]) {
        for decl in decls {
            match decl {
                // Already handled
                hir::Decl::Import(_) |
                hir::Decl::Struct(_) => {},

                hir::Decl::Function(func) => {
                    let hir::Function {name, sig: _, body: _} = func;

                    // Note that we can't actually insert the resolved signature here because then
                    // the parameters of the function wouldn't be in scope when resolving the body.
                    let func_data = nir::DefData::new_func();
                    // You're allowed to redefine functions that are already at higher levels of
                    // scope as long as the same level doesn't define the same name more than once
                    let insert_res = self.top_scope().functions.insert_with(name.value.clone(), func_data);
                    if let Err(_) = insert_res {
                        self.diag.emit_error(format!("the name `{}` is defined multiple times", name));
                    }
                },

                // Ignored in this pass
                hir::Decl::Impl(_) => {},
            }
        }
    }

    fn insert_funcs_impls(&mut self, decls: &[hir::Decl], self_ty: Option<DefId>) {
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
                    for method in methods {
                        let func = match self.resolve_function(method, Some(self_ty)) {
                            Some(func) => func,
                            None => continue,
                        };

                        // Insert into type info so the methods can be looked up by name later
                        let mut store = self.def_store.lock();
                        let ty_info = store.data_mut(self_ty).unwrap_type_mut();
                        if ty_info.methods.contains_key(&method.name.value) {
                            self.diag.emit_error(format!("duplicate definitions with name `{}`", method.name));
                            continue;

                        } else {
                            // Only insert the method if a method with that name hasn't been
                            // inserted yet. This means that the first decl of every method name
                            // will be kept in the type info.
                            ty_info.methods.insert(method.name.value.clone(), func.name);
                        }

                        self.functions.push(func);
                    }
                    self.pop_scope();
                },

                hir::Decl::Function(func) => {
                    if let Some(func) = self.resolve_function(func, self_ty) {
                        self.functions.push(func);
                    }
                },
            }
        }
    }

    fn resolve_function(&mut self, func: &hir::Function, self_ty: Option<DefId>) -> Option<nir::Function> {
        let hir::Function {name, sig, body} = func;

        // Name should be in the top scope at this point
        let name = self.top_scope().functions.id(&name.value)
            .expect("bug: all functions should be inserted in the scope at this point");

        // Push a new scope for this function so that after we're done with it the parameters are
        // not in scope anymore
        self.push_scope(ScopeKind::Function);

        // Resolve function parameters before the body based on the current scope up until now
        let sig = self.resolve_sig(sig, self_ty);
        let body = self.resolve_block(body, self_ty);

        self.pop_scope();

        let mut store = self.def_store.lock();
        match store.data_mut(name) {
            nir::DefData::Function(dsig@None) => {
                *dsig = Some(sig.clone());
            },
            nir::DefData::Function(Some(_)) => {
                // Signature was already initialized, this must be from a duplicate function decl
                // that we allowed through in order to keep name resolution going. This is where
                // we should ignore it.
                return None;
            },
            _ => unreachable!("bug: expected a function"),
        }

        Some(nir::Function {name, sig, body})
    }

    fn resolve_sig(&mut self, sig: &hir::FuncSig, self_ty: Option<DefId>) -> nir::FuncSig {
        let hir::FuncSig {self_param, params, return_type} = sig;

        // Make sure there are no duplicate parameters
        let mut duplicate_count = 0;
        let params = params.iter().map(|param| {
            let hir::FuncParam {name, ty} = param;

            let name = match self.top_scope().variables.insert_with(name.value.clone(), nir::DefData::FuncParam) {
                Ok(id) => id,
                Err(_) => {
                    self.diag.emit_error(format!("identifier `{}` is bound more than once in this parameter list", name));

                    // Generate a fresh name so there is at least the right number of params
                    duplicate_count += 1;
                    let fresh_name = format!("{}$p{}", name, duplicate_count).into();
                    self.top_scope().variables.insert_with(fresh_name, nir::DefData::Error)
                        .expect("bug: fresh variables should not collide")
                },
            };

            let ty = self.resolve_ty(ty, self_ty);

            nir::FuncParam {name, ty}
        }).collect();

        let return_type = return_type.as_ref()
            .map(|return_type| self.resolve_ty(return_type, self_ty))
            .unwrap_or_else(|| self.prims.unit());

        nir::FuncSig {params, return_type}
    }

    fn resolve_block(&mut self, block: &hir::Block, self_ty: Option<DefId>) -> nir::Block {
        let hir::Block {decls, stmts, ret, span} = block;

        self.push_scope(ScopeKind::Block);

        self.insert_decls(decls, self_ty);
        let stmts = stmts.iter()
            .map(|stmt| self.resolve_stmt(stmt, self_ty))
            .collect();
        let ret = ret.as_ref().map(|ret| self.resolve_expr(ret, self_ty));

        self.pop_scope();

        nir::Block {stmts, ret}
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
        let name = self.top_scope().variables.insert_overwrite_with(name.value.clone(), nir::DefData::Variable);
        let ty = ty.as_ref().map(|ty| self.resolve_ty(ty, self_ty));
        let expr = self.resolve_expr(expr, self_ty);

        nir::VarDecl {name, ty, expr}
    }

    fn resolve_expr(&mut self, expr: &hir::Expr, self_ty: Option<DefId>) -> nir::Expr {
        use hir::Expr::*;
        match expr {
            Assign(assign) => nir::Expr::Assign(Box::new(self.resolve_assign(assign, self_ty))),
            BoolOr(bool_or) => todo!(),
            BoolAnd(bool_and) => todo!(),
            MethodCall(call) => nir::Expr::MethodCall(Box::new(self.resolve_method_call(call, self_ty))),
            FieldAccess(access) => nir::Expr::FieldAccess(Box::new(self.resolve_field_access(access, self_ty))),
            Cond(cond) => nir::Expr::Cond(Box::new(self.resolve_cond(cond, self_ty))),
            Call(call) => nir::Expr::Call(Box::new(self.resolve_func_call(call, self_ty))),
            Return(ret) => nir::Expr::Return(Box::new(self.resolve_return(ret, self_ty))),
            Break(span) => nir::Expr::Break,
            Continue(span) => nir::Expr::Continue,
            StructLiteral(struct_lit) => nir::Expr::StructLiteral(self.resolve_struct_lit(struct_lit, self_ty)),
            BStrLiteral(lit, span) => nir::Expr::BStrLiteral(lit.clone()),
            IntegerLiteral(int_lit) => nir::Expr::IntegerLiteral(self.resolve_int_lit(int_lit)),
            &RealLiteral(value, span) => nir::Expr::RealLiteral(value),
            &ComplexLiteral(value, span) => nir::Expr::ComplexLiteral(value),
            &BoolLiteral(value, span) => nir::Expr::BoolLiteral(value),
            UnitLiteral(span) => nir::Expr::UnitLiteral,
            SelfValue(span) => nir::Expr::SelfValue,
            Path(path) => nir::Expr::Var(self.resolve_path(path)),
        }
    }

    fn resolve_assign(&mut self, assign: &hir::Assign, self_ty: Option<DefId>) -> nir::Assign {
        let hir::Assign {lvalue, expr} = assign;

        let lvalue = match lvalue {
            hir::LValue::FieldAccess(access) => {
                nir::LValue::FieldAccess(self.resolve_field_access(access, self_ty))
            },

            hir::LValue::Path(path) => nir::LValue::Path(self.resolve_path(path)),
        };
        let expr = self.resolve_expr(expr, self_ty);

        nir::Assign {lvalue, expr}
    }

    fn resolve_method_call(&mut self, call: &hir::MethodCall, self_ty: Option<DefId>) -> nir::MethodCall {
        let hir::MethodCall {lhs, method_name, args, span} = call;

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
        let hir::Cond {conds, else_body, span} = cond;

        let conds = conds.iter().map(|(if_cond, if_body)| {
            let if_cond = self.resolve_expr(if_cond, self_ty);
            let if_body = self.resolve_block(if_body, self_ty);
            (if_cond, if_body)
        }).collect();
        let else_body = else_body.as_ref().map(|body| self.resolve_block(body, self_ty));

        nir::Cond {conds, else_body}
    }

    fn resolve_func_call(&mut self, call: &hir::FuncCall, self_ty: Option<DefId>) -> nir::FuncCall {
        let hir::FuncCall {value, args, span} = call;

        let value = self.resolve_expr(value, self_ty);
        let args = args.into_iter().map(|expr| self.resolve_expr(expr, self_ty)).collect();

        nir::FuncCall {value, args}
    }

    fn resolve_return(&mut self, ret: &hir::Return, self_ty: Option<DefId>) -> nir::Return {
        todo!()
    }

    fn resolve_struct_lit(&mut self, struct_lit: &hir::StructLiteral, self_ty: Option<DefId>) -> nir::StructLiteral {
        let hir::StructLiteral {name, field_values, span} = struct_lit;

        let mut fields = HashMap::new();
        let struct_name = self.resolve_named_ty(name, self_ty);
        for field_value in field_values {
            let hir::StructFieldValue {name: field_name_ident, value} = field_value;

            let field_name = match self.resolve_field_name(struct_name, field_name_ident) {
                Some(name) => name,
                None => continue,
            };

            // Make sure none of the names are specified more than once
            if fields.contains_key(&field_name) {
                self.diag.emit_error(format!("field `{}` specified more than once", field_name_ident));
                continue;
            }

            let value = self.resolve_expr(value, self_ty);
            fields.insert(field_name, value);
        }

        // All the fields in `fields` should be valid and unique, so make sure we have the right
        // amount of fields for this type
        let store = self.def_store.lock();
        if let nir::DefData::Type(ty_info) = store.data(struct_name) {
            let expected_fields = ty_info.fields.struct_fields().len();
            if fields.len() != expected_fields {
                let name_ident = store.symbol(struct_name);
                self.diag.emit_error(format!("missing fields in initializer for `{}` (expected: {})", name_ident, expected_fields))
            }
        }

        nir::StructLiteral {name: struct_name, field_values: fields}
    }

    fn resolve_int_lit(&mut self, int_lit: &hir::IntegerLiteral) -> nir::IntegerLiteral {
        let &hir::IntegerLiteral {value, suffix, span} = int_lit;

        let type_hint = suffix.map(|suffix| todo!());

        nir::IntegerLiteral {value, type_hint}
    }

    fn resolve_field_name(&mut self, self_ty: DefId, field_name: &hir::Ident) -> Option<DefId> {
        let store = self.def_store.lock();
        let ty_info = match store.data(self_ty) {
            nir::DefData::Type(ty_info) => ty_info,
            nir::DefData::Error => return None,
            _ => unreachable!("bug: expected def to be either type or error"),
        };
        let field = ty_info.fields.struct_fields().id(&field_name.value);

        field
    }

    /// Resolves a path. Returns something even if the path wasn't found so that name
    /// resolution may continue.
    fn resolve_path(&mut self, path: &hir::Path) -> DefId {
        match self.lookup_path(path) {
            Some(func) => func,
            None => {
                // Insert a fake path so name resolution may continue
                self.top_scope().functions.insert_overwrite_with("$error".into(), nir::DefData::Error)
            },
        }
    }

    /// Resolves a variable. Returns something even if the variable wasn't found so that name
    /// resolution may continue.
    fn resolve_var(&mut self, name: &hir::Ident) -> DefId {
        match self.lookup_name(name) {
            Some(var) => var,
            None => {
                // Insert a fake variable so name resolution may continue
                self.top_scope().variables.insert_overwrite_with("$error".into(), nir::DefData::Error)
            },
        }
    }

    /// Attempts to resolve a named type and emits an error if the type could not be resolved
    fn resolve_named_ty(&mut self, ty: &hir::NamedTy, self_ty: Option<DefId>) -> DefId {
        use hir::NamedTy::*;
        let ty = match ty {
            SelfType(span) => match self_ty {
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
                self.top_scope().types.insert_overwrite_with("$error".into(), nir::DefData::Error)
            },
        }
    }

    /// Attempts to resolve a type and emits an error if the type could not be resolved
    fn resolve_ty(&mut self, ty: &hir::Ty, self_ty: Option<DefId>) -> DefId {
        use hir::Ty::*;
        let ty = match ty {
            Unit(span) => Some(self.prims.unit()),
            SelfType(span) => match self_ty {
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
                self.top_scope().types.insert_overwrite_with("$error".into(), nir::DefData::Error)
            },
        }
    }

    /// Attempts to lookup a path
    fn lookup_path(&self, path: &hir::Path) -> Option<DefId> {
        let hir::Path {prefix, components} = path;

        match prefix {
            //TODO: Lookup starting from the given prefix
            Some(prefix) => todo!(),
            None => match &components[..] {
                [name] => self.lookup_name(name),
                //TODO: Lookup the module or type and lookup the final component name in its decls
                _ => todo!(),
            }
        }
    }

    /// Attempts to lookup a name by walking up the scope stack
    fn lookup_name(&self, name: &hir::Ident) -> Option<DefId> {
        // Search variables
        for scope in self.scope_stack.iter().rev() {
            use ScopeKind::*;
            match scope.kind {
                Module => break,
                Impl => break,
                Function => match scope.variables.id(&name.value) {
                    Some(id) => return Some(id),
                    // Stop searching for variables once we reach a function boundary
                    None => break,
                },
                Block => if let Some(id) = scope.variables.id(&name.value) {
                    return Some(id);
                },
            }
        }

        // Search functions
        for scope in self.scope_stack.iter().rev() {
            use ScopeKind::*;
            match scope.kind {
                Module => break,
                Impl => if let Some(id) = scope.functions.id(&name.value) {
                    return Some(id);
                },
                Function => if let Some(id) = scope.functions.id(&name.value) {
                    return Some(id);
                },
                Block => if let Some(id) = scope.functions.id(&name.value) {
                    return Some(id);
                },
            }
        }

        // Search types
        for scope in self.scope_stack.iter().rev() {
            use ScopeKind::*;
            match scope.kind {
                Module => break,
                Impl => if let Some(id) = scope.types.id(&name.value) {
                    return Some(id);
                },
                Function => if let Some(id) = scope.types.id(&name.value) {
                    return Some(id);
                },
                Block => if let Some(id) = scope.types.id(&name.value) {
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
