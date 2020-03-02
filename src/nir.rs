//! Nameless IR - An IR with all names replaced with `DefId`s.
//!
//! This is the result of name resolution. Names are still kept around so they can be retrieved
//! based on the `DefId` when we're generating errors.
//!
//! Note that this IR still contains method names as `Ident`s since those don't get resolved
//! until later.

use crate::def_id::DefId;
use crate::symbol_table::SymbolTable;

#[derive(Debug, PartialEq)]
pub struct Package {
    pub root: Module,
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub decls: Vec<Decl>,
}

#[derive(Debug, PartialEq)]
pub enum Decl {
//    Struct(Struct),
//    Function(Function),
}

#[derive(Debug, PartialEq)]
pub struct Struct {
    methods: Vec<Function>,
}

#[derive(Debug, PartialEq)]
pub struct Function {
}

pub type DefTable = SymbolTable<String, DefId>;
