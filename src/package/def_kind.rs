use std::sync::Arc;

use super::DefId;

#[derive(Debug)]
pub enum DefKind {
    Struct(DefStruct),
    Function(DefFuncSig),
}

#[derive(Debug)]
pub struct DefStruct {
    pub fields: Vec<DefStructField>,
}

#[derive(Debug)]
pub struct DefStructField {
    pub name: Arc<str>,
    pub ty: DefTy,
}

#[derive(Debug)]
pub struct DefFuncSig {
    pub self_param: Option<DefId>,
    pub params: Vec<DefFuncParam>,
    pub return_type: DefTy,
}

#[derive(Debug)]
pub struct DefFuncParam {
    pub name: Arc<str>,
    pub ty: DefTy,
}

#[derive(Debug)]
pub enum DefTy {
    Def(DefId),
}
