use super::DefId;

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Def(DefId),
}
