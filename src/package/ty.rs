use super::DefSpan;

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Def(DefSpan),
}
