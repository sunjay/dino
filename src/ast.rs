#[derive(Debug)]
pub struct Program {
    decls: Vec<Decl>,
}

#[derive(Debug)]
pub enum Decl {
    Function(Function),
}

#[derive(Debug)]
pub struct Function {
    name: String,
}
