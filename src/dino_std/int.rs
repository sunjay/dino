use crate::primitives::Primitives;
use crate::package::{Module, FuncSig, FuncParam, insert_sig};

pub fn define_module(parent: &mut Module, prims: &Primitives) {
    let mut mod_int = Module::default();

    let int = prims.int();
    let unit = prims.unit();

    insert_sig!(mod_int; fn print_int(value: int) -> unit);

    assert!(parent.modules.insert("int".into(), mod_int).is_none(), "bug: module `int` defined twice");
}
