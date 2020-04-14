use std::sync::Arc;

//use crate::package::{DefId, Packages, DefKind};
type DefId = usize; //TODO: Delete

macro_rules! primitives {
    (
        $(#[$attr:meta])*
        $vis:vis struct $struct_name:ident {
            $($prim:ident),* $(,)?
        }
    ) => {
        $(#[$attr])*
        $vis struct $struct_name {
            $(
                $prim: DefId
            ),*
        }

        impl $struct_name {
//            pub fn new(packages: &mut Packages) -> Self {
//                // The primitives are inserted as an unnamed package that is implicitly available
//                // in every scope. Since it is unnamed, it cannot be looked up as a regular package
//                let pkg = packages.insert("".into());
//                let mut store = pkg.store_mut();
//                let mut scopes = pkg.scopes_mut();
//                let root = scopes.root_mut();
//
//                $(
//                    let name: Arc<str> = stringify!($prim).into();
//                    let $prim = store.push(name.clone(), DefKind::Struct);
//                    root.insert_type(name, $prim).expect("bug: duplicate primitive");
//                )*
//
//                Self {
//                    $($prim),*
//                }
//            }

            $(
                pub fn $prim(&self) -> DefId {
                    self.$prim
                }
            )*
        }
    };
}

primitives! {
    /// The core primitives of the compiler
    #[derive(Debug)]
    pub struct Primitives {
        unit,
        bool,
        uint,
        int,
        real,
        complex,
        bstr,
    }
}
