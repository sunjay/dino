use crate::nir::DefId;

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
            pub fn new() -> Self {
                //TODO: Figure out what params this method should take and how it should work
                todo!();
            }

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
