use crate::unique::Unique;
use crate::outptr::OutPtr;
use crate::dbool::DBool;

/// The dino unit type
#[repr(C)]
pub struct DUnit {
    // Zero-sized types are not allowed in C
    __unused: u8,
}

impl DUnit {
    pub fn new() -> Unique<Self> {
        // This should never be dereferenced, so it's fine to return a null pointer
        Unique::empty()
    }
}

/// Creates a new DUnit
#[no_mangle]
pub extern fn __dino__DUnit_from_unit_literal(mut out: OutPtr<DUnit>) {
    out.write(DUnit::new());
}

#[no_mangle]
pub extern fn unit__eq(_x: &DUnit, _y: &DUnit, mut out: OutPtr<DBool>) {
    // Unit is always equal to itself
    out.write(DBool::new(true));
}

#[no_mangle]
pub extern fn print_unit(_x: &DUnit, mut out: OutPtr<DUnit>) {
    unsafe {
        super::printf(b"()\n\0" as *const u8);
    }

    out.write(DUnit::new());
}
