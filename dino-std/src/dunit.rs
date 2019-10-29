use crate::unique::Unique;
use crate::dbool::DBool;

/// The dino unit type
#[repr(C)]
pub struct DUnit {
    // Zero-sized types are not allowed in C
    __unused: u8,
}

impl DUnit {
    pub fn new() -> Unique<DUnit> {
        // This should never be dereferenced, so it's fine to return a null pointer
        Unique::empty()
    }
}

/// Creates a new DUnit
#[no_mangle]
pub extern fn __dino__DUnit_from_unit_literal() -> Unique<DUnit> {
    DUnit::new()
}

#[no_mangle]
pub extern fn unit_eq(_x: &DUnit, _y: &DUnit) -> Unique<DBool> {
    // Unit is always equal to itself
    DBool::new(true)
}

#[no_mangle]
pub extern fn print_unit(_x: &DUnit) -> Unique<DUnit> {
    unsafe {
        super::printf(b"()\n\0" as *const u8);
    }

    DUnit::new()
}
