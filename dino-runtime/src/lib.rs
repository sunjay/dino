#![no_std]

// Needed to define #[panic_handler]
#[allow(unused_imports)]
use panic_halt;

// A list of C functions/items that are being imported
extern {
    // See: https://www.hboehm.info/gc/gcinterface.html

    /// Allocates and clears nbytes of storage. Requires (amortized) time proportional to nbytes.
    /// The resulting object will be automatically deallocated when unreferenced. References from
    /// objects allocated with the system malloc are usually not considered by the collector.
    #[link_name = "GC_malloc"]
    pub(crate) fn gc_malloc(nbytes: libc::size_t) -> *mut libc::c_void;
}

#[no_mangle]
pub unsafe extern fn __dino__alloc(size: libc::size_t) -> *mut libc::c_void {
    //TODO: Check if NULL, print a message and exit
    gc_malloc(size)
}
