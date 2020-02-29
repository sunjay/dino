//! Pointer and allocation utilities

extern {
    // See: https://www.hboehm.info/gc/gcinterface.html

    /// Allocates and clears nbytes of storage. Requires (amortized) time proportional to nbytes.
    /// The resulting object will be automatically deallocated when unreferenced. References from
    /// objects allocated with the system malloc are usually not considered by the collector.
    #[link_name = "GC_malloc"]
    pub(crate) fn gc_malloc(nbytes: libc::size_t) -> *mut libc::c_void;

    /// Allocates nbytes of storage. Requires (amortized) time proportional to nbytes. The resulting
    /// object will be automatically deallocated when unreferenced. The client promises that the
    /// resulting object will never contain any pointers. The memory is not cleared. This is the
    /// preferred way to allocate strings, floating point arrays, bitmaps, etc.
    #[link_name = "GC_malloc_atomic"]
    pub(crate) fn gc_malloc_no_ptr(nbytes: libc::size_t) -> *mut libc::c_void;

    /// Identical to GC_MALLOC, except that the resulting object is not automatically deallocated.
    /// Unlike the system-provided malloc, the collector does scan the object for pointers to
    /// garbage-collectable memory, even if the block itself does not appear to be reachable.
    /// (Objects allocated in this way are effectively treated as roots by the collector.)
    #[link_name = "GC_malloc_uncollectable"]
    pub(crate) fn gc_malloc_uncollectable(nbytes: libc::size_t) -> *mut libc::c_void;
}

/// Allocates the given size of memory and returns a pointer to that memory
///
/// Note that this value can contain other pointers
#[no_mangle]
pub unsafe extern fn __dino__alloc(size: libc::size_t) -> *mut libc::c_void {
    sanitize_ptr(gc_malloc(size))
}

/// Allocates the given size of memory and returns a pointer to that memory
///
/// Note that this value may NOT contain any other pointers
#[no_mangle]
pub unsafe extern fn __dino__alloc_value(size: libc::size_t) -> *mut libc::c_void {
    sanitize_ptr(gc_malloc_no_ptr(size))
}

/// Allocates the given size of memory and returns a pointer to that memory
///
/// The value will never be freed for the duration of the program.
#[no_mangle]
pub unsafe extern fn __dino__alloc_static(size: libc::size_t) -> *mut libc::c_void {
    sanitize_ptr(gc_malloc_uncollectable(size))
}

/// Checks if the pointer is NULL and exits if necessary.
unsafe fn sanitize_ptr(value_ptr: *mut libc::c_void) -> *mut libc::c_void {
    if value_ptr.is_null() {
        //TODO: Error handling: ran out of memory
        //TODO: Print a message
        libc::exit(1);
    }

    value_ptr
}
