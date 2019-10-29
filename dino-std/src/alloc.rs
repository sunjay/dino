//! Pointer and allocation utilities

use core::mem;

use crate::unique::Unique;

// A list of C functions/items that are being imported
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

/// Allocates a value of the given type, initializes it, and then returns the pointer to that value
///
/// Note that this value can contain other pointers
pub(crate) unsafe fn alloc_struct<T>(value: T) -> Unique<T> {
    let value_ptr = gc_malloc(mem::size_of::<T>()) as *mut T;
    initialize_ptr(value_ptr, value)
}

/// Allocates a value of the given type, initializes it, and then returns the pointer to that value
///
/// Note that this value may NOT contain any other pointers
pub(crate) unsafe fn alloc_no_ptr<T>(value: T) -> Unique<T> {
    let value_ptr = gc_malloc_no_ptr(mem::size_of::<T>()) as *mut T;
    initialize_ptr(value_ptr, value)
}

/// Allocates a value of the given type, initializes it, and then returns the pointer to that value
///
/// The value will never be collected by the garbage collector.
pub(crate) unsafe fn alloc_static<T>(value: T) -> Unique<T> {
    let value_ptr = gc_malloc_uncollectable(mem::size_of::<T>()) as *mut T;
    initialize_ptr(value_ptr, value)
}

/// Attempts to initialize the pointer with the given value and then return Unique<T>. This may
/// fail if `value_ptr` is NULL.
unsafe fn initialize_ptr<T>(value_ptr: *mut T, value: T) -> Unique<T> {
    if value_ptr.is_null() {
        //TODO: Error handling: ran out of memory
        libc::exit(1);
    }

    value_ptr.write(value);
    Unique::new_unchecked(value_ptr)
}
