//! Interface to functions provided by the runtime

use core::mem;

use crate::unique::Unique;

extern {
    /// Allocates nbytes of storage.
    pub(crate) fn __dino__alloc(nbytes: libc::size_t) -> *mut libc::c_void;

    /// Allocates nbytes of storage. The resulting memory may not ever contain any pointers.
    pub(crate) fn __dino__alloc_value(nbytes: libc::size_t) -> *mut libc::c_void;

    /// Allocates nbytes of storage. The resulting memory will never be deallocated.
    pub(crate) fn __dino__alloc_static(nbytes: libc::size_t) -> *mut libc::c_void;
}

/// Allocates a value of the given type, initializes it, and then returns the pointer to that value
///
/// Note that this value can contain other pointers
pub(crate) fn alloc_struct<T>(value: T) -> Unique<T> {
    unsafe {
        let value_ptr = __dino__alloc(mem::size_of::<T>()) as *mut T;
        initialize_ptr(value_ptr, value)
    }
}

/// Allocates a value of the given type, initializes it, and then returns the pointer to that value
///
/// Note that this value may NOT contain any other pointers
pub(crate) fn alloc_no_ptr<T>(value: T) -> Unique<T> {
    unsafe {
        let value_ptr = __dino__alloc_value(mem::size_of::<T>()) as *mut T;
        initialize_ptr(value_ptr, value)
    }
}

/// Allocates a value of the given type, initializes it, and then returns the pointer to that value
///
/// The value will never be collected by the garbage collector.
pub(crate) fn alloc_static<T>(value: T) -> Unique<T> {
    unsafe {
        let value_ptr = __dino__alloc_static(mem::size_of::<T>()) as *mut T;
        initialize_ptr(value_ptr, value)
    }
}

/// Attempts to initialize the pointer with the given value and then return it. This may
/// fail if `value_ptr` is NULL.
unsafe fn initialize_ptr<T>(value_ptr: *mut T, value: T) -> Unique<T> {
    if value_ptr.is_null() {
        //TODO: Error handling: ran out of memory
        libc::exit(1);
    }

    value_ptr.write(value);
    Unique::new_unchecked(value_ptr)
}
