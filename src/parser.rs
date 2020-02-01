//! A parser designed for incremental editing and producing good error messages.
//!
//! There are two main aspects to this:
//! 1. Spans - represents a position within a file
//! 2. Edits - ability to re-build the AST completely based on changes to the source files
//!
//! This implementation avoids complicated book keeping by completely re-building the AST
//! whenever an edit occurs. We can do this efficiently by re-using the parts we know haven't
//! changed.
//!
//! The parser is also designed to understand its own grammar in a way that makes generating
//! useful parse errors straightforward. If the parser knows which alternatives it was expecting,
//! it can produce an error listing what it found and what it wanted to find.
//!
//! Error recovery is important for good editor support. The parser will attempt to intelligently
//! guess how to fix some parse errors so it can continue parsing in as many cases as possible.
//! The errors will still be reported. This is to allow the compiler to produce as many errors at
//! once as possible.

mod lexer;
mod codemap;
