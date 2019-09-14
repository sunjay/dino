use lalrpop_util::lalrpop_mod;

// Load the generated parsers into a private module
lalrpop_mod!(grammar, "/parser/grammar.rs");
