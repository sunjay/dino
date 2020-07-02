use std::path::Path;
use std::sync::Arc;

use crate::ast;
use crate::source_files::FileHandle;

#[salsa::query_group(ParserStorage)]
pub trait ParserDb: salsa::Database {
    #[salsa::input]
    fn source_file(&self, path: Arc<Path>) -> FileHandle;

    fn parse(&self, file: FileHandle) -> Arc<ast::Module>;
}

fn parse<Db: ParserDb>(db: &Db, file: FileHandle) -> Arc<ast::Module> {
    todo!()
}
