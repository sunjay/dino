mod scanner;
mod lexer;
mod token_stream;

pub use scanner::*;
pub use lexer::*;
pub use token_stream::*;

use crate::ast;
use crate::ast::*;
use crate::diagnostics::Diagnostics;

use TokenKind::*;

pub fn parse_module<'a>(source: &'a str, diag: &'a Diagnostics) -> Module<'a> {
    let scanner = Scanner::new(source.as_bytes());
    let lexer = Lexer::new(scanner, diag);
    let tstream = lexer.into();
    let mut parser = ModuleParser {
        tstream,
        diag,
    };

    parser.module()
}

struct ModuleParser<'a> {
    tstream: TokenStream,
    diag: &'a Diagnostics,
}

impl<'a> ModuleParser<'a> {
    fn module(&mut self) -> Module<'a> {
        let mut decls = Vec::new();
        while self.tstream.peek().kind != Eof {
            if let Some(decl) = self.decl() {
                decls.push(decl);
            }
        }

        Module {decls}
    }

    fn decl(&mut self) -> Option<Decl<'a>> {
        use lexer::Keyword::*;
        Some(match self.tstream.peek().kind {
            Keyword(Use) => Decl::Import(self.use_decl()?),
            Keyword(Struct) => Decl::Struct(self.struct_decl()),
            Keyword(Impl) => Decl::Impl(self.impl_decl()),
            Keyword(Fn) => Decl::Function(self.func_decl()),
            _ => {
                let token = self.tstream.next();
                self.unexpected_token(token, &[Keyword(Use), Keyword(Struct),
                    Keyword(Impl), Keyword(Fn)]);
                return None;
            },
        })
    }

    /// use_decl := "use" import_path ";"
    /// import_path := (path_component "::")* import_path_selection
    /// import_path_selection := path_component | "*" | "{" import_names_list "}"
    /// import_names_list := (import_name ",")* import_name?
    /// import_name := (ident | self_value) ("as" ident)?
    fn use_decl(&mut self) -> Option<ImportPath<'a>> {
        // The `use` token
        self.tstream.next();

        self.import_path()
    }

    fn struct_decl(&mut self) -> Struct<'a> {
        // The `struct` token
        self.tstream.next();
        todo!()
    }

    fn impl_decl(&mut self) -> Impl<'a> {
        // The `impl` token
        self.tstream.next();
        todo!()
    }

    fn func_decl(&mut self) -> Function<'a> {
        // The `fn` token
        self.tstream.next();
        todo!()
    }

    fn import_path(&mut self) -> Option<ImportPath<'a>> {
        let mut components = Vec::new();
        let selection = loop {
            let token = self.tstream.next();
            let component = match &token.kind {
                Star => break Some(ImportSelection::All),
                OpenDelim(Delim::Brace) => {
                    let names = self.import_names_list()?;
                    self.match_next(CloseDelim(Delim::Brace));
                    break Some(ImportSelection::Names(names));
                },
                _ => self.path_component(token)?,
            };
            components.push(component);

            let token = self.tstream.next();
            match &token.kind {
                DoubleColon => continue,
                Semicolon => break None,
                _ => {
                    self.unexpected_token(token, &[DoubleColon, Semicolon]);
                    return None;
                },
            }
        };

        Some(ImportPath {path: IdentPath {components}, selection})
    }

    fn import_names_list(&mut self) -> Option<Vec<ast::Ident<'a>>> {
        let mut names = Vec::new();

        while self.tstream.peek().kind != CloseDelim(Delim::Brace) {
            let token = self.match_next(Ident)?;
            let ident = "TODO";
            names.push(ident)
        }

        Some(names)
    }

    fn ident_path(&mut self) -> Option<IdentPath<'a>> {
        let mut components = Vec::new();
        loop {
            let token = self.tstream.next();
            let component = self.path_component(token)?;
            components.push(component);

            let token = self.tstream.next();
            match &token.kind {
                DoubleColon => continue,
                Semicolon => break,
                _ => {
                    self.unexpected_token(token, &[DoubleColon, Semicolon]);
                    return None;
                },
            }
        }

        Some(IdentPath {components})
    }

    fn path_component(&mut self, token: Token) -> Option<PathComponent<'a>> {
        use lexer::Keyword::*;

        Some(match &token.kind {
            Ident => {
                let ident = "TODO";
                PathComponent::Ident(ident)
            },

            Keyword(Package) => PathComponent::Package,
            Keyword(SelfType) => PathComponent::SelfType,
            Keyword(SelfValue) => PathComponent::SelfValue,
            Keyword(Super) => PathComponent::Super,

            _ => {
                let token = self.tstream.next();
                self.unexpected_token(token, &[Ident, Keyword(Package), Keyword(SelfType),
                    Keyword(SelfValue), Keyword(Super)]);
                return None;
            },
        })
    }

    fn match_next(&mut self, kind: TokenKind) -> Option<Token> {
        let token = self.tstream.next();
        if token.kind != kind {
            self.unexpected_token(token, &[kind]);
            None
        } else {
            Some(token)
        }
    }

    fn unexpected_token(&self, token: Token, expected: &[TokenKind]) {
        if token.kind == Error {
            // No need to produce further errors on an error token
            return;
        }
        todo!()
    }
}
