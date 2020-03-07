mod scanner;
mod lexer;

use crate::ast;
use crate::ast::*;
use crate::diagnostics::Diagnostics;

use scanner::Scanner;
use lexer::{Lexer, Token, TokenKind, Lit, Delim, Keyword};

use TokenKind::*;

pub fn parse_module<'a>(source: &'a str, diag: &'a Diagnostics) -> Module<'a> {
    let scanner = Scanner::new(source.as_bytes());
    let lexer = Lexer::new(scanner, diag);
    let mut parser = ModuleParser {
        lexer,
        diag,
    };

    parser.module()
}

struct ModuleParser<'a> {
    lexer: Lexer<'a>,
    diag: &'a Diagnostics,
}

impl<'a> ModuleParser<'a> {
    fn module(&mut self) -> Module<'a> {
        let mut decls = Vec::new();
        while self.lexer.peek().kind != Eof {
            if let Some(decl) = self.decl() {
                decls.push(decl);
            }
        }

        Module {decls}
    }

    fn decl(&mut self) -> Option<Decl<'a>> {
        use Keyword::*;
        Some(match self.lexer.peek().kind {
            Keyword(Use) => Decl::Import(self.use_decl()?),
            Keyword(Struct) => Decl::Struct(self.struct_decl()),
            Keyword(Impl) => Decl::Impl(self.impl_decl()),
            Keyword(Fn) => Decl::Function(self.func_decl()),
            _ => {
                let token = self.lexer.next();
                self.unexpected_token(token, &[Keyword(Use), Keyword(Struct),
                    Keyword(Impl), Keyword(Fn)]);
                return None;
            },
        })
    }

    fn use_decl(&mut self) -> Option<ImportPath<'a>> {
        // The `use` token
        self.lexer.next();

        self.import_path()
    }

    fn struct_decl(&mut self) -> Struct<'a> {
        // The `struct` token
        self.lexer.next();
        todo!()
    }

    fn impl_decl(&mut self) -> Impl<'a> {
        // The `impl` token
        self.lexer.next();
        todo!()
    }

    fn func_decl(&mut self) -> Function<'a> {
        // The `fn` token
        self.lexer.next();
        todo!()
    }

    fn import_path(&mut self) -> Option<ImportPath<'a>> {
        let mut components = Vec::new();
        let selection = loop {
            let token = self.lexer.next();
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

            let token = self.lexer.next();
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

        while self.lexer.peek().kind != CloseDelim(Delim::Brace) {
            let token = self.match_next(Ident)?;
            let ident = token.to_str()
                .expect("bug: idents should be valid utf-8 by definition");
            names.push(ident)
        }

        Some(names)
    }

    fn ident_path(&mut self) -> Option<IdentPath<'a>> {
        let mut components = Vec::new();
        loop {
            let token = self.lexer.next();
            let component = self.path_component(token)?;
            components.push(component);

            let token = self.lexer.next();
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

    fn path_component(&mut self, token: Token<'a>) -> Option<PathComponent<'a>> {
        use Keyword::*;

        Some(match &token.kind {
            Ident => {
                let ident = token.to_str()
                    .expect("bug: idents should be valid utf-8 by definition");
                PathComponent::Ident(ident)
            },

            Keyword(Package) => PathComponent::Package,
            Keyword(SelfType) => PathComponent::SelfType,
            Keyword(SelfValue) => PathComponent::SelfValue,
            Keyword(Super) => PathComponent::Super,

            _ => {
                let token = self.lexer.next();
                self.unexpected_token(token, &[Ident, Keyword(Package), Keyword(SelfType),
                    Keyword(SelfValue), Keyword(Super)]);
                return None;
            },
        })
    }

    fn match_next(&mut self, kind: TokenKind) -> Option<Token<'a>> {
        let token = self.lexer.next();
        if token.kind != kind {
            self.unexpected_token(token, &[kind]);
            None
        } else {
            Some(token)
        }
    }

    fn unexpected_token(&self, token: Token<'a>, expected: &[TokenKind]) {
        todo!()
    }
}
