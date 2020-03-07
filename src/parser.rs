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

/// An unrecoverable parse error
///
/// Many errors are recoverable, so we can still return `Ok` for those, this is when we want to
/// return `Err` from a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Unrecoverable;

struct ModuleParser<'a> {
    lexer: Lexer<'a>,
    diag: &'a Diagnostics,
}

impl<'a> ModuleParser<'a> {
    fn module(&mut self) -> Module<'a> {
        let mut decls = Vec::new();
        while self.lexer.peek().kind != Eof {
            if let Ok(decl) = self.decl() {
                decls.push(decl);
            }
        }

        Module {decls}
    }

    fn decl(&mut self) -> Result<Decl<'a>, Unrecoverable> {
        use Keyword::*;
        Ok(match self.lexer.peek().kind {
            Keyword(Use) => Decl::Import(self.use_decl()?),
            Keyword(Struct) => Decl::Struct(self.struct_decl()?),
            Keyword(Impl) => Decl::Impl(self.impl_decl()),
            Keyword(Fn) => Decl::Function(self.func_decl()),
            _ => {
                let token = self.lexer.next();
                self.unexpected_token(token, &[Keyword(Use), Keyword(Struct),
                    Keyword(Impl), Keyword(Fn)]);
                return Err(Unrecoverable);
            },
        })
    }

    fn use_decl(&mut self) -> Result<ImportPath<'a>, Unrecoverable> {
        // The `use` token
        self.lexer.next();

        let path = self.import_path()?;

        self.match_next(Semicolon)?;

        Ok(path)
    }

    fn import_path(&mut self) -> Result<ImportPath<'a>, Unrecoverable> {
        let mut components = Vec::new();
        let selection = loop {
            let token = self.lexer.next();
            let component = match &token.kind {
                Star => break Ok(ImportSelection::All),
                OpenDelim(Delim::Brace) => {
                    let names = self.import_names_list()?;
                    break Ok(ImportSelection::Names(names));
                },
                _ => self.path_component(token)?,
            };
            components.push(component);

            let token = self.lexer.next();
            match &token.kind {
                DoubleColon => continue,
                Semicolon => break Err(Unrecoverable),
                _ => {
                    self.unexpected_token(token, &[DoubleColon, Semicolon]);
                    return Err(Unrecoverable);
                },
            }
        };

        Ok(ImportPath {path: IdentPath {components}, selection})
    }

    fn import_names_list(&mut self) -> Result<Vec<ast::Ident<'a>>, Unrecoverable> {
        let mut names = Vec::new();

        loop {
            let token = self.lexer.next();
            match &token.kind {
                CloseDelim(Delim::Brace) => break,
                Ident => {
                    let ident = token.to_str()
                        .expect("bug: idents should be valid utf-8 by definition");
                    names.push(ident);
                },
                _ => {
                    self.unexpected_token(token, &[CloseDelim(Delim::Brace), Ident]);
                    return Err(Unrecoverable);
                },
            }
        }
        while self.lexer.peek().kind != CloseDelim(Delim::Brace) {
            let token = self.match_next(Ident)?;

            let token = self.lexer.next();
            match &token.kind {
                Comma => continue,
                CloseDelim(Delim::Brace) => break,
                _ => {
                    self.unexpected_token(token, &[DoubleColon, Semicolon]);
                    return Err(Unrecoverable);
                },
            }
        }

        Ok(names)
    }

    fn struct_decl(&mut self) -> Result<Struct<'a>, Unrecoverable> {
        // The `struct` token
        self.lexer.next();

        let name_token = self.match_next(Ident)?;
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

    fn ident_path(&mut self) -> Result<IdentPath<'a>, Unrecoverable> {
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
                    return Err(Unrecoverable);
                },
            }
        }

        Ok(IdentPath {components})
    }

    fn path_component(&mut self, token: Token<'a>) -> Result<PathComponent<'a>, Unrecoverable> {
        use Keyword::*;

        Ok(match &token.kind {
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
                return Err(Unrecoverable);
            },
        })
    }

    fn match_next(&mut self, kind: TokenKind) -> Result<Token<'a>, Unrecoverable> {
        let token = self.lexer.next();
        if token.kind != kind {
            self.unexpected_token(token, &[kind]);
            Err(Unrecoverable)
        } else {
            Ok(token)
        }
    }

    fn unexpected_token(&self, token: Token<'a>, expected: &[TokenKind]) {
        if token.kind == Error {
            // No need to produce further errors on an error token
            return;
        }
        todo!()
    }
}
