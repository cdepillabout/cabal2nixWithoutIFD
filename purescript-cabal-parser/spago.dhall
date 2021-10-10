{ name = "cabal-parser"
, dependencies = [ "parser-combinator", "prelude" ]
, packages = ../packages.dhall
, backend = "purenix"
, sources = [ "src/**/*.purs" ]
}
