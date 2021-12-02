{ name = "cabal-parser"
, dependencies = [ "arrays", "control", "parser-combinator", "prelude", "unsafe-coerce" ]
, packages = ../packages.dhall
, backend = "purenix"
, sources = [ "src/**/*.purs" ]
}
